
library(shiny)
library(httr)
library(jsonlite)

# --- UI Funkce Modulu ---
analysisHubUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("4. Rozcestník Analýz"),
    p("Vyberte typ analýzy, kterou chcete provést, nebo popište svůj cíl a nechte si poradit od AI."),
    hr(),
    
    fluidRow(
      column(6,
             h4("Manuální výběr analýzy"),

             uiOutput(ns("analysis_choice_ui"))
      ),
      column(6,
             h4("AI Nápověda"),
             textAreaInput(ns("ai_query"), "Popište, co chcete z dat zjistit:",
                           placeholder = "Např. 'Jaký je vztah mezi výškou a váhou?' nebo 'Liší se průměrná spokojenost mezi odděleními A a B?'",
                           rows = 4),
             actionButton(ns("ask_ai_btn"), "Navrhnout pomocí AI", icon("robot"), class = "btn-info"),
             # Zobrazení odpovědi AI
             uiOutput(ns("ai_response_ui"))
      )
    ),
    hr(),
    # Tlačítko pro pokračování bude aktivní, jen když je něco vybráno
    uiOutput(ns("proceed_button_ui"))
  )
}

# --- Server Funkce Modulu ---
analysisHubServer <- function(id, reactive_prepared_data, reactive_col_types) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    local_rv <- reactiveValues(
      ai_response_text = NULL,
      ai_suggested_key = NULL, 
      loading_ai = FALSE,
      current_selection = character(0) 
    )
    

    analysis_options <- reactive({
      list(
        "Porovnání skupin" = "comparison",
        "Korelace" = "correlation",
        "Regrese" = "regression",
        "Test závislosti" = "dependency"
      )
    })
    
    
    # --- Dynamické Radio Buttons UI ---
    output$analysis_choice_ui <- renderUI({
      opts <- analysis_options()
      choice_names_ui <- lapply(names(opts), function(name) {
        key <- opts[[name]]
        if (!is.null(local_rv$ai_suggested_key) && key == local_rv$ai_suggested_key) {
         
          HTML(paste0(strong(name), span(" (Doporučeno AI)", style="color: #007bff; font-size: 0.9em;")))
        } else {
          strong(name)
        }
      })
      
      radioButtons(ns("analysis_choice_input"), 
                   label = "Zvolte typ analýzy:",
                   choiceNames = choice_names_ui,
                   choiceValues = unname(unlist(opts)),
                   selected = local_rv$current_selection 
      )
    })
    
   
    observeEvent(input$analysis_choice_input, {
      local_rv$current_selection <- input$analysis_choice_input
    }, ignoreNULL = FALSE) 
    
    
    # --- AI Backend Volání ---
    observeEvent(input$ask_ai_btn, {
      req(input$ai_query, nchar(input$ai_query) > 5)
      req(reactive_prepared_data())
      
      current_data <- reactive_prepared_data()
      column_names <- names(current_data)
      req(length(column_names) > 0)
      
      local_rv$loading_ai <- TRUE
      local_rv$ai_response_text <- NULL
      local_rv$ai_suggested_key <- NULL
      
      # --- API Klíč - POUZE PRO DEMO ---
     
      api_key <- Sys.getenv("OPENROUTER_API_KEY", "NA"); if (api_key == "NA" || nchar(api_key) < 10) { cat("DEBUG COMPARISON: Missing API Key\n"); rv$ai_error <- "API klíč (OPENROUTER_API_KEY) není nastaven."; rv$is_interpreting <- FALSE; return() }
      
      api_url <- "https://openrouter.ai/api/v1/chat/completions"
      
      print("Hub Module: Volám AI API...")
      showModal(modalDialog("Kontaktuji AI asistenta...", footer=NULL))
      
      columns_text <- paste(column_names, collapse = ", ")
      full_user_prompt <- paste0(
        "Mám dataset s následujícími sloupci: ", columns_text, ".\n\n",
        "Můj dotaz je: ", input$ai_query
      )
      
    
      system_prompt <- paste0(
        prompt <- "You are a helpful assistant for data analysis. The user will describe what they want to find out from their dataset.\n\n
Your task is to classify the request into one of the following types of analysis:\n\n
        1. Porovnání skupin\n, 
        2. Korelace\n,
        3. Regrese\n,
        4. Test závislosti\n\n,
          Rules:\n
          The response will be in Czech.\n
          - Respond with **only one** of the above categories.\n
          - Include a **short explanation** (1–3 sentences) why this analysis is appropriate.\n
          - If you are not sure what type of analysis to recommend, ask the user to clarify their problem.\n\n
          Forbidden:\n
          - Do not mention statistical tests.\n
          - Do not include code or formulas.\n
          - Do not guess multiple types — pick only one or ask for clarification."
      )
     
      response <- tryCatch({
        httr::POST(
          url = api_url,
          httr::add_headers(
            "Authorization" = paste("Bearer", api_key),
            "Content-Type" = "application/json"
          ),
          body = jsonlite::toJSON(list(
            model = "deepseek/deepseek-chat-v3-0324:free",
            messages = list(
              list(role = "system", content = system_prompt),
              list(role = "user", content = full_user_prompt)
            ),
            max_tokens = 150
          ), auto_unbox = TRUE),
          encode = "json",
          timeout(60)
        )
      }, error = function(e) {
        print(paste("Hub Module: Chyba při volání API:", e$message))
        return(list(error_message = e$message, status_code = 500))
      })
      
      removeModal()
      local_rv$loading_ai <- FALSE
      
      if (!is.null(response$error_message) || httr::status_code(response) >= 300) {
        status <- if(!is.null(response$error_message)) response$status_code else httr::status_code(response)
        error_body <- if(!is.null(response$error_message)) response$error_message else httr::content(response, "text", encoding="UTF-8")
        print(paste("Hub Module: AI API vrátilo chybu", status, ":", error_body))
        local_rv$ai_response_text <- paste("⚠️ Chyba při komunikaci s AI (", status, "). Zkuste to prosím znovu později.")
        showNotification("Chyba při získávání návrhu od AI.", type="error")
      } else {
        content <- httr::content(response, "parsed")
        ai_message <- content$choices[[1]]$message$content
        
        if (is.null(ai_message)) ai_message <- ""
        print(paste("Hub Module: AI odpověď přijata:", ai_message))
        local_rv$ai_response_text <- ai_message
        
        first_line <- trimws(strsplit(ai_message, "\n")[[1]][1])
        print(paste("Hub Module: První řádek AI odpovědi:", first_line))
        
        suggested_key <- NULL
      
        if (startsWith(tolower(first_line), "porovnání skupin")) { 
          suggested_key <- "comparison"
        } else if (startsWith(tolower(first_line), "korelace")) {
          suggested_key <- "correlation"
        } else if (startsWith(tolower(first_line), "regrese")) {
          suggested_key <- "regression"
        } else if (startsWith(tolower(first_line), "test závislosti")) {
          suggested_key <- "dependency"
        } else {
          print("Hub Module: AI nedoporučila konkrétní analýzu (první řádek neodpovídá).")
        }
        
        local_rv$ai_suggested_key <- suggested_key
        
        if (!is.null(suggested_key)) {
          print(paste("Hub Module: Aktualizuji výběr na:", suggested_key))
          local_rv$current_selection <- suggested_key 
          
        } else {
          
          print("Hub Module: AI návrh nebyl jednoznačný, výběr se nemění.")
        }
      }
    })
    
    # --- UI pro Odpověď AI ---
    output$ai_response_ui <- renderUI({
      if (local_rv$loading_ai) {
        p(em("AI přemýšlí..."), icon("spinner", class="fa-spin"))
      } else if (!is.null(local_rv$ai_response_text)) {
        div(class = "mt-4 p-3 bg-light border rounded",
            strong("Odpověď AI:"),
            
            tags$pre(local_rv$ai_response_text, style = "white-space: pre-wrap; word-wrap: break-word;")
        )
      }
    })
    
    
    # --- UI pro Tlačítko Pokračovat ---
    output$proceed_button_ui <- renderUI({

      if (length(local_rv$current_selection) > 0 && nchar(local_rv$current_selection) > 0) {
        actionButton(ns("proceed_to_analysis_btn"), "Pokračovat k vybrané analýze",
                     icon("arrow-right"), class = "btn-success", style = "margin-top: 20px;")
      } else {
        p(em("Nejprve vyberte typ analýzy (manuálně nebo pomocí AI)."), style = "margin-top: 20px;")
      }
    })
    
   
    return(
      list(
        selected_analysis = reactive({ local_rv$current_selection }), 
        proceed_event = reactive({ input$proceed_to_analysis_btn }) 
      )
    )
    
  }) # Konec moduleServer
}