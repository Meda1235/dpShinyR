
inputColMissingUI <- function(id) {
  ns <- NS(id) 
  tagList(
  
    h4("1. Nahrání a základní nastavení souboru"),
    fluidRow(
      column(4,
             fileInput(ns("file1"), "Vyberte CSV nebo Excel soubor",
                       accept = c(".csv", ".xlsx", ".xls", "text/csv", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
      ),
      column(2,
             selectInput(ns("sep"), "Oddělovač (pro CSV)",
                         choices = c("Středník (;)" = ";", "Čárka (,)" = ",", "Tabulátor (\\t)" = "\t"),
                         selected = ",")
      ),
      column(2,
             selectInput(ns("dec"), "Desetinný oddělovač", choices = c("Čárka (,)" = ",", "Tečka (.)" = "."), selected = ",")
      ),
      column(2,
             checkboxInput(ns("header"), "Hlavička", TRUE)
      ),
      column(2,
             conditionalPanel(
               condition = paste0("output['", ns("is_excel"), "'] == true"),
               numericInput(ns("sheet_index"), "Číslo listu", value = 1, min = 1, step = 1)
             )
      )
    ),
    hr(),
    
    # --- Sekce 2: Analýza a náhled ---
    h4("2. Náhled dat a analýza typů sloupců"),
    actionButton(ns("analyze_columns_btn"), "Analyzovat a Převést Typy (Heuristika + AI)", icon("cogs"), class="btn-primary"),
    br(), br(),
    h5("Náhled prvních 10 řádků načtených dat:"),
    DTOutput(ns("data_table_preview")),
    br(),
    h5("Analýza typů sloupců a návrhy AI:"),
    DTOutput(ns("col_analysis_table")),

    hr(),
    
    # --- Sekce 3: Manuální úprava typů ---
    h4("3. Manuální úprava typu sloupce"),
    fluidRow(
      column(4, uiOutput(ns("col_to_update_ui"))),
      column(4, selectInput(ns("new_col_type"), "Nový typ", choices = c("Numerický" = "Numeric", "Kategorický" = "Categorical"))),
      column(4, actionButton(ns("update_type_btn"), "Aktualizovat Typ", icon("edit")))
    ),
    hr(),
    
    # --- Sekce 4: Imputace Chybějících Hodnot ---
    h4("4. Imputace chybějících hodnot"),
    p("Po analýze sloupců (krok 2) klikněte na tlačítko níže pro zobrazení možností imputace."),
    actionButton(ns("configure_impute_btn"), "Zobrazit/Aktualizovat Konfiguraci Imputace", icon("wrench")),
    br(),br(),
    # Místo, kam server vloží dynamické ovládací prvky pro imputaci (po kliknutí na tlačítko)
    uiOutput(ns("imputation_controls_ui")),
    # Místo, kam server vloží tlačítko pro aplikaci imputace (po kliknutí na tlačítko)
    uiOutput(ns("apply_imputation_button_ui")),
    

    shiny::tags$div(style = "display: none;",
                    shiny::textOutput(ns("is_csv")),
                    shiny::textOutput(ns("is_excel"))
    )
    
  )
}

# --- Server Funkce Modulu ---
inputColMissingServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    print(paste("Initializing module server for ID:", id))
    
    # Lokální reaktivní hodnoty
    local_rv <- reactiveValues(
      raw_data = NULL,
      col_types = NULL,
      data_types_updated = 0,
      ai_response = NULL,
      ai_error = NULL,
      file_ext = NULL,
      show_imputation_controls = FALSE 
    )
    

    output$is_csv <- reactive({ !is.null(local_rv$file_ext) && local_rv$file_ext == "csv" })
    output$is_excel <- reactive({ !is.null(local_rv$file_ext) && local_rv$file_ext %in% c("xlsx", "xls") })
    outputOptions(output, "is_csv", suspendWhenHidden = FALSE)
    outputOptions(output, "is_excel", suspendWhenHidden = FALSE)
    
    # --- 1. Data Loading ---
    observeEvent(input$file1, {
      print("input$file1 event triggered.") 
      inFile <- input$file1
     
      local_rv$raw_data <- NULL; local_rv$col_types <- NULL; local_rv$ai_response <- NULL; local_rv$file_ext <- NULL
      local_rv$show_imputation_controls <- FALSE
      print("Imputation controls flag reset to FALSE.") 
      
      if (is.null(inFile)) {
        print("No file selected.") 
        return(NULL)
      }
      print(paste("Loading file:", inFile$name)) 
      fpath <- inFile$datapath; ext <- tools::file_ext(fpath); local_rv$file_ext <- tolower(ext)
      showModal(modalDialog("Načítám soubor...", footer=NULL))
      
      csv_sep <- input$sep
      current_locale <- locale(decimal_mark = input$dec)
      
      df <- tryCatch({
        if (local_rv$file_ext == "csv") {
          read_delim(fpath, delim = csv_sep, col_names = input$header, col_types = cols(.default = "c"), locale = current_locale, na = c("NA", "NaN", "NULL", "None", "#N/A"), guess_max = 10000)
        } else if (local_rv$file_ext %in% c("xlsx", "xls")) {
          read_excel(fpath, sheet = input$sheet_index, col_names = input$header, col_types = "text", na = c("NA", "NaN", "NULL", "None", "#N/A"))
        } else { stop("Nepodporovaný typ souboru.") }
      }, error = function(e) {
        print(paste("Error reading file:", e$message)) 
        showNotification(paste("Chyba při čtení souboru:", e$message), type = "error");
        return(NULL)
      })
      
      removeModal()
      if (!is.null(df)) {
        print("File read successfully.")
        df <- df %>% mutate(across(everything(), ~ trimws(as.character(.)))) %>% mutate(across(everything(), ~ na_if(., "")))
        original_names <- names(df); clean_names <- make.names(original_names, unique = TRUE)
        if(!identical(original_names, clean_names)) { names(df) <- clean_names; showNotification("Některé názvy sloupců byly upraveny...", type="warning"); print("Původní názvy:"); print(original_names); print("Upravené názvy:"); print(clean_names) }
        local_rv$raw_data <- df
        print("Data stored in local_rv$raw_data. Dimensions:") 
        print(dim(local_rv$raw_data))
        showNotification("Soubor úspěšně načten. Dalším krokem je analýza a převod typů sloupců.", type = "message")
      } else {
        print("File reading failed or returned NULL.") 
      }
    })
    

    output$data_table_preview <- renderDT({
      req(local_rv$raw_data)
      print("Rendering data_table_preview...") 
      datatable(head(local_rv$raw_data, 10), options = list(scrollX = TRUE, searching = FALSE, paging = FALSE, info = FALSE), rownames = FALSE)
    })
    
 
    output$col_to_update_ui <- renderUI({
      print("Rendering col_to_update_ui...") 
      choices_vec <- if (!is.null(local_rv$col_types) && "Column" %in% names(local_rv$col_types)) { local_rv$col_types$Column }
      else if (!is.null(local_rv$raw_data)) { names(local_rv$raw_data) }
      else { NULL }
      if (is.null(choices_vec)) { return(p("Nejprve nahrajte data a analyzujte sloupce.")) }
      selectInput(ns("col_to_update"), "Sloupec k aktualizaci", choices = choices_vec)
    })
    
  
    output$col_analysis_table <- renderDT({
      req(local_rv$col_types)
      print("Rendering col_analysis_table...") 
      display_df <- local_rv$col_types

      if (!is.data.frame(display_df)) { return(datatable(data.frame(Zprava="Data analýzy sloupců jsou neplatná."), options = list(searching = FALSE, paging = FALSE, info=FALSE))) }
      req_display_cols <- c("Column", "DetectedType", "UniqueValues", "MissingValues", "MissingPercent", "OriginalRClass", "AISuggestion")
      for(col in req_display_cols) {
        if (!col %in% names(display_df)) {
          default_val <- switch(col,
                                "AISuggestion" = NA_character_,
                                "MissingValues" = 0,
                                "MissingPercent" = "0.00%",
                                NA)
          display_df[[col]] <- default_val
        }
      }
      if (!all(req_display_cols %in% names(display_df))) {
        return(datatable(data.frame(Zprava="Data analýzy sloupců jsou neúplná."), options = list(searching = FALSE, paging = FALSE, info=FALSE)))
      }
      display_df_ordered <- display_df[, req_display_cols, drop = FALSE]
      missing_col_index <- which(req_display_cols == "MissingValues") - 1
      
      datatable(display_df_ordered,
                options = list(scrollX = TRUE,
                               searching = TRUE,
                               pageLength = 10,
                               order = list(list(missing_col_index, 'desc')) 
                ),
                rownames = FALSE,
                selection = 'none',
                colnames = c('Sloupec', 'Detekovaný Typ', 'Unikátní Hodn.', 'Chybějící Počet', 'Chybějící %', 'Aktuální R Třída', 'Návrh AI') # Přeložené názvy
      )
    })
    
    # --- 2. Analyze Column Types (Heuristic + AI Button) ---
    observeEvent(input$analyze_columns_btn, {
      print("--- analyze_columns_btn OBSERVED ---") 
      req(local_rv$raw_data)
      print("analyze_columns_btn: Proceeding with analysis.") 
      local_rv$show_imputation_controls <- FALSE 
      print("Imputation controls flag reset to FALSE by analysis button.") 
      
      df_for_analysis <- local_rv$raw_data
      col_names <- names(df_for_analysis)
      
      # --- Heuristická analýza a konverze ---
      showModal(modalDialog("Spouštím heuristickou analýzu a konverzi...", footer=NULL))
      print("Starting heuristic analysis...") 
      analysis_result <- run_heuristic_analysis(df_for_analysis)
      if (is.null(analysis_result)) { removeModal(); showNotification("Heuristická analýza selhala.", type = "error"); return() }
      if (!"AISuggestion" %in% names(analysis_result)) { analysis_result$AISuggestion <- NA_character_ }
      local_rv$col_types <- analysis_result 
      print("Heuristic analysis stored in local_rv$col_types.") 
      
      print("Starting data type conversion based on analysis...") 
      data_to_convert <- local_rv$raw_data
      analysis_for_conversion <- local_rv$col_types
      current_locale <- locale(decimal_mark = input$dec)
      conversion_warnings <- c()
    
      for (i in 1:nrow(analysis_for_conversion)) {
        col_name <- analysis_for_conversion$Column[i]
        detected_type <- analysis_for_conversion$DetectedType[i]
        type_row_index <- i
        
        if (col_name %in% names(data_to_convert)) {
          original_col <- data_to_convert[[col_name]]
          if (detected_type == "Numeric" && !is.numeric(original_col)) {
            converted_col <- suppressWarnings(parse_double(as.character(original_col), na = c("", "NA", "NaN", "NULL", "None", "#N/A"), locale = current_locale))
            num_new_na <- sum(is.na(converted_col) & !is.na(original_col))
            if (num_new_na > 0) {
              warning_msg <- paste("Sloupec '", col_name, "': Převod na Numeric vytvořil ", num_new_na, " nových NA.", sep="")
              conversion_warnings <- c(conversion_warnings, warning_msg); print(paste("  WARN:", warning_msg))
            }
            data_to_convert[[col_name]] <- converted_col
            local_rv$col_types$OriginalRClass[type_row_index] <- class(converted_col)[1]
            
          } else if (detected_type == "Categorical" && !is.factor(original_col) && !is.character(original_col)) {
            converted_col <- as.factor(original_col)
            data_to_convert[[col_name]] <- converted_col
            local_rv$col_types$OriginalRClass[type_row_index] <- class(converted_col)[1] 
            
          } else if (detected_type == "Categorical" && is.numeric(original_col)) {
            converted_col <- as.character(original_col)
            data_to_convert[[col_name]] <- converted_col
            local_rv$col_types$OriginalRClass[type_row_index] <- class(converted_col)[1] 
          }
          else {
            current_class_in_data <- class(data_to_convert[[col_name]])[1]
            if(local_rv$col_types$OriginalRClass[type_row_index] != current_class_in_data){
              local_rv$col_types$OriginalRClass[type_row_index] <- current_class_in_data
            }
          }
        }
      }
      local_rv$raw_data <- data_to_convert 
      print("Data conversion finished. Updated local_rv$raw_data and local_rv$col_types.")
      if (length(conversion_warnings) > 0) { showNotification(paste("Varování:", paste(conversion_warnings, collapse=" ")), type="warning", duration = 10) }
      removeModal() 
      
      # --- AI část ---
     
      showNotification("Analýza a konverze typů dokončena. Pokračuji s AI.", type = "message", duration = 5)
      updateSelectInput(session, "col_to_update", choices = local_rv$col_types$Column)
      
      print("Starting AI analysis...") 
      df_for_ai <- local_rv$raw_data
      n_rows_for_ai <- 50; sample_df <- head(df_for_ai, n_rows_for_ai)
      csv_data_string <- tryCatch({
        sample_df_formatted <- sample_df %>%
          mutate(across(where(is.numeric), ~ format(.x, scientific = FALSE, decimal.mark = ".")))
        readr::format_csv(sample_df_formatted, na = "NA", quote = "all")
      }, error = function(e) { print(paste("Error creating CSV for AI:", e$message)); NULL })
      
      if(is.null(csv_data_string)) { showNotification("Nelze vytvořit data pro AI.", type="error"); return() }
      
      ai_user_content <- csv_data_string;
      system_prompt <- paste(
  
        "You are an expert data type classifier. Analyze the provided CSV data.",
        "For each column, determine if it is primarily NUMERIC (C) or CATEGORICAL (K).",
        "Output ONLY a single string containing the classification for each column, separated by semicolons.",
        "The output must contain exactly one letter (C or K) for each column in the input data.",
        "Example: If there are 3 columns, output could be: C;K;C",
        "Do not include column names, explanations, or any other text.",
        "Use 'C' for numeric/continuous data (even if represented as integers).",
        "Use 'K' for categorical/discrete data (strings, factors, low cardinality integers, booleans).",
        "Handle potential NA values appropriately when classifying.",
        "CSV data uses comma delimiter, dot decimal separator, and double quotes for text.",
        "The classification string MUST end directly after the last column's classification (no trailing semicolon)."
      )
      api_key <- Sys.getenv("OPENROUTER_API_KEY", "NA"); 
      if (api_key == "NA" || nchar(api_key) < 10) { 
        cat("DEBUG COMPARISON: Missing API Key\n"); 
       
        local_rv$ai_error <- "API klíč (OPENROUTER_API_KEY) není nastaven."; 
     
        showNotification(local_rv$ai_error, type = "error", duration=10) 
       
        return()
      }
      
      api_body <- list( model = "deepseek/deepseek-chat-v3-0324:free", messages = list(list(role = "system", content = system_prompt), list(role = "user", content = ai_user_content)), max_tokens = length(col_names) * 2 + 50, temperature = 0.0 )
      json_payload_to_send <- jsonlite::toJSON(api_body, auto_unbox = TRUE, pretty = FALSE)
      
      showModal(modalDialog("Posílám vzorek dat AI pro analýzu typů...", footer=NULL))
      response <- NULL; response <- tryCatch({
        POST( url = "https://openrouter.ai/api/v1/chat/completions", add_headers( Authorization = paste("Bearer", api_key), `Content-Type` = "application/json", `HTTP-Referer` = "http://localhost:1234", `X-Title` = "MojeShinyApp" ), body = json_payload_to_send, encode = "raw", timeout(120) )
      }, error = function(e) { print(paste("AI network error:", e$message)); return(e) })
      removeModal()
      
      if (inherits(response, "error")) {
        showNotification(paste("Chyba při kontaktování AI:", response$message), type = "error", duration = 10);
        local_rv$ai_response <- paste("Chyba sítě:", response$message);
      } else if (!is.null(response)) {
        status <- status_code(response); response_text <- content(response, "text", encoding = "UTF-8");
        print(paste("AI API Status:", status)) 
        parsed_response <- tryCatch(jsonlite::fromJSON(response_text), error=function(e) { print(paste("AI JSON parsing error:", e$message)); NULL })
        
        if(is.null(parsed_response)) {
          showNotification("Chyba parsování JSON odpovědi od AI.", type="error", duration=10);
          local_rv$ai_response <- paste("Chyba JSON:", substr(response_text, 1, 200), "...");
        } else {
          if (status >= 200 && status < 300) {
            ai_message_content <- NULL
     
            if (!is.null(parsed_response$choices) && is.data.frame(parsed_response$choices) && nrow(parsed_response$choices) > 0) {
              if ("message" %in% names(parsed_response$choices)) {
                message_element <- parsed_response$choices$message
                message_df <- NULL
                if(is.list(message_element) && length(message_element) > 0 && is.data.frame(message_element[[1]])){ message_df <- message_element[[1]] }
                else if(is.data.frame(message_element)) { message_df <- message_element }
                
                if (!is.null(message_df) && is.data.frame(message_df)){
                  if ("content" %in% names(message_df) && nrow(message_df) > 0) {
                    ai_message_content <- as.character(message_df$content[1])
                  }
                }
              }
            }
            
            if (!is.null(ai_message_content) && nchar(trimws(ai_message_content)) > 0) {
              ai_message_clean <- trimws(ai_message_content); local_rv$ai_response <- ai_message_clean;
              print(paste("AI Clean Response:", ai_message_clean)) 
              response_lines <- strsplit(ai_message_clean, "\n", fixed = TRUE)[[1]]
        
              first_line <- NULL
              for (line in response_lines) {
                trimmed_line <- trimws(line)
                if (nchar(trimmed_line) > 0) {
                  first_line <- trimmed_line
                  break
                }
              }
              
              if (is.null(first_line)) {
                print("AI Error: Could not find a non-empty first line in the response.")
                suggested_types_raw <- character(0) # Prázdný vektor, způsobí chybu formátu níže
              } else {
                print(paste("AI Extracted First Line:", first_line)) 
                # 3. Zpracujeme POUZE první řádek
                suggested_types_str <- gsub("\\s+", "", first_line) # Odstraníme mezery jen z prvního řádku
                suggested_types_str <- sub(";$", "", suggested_types_str) # Odstraníme případný středník na konci
                suggested_types_raw <- strsplit(suggested_types_str, ";", fixed = TRUE)[[1]]
                suggested_types_raw <- suggested_types_raw[suggested_types_raw != ""] # Odstraníme prázdné prvky
              }
              expected_cols <- length(col_names)

              
              if (length(suggested_types_raw) == expected_cols && all(toupper(suggested_types_raw) %in% c("C", "K"))) {
                ai_suggestion_types <- ifelse(toupper(suggested_types_raw) == "C", "Numeric", "Categorical")
                if (!is.null(local_rv$col_types) && nrow(local_rv$col_types) == expected_cols) {
                  if (!"AISuggestion" %in% names(local_rv$col_types)) { local_rv$col_types$AISuggestion <- NA_character_ }
                  local_rv$col_types$AISuggestion <- ai_suggestion_types
                  showNotification("Návrhy AI úspěšně přidány.", type="message");
                  print("AI suggestions added to col_types.") 
                  local_rv$data_types_updated <- local_rv$data_types_updated + 1 
                } else { print("AI Error: Mismatch between AI suggestions and analysis table rows.")}
              } else { print(paste("AI Error: Invalid format in AI response:", ai_message_clean))}
            } else { print("AI Error: Could not extract content from AI response structure.")}
          } else {
          .
            error_message_from_api <- "Neznámá chyba API";
            if (!is.null(parsed_response$error) && is.list(parsed_response$error) && !is.null(parsed_response$error$message)) { error_message_from_api <- parsed_response$error$message }
            else if (nchar(response_text) > 0) { error_message_from_api <- substr(response_text, 1, 300) }
            else { error_message_from_api <- paste("Status kód:", status) }
            showNotification(paste("Chyba AI API:", status, "-", error_message_from_api), type = "error", duration=15);
            local_rv$ai_response <- paste("Chyba API", status, ":", error_message_from_api);
            print(paste("Chyba AI API:", status, "Odpověď:", response_text))
          }
        }
      } else {
        showNotification("Požadavek na AI selhal (žádná odpověď).", type="error", duration=10);
        local_rv$ai_response <- "Požadavek selhal (žádná odpověď).";
        print("AI Error: No response received.") 
      }
      
      print("--- analyze_columns_btn FINISHED ---") # DEBUG
    }) # Konec observeEvent analyze_columns_btn
    
    # --- Display AI Analysis Response in Text Box ---
    output$ai_analysis_output <- renderText({
      print("Rendering ai_analysis_output...") # DEBUG
      if (is.null(local_rv$ai_response)) { "Klikněte na 'Analyzovat...' pro získání návrhů AI." }
      else { local_rv$ai_response }
    })
    
    # --- 3. Update Column Type Manually ---
    observeEvent(input$update_type_btn, {
      print("--- update_type_btn OBSERVED ---") # DEBUG
      req(local_rv$raw_data, local_rv$col_types, input$col_to_update, input$new_col_type)
      local_rv$show_imputation_controls <- FALSE # Reset imputace
      print("Imputation controls flag reset to FALSE by manual update button.") # DEBUG
      
      col_name <- input$col_to_update; new_type <- input$new_col_type
      # ... (kód pro manuální aktualizaci stejný jako dříve) ...
      if (!col_name %in% names(local_rv$raw_data)) { showNotification(paste("Sloupec '", col_name, "' nenalezen."), type = "error"); return() }
      if (!col_name %in% local_rv$col_types$Column) { showNotification(paste("Sloupec '", col_name, "' nenalezen v analýze."), type = "error"); return() }
      
      current_data_df <- local_rv$raw_data; current_col_types_df <- local_rv$col_types
      tryCatch({
        original_col <- current_data_df[[col_name]]; original_class <- class(original_col)[1]; converted_col <- original_col; data_changed <- FALSE; current_locale <- locale(decimal_mark = input$dec); target_class_after_conversion <- original_class
        
        if (new_type == "Numeric" && !is.numeric(original_col)) {
          converted_col <- suppressWarnings(parse_double(as.character(original_col), na = c("", "NA", "NaN", "NULL", "None", "#N/A"), locale = current_locale))
          target_class_after_conversion <- "numeric"; data_changed <- TRUE
          num_new_na <- sum(is.na(converted_col) & !is.na(original_col)); if (num_new_na > 0) { showNotification(paste("Varování: Převod '", col_name, "' na Numeric vytvořil ", num_new_na, " NA."), type = "warning", duration=7) }
        } else if (new_type == "Categorical" && !is.factor(original_col) && !is.character(original_col)) {
          converted_col <- as.factor(original_col); target_class_after_conversion <- "factor"; data_changed <- TRUE
        } else if (new_type == "Categorical" && is.numeric(original_col)) {
          converted_col <- as.character(original_col); target_class_after_conversion <- "character"; data_changed <- TRUE
        }
        
        type_row_index <- which(current_col_types_df$Column == col_name)[1]
        
        if (data_changed) {
          current_data_df[[col_name]] <- converted_col; local_rv$raw_data <- current_data_df;
          # Aktualizace analýzy
          current_col_types_df$DetectedType[type_row_index] <- new_type
          current_col_types_df$OriginalRClass[type_row_index] <- target_class_after_conversion
          new_unique <- length(unique(na.omit(converted_col)))
          new_missing <- sum(is.na(converted_col))
          new_missing_pct <- sprintf("%.2f%%", (new_missing / nrow(current_data_df)) * 100)
          current_col_types_df$UniqueValues[type_row_index] <- new_unique
          current_col_types_df$MissingValues[type_row_index] <- new_missing
          current_col_types_df$MissingPercent[type_row_index] <- new_missing_pct
          local_rv$col_types <- current_col_types_df
          showNotification(paste("Sloupec '", col_name, "' převeden na", target_class_after_conversion, "."), type = "message");
          local_rv$data_types_updated <- local_rv$data_types_updated + 1 # Trigger update
        } else {
          if (current_col_types_df$DetectedType[type_row_index] != new_type) {
            current_col_types_df$DetectedType[type_row_index] <- new_type;
            local_rv$col_types <- current_col_types_df;
            showNotification(paste("Typ v analýze pro '", col_name, "' aktualizován na", new_type, "."), type = "message")
            local_rv$data_types_updated <- local_rv$data_types_updated + 1 # Trigger update
          } else { showNotification(paste("Typ '", col_name, "' již je", new_type, "."), type = "message") }
        }
      }, error = function(e) { showNotification(paste("Chyba při manuální aktualizaci typu:", e$message), type = "error") })
      print("--- update_type_btn FINISHED ---") # DEBUG
    }) # Konec observeEvent update_type_btn
    
    # --- Re-render tables on update ---
    observeEvent(local_rv$data_types_updated, {
      req(local_rv$data_types_updated > 0)
      print("--- data_types_updated OBSERVED --- Triggering table refresh.") # DEBUG
      # Zde se znovu vykreslí tabulky (DT::renderDT je reaktivní)
    }, ignoreInit = TRUE)
    
    # --- LOGIKA PRO IMPUTACI ---
    
    # Reaktivní funkce pro získání sloupců s NA
    cols_needing_imputation <- reactive({
      print("--- Reactive: cols_needing_imputation CALLED ---") # DEBUG
      # Závislost na datech a typech
      req(local_rv$raw_data, local_rv$col_types)
      print("cols_needing_imputation: Data and col_types available.") # DEBUG
      
      if (!is.data.frame(local_rv$col_types) || !"MissingValues" %in% names(local_rv$col_types) || nrow(local_rv$col_types) != ncol(local_rv$raw_data)) {
        print("cols_needing_imputation: Invalid col_types or row/col mismatch.") # DEBUG
        return(NULL) # Neplatný stav analýzy
      }
      
      na_info <- tryCatch({
        local_rv$col_types %>% dplyr::filter(MissingValues > 0)
      }, error = function(e) {
        print(paste("cols_needing_imputation: Error during filter:", e$message)) # DEBUG
        return(NULL) # Chyba při filtrování
      })
      
      if (is.null(na_info)) {
        print("cols_needing_imputation: Filter resulted in NULL (error).") # DEBUG
        return(NULL)
      }
      
      if (nrow(na_info) == 0) {
        print("cols_needing_imputation: Filter resulted in 0 rows (no NA found).") # DEBUG
        return(na_info) # Vrátíme prázdný data frame
      }
      
      print(paste("cols_needing_imputation: Found", nrow(na_info), "columns with NA > 0.")) # DEBUG
      return(na_info) # Vrátíme data frame se sloupci s NA
    })
    
    # Tlačítko pro zobrazení/aktualizaci UI imputace
    observeEvent(input$configure_impute_btn, {
      print("--- configure_impute_btn OBSERVED ---") # DEBUG
      # Zkontrolujeme, zda máme data a analýzu
      if (is.null(local_rv$raw_data) || is.null(local_rv$col_types) || !is.data.frame(local_rv$col_types) || nrow(local_rv$col_types) != ncol(local_rv$raw_data)) {
        print("configure_impute_btn: Prerequisite check failed (missing data or analysis).") # DEBUG
        showNotification("Nejprve prosím nahrajte data a spusťte 'Analyzovat a Převést Typy'.", type="error");
        local_rv$show_imputation_controls <- FALSE # Pojistka
        return()
      }
      
      # Pokud jsou data a analýza OK, nastavíme příznak pro zobrazení
      local_rv$show_imputation_controls <- TRUE
      print(paste("configure_impute_btn: Set show_imputation_controls to:", local_rv$show_imputation_controls)) # DEBUG
      showNotification("Konfigurace imputace zobrazena/aktualizována.", type="message", duration = 3)
    })
    
    # Generování UI pro ovládací prvky imputace
    output$imputation_controls_ui <- renderUI({
      # Tento blok se spustí pokaždé, když se změní některá z jeho reaktivních závislostí
      # Hlavní závislostí je local_rv$show_imputation_controls
      print("--- renderUI: imputation_controls_ui CALLED ---") # DEBUG
      print(paste("renderUI[controls]: Current show_imputation_controls =", local_rv$show_imputation_controls)) # DEBUG
      
      # Zobrazíme UI jen pokud je příznak TRUE
      if (!local_rv$show_imputation_controls) {
        print("renderUI[controls]: Flag is FALSE, returning NULL.") # DEBUG
        return(NULL)
      }
      
      # Potřebujeme data a analýzu pro generování
      req(local_rv$raw_data, local_rv$col_types)
      print("renderUI[controls]: Data and col_types available, proceeding.") # DEBUG
      
      # Získáme sloupce, které potřebují imputaci
      na_info_df <- cols_needing_imputation() # Voláme reaktivní funkci
      
      # Zpracujeme výsledek reaktivní funkce
      if (is.null(na_info_df)) {
        # Toto by mělo nastat jen při chybě v cols_needing_imputation nebo neplatných datech/typech
        print("renderUI[controls]: cols_needing_imputation returned NULL. Displaying error/wait message.") # DEBUG
        return(p("Čekání na data nebo chyba při analýze chybějících hodnot."))
      } else if (nrow(na_info_df) == 0) {
        # Žádné sloupce s NA
        print("renderUI[controls]: cols_needing_imputation returned 0 rows. Displaying 'No NA' message.") # DEBUG
        return(p(style="color: green; font-weight: bold;", "Nebyly nalezeny žádné chybějící hodnoty vyžadující imputaci."))
      } else {
        # Máme sloupce s NA, generujeme UI
        print(paste("renderUI[controls]: Generating controls for", nrow(na_info_df), "columns.")) # DEBUG
        imputation_controls_list <- lapply(1:nrow(na_info_df), function(i) {
          col_name <- na_info_df$Column[i]
          col_type <- na_info_df$DetectedType[i] # Předpokládáme, že DetectedType existuje
          original_class <- na_info_df$OriginalRClass[i] # Předpokládáme, že OriginalRClass existuje
          missing_count <- na_info_df$MissingValues[i]
          
          input_id <- ns(paste0("impute_method_", make.names(col_name)))
          choices <- list("Nedělat nic" = "none")
          common_choices <- c("Modus (Nejčastější)" = "mode", "Odstranit řádek s NA" = "remove_row")
          
          if (col_type == "Numeric") {
            choices <- c(choices, "Průměr" = "mean", "Medián" = "median", "Nula" = "zero", common_choices)
          } else {
            choices <- c(choices, common_choices, "Konstanta ('Neznámý')" = "constant_unknown")
            if (original_class == "logical") {
              choices <- c(choices, "Konstanta PRAVDA" = "constant_true", "Konstanta NEPRAVDA" = "constant_false")
            }
          }
          
          # Pro jednoduchost zatím bez zachování výběru
          selected_value <- "none"
          
          fluidRow(
            column(4, tags$strong(paste0(col_name, ":"))),
            column(6, selectInput(inputId = input_id, label = NULL, choices = choices, selected = selected_value)),
            column(2, helpText(paste(missing_count, "NA")))
          )
        }) # Konec lapply
        
        print("renderUI[controls]: Finished generating list of controls.") # DEBUG
        return(tagList(imputation_controls_list))
      }
    }) # Konec renderUI pro controls
    
    # Generování UI pro tlačítko Aplikovat
    output$apply_imputation_button_ui <- renderUI({
      print("--- renderUI: apply_imputation_button_ui CALLED ---") # DEBUG
      print(paste("renderUI[button]: Current show_imputation_controls =", local_rv$show_imputation_controls)) # DEBUG
      
      # Zobrazíme UI jen pokud je příznak TRUE
      if (!local_rv$show_imputation_controls) {
        print("renderUI[button]: Flag is FALSE, returning NULL.") # DEBUG
        return(NULL)
      }
      
      # Potřebujeme vědět, jestli jsou nějaké NA
      req(local_rv$raw_data, local_rv$col_types)
      na_info_df <- cols_needing_imputation()
      
      # Zobrazíme tlačítko jen pokud existují sloupce s NA
      if (!is.null(na_info_df) && nrow(na_info_df) > 0) {
        print("renderUI[button]: NA found, rendering the 'Apply' button.") # DEBUG
        actionButton(ns("apply_impute_btn_exec"), "Aplikovat Vybrané Imputace", icon("check"), class = "btn-success", style="margin-top: 15px;")
      } else {
        print("renderUI[button]: No NA found or error, rendering NULL (no button).") # DEBUG
        return(NULL) # Pokud nejsou žádné NA nebo nastala chyba, tlačítko nezobrazíme
      }
    }) # Konec renderUI pro button
    
    # Aplikace vybraných imputací
    observeEvent(input$apply_impute_btn_exec, {
      print("--- apply_impute_btn_exec OBSERVED ---") # DEBUG
      req(local_rv$raw_data, local_rv$col_types)
      na_info <- cols_needing_imputation() # Získáme sloupce a jejich info
      
      # ... (zbytek kódu pro sběr konfigurace a volání impute_missing_values - stejný) ...
      if (is.null(na_info) || nrow(na_info) == 0) { showNotification("Nebyly nalezeny sloupce s NA pro imputaci.", type = "warning"); return() }
      
      imputation_config <- list(); skipped_cols_config <- c(); valid_inputs_found = FALSE
      print("apply_impute_btn_exec: Collecting imputation configuration...") # DEBUG
      for (i in 1:nrow(na_info)) {
        col_name <- na_info$Column[i]; input_id_local_part <- paste0("impute_method_", make.names(col_name));
        selected_method <- input[[input_id_local_part]] # Získání hodnoty z UI
        if (!is.null(selected_method)) {
          valid_inputs_found = TRUE;
          print(paste("  Column:", col_name, "-> Selected method:", selected_method)) # DEBUG
          if (selected_method != "none") {
            imputation_config[[col_name]] <- selected_method
          } else { skipped_cols_config <- c(skipped_cols_config, col_name) }
        } else { print(paste("  WARN: Input value for", input_id_local_part, "is NULL.")) }
      }
      
      if (!valid_inputs_found) { showNotification("Nelze přečíst konfiguraci imputace.", type = "error"); return() }
      if (length(imputation_config) == 0) { showNotification("Nebyly vybrány žádné metody imputace (kromě 'Nedělat nic').", type = "message"); return() }
      
      print(paste("apply_impute_btn_exec: Configuration collected:", length(imputation_config), "methods to apply.")) # DEBUG
      showModal(modalDialog("Aplikuji vybrané metody imputace...", footer = NULL))
      
      col_info_for_function <- local_rv$col_types; current_dec_separator <- input$dec
      imputation_result <- tryCatch({
        print("Calling impute_missing_values function...") # DEBUG
        impute_missing_values( df = local_rv$raw_data, imputation_config = imputation_config, col_types_info = col_info_for_function, dec_separator = current_dec_separator )
      }, error = function(e) {
        removeModal(); showNotification(paste("Chyba během procesu imputace:", e$message), type = "error", duration = 10);
        print(paste("Error during imputation function call:", e$message)); print(imputation_config); return(NULL)
      })
      removeModal()
      
      if (!is.null(imputation_result) && is.list(imputation_result) && !is.null(imputation_result$data) && !is.null(imputation_result$summary)) {
        print("Imputation successful. Updating data and analysis.") # DEBUG
        local_rv$raw_data <- imputation_result$data;
        showNotification(imputation_result$summary$message, type = "message", duration = 8)
        if (length(imputation_result$summary$cols_skipped) > 0) { showNotification(paste("Některé sloupce přeskočeny:", paste(imputation_result$summary$cols_skipped, collapse=", ")), type = "warning", duration = 6) }
        
        # Aktualizace analýzy po imputaci
        showModal(modalDialog("Aktualizuji analýzu sloupců po imputaci...", footer = NULL))
        existing_ai_suggestions <- if (!is.null(local_rv$col_types) && "AISuggestion" %in% names(local_rv$col_types)) { local_rv$col_types %>% dplyr::select(Column, AISuggestion) %>% dplyr::filter(!is.na(AISuggestion)) } else { NULL }
        new_analysis <- tryCatch({ run_heuristic_analysis(local_rv$raw_data) }, error = function(e){ print(paste("Error re-running analysis:", e$message)); NULL })
        if (!is.null(new_analysis)) {
          if (!"AISuggestion" %in% names(new_analysis)) { new_analysis$AISuggestion <- NA_character_ }
          if (!is.null(existing_ai_suggestions)) { new_analysis <- new_analysis %>% dplyr::select(-any_of("AISuggestion")) %>% dplyr::left_join(existing_ai_suggestions, by = "Column") }
          local_rv$col_types <- new_analysis;
          print("Column analysis updated after imputation.") # DEBUG
          showNotification("Analýza sloupců aktualizována.", type = "message")
          local_rv$data_types_updated <- local_rv$data_types_updated + 1 # Trigger refresh
        } else { showNotification("Aktualizace analýzy po imputaci selhala.", type = "warning") }
        removeModal()
        # Poznámka: local_rv$show_imputation_controls zůstává TRUE, takže UI se překreslí a ukáže aktuální stav (pravděpodobně už bez NA)
        
      } else if (!is.null(imputation_result) && is.list(imputation_result) && is.null(imputation_result$data)) {
        print("Imputation resulted in empty dataset.") # DEBUG
        showNotification("Proces imputace vrátil prázdný dataset.", type="warning")
        local_rv$raw_data <- NULL; local_rv$col_types <- NULL; local_rv$show_imputation_controls <- FALSE
        local_rv$data_types_updated <- local_rv$data_types_updated + 1
      } else {
        print("Imputation failed or returned unexpected result.") # DEBUG
        # Chybová notifikace byla zobrazena už v tryCatch nebo bude zobrazena zde
        if(is.null(imputation_result)) showNotification("Proces imputace selhal.", type="error") else showNotification("Proces imputace vrátil neočekávaný výsledek.", type="error")
      }
      print("--- apply_impute_btn_exec FINISHED ---") # DEBUG
    })
    
    
    # --- NÁVRATOVÁ HODNOTA MODULU ---
    print("Module server setup complete.") # DEBUG
    return( list( data = reactive({ local_rv$raw_data }), col_types = reactive({ local_rv$col_types }), decimal_sep = reactive({ input$dec }) ) )
    
  }) # Konec moduleServer
}

# --- POTŘEBNÉ POMOCNÉ FUNKCE (pokud nejsou jinde) ---

# Tato funkce by měla být v R/utils.R nebo podobném souboru
# Tato funkce by měla být v R/utils.R nebo podobném souboru
run_heuristic_analysis <- function(df) {
  print("--- run_heuristic_analysis CALLED ---") # DEBUG
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
    print("run_heuristic_analysis: Input df is NULL, empty or has 0 columns.") # DEBUG
    return(NULL)
  }
  results <- list()
  total_rows <- nrow(df)
  
  # --- NOVÉ: Dynamický práh podle Python logiky (definovaný jednou) ---
  if (total_rows < 200) {
    category_threshold <- 8
  } else if (total_rows < 1000) {
    category_threshold <- 10
  } else {
    category_threshold <- 15
  }
  print(paste("Dynamic category threshold set to:", category_threshold, "based on total rows:", total_rows)) # DEBUG
  # --- KONEC NOVÉHO ---
  
  for (col_name in names(df)) {
    print(paste("Analyzing column:", col_name)) # DEBUG
    column_data <- df[[col_name]]
    original_class <- class(column_data)[1]
    missing_values <- sum(is.na(column_data))
    missing_percent <- sprintf("%.2f%%", (missing_values / total_rows) * 100)
    unique_values_all <- unique(column_data)
    unique_values_non_na <- unique(na.omit(column_data))
    unique_count <- length(unique_values_non_na)
    
    # Heuristika typu
    detected_type <- "Categorical" # Default
    potential_numeric <- suppressWarnings(as.numeric(as.character(unique_values_non_na)))
    non_na_count <- length(column_data) - missing_values # Celkový počet ne-NA hodnot
    numeric_convertible_count <- sum(!is.na(potential_numeric)) # Počet unikátních ne-NA hodnot, které jsou čísla
    
    # --- OPRAVENÁ PODMÍNKA ---
    # Kontrola: Jsou VŠECHNY unikátní ne-NA hodnoty převoditelné na číslo A existuje více než jedna unikátní hodnota?
    all_unique_are_numeric <- (unique_count > 0 && numeric_convertible_count == unique_count)
    
    if (all_unique_are_numeric && unique_count > 1) {
      # --- ZMĚNĚNÁ LOGIKA PRAHU ---
      # Pokud je počet unikátních hodnot VĚTŠÍ než dynamický práh, považujeme za Numeric
      if (unique_count > category_threshold) {
        detected_type <- "Numeric"
        print(paste("  Column:", col_name, "- Detected as Numeric (unique count", unique_count, "> threshold", category_threshold, ")")) # DEBUG
      } else {
        # Jinak (pokud je unikátních <= prahu), zůstává Categorical (low unique numeric)
        detected_type <- "Categorical"
        print(paste("  Column:", col_name, "- Detected as Categorical (low unique numeric: count", unique_count, "<= threshold", category_threshold, ")")) # DEBUG
      }
      # --- KONEC ZMĚNĚNÉ LOGIKY ---
    } else if (unique_count <= 1 && non_na_count > 0) {
      # Pokud je jen jedna unikátní hodnota (nebo samé NA), je to spíše konstanta/kategorická
      detected_type <- "Categorical"
      print(paste("  Column:", col_name, "- Detected as Categorical (single unique value or NA only)")) # DEBUG
    } else {
      # Pokud nelze všechny unikátní převést nebo je mix nebo sloupec byl prázdný
      detected_type <- "Categorical" # Zůstává Categorical (default or mixed)
      print(paste("  Column:", col_name, "- Detected as Categorical (default, mixed types, or empty unique)")) # DEBUG
    }
    # --- KONEC OPRAVENÉ PODMÍNKY ---
    
    # Zvláštní případ pro logické - PONECHÁNO BEZE ZMĚNY
    # (Tato kontrola proběhne *po* výše uvedené logice)
    logical_like_values <- c("TRUE", "FALSE", "T", "F", "1", "0", TRUE, FALSE)
    # Kontrolujeme, zda unikátní hodnoty jsou podmnožinou logických + zda jsou max 2 unikátní
    is_potentially_logical <- all(unique_values_non_na %in% logical_like_values)
    
    if (is_potentially_logical && unique_count <= 2 && unique_count > 0) { # Přidána podmínka > 0 pro jistotu
      detected_type <- "Categorical" # Považujeme za kategorickou
      print(paste("  Column:", col_name, "- Re-classified as Categorical (logical-like)")) # DEBUG
    }
    
    results[[col_name]] <- data.frame(
      Column = col_name,
      OriginalRClass = original_class,
      DetectedType = detected_type,
      UniqueValues = unique_count,
      MissingValues = missing_values,
      MissingPercent = missing_percent,
      stringsAsFactors = FALSE
    )
  }
  print("--- run_heuristic_analysis FINISHED ---") # DEBUG
  bind_rows(results)
}

# Tato funkce by měla být v R/utils.R nebo podobném souboru
impute_missing_values <- function(df, imputation_config, col_types_info, dec_separator = ".") {
  print("--- impute_missing_values CALLED ---") # DEBUG
  df_original_rows <- nrow(df)
  imputed_df <- df
  cols_imputed <- c()
  cols_removed <- c()
  cols_skipped <- c()
  cols_error <- c()
  
  # Nejprve provedeme odstranění řádků, pokud je požadováno
  cols_to_remove_rows <- names(imputation_config)[imputation_config == "remove_row"]
  if (length(cols_to_remove_rows) > 0) {
    print(paste("Removing rows with NA in columns:", paste(cols_to_remove_rows, collapse=", "))) # DEBUG
    rows_before <- nrow(imputed_df)
    imputed_df <- imputed_df %>% tidyr::drop_na(any_of(cols_to_remove_rows))
    rows_after <- nrow(imputed_df)
    cols_removed <- cols_to_remove_rows
    print(paste("Rows removed:", rows_before - rows_after)) # DEBUG
    # Odstraníme tyto sloupce z dalšího zpracování
    imputation_config <- imputation_config[!names(imputation_config) %in% cols_to_remove_rows]
  }
  
  # Projdeme zbylé konfigurace
  for (col_name in names(imputation_config)) {
    method <- imputation_config[[col_name]]
    print(paste("Processing column:", col_name, "with method:", method)) # DEBUG
    
    if (!col_name %in% names(imputed_df)) {
      print(paste("  WARN: Column", col_name, "not found in dataframe (possibly removed?). Skipping.")) # DEBUG
      cols_skipped <- c(cols_skipped, col_name)
      next
    }
    
    column_data <- imputed_df[[col_name]]
    missing_idx <- is.na(column_data)
    num_missing <- sum(missing_idx)
    
    if (num_missing == 0) {
      print("  No missing values found. Skipping.") # DEBUG
      cols_skipped <- c(cols_skipped, col_name)
      next
    }
    
    imputed_value <- NA
    tryCatch({
      if (method == "mean") {
        if (!is.numeric(column_data)) stop("Cannot calculate mean for non-numeric column.")
        imputed_value <- mean(column_data, na.rm = TRUE)
      } else if (method == "median") {
        if (!is.numeric(column_data)) stop("Cannot calculate median for non-numeric column.")
        imputed_value <- median(column_data, na.rm = TRUE)
      } else if (method == "mode") {
        # Výpočet modu (nejčastější hodnoty)
        tbl <- table(column_data[!missing_idx])
        if (length(tbl) > 0) {
          imputed_value <- names(tbl)[which.max(tbl)]
          # Převedeme zpět na původní typ, pokud je to možné
          if (is.numeric(column_data)) imputed_value <- as.numeric(imputed_value)
          else if (is.factor(column_data)) imputed_value <- as.factor(imputed_value)
          else if (is.logical(column_data)) imputed_value <- as.logical(imputed_value)
          # Pro character zůstane character
        } else {
          # Pokud jsou všechny hodnoty NA, nemůžeme určit modus
          stop("Cannot determine mode (all values are NA).")
        }
      } else if (method == "zero") {
        if (!is.numeric(column_data)) stop("Cannot impute zero for non-numeric column.")
        imputed_value <- 0
      } else if (method == "constant_unknown") {
        # Zajistíme, že konstanta je stejného typu jako sloupec, pokud možno
        if (is.factor(column_data)) {
          # Přidáme level 'Neznámý', pokud ještě neexistuje
          lvls <- levels(column_data)
          if (!"Neznámý" %in% lvls) {
            column_data <- factor(column_data, levels = c(lvls, "Neznámý"))
            imputed_df[[col_name]] <- column_data # Aktualizujeme data frame s novými úrovněmi
          }
          imputed_value <- factor("Neznámý", levels = levels(column_data))
        } else if (is.numeric(column_data)) {
          # Tady by to nemělo nastat, ale pro jistotu
          warning(paste("Imputing string 'Neznámý' into numeric column", col_name))
          imputed_value <- "Neznámý" # Toto pravděpodobně způsobí coercion na character
        } else {
          imputed_value <- "Neznámý" # Pro character nebo jiné
        }
      } else if (method == "constant_true") {
        if (!is.logical(column_data)) stop("Can only impute TRUE for logical columns.")
        imputed_value <- TRUE
      } else if (method == "constant_false") {
        if (!is.logical(column_data)) stop("Can only impute FALSE for logical columns.")
        imputed_value <- FALSE
      } else {
        stop(paste("Unknown imputation method:", method))
      }
      
      # Aplikace imputace
      print(paste("  Imputing", num_missing, "values with:", imputed_value)) # DEBUG
      # Použijeme `replace` pro zachování typu sloupce, kde je to možné
      imputed_df[[col_name]] <- replace(imputed_df[[col_name]], missing_idx, imputed_value)
      
      # Speciální kontrola pro faktory po imputaci (může změnit typ na character)
      if (is.factor(column_data) && !is.factor(imputed_df[[col_name]]) && method != "constant_unknown") {
        print(paste("  WARN: Factor column", col_name, "might have been coerced. Re-applying factor."))
        # Zkusíme znovu aplikovat faktor s původními a případně novou úrovní
        original_levels <- levels(column_data)
        all_values_after_impute <- unique(imputed_df[[col_name]])
        new_levels <- setdiff(all_values_after_impute, original_levels)
        final_levels <- c(original_levels, new_levels)
        # Odebereme NA level, pokud vznikl
        final_levels <- final_levels[!is.na(final_levels)]
        imputed_df[[col_name]] <- factor(imputed_df[[col_name]], levels = final_levels)
      }
      
      cols_imputed <- c(cols_imputed, col_name)
      
    }, error = function(e) {
      print(paste("  ERROR imputing column", col_name, ":", e$message)) # DEBUG
      cols_error <- c(cols_error, col_name)
    })
  }
  
  rows_final <- nrow(imputed_df)
  rows_change_msg <- if (df_original_rows != rows_final) {
    paste("Počet řádků změněn z", df_original_rows, "na", rows_final, "kvůli odstranění NA.")
  } else {
    "Počet řádků nezměněn."
  }
  imputation_summary <- paste("Imputace dokončena.",
                              "Imputované sloupce:", length(cols_imputed),
                              "(", paste(cols_imputed, collapse=", "), ").",
                              "Řádky odstraněny pro sloupce:", length(cols_removed),
                              "(", paste(cols_removed, collapse=", "), ").",
                              rows_change_msg)
  if (length(cols_error) > 0) {
    imputation_summary <- paste(imputation_summary, "CHYBA u sloupců:", paste(cols_error, collapse=", "), ".")
  }
  if (length(cols_skipped) > 0) {
    imputation_summary <- paste(imputation_summary, "Přeskočené sloupce (bez NA nebo chyba):", paste(cols_skipped, collapse=", "), ".")
  }
  
  print("--- impute_missing_values FINISHED ---") # DEBUG
  return(list(
    data = imputed_df,
    summary = list(
      message = imputation_summary,
      cols_imputed = cols_imputed,
      cols_removed = cols_removed,
      cols_skipped = cols_skipped,
      cols_error = cols_error,
      rows_initial = df_original_rows,
      rows_final = rows_final
    )
  ))
}

# --- Spuštění App (pokud je toto hlavní soubor) ---
# shinyApp(ui = ui, server = server)