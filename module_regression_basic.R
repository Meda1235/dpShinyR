# R/module_regression_basic.R (Krok 1: Vrácení conditionalPanel a více verbatimTextOutput)

# --- Načtení potřebných knihoven ---
library(shiny)
library(dplyr)
library(stats) # Pro lm
library(shinycssloaders) # Pro withSpinner
library(rlang) # Pro %||%

# --- UI Funkce Modulu ---
regressionBasicUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Základní Regresní Analýza (Test Krok 1)"),
    p("Vyberte závislou a nezávislé proměnné."),
    fluidRow(
      # --- Vstupní část (stejná) ---
      column(4,
             wellPanel(
               selectInput(ns("y_var_basic"), "Závislá proměnná (Y):", choices = NULL),
               uiOutput(ns("x_var_ui_placeholder_basic")),
               actionButton(ns("run_analysis_basic"), "Spustit Základní Analýzu", icon = icon("play"), class = "btn-warning")
             )
      ),
      # --- Výstupní část ---
      column(8,
             h4("Výsledky Základní Analýzy"),
             # Zobrazení chyby analýzy
             uiOutput(ns("analysis_error_ui")), # Přejmenováno pro konzistenci
             # Zobrazení výsledků pomocí conditionalPanel
             conditionalPanel(
               condition = "output.showResultsBasic === true", 
               ns = ns,
               tagList(
                 h6("TEST: Conditional Panel je viditelný!"),
                 hr(),
                 tags$h5("Metoda a Metriky (Debug):"),
                 verbatimTextOutput(ns("debug_summary_output")),
                 tags$h5("Koeficienty (Debug):"),
                 verbatimTextOutput(ns("debug_coeffs_output")),
                 tags$h5("Plot Data (Debug):"),
                 verbatimTextOutput(ns("debug_plot_data_output")),
                 # Ponecháme AI část (ale zatím prázdnou)
                 tags$hr(), tags$h5("AI Interpretace (zatím neaktivní)"),
               )
             )
      ) # konec column 8
    ) # konec fluidRow
  ) # konec tagList
}


# --- Server Funkce Modulu ---
regressionBasicServer <- function(id, reactive_data, reactive_col_types) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reaktivní hodnoty pro ukládání stavu ---
    rv <- reactiveValues(
      analysis_results = NULL,
      selected_method_info = NULL,
      plot_data = NULL,
      analysis_error = NULL,
      is_analyzing = FALSE
    )
    

    observe({ req(reactive_col_types()); col_info <- reactive_col_types(); numeric_cols <- col_info$Column[col_info$DetectedType == "Numeric"]; choices_y <- setNames(numeric_cols, numeric_cols); updateSelectInput(session, "y_var_basic", choices = c("Vyberte..." = "", choices_y), selected = isolate(input$y_var_basic)) })
    output$x_var_ui_placeholder_basic <- renderUI({ req(reactive_col_types()); col_info <- reactive_col_types(); numeric_cols <- col_info$Column[col_info$DetectedType == "Numeric"]; y_selected <- input$y_var_basic; choices_x <- numeric_cols; if (!is.null(y_selected) && nchar(y_selected) > 0 && y_selected %in% choices_x) {choices_x <- choices_x[choices_x != y_selected]}; current_selection <- isolate(input$x_vars_basic); valid_current_selection <- intersect(current_selection, choices_x); checkboxGroupInput(ns("x_vars_basic"), label = tags$label("Nezávislé proměnné (X):", class = "control-label"), choices = setNames(choices_x, choices_x), selected = valid_current_selection) })
    

    output$analysis_loading_ui <- renderUI({
      if (rv$is_analyzing) { tags$div(style="display: inline-block; margin-left: 15px;", icon("spinner", class = "fa-spin", style="color: #007bff;")) } else { NULL }
    })
    
    # --- Resetování výsledků (z původního modulu) ---
    observeEvent(c(input$y_var_basic, input$x_vars_basic), { # Zjednodušeno jen na vstupy
      req( !is.null(input$y_var_basic) || !is.null(input$x_vars_basic) )
      cat("DEBUG BASIC K1: Input changed, resetting results.\n") # DEBUG
      rv$analysis_results <- NULL
      rv$selected_method_info <- NULL
      rv$plot_data <- NULL
      rv$analysis_error <- NULL
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
    # --- Spuštění Analýzy ---
    observeEvent(input$run_analysis_basic, {
      cat("\n--- run_analysis_basic K1 START ---\n") # DEBUG
      rv$analysis_error <- NULL; rv$analysis_results <- NULL; rv$selected_method_info <- NULL; rv$plot_data <- NULL
      rv$is_analyzing <- TRUE # Zapnout spinner
      
      y_selected <- input$y_var_basic
      x_selected <- input$x_vars_basic
      
      # Validace vstupů
      if (is.null(y_selected) || nchar(y_selected) == 0) { rv$analysis_error <- "Chyba: Vyberte Y."; cat("DEBUG BASIC K1: No Y\n"); rv$is_analyzing <- FALSE; return() } # DEBUG
      if (is.null(x_selected) || length(x_selected) == 0) { rv$analysis_error <- "Chyba: Vyberte X."; cat("DEBUG BASIC K1: No X\n"); rv$is_analyzing <- FALSE; return() } # DEBUG
      cat("DEBUG BASIC K1: Inputs OK.\n") # DEBUG
      
      req(reactive_data()); req(reactive_col_types())
      data_in <- reactive_data(); col_types <- reactive_col_types()
      
      # Příprava dat a modelování (v tryCatch)
      analysis_status <- tryCatch({
        cat("DEBUG BASIC K1: Inside tryCatch - Preparing data...\n") # DEBUG
        selected_cols <- c(y_selected, x_selected)
        df_model <- data_in %>%
          select(all_of(selected_cols)) %>%
          mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
          na.omit()
        
        if (nrow(df_model) < (length(x_selected) + 2)) { stop(paste("Nedostatek kompletních pozorování (nalezeno:", nrow(df_model), ").")) }
        cat("DEBUG BASIC K1: Data prepared. Rows:", nrow(df_model), "\n") # DEBUG
        
        # Formule
        y_name_bt <- paste0("`", y_selected, "`"); x_names_bt <- paste0("`", x_selected, "`")
        formula_str <- paste(y_name_bt, "~", paste(x_names_bt, collapse = " + "))
        cat("DEBUG BASIC K1: Formula:", formula_str, "\n") # DEBUG
        
        # Model
        lm_fit <- lm(as.formula(formula_str), data = df_model)
        model_summary <- summary(lm_fit)
        cat("DEBUG BASIC K1: Model fitted and summarized.\n") # DEBUG
        
        # --- Příprava výstupů pro rv (podobně jako v původním modulu) ---
        temp_results <- list()
        coeffs <- coef(model_summary)
        cis <- tryCatch(confint(lm_fit), error = function(e) NULL)
        intercept_idx <- which(rownames(coeffs) == "(Intercept)")
        if (length(intercept_idx) > 0) temp_results$intercept <- coeffs[intercept_idx, 1]
        var_indices <- which(rownames(coeffs) != "(Intercept)")
        coef_list <- list()
        for (i in var_indices) {
          row_name <- rownames(coeffs)[i]; clean_name <- gsub("^`|`$", "", row_name)
          coef_list[[clean_name]] <- list(name = clean_name, coef = coeffs[i, 1], stderr = coeffs[i, 2], t_value = coeffs[i, 3], p_value = coeffs[i, 4], ciLow = if(!is.null(cis) && row_name %in% rownames(cis)) cis[row_name, 1] else NA, ciHigh = if(!is.null(cis) && row_name %in% rownames(cis)) cis[row_name, 2] else NA)
        }
        temp_results$coefficients <- unname(coef_list)
        temp_results$r2 <- model_summary$r.squared
        temp_results$r2_adjusted <- model_summary$adj.r.squared
        temp_results$rmse <- sqrt(mean(residuals(lm_fit)^2))
        temp_results$f_statistic_info <- model_summary$fstatistic
        # Uložíme i zachycený výstup pro verbatimTextOutput
        temp_results$summary_text <- capture.output(print(model_summary))
        
        temp_plot_data <- list(
          scatter = NULL, 
          residuals = list(predicted = fitted(lm_fit), residuals = residuals(lm_fit))
        )
        
        temp_method_info <- list(method = "ols", reason = "Základní test")
        
        # --- Aktualizace rv přímo zde ---
        cat("DEBUG BASIC K1: Assigning results directly to rv...\n") # DEBUG
        rv$analysis_results <- temp_results
        rv$selected_method_info <- temp_method_info
        rv$plot_data <- temp_plot_data
        rv$analysis_error <- NULL 
        cat("DEBUG BASIC K1: Reactive values assigned.\n") # DEBUG
        
        return("Success")
        
      }, error = function(e) {
        cat("--- ERROR captured by tryCatch (Basic K1) ---\n"); print(e) # DEBUG
        rv$analysis_error <- paste("Chyba:", e$message)
        rv$analysis_results <- NULL; rv$selected_method_info <- NULL; rv$plot_data <- NULL;
        cat("DEBUG BASIC K1: rv$analysis_error set to:", rv$analysis_error, "\n") # DEBUG
        return("Error")
      }) # konec tryCatch
      
      # Vypnout spinner
      rv$is_analyzing <- FALSE
      cat("DEBUG BASIC K1: rv$is_analyzing set to FALSE. Status:", analysis_status, "\n") # DEBUG
      cat("--- run_analysis_basic K1 END ---\n") # DEBUG
      
    }) # konec observeEvent
    
    # --- Výstup pro podmíněné zobrazení (conditionalPanel) ---
    output$showResultsBasic <- reactive({
      cat("DEBUG BASIC K1: Evaluating showResultsBasic reactive. !is.null(rv$analysis_results) =", !is.null(rv$analysis_results), "&& is.null(rv$analysis_error) =", is.null(rv$analysis_error), "\n") # DEBUG
      !is.null(rv$analysis_results) && is.null(rv$analysis_error)
    })
    outputOptions(output, 'showResultsBasic', suspendWhenHidden = FALSE) # Důležité pro JS
    
    
    # --- Přímé renderování výstupů ---
    
    # Zobrazení chyby analýzy
    output$analysis_error_ui <- renderUI({
      if (!is.null(rv$analysis_error)) { tags$div(class = "alert alert-danger", role = "alert", tags$strong("Chyba analýzy: "), rv$analysis_error) } else {NULL}
    })
    
    # Jednoduché textové výstupy pro debugování
    output$debug_summary_output <- renderPrint({
      req(output$showResultsBasic())
      cat("DEBUG BASIC K1: Rendering debug_summary_output...\n") # DEBUG
      print("--- Metoda a Metriky ---")
      print(rv$selected_method_info)
      print("--- Výsledky (souhrn text) ---")
      # Vypíšeme zachycený text summary
      cat(paste(rv$analysis_results$summary_text, collapse = "\n"))
    })
    
    output$debug_coeffs_output <- renderPrint({
      req(output$showResultsBasic())
      cat("DEBUG BASIC K1: Rendering debug_coeffs_output...\n") # DEBUG
      print("--- Koeficienty (struktura) ---")
      print(rv$analysis_results[c("intercept", "coefficients")])
    })
    
    output$debug_plot_data_output <- renderPrint({
      req(output$showResultsBasic())
      cat("DEBUG BASIC K1: Rendering debug_plot_data_output...\n") # DEBUG
      print("--- Data pro grafy (struktura) ---")
      str(rv$plot_data)
    })
    
    # AI část zatím prázdná/neaktivní
    output$ai_error_ui <- renderUI({ NULL }) # Zatím skrýt
    output$ai_interpretation_ui <- renderUI({ NULL }) # Zatím skrýt
    
  }) # konec moduleServer
}