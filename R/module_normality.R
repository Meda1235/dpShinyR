

library(shiny)
library(DT)
library(dplyr)
library(MASS) 

normalityUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Analýza Normality"),
    p("Tato sekce testuje normalitu numerických sloupců (na datech po případném zpracování outlierů) a umožňuje jejich transformaci."),
    hr(),
    h4("Výsledky Testů Normality"),
    DTOutput(ns("results_table")),
    hr(),
    uiOutput(ns("column_controls_ui"))
  )
}

# --- Server Funkce Modulu ---
normalityServer <- function(id, reactive_data, reactive_col_types) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(normality_results = NULL, user_test_override = list()) 
    
    format_p_value_r <- function(p_value) { if (!is.numeric(p_value) || is.na(p_value)) return("-"); if (p_value < 0.0001) return(format(p_value, scientific = TRUE, digits = 2)); return(round(p_value, 4)) }
    
    normality_analysis <- reactive({
      req(reactive_data(), reactive_col_types())
      df <- reactive_data(); col_types <- reactive_col_types(); overrides <- local_rv$user_test_override
      print("Normality Module: Spouštím analýzu normality (po outlierech)...")
      if (!is.data.frame(col_types)) { print("Normality Module: Chyba - col_types není data frame."); return(NULL) }
      numeric_cols_info <- filter(col_types, DetectedType == "Numeric")
      if (nrow(numeric_cols_info) == 0) { print("Normality Module: Nenalezeny žádné numerické sloupce."); empty_df <- data.frame(Sloupec=character(), Použitý.Test=character(), Důvod=character(), P.hodnota=character(), Normální..α.0.05.=character(), Varování=character(), .is_normal_bool=logical(), .test_run=character(), stringsAsFactors=FALSE); local_rv$normality_results <- empty_df; return(local_rv$normality_results) }
      
      results_list <- list()
      for (col_name in numeric_cols_info$Column) {
        if (!col_name %in% names(df)) next
        original_series_na <- df[[col_name]]
        if(!is.numeric(original_series_na)){ original_series_na <- suppressWarnings(as.numeric(as.character(original_series_na))) }
        series <- original_series_na[!is.na(original_series_na)] 
        
        test_used <- "N/A"; p_value <- NA; stat_value <- NA; reason <- "N/A"; test_to_run <- "N/A"; is_normal <- NA
        warning_parts <- c()
        
        if (length(series) < 3 || length(unique(series)) < 2) {
          reason <- "Nedostatek dat (< 3) / unikátních hodnot (< 2)"
          print(paste("Normality Module: Přeskakuji test pro", col_name, "-", reason))
        } else {
          preferred_test <- overrides[[col_name]]; n <- length(series)
          if (!is.null(preferred_test)) { test_to_run <- preferred_test; reason <- "Test zvolen uživatelem." } else if (n < 50) { test_to_run <- "shapiro"; reason <- "Výchozí test (N < 50)." } else { test_to_run <- "ks"; reason <- "Výchozí test (N >= 50)." }
          test_result <- NULL
          tryCatch({
            if (test_to_run == "shapiro") { test_result <- shapiro.test(series); test_used <- "Shapiro-Wilk"; p_value <- test_result$p.value; stat_value <- test_result$statistic }
            else if (test_to_run == "ks") {
              current_sd <- sd(series, na.rm = TRUE); if(is.na(current_sd) || current_sd == 0) { p_value <- 0; stat_value <- NA; test_used <- "Kolmogorov-Smirnov"; reason <- paste(reason, "SD=0/NA.") } else { standardized_series <- scale(series); test_result <- ks.test(standardized_series, "pnorm"); test_used <- "Kolmogorov-Smirnov"; p_value <- test_result$p.value; stat_value <- test_result$statistic }
            } else { test_used = "Žádný"; reason = "Test nebyl spuštěn."; p_value = NA; stat_value = NA }
          }, error = function(e) { warning(paste("Normality Module: Chyba testu", col_name, ":", e$message)); test_used <- "Chyba Testu"; reason <- e$message; p_value <- NA; stat_value = NA })
          is_normal <- !is.na(p_value) && p_value > 0.05
          
    
          missing_count <- sum(is.na(original_series_na))
          if (missing_count > 0) { warning_parts <- c(warning_parts, paste0(missing_count, " NA (před testem).")) }
        }
        
        results_list[[col_name]] <- data.frame( Sloupec = col_name, Použitý.Test = test_used, Důvod = reason, P.hodnota = format_p_value_r(p_value), Normální..α.0.05. = ifelse(is.na(p_value), "-", ifelse(is_normal, "✅ Ano", "❌ Ne")), Varování = if(length(warning_parts) > 0) paste(warning_parts, collapse=" ") else "-", .is_normal_bool = is_normal, .test_run = test_to_run, stringsAsFactors = FALSE )
      }
      
      if (length(results_list) > 0) { results_df <- bind_rows(results_list); print("Normality Module: Analýza normality dokončena."); local_rv$normality_results <- results_df; return(results_df) }
      else { print("Normality Module: Žádné sloupce nebyly analyzovány."); empty_df <- data.frame(Sloupec=character(), Použitý.Test=character(), Důvod=character(), P.hodnota=character(), Normální..α.0.05.=character(), Varování=character(), .is_normal_bool=logical(), .test_run=character(), stringsAsFactors=FALSE); local_rv$normality_results <- empty_df; return(empty_df) }
    }) 
    
    output$results_table <- renderDT({
      results_df_internal <- normality_analysis(); print("--- Inside renderDT (Normality Module AFTER Outliers) ---"); print(paste("Class:", class(results_df_internal))); print("Names:"); print(names(results_df_internal))
      if (is.null(results_df_internal) || !is.data.frame(results_df_internal) || nrow(results_df_internal) == 0) { print("DEBUG: results_df_internal is NULL/empty."); return(datatable(data.frame(Zprava = "Žádné výsledky normality k zobrazení."), rownames = FALSE, options = list(searching = FALSE, paging = FALSE))) }
      required_internal_cols <- c("Sloupec", "Použitý.Test", "Důvod", "P.hodnota", "Normální..α.0.05.", "Varování"); if (!all(required_internal_cols %in% names(results_df_internal))) { print("Chyba: Chybí očekávané interní sloupce."); print(names(results_df_internal)); return(datatable(data.frame(Zprava = "Chyba: Neočekávaná struktura výsledků."), rownames = FALSE, options = list(searching = FALSE, paging = FALSE))) }
      
   
      display_df <- tryCatch({ dplyr::select(results_df_internal, Sloupec, Použitý.Test, Důvod, P.hodnota, `Normální..α.0.05.`, Varování) }, error = function(e){ print(paste("Chyba při dplyr::select:", e$message)); NULL })
      if (is.null(display_df) || !is.data.frame(display_df)) { print("Chyba: Nepodařilo se vytvořit 'display_df'."); return(datatable(data.frame(Zprava = "Chyba: Nepodařilo se připravit data."), rownames = FALSE, options = list(searching = FALSE, paging = FALSE))) }
      
      colnames(display_df) <- c('Sloupec', 'Použitý Test', 'Důvod Testu', 'P-hodnota', 'Normální (α=0.05)', 'Varování'); print("DEBUG: Rendering datatable...")
      datatable( display_df, selection = 'single', rownames = FALSE, options = list( searching = TRUE, pageLength = 10, scrollX = TRUE, rowCallback = JS( "function(row, data, index) { if (data[4] != null && typeof data[4] === 'string' && data[4].includes('❌ Ne')) { $(row).css('background-color', 'rgba(255, 0, 0, 0.1)'); } else if (data[4] != null && typeof data[4] === 'string' && data[4].includes('✅ Ano')) { $(row).css('background-color', 'rgba(0, 255, 0, 0.05)'); } }" ) ) )
    })
    
  
    output$column_controls_ui <- renderUI({
      full_results <- local_rv$normality_results; req(full_results); selected_row_index <- input$results_table_rows_selected
      if (is.null(selected_row_index) || nrow(full_results) == 0 || selected_row_index > nrow(full_results)) { return(p("Vyberte řádek v tabulce výše pro zobrazení ovládacích prvků.")) }
      selected_data <- full_results[selected_row_index, ]; req(".is_normal_bool" %in% names(selected_data), ".test_run" %in% names(selected_data), "Sloupec" %in% names(selected_data))
      col_name <- selected_data$Sloupec; is_normal <- selected_data$.is_normal_bool; current_test <- selected_data$.test_run
      print(paste("Normality Module: Zobrazuji ovládací prvky pro", col_name))
      test_choices <- c( "Automaticky (výchozí)" = "auto", "Shapiro-Wilk" = "shapiro", "Kolmogorov-Smirnov" = "ks", "Nespouštět test" = "none" ); current_override <- local_rv$user_test_override[[col_name]]; selected_test_value <- if (is.null(current_override)) "auto" else current_override
      transform_choices <- c( "-- Vyberte transformaci --" = "", "Logaritmická (log(x+1))" = "log", "Odmocninová (sqrt(x))" = "sqrt", "Box-Cox" = "boxcox" )
      
      tagList(
        h4(paste("Ovládací prvky pro sloupec:", col_name)),
    
        fluidRow( column(6, selectInput(ns("override_test_select"), "Zvolit jiný test normality:", choices = test_choices, selected = selected_test_value) ), column(6, style = "margin-top: 25px;", actionButton(ns("apply_override_btn"), "Přepsat test", icon("sync")) ) ),
       
        if (isTRUE(!is_normal)) { tagList( hr(), h5("Transformace dat"), p("Data nejsou normální, můžete zkusit transformaci."), fluidRow( column(6, selectInput(ns("transform_select"), "Metoda transformace:", choices = transform_choices, selected = "") ), column(6, style = "margin-top: 25px;", actionButton(ns("request_transform_btn"), "Aplikovat transformaci", icon("magic"), class = "btn-warning") ) ) ) }
        else { p("Data se zdají být normálně distribuovaná (nebo test nebyl spuštěn/selhal), transformace není nabízena.") }
      )
    }) 
    
    observeEvent(input$apply_override_btn, { req(input$results_table_rows_selected, local_rv$normality_results, nrow(local_rv$normality_results) > 0); selected_row_index <- input$results_table_rows_selected; if(selected_row_index > nrow(local_rv$normality_results)) return(); selected_col_name <- local_rv$normality_results$Sloupec[selected_row_index]; chosen_test <- input$override_test_select; print(paste("Normality Module: Override testu pro", selected_col_name, "na", chosen_test)); if (chosen_test == "auto") { local_rv$user_test_override[[selected_col_name]] <- NULL; showNotification(paste("Výběr testu pro", selected_col_name, "vrácen na auto."), type = "message") } else { local_rv$user_test_override[[selected_col_name]] <- chosen_test; showNotification(paste("Pro", selected_col_name, "bude použit test:", chosen_test), type = "message") } })
    transform_request_event <- eventReactive(input$request_transform_btn, { req(input$results_table_rows_selected, local_rv$normality_results, nrow(local_rv$normality_results) > 0); selected_row_index <- input$results_table_rows_selected; if(selected_row_index > nrow(local_rv$normality_results)) return(NULL); selected_col_name <- local_rv$normality_results$Sloupec[selected_row_index]; chosen_method <- input$transform_select; if (is.null(chosen_method) || chosen_method == "") { showNotification("Vyberte metodu transformace.", type = "warning"); return(NULL) }; print(paste("Normality Module: Požadavek na transformaci", selected_col_name, "metodou", chosen_method)); list( column = selected_col_name, method = chosen_method, timestamp = Sys.time() ) })
    
    return( list( results = normality_analysis, transform_request = transform_request_event ) )
  }) 
}