
library(shiny)
library(DT)
library(dplyr)
library(plotly)
library(scales)


outliersUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Analýza a Zpracování Odlehlých Hodnot"),
    p("Identifikujte a zpracujte odlehlé hodnoty ve vašich numerických datech pomocí Z-skóre. Upravte prahové hodnoty pomocí posuvníků."),
    fluidRow(
      column(6,
             sliderInput(ns("upper_z"), "Horní práh Z-skóre:", min = 1, max = 5, value = 3, step = 0.1)
      ),
      column(6,
             sliderInput(ns("lower_z"), "Dolní práh Z-skóre:", min = -5, max = -1, value = -3, step = 0.1)
      )
    ),
    hr(),
    h4("Přehled Odlehlých Hodnot"),
    DTOutput(ns("summary_table")),
    hr(),
    uiOutput(ns("column_details_ui")) # UI pro detaily a handling
  )
}


# --- Server Funkce Modulu ---
outliersServer <- function(id, reactive_data, reactive_col_types) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # --- KROK 1: Interní reaktivní hodnoty modulu ---
    local_rv <- reactiveValues(
      outlier_summary = NULL,
      modified_data = NULL  
    )
    

    observeEvent(reactive_data(), {
   
      print("Outlier Module: Detekována změna VSTUPNÍCH dat (reactive_data). Resetuji local_rv$modified_data.")
      local_rv$modified_data <- NULL

    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # --- KROK 2: Reaktivní výraz pro AKTUÁLNĚ ZOBRAZOVANÁ data ---
    current_data_for_display <- reactive({
      req(reactive_data())
      if (!is.null(local_rv$modified_data) && is.data.frame(local_rv$modified_data)) {
     
        return(local_rv$modified_data)
      } else {

        return(reactive_data())
      }
    })
    
    # --- KROK 3: Reaktivní výraz pro analýzu outlierů ---

    outlier_analysis <- reactive({

      req(current_data_for_display(), reactive_col_types(), input$lower_z, input$upper_z)
      df_to_analyze <- current_data_for_display() # <--- ZMĚNA: Používá aktuální data
      col_types <- reactive_col_types()
      lower_z_thresh <- input$lower_z
      upper_z_thresh <- input$upper_z
      
      # print("Outlier Module: Spouštím analýzu outlierů (na základě current_data_for_display)...") 
      
    
      if (!is.data.frame(col_types) || !("Column" %in% names(col_types)) || !("DetectedType" %in% names(col_types))) {
        print("Outlier Module: Chyba - col_types není validní data frame.")
        showNotification("Chyba: Typy sloupců nejsou správně definovány.", type="error")
        return(NULL) # Vracíme NULL při chybě
      }
      
      numeric_cols <- col_types %>% filter(DetectedType == "Numeric") %>% pull(Column)
      if(length(numeric_cols) == 0) {
        print("Outlier Module: Nenalezeny žádné numerické sloupce pro analýzu outlierů.")
        local_rv$outlier_summary <- data.frame(Zprava="Žádné numerické sloupce.") # Nastavíme summary pro zobrazení zprávy
        return(local_rv$outlier_summary) # Vrátíme toto summary
      }
      
      summary_list <- list()
      
      for (col_name in numeric_cols) {
        if (!col_name %in% names(df_to_analyze)) {
          # print(paste("Varování: Sloupec", col_name, "není v df_to_analyze."))
          next # Přeskočíme sloupec, který není v datech
        }
        
        series_orig <- df_to_analyze[[col_name]] 
        
        # Zajistíme, že data jsou numerická (i když by měla být dle col_types)
        if(!is.numeric(series_orig)) {
         
          series_num <- suppressWarnings(as.numeric(as.character(series_orig)))
          if(all(is.na(series_num[!is.na(series_orig)]))) {
            # print(paste("Varování: Sloupec", col_name, "nelze převést na numerický."))
            next
          }
          series_orig <- series_num
        }
        
        series <- series_orig[!is.na(series_orig)] 
        mean_val <- NA; sd_val <- NA; median_val <- NA
        lower_bound <- NA; upper_bound <- NA
        num_outliers <- 0; below_count <- 0; above_count <- 0
        
        if (length(series) >= 1) {
          mean_val <- mean(series, na.rm = TRUE)
          median_val <- median(series, na.rm = TRUE)
        }
        if (length(series) >= 2) {
          sd_val <- sd(series, na.rm = TRUE)
        }
        
        # Hranice a outliery počítáme jen pokud máme platné SD > 0
        if (!is.na(sd_val) && sd_val > 1e-9) { # Použijeme malou toleranci pro sd blízko nule
          lower_bound <- mean_val + lower_z_thresh * sd_val
          upper_bound <- mean_val + upper_z_thresh * sd_val

          outlier_mask_orig <- series_orig < lower_bound | series_orig > upper_bound
          num_outliers <- sum(outlier_mask_orig, na.rm = TRUE)
          below_count <- sum(series_orig < lower_bound, na.rm = TRUE)
          above_count <- sum(series_orig > upper_bound, na.rm = TRUE)
        } else {
    
          num_outliers <- 0
          below_count <- 0
          above_count <- 0
   
        }
        
        summary_list[[col_name]] <- data.frame(
          Sloupec = col_name,
          Celkem.Hodnot = length(series_orig), 
          Průměr = mean_val,
          SD = sd_val,
          Medián = median_val,
          Dolní.Hranice = lower_bound,
          Horní.Hranice = upper_bound,
          Počet.Outlierů = as.integer(num_outliers), 
          Pod.Hranicí = as.integer(below_count),
          Nad.Hranicí = as.integer(above_count),
          stringsAsFactors = FALSE
        )
      } 
      if (length(summary_list) > 0) {
        summary_df <- bind_rows(summary_list)
        # print("Outlier Module: Analýza outlierů dokončena (na základě current_data_for_display).") 
        local_rv$outlier_summary <- summary_df
        return(summary_df)
      } else {
        print("Outlier Module: Žádné sloupce nebyly analyzovány na outliery (možná jen nenumerické?).")
        local_rv$outlier_summary <- data.frame(Zprava="Žádné numerické sloupce k analýze nebo chyba.")
        return(local_rv$outlier_summary)
      }
    })
    
    
    # --- KROK 4: Zobrazení přehledové tabulky ---
 
    output$summary_table <- renderDT({
  
      summary_df <- outlier_analysis() 
      
    
      if (is.null(summary_df) || !is.data.frame(summary_df) || nrow(summary_df) == 0 || "Zprava" %in% names(summary_df)) {
   
        msg <- if (!is.null(summary_df) && "Zprava" %in% names(summary_df)) summary_df$Zprava[1] else "Probíhá výpočet nebo nejsou data..."
        return(datatable(data.frame(Zprava=msg), options = list(searching=FALSE, paging=FALSE, info=FALSE), rownames=FALSE))
      }

      display_df <- summary_df %>%
        mutate(across(where(is.numeric) & !starts_with("Počet") & !starts_with("Celkem"), ~round(.x, 2))) %>%
        mutate(across(starts_with("Počet") | starts_with("Celkem"), ~as.integer(.x)))
      colnames(display_df) <- c('Sloupec', 'Celkem Hodnot', 'Průměr', 'Směr. Odch.', 'Medián', 'Dolní Hranice (Z)', 'Horní Hranice (Z)', 'Outlierů Celkem', 'Pod Hranicí', 'Nad Hranicí')
      
      datatable(
        display_df,
        selection = 'single',
        rownames = FALSE,
        options = list(
          searching = TRUE,
          pageLength = 5,
          lengthMenu = c(5, 10, 25, -1),
          scrollX = TRUE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Czech.json')
        )
      )
    })
    

    output$column_details_ui <- renderUI({
      
      summary_res <- local_rv$outlier_summary 
      req(summary_res) 
     
      if("Zprava" %in% names(summary_res)) return(NULL)
      
      selected_row_index <- input$summary_table_rows_selected
      
      if (is.null(selected_row_index)) {
        return(p("Vyberte řádek v přehledové tabulce pro zobrazení detailů a možností zpracování."))
      }
    
      req(nrow(summary_res) >= selected_row_index)
      
      selected_col_name <- summary_res$Sloupec[selected_row_index]

      num_outliers_selected <- summary_res$`Počet.Outlierů`[selected_row_index]
      
      tagList(
        hr(),
        h4(paste("Detaily a Zpracování pro Sloupec:", selected_col_name)),
        fluidRow(
          column(6, h5("Box Plot a Outliery"), plotlyOutput(ns("outlier_boxplot")) ),
          column(6,
                 h5("Identifikované Odlehlé Hodnoty"), DTOutput(ns("outlier_details_table")), hr(),
                 # Zobrazíme handling jen pokud AKTUÁLNĚ existují outliery
                 if(num_outliers_selected > 0) {
                   tagList(
                     h5("Zpracování Odlehlých Hodnot"),
                     selectInput(ns("handling_strategy"), "Strategie:",
                                 choices = c("Nahradit Průměrem" = "replace_mean",
                                             "Nahradit Mediánem" = "replace_median",
                                             "Omezit na Hranice (Clip/Squish)" = "clip",
                                             "Nahradit Vlastní Hodnotou" = "replace_custom"
                                       
                                 )),
                     conditionalPanel(
                       condition = paste0("input['", ns("handling_strategy"), "'] == 'replace_custom'"),
                       numericInput(ns("custom_replacement_value"), "Vlastní hodnota:", value = 0)
                     ),
              
                     actionButton(ns("apply_handling_btn"), "Aplikovat Strategii na Tento Sloupec", icon("check"), class="btn-primary"),
                   )
                 } else {
                   p("V tomto sloupci nejsou podle aktuálních kritérií žádné odlehlé hodnoty k zpracování.")
                 }
          )
        )
      )
    })
    
    

    output$outlier_boxplot <- renderPlotly({
    
      req(local_rv$outlier_summary)
  
      if("Zprava" %in% names(local_rv$outlier_summary)) return(plotly_empty() %>% layout(title="Chyba v datech nebo nejsou data"))
      
      selected_row_index <- input$summary_table_rows_selected
      req(selected_row_index)
      req(nrow(local_rv$outlier_summary) >= selected_row_index) 
      selected_col_name <- local_rv$outlier_summary$Sloupec[selected_row_index]
      
     
      plot_data_full <- current_data_for_display() 
      req(plot_data_full, selected_col_name %in% names(plot_data_full)) 
      
      plot_data_values <- plot_data_full[[selected_col_name]]

      if(!is.numeric(plot_data_values)) {
        plot_data_values <- suppressWarnings(as.numeric(as.character(plot_data_values)))
        if(all(is.na(plot_data_values))) {
          warning(paste("Data pro boxplot sloupce", selected_col_name, "nelze převést na numerická!"))
          return(plotly_empty() %>% layout(title=paste("Chyba: Data sloupce", selected_col_name, "nejsou numerická.")))
        }
      }
      
      values_no_na <- plot_data_values[!is.na(plot_data_values)]
      if(length(values_no_na) == 0) return(plotly_empty() %>% layout(title=paste("Box Plot pro", selected_col_name, "(žádné hodnoty)")))
      
 
      col_summary_plot <- local_rv$outlier_summary %>% filter(Sloupec == selected_col_name)
   
      lower_bound_plot <- col_summary_plot$Dolní.Hranice[1]
      upper_bound_plot <- col_summary_plot$Horní.Hranice[1]

      is_outlier <- rep(FALSE, length(plot_data_values)) 
    
      if(!is.na(lower_bound_plot)) {
        is_outlier <- is_outlier | (plot_data_values < lower_bound_plot)
      }
      if(!is.na(upper_bound_plot)) {
        is_outlier <- is_outlier | (plot_data_values > upper_bound_plot)
      }
      is_outlier[is.na(is_outlier)] <- FALSE # NA hodnoty nejsou outliery
      
      plot_df <- data.frame(

        value = plot_data_values,
        outlier_flag = is_outlier
    
      )
   
      plot_df$hover_text = paste("Hodnota:", round(plot_df$value, 3))
      
      
    
      p <- plot_ly(plot_df, x = selected_col_name, y = ~value, type = 'box', name = selected_col_name,
                   boxpoints = 'suspectedoutliers', # Zobrazí podezřelé outliery dle IQR
              
                   marker = list(color = 'rgba(90, 150, 200, 0.7)'),
                   line = list(color = 'rgba(90, 150, 200, 1)')
      )
      
      # Přidáme body identifikované naším Z-skóre kritériem
      outliers_to_plot <- filter(plot_df, outlier_flag == TRUE)
      if(nrow(outliers_to_plot) > 0) {
        p <- p %>% add_trace(data = outliers_to_plot,
                             x = selected_col_name, y = ~value, type = 'scatter', mode = 'markers',
                             name = 'Outliers (Z-skóre)', text = ~hover_text, hoverinfo = 'text',
                             marker = list(color = 'rgba(255, 60, 60, 0.8)', size = 8, symbol = 'x') # Zvýrazníme je
        )
      }
      
      p %>% layout( title = paste("Box Plot pro", selected_col_name),
                    xaxis = list(title = ""), yaxis = list(title = "Hodnota"),
                    showlegend = (nrow(outliers_to_plot) > 0) # Zobraz legendu jen pokud jsou Z-score outliery
      )
    })
    
    # --- KROK 4: Zobrazení tabulky s detaily outlierů ---

    output$outlier_details_table <- renderDT({
    
      req(local_rv$outlier_summary)

      if("Zprava" %in% names(local_rv$outlier_summary)) return(datatable(data.frame(Zprava="Chyba v datech nebo nejsou data."), options=list(dom='t', searching=FALSE), rownames=FALSE))
      
      selected_row_index <- input$summary_table_rows_selected
      req(selected_row_index)
      req(nrow(local_rv$outlier_summary) >= selected_row_index) 
      selected_col_name <- local_rv$outlier_summary$Sloupec[selected_row_index]
      

      data_full <- current_data_for_display() 
      req(data_full, selected_col_name %in% names(data_full))
      
      data_col <- data_full[[selected_col_name]]
  
      if(!is.numeric(data_col)) {
        data_col <- suppressWarnings(as.numeric(as.character(data_col)))
        if(all(is.na(data_col))) {
          warning(paste("Data pro detailní tabulku sloupce", selected_col_name, "nelze převést na numerická!"))
          return(datatable(data.frame(Zprava=paste("Data sloupce", selected_col_name, "nejsou numerická.")), options=list(dom='t', searching=FALSE), rownames=FALSE))
        }
      }
      
      values_no_na <- data_col[!is.na(data_col)]
      if(length(values_no_na) == 0) return(datatable(data.frame(Zprava="Žádná data k zobrazení."), options=list(dom='t', searching=FALSE), rownames=FALSE))
      
      
      col_summary_detail <- local_rv$outlier_summary %>% filter(Sloupec == selected_col_name)
      mean_val <- col_summary_detail$Průměr[1]; sd_val <- col_summary_detail$SD[1]
      lower_bound_detail <- col_summary_detail$Dolní.Hranice[1]
      upper_bound_detail <- col_summary_detail$Horní.Hranice[1]
      
      # Identifikujeme outliery v AKTUÁLNÍCH datech podle AKTUÁLNÍCH hranic
      is_outlier_detail <- rep(FALSE, length(data_col))
      if(!is.na(lower_bound_detail)) {
        is_outlier_detail <- is_outlier_detail | (data_col < lower_bound_detail)
      }
      if(!is.na(upper_bound_detail)) {
        is_outlier_detail <- is_outlier_detail | (data_col > upper_bound_detail)
      }
      is_outlier_detail[is.na(is_outlier_detail)] <- FALSE # NA nejsou outliery
      
      outlier_indices <- which(is_outlier_detail)
      
 
      if (length(outlier_indices) == 0) {
        return(datatable(data.frame(Zprava="Nebyly nalezeny žádné odlehlé hodnoty podle zadaných prahů (v aktuálních datech)."), options=list(dom='t', searching=FALSE, paging=FALSE, info=FALSE), rownames=FALSE))
      }
      
    
      outlier_values <- data_col[outlier_indices]
      outlier_zscores <- numeric(length(outlier_values)) 
      # Počítáme Z-skóre jen pokud máme platný průměr a SD > 0
      if(!is.na(mean_val) && !is.na(sd_val) && sd_val > 1e-9) {
        outlier_zscores <- (outlier_values - mean_val) / sd_val
      } else {
        outlier_zscores <- rep(NA_real_, length(outlier_values))
      }
      
      outlier_df <- data.frame(
        Index = outlier_indices, 
        Hodnota = outlier_values,
        `Z.skóre` = round(outlier_zscores, 3)
      )
      
      datatable(outlier_df, rownames = FALSE, colnames = c('Index Řádku', 'Hodnota', 'Z-skóre'),
                options = list(pageLength = 5, scrollY = "150px", searching=FALSE, info=FALSE, paging=TRUE, dom='tip', 
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Czech.json'))
      )
    })
    

    observeEvent(input$apply_handling_btn, {
     
      req(current_data_for_display(), local_rv$outlier_summary) 
    
      if("Zprava" %in% names(local_rv$outlier_summary)) {
        showNotification("Chyba: Nelze aplikovat handling, problém se shrnutím outlierů.", type="error")
        return()
      }
      
      data_to_modify <- current_data_for_display() 
      
      strategy <- input$handling_strategy
     
      
 
      summary_for_handling <- local_rv$outlier_summary
      
      
      selected_row_index <- input$summary_table_rows_selected
      req(selected_row_index, nrow(summary_for_handling) >= selected_row_index)
      target_col <- summary_for_handling$Sloupec[selected_row_index]
      
  
      col_summary <- summary_for_handling %>% filter(Sloupec == target_col)
      if(nrow(col_summary) == 0) {
        showNotification(paste("Chyba: Nenalezeno shrnutí pro sloupec", target_col), type="error")
        return()
      }
      mean_val <- col_summary$Průměr[1]; median_val <- col_summary$Medián[1]
      lower_bound <- col_summary$Dolní.Hranice[1]; upper_bound <- col_summary$Horní.Hranice[1]
     
      custom_val <- NULL
      if(strategy == "replace_custom") {
       
        custom_val_input <- input$custom_replacement_value
        req(custom_val_input)
        
        if(!is.numeric(custom_val_input)){
          showNotification("Vlastní hodnota pro nahrazení musí být číslo.", type="error")
          return() 
        }
        custom_val <- custom_val_input 
      }
      
      print(paste("Outlier Module: Aplikuji strategii", strategy, "na sloupec:", target_col)) 
      showModal(modalDialog("Zpracovávám odlehlé hodnoty...", footer = NULL))
      
      data_modified <- data_to_modify 
      rows_affected_total <- 0
      
      
      result <- tryCatch({
        series_orig <- data_modified[[target_col]]
        
       
        if(!is.numeric(series_orig)) {
          series_num <- suppressWarnings(as.numeric(as.character(series_orig)))
          if(all(is.na(series_num[!is.na(series_orig)]))) {
            stop(paste("Sloupec", target_col, "není numerický a nelze ho zpracovat."))
          }
          series_orig <- series_num
          data_modified[[target_col]] <- series_orig 
        }
        
   
        outlier_mask <- rep(FALSE, length(series_orig))
        if(!is.na(lower_bound)) {
          outlier_mask <- outlier_mask | (series_orig < lower_bound)
        }
        if(!is.na(upper_bound)) {
          outlier_mask <- outlier_mask | (series_orig > upper_bound)
        }
        outlier_mask[is.na(series_orig)] <- FALSE 
        
        rows_affected_col <- sum(outlier_mask)
        
        if (rows_affected_col == 0) {
  
        } else {
        
          if (strategy == "replace_mean") {

            
            # --- NOVÝ KÓD PRO PRŮMĚR ---
            series_non_outliers <- series_orig[!outlier_mask & !is.na(series_orig)]
            if (length(series_non_outliers) == 0) {
              stop(paste("Nelze nahradit průměrem pro sloupec", target_col, "- všechny hodnoty jsou outliery nebo NA."))
            }
            replacement_mean <- mean(series_non_outliers, na.rm = TRUE) 
            if(is.na(replacement_mean)) stop(paste("Výpočet průměru non-outlier hodnot selhal pro", target_col))
            print(paste("Nahrazuji průměrem (z non-outliers):", replacement_mean)) 
            data_modified[[target_col]][outlier_mask] <- replacement_mean

            
          } else if (strategy == "replace_median") {

      
            series_non_outliers <- series_orig[!outlier_mask & !is.na(series_orig)]
            if (length(series_non_outliers) == 0) {
              stop(paste("Nelze nahradit mediánem pro sloupec", target_col, "- všechny hodnoty jsou outliery nebo NA."))
            }
            replacement_median <- median(series_non_outliers, na.rm = TRUE) 
            if(is.na(replacement_median)) stop(paste("Výpočet mediánu non-outlier hodnot selhal pro", target_col))
            print(paste("Nahrazuji mediánem (z non-outliers):", replacement_median)) 
            data_modified[[target_col]][outlier_mask] <- replacement_median
         
            
          } else if (strategy == "replace_custom") {
       
            data_modified[[target_col]][outlier_mask] <- custom_val
          } else if (strategy == "clip") {
            
            if(is.na(lower_bound) || is.na(upper_bound)) stop(...)
            data_modified[[target_col]] <- scales::oob_squish(series_orig, range=c(lower_bound, upper_bound))
            rows_affected_col <- sum(series_orig != data_modified[[target_col]], na.rm = TRUE) 
          }
          rows_affected_total <- rows_affected_col 
        } 
        
        
        removeModal()
        showNotification(paste0("Strategie '", strategy, "' aplikována na sloupec '", target_col, "'. ", rows_affected_total, " hodnot upraveno."), type = "message", duration=5)
        
 
        local_rv$modified_data <- data_modified
   
        return(TRUE) 
        
      }, error = function(e) {
        removeModal()
        print(paste("!!! Outlier Handling Error:", e$message)) 
        showNotification(
          paste("Chyba při zpracování outlierů:", e$message),
          type = "error", duration = 10
        )
   
        return(FALSE) 
      }) 
      
    }) 
   
    handled_data_output <- reactive({
  
      local_rv$modified_data

    })
    

    return(
      list(
      
        summary = outlier_analysis, 
        handled_data = handled_data_output 
      )
    )
    
  }) # Konec moduleServer
}