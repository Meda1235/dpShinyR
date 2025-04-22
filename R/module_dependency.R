
library(shiny)
library(dplyr)
library(tidyr) 
library(DT)
library(plotly)
library(httr)
library(jsonlite)
library(stats) 
library(shinycssloaders)
library(rlang) 
library(car)   


formatPValueR <- function(pValue) {
  if (!is.numeric(pValue) || is.na(pValue)) return('-')
  if (pValue < 0.001) return(format(pValue, scientific = TRUE, digits = 2))
  return(format(round(pValue, 3), nsmall = 3))
}

formatStatR <- function(stat) {
  if (!is.numeric(stat) || is.na(stat)) return('-')
  return(format(round(stat, 3), nsmall = 3))
}

# --- UI Funkce Modulu ---
dependencyTestUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Test Závislosti / Porovnání Skupin"),
    p("Vyberte proměnné (kategoriální a/nebo číselné) a metodu testování."),
    fluidRow(
      # --- Vstupní část ---
      column(4,
             wellPanel(
               tags$b("1. Výběr proměnných (min. 2)"),
               uiOutput(ns("column_select_ui")),
               tags$hr(),
               tags$b("2. Výběr metody"),
               selectInput(ns("method"), "Metoda testování:",
                           choices = c("Automaticky (doporučeno)" = "auto",
                                       "Chí-kvadrát (χ²)" = "chi2",
                                       "Fisherův přesný test" = "fisher",
                                       "ANOVA" = "anova",
                                       "Kruskal-Wallis" = "kruskal",
                                       "Studentův t-test" = "t.test",
                                       "Wilcoxonův test" = "wilcoxon",
                                       "Mann-Whitney U test" = "mannwhitney"),
                           selected = "auto"),
               checkboxInput(ns("paired"), "Párová data (pro t-test/Wilcoxon mezi 2 numerickými)", value = FALSE),
               tags$hr(),
               actionButton(ns("run_analysis"), "Spustit Analýzu", icon = icon("play"), class = "btn-primary"),
               uiOutput(ns("analysis_loading_ui"))
             )
      ), # konec column 4
      
      # --- Výstupní část ---
      column(8,
             h4("Výsledky Analýzy"),
             uiOutput(ns("analysis_error_ui")),
             conditionalPanel(
               condition = "output.showDependencyResults === true",
               ns = ns,
               tagList(
                 wellPanel(style = "background-color: #f8f9fa;", uiOutput(ns("test_summary_ui"))),
                 tags$h5("Detailní výsledky"),
                 uiOutput(ns("results_detail_ui")) %>% withSpinner(type = 4, color="#0dc5c1"),
                 # --- ZMĚNA: Přidání placeholderu pro graf ---
                 tags$h5("Vizualizace"),
                 plotlyOutput(ns("dependency_plot")) %>% withSpinner(type = 4, color="#0dc5c1"),
                 # --- KONEC ZMĚNY ---
                 tags$hr(), tags$h5("AI Interpretace"),
                 uiOutput(ns("ai_error_ui")),
                 uiOutput(ns("ai_interpretation_ui"))
               ) # konec tagList
             ) # konec conditionalPanel
      ) # konec column 8
    ) # konec fluidRow
  ) # konec tagList
}


# --- Server Funkce Modulu ---
dependencyTestServer <- function(id, reactive_data, reactive_col_types) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      results = NULL,
      analysis_error = NULL,
      is_analyzing = FALSE,
      ai_interpretation = NULL,
      ai_error = NULL,
      is_interpreting = FALSE
    )
    
    # --- Dynamické checkboxy ---
    output$column_select_ui <- renderUI({
      req(reactive_col_types())
      col_info <- reactive_col_types()
      numeric_cols <- col_info$Column[col_info$DetectedType == "Numeric"]
      categorical_cols <- col_info$Column[col_info$DetectedType %in% c("Categorical", "Factor", "Character")]
      
      no_num_msg <- if(length(numeric_cols) == 0) p("Žádné numerické sloupce.", class="text-muted") else NULL
      no_cat_msg <- if(length(categorical_cols) == 0) p("Žádné kategoriální sloupce.", class="text-muted") else NULL
      
      tagList(
        fluidRow(
          column(6,
                 tags$p(tags$strong("Kategoriální proměnné:")),
                 no_cat_msg,
                 checkboxGroupInput(ns("selected_cat_cols"), label = NULL,
                                    choices = setNames(categorical_cols, categorical_cols),
                                    selected = isolate(intersect(input$selected_cat_cols, categorical_cols)))
          ),
          column(6,
                 tags$p(tags$strong("Numerické proměnné:")),
                 no_num_msg,
                 checkboxGroupInput(ns("selected_num_cols"), label = NULL,
                                    choices = setNames(numeric_cols, numeric_cols),
                                    selected = isolate(intersect(input$selected_num_cols, numeric_cols)))
          )
        )
      )
    })
    
    # --- Indikátor načítání ---
    output$analysis_loading_ui <- renderUI({
      if (rv$is_analyzing) { tags$div(style="display: inline-block; margin-left: 15px;", icon("spinner", class = "fa-spin", style="color: #007bff;")) } else { NULL }
    })
    
    # --- Reset výsledků ---
    observeEvent(c(input$selected_cat_cols, input$selected_num_cols, input$method, input$paired), {
      if(is.null(rv$results) ||
         !identical(input$method, isolate(rv$results$input_method %||% input$method)) ||
         !identical(input$paired, isolate(rv$results$input_paired %||% input$paired)))
      {
        cat("DEBUG DEPENDENCY: Input changed, resetting results.\n") # DEBUG
        rv$results <- NULL
        rv$analysis_error <- NULL
        rv$ai_interpretation <- NULL
        rv$ai_error <- NULL
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # --- Spuštění Analýzy ---
    observeEvent(input$run_analysis, {
      cat("\n--- run_dependency_analysis START ---\n") # DEBUG
      rv$analysis_error <- NULL; rv$results <- NULL; rv$ai_interpretation <- NULL; rv$ai_error <- NULL
      rv$is_analyzing <- TRUE
      
      selected_cat <- input$selected_cat_cols
      selected_num <- input$selected_num_cols
      selected_cols <- c(selected_cat, selected_num)
      method <- input$method
      paired <- input$paired
      
      if (length(selected_cols) < 2) {
        rv$analysis_error <- "Chyba: Vyberte alespoň dvě proměnné."; rv$is_analyzing <- FALSE; return()
      }
      cat("DEBUG DEPENDENCY: Inputs OK (Cols:", paste(selected_cols, collapse=", "), "Method:", method, "Paired:", paired, ")\n") # DEBUG
      
      req(reactive_data(), reactive_col_types())
      data_in <- reactive_data()
      col_types_df <- reactive_col_types()
      
      analysis_status <- tryCatch({
        cat("DEBUG DEPENDENCY: Inside tryCatch - Preparing data...\n") # DEBUG
        
        selected_types_info <- col_types_df %>% dplyr::filter(Column %in% selected_cols)
        n_selected <- length(selected_cols)
        n_cat <- sum(selected_types_info$DetectedType %in% c("Categorical", "Factor", "Character"))
        n_num <- sum(selected_types_info$DetectedType == "Numeric")
        
        cat(sprintf("DEBUG DEPENDENCY: Selected breakdown: %d total, %d categorical, %d numeric\n", n_selected, n_cat, n_num))
        
        sub_df <- data_in %>%
          dplyr::select(all_of(selected_cols))
        
        if (n_num > 0) {
          num_cols_to_convert <- selected_types_info$Column[selected_types_info$DetectedType == "Numeric"]
          sub_df <- sub_df %>%
            dplyr::mutate(across(all_of(num_cols_to_convert), ~as.numeric(as.character(.))))
        }
        if (n_cat > 0) {
          cat_cols_to_convert <- selected_types_info$Column[selected_types_info$DetectedType %in% c("Categorical", "Factor", "Character")]
          sub_df <- sub_df %>%
            dplyr::mutate(across(all_of(cat_cols_to_convert), as.factor))
        }
        
        sub_df_complete <- na.omit(sub_df)
        n_complete <- nrow(sub_df_complete)
        cat("DEBUG DEPENDENCY: Data prepared after NA omit. Rows:", n_complete, "\n") # DEBUG
        if (n_complete < 5) {
          stop(paste("Nedostatek kompletních pozorování po odstranění NA (nalezeno:", n_complete, "). Minimum je 5."))
        }
        
        actual_method <- method
        reason <- ""
        cat_cols_in_scope <- selected_types_info$Column[selected_types_info$DetectedType %in% c("Categorical", "Factor", "Character")]
        num_cols_in_scope <- selected_types_info$Column[selected_types_info$DetectedType == "Numeric"]
        
        if (method == "auto") {

          cat("DEBUG DEPENDENCY: Auto method selected. Determining test...\n")
          if (n_cat == 2 && n_num == 0) {
            cat_col1 <- cat_cols_in_scope[1]
            cat_col2 <- cat_cols_in_scope[2]
    
            cont_table_check <- table(sub_df_complete[[cat_col1]], sub_df_complete[[cat_col2]])
            expected_freq <- tryCatch(suppressWarnings(chisq.test(cont_table_check))$expected, error = function(e) NULL)
            if(!is.null(expected_freq) && any(expected_freq < 5)) {
              actual_method <- "fisher"; reason <- "Auto: 2 kategoriální (očekávané < 5) -> Fisherův test"
            } else {
              actual_method <- "chi2"; reason <- "Auto: 2 kategoriální (očekávané >= 5) -> χ² test"
            }
          }
          else if (n_cat >= 2 && n_num == 0) { actual_method <- "chi2"; reason <- "Auto: >2 kategoriální -> χ² test (mezi prvními dvěma)" }
          else if (n_cat == 1 && n_num == 1) {
            cat_col_name <- cat_cols_in_scope[1]
            num_col_name <- num_cols_in_scope[1]
            num_levels <- length(levels(sub_df_complete[[cat_col_name]]))
            
            if(num_levels == 2) {
              groups <- split(sub_df_complete[[num_col_name]], sub_df_complete[[cat_col_name]])
              normality_p_group1 <- if(length(groups[[1]]) >= 3) tryCatch(shapiro.test(groups[[1]])$p.value, error = function(e) 0) else 0
              normality_p_group2 <- if(length(groups[[2]]) >= 3) tryCatch(shapiro.test(groups[[2]])$p.value, error = function(e) 0) else 0
              levene_p <- tryCatch(car::leveneTest(as.formula(paste0("`", num_col_name, "` ~ `", cat_col_name, "`")), data = sub_df_complete)$`Pr(>F)`[1], error=function(e) 0)
              
              if (normality_p_group1 > 0.05 && normality_p_group2 > 0.05 && levene_p > 0.05) {
                actual_method <- "t.test"; reason <- "Auto: 1 kat (2 úrovně, normální, homosked.) + 1 num -> t-test"
              } else {
                actual_method <- "mannwhitney"; reason <- "Auto: 1 kat (2 úrovně, nenormální/heterosked.) + 1 num -> Mann-Whitney U test"
              }
            } else if (num_levels > 2) {
              normality_p <- if(n_complete >= 3) tryCatch(shapiro.test(sub_df_complete[[num_col_name]])$p.value, error = function(e) 0) else 0
              if (normality_p > 0.05) { actual_method <- "anova"; reason <- "Auto: 1 kat (>2 úrovně, normální) + 1 num -> ANOVA" }
              else { actual_method <- "kruskal"; reason <- "Auto: 1 kat (>2 úrovně, nenormální) + 1 num -> Kruskal-Wallis" }
            } else {
              stop("Auto: Kategoriální proměnná má méně než 2 úrovně.")
            }
          }
          else if (n_cat == 1 && n_num > 1) { actual_method <- "anova"; reason <- "Auto: 1 kat + více num -> ANOVA (pro každý num)" }
          else if (n_cat == 0 && n_num == 2) {
            num1_norm_p <- if(length(sub_df_complete[[selected_cols[1]]])>=3) tryCatch(shapiro.test(sub_df_complete[[selected_cols[1]]])$p.value, error=function(e) 0) else 0
            num2_norm_p <- if(length(sub_df_complete[[selected_cols[2]]])>=3) tryCatch(shapiro.test(sub_df_complete[[selected_cols[2]]])$p.value, error=function(e) 0) else 0
            if (num1_norm_p > 0.05 && num2_norm_p > 0.05) {
              actual_method <- "t.test"; reason = paste("Auto: 2 num (normální) -> ", if(paired) "Párový t-test" else "Nepárový t-test")
            } else {
              actual_method <- if(paired) "wilcoxon" else "mannwhitney"; reason = paste("Auto: 2 num (nenormální) ->", if(paired) "Wilcoxonův párový test" else "Mann-Whitney U test")
            }
          }
          else { stop("Auto: Tato kombinace vstupů není automaticky podporována.") }
          cat("DEBUG DEPENDENCY: Auto selected test:", actual_method, "\n")
        }
        
        test_output <- list()
        test_output$columns <- selected_cols
        test_output$input_method <- method
        test_output$input_paired <- paired
        test_output$test_name <- actual_method
        test_output$reason <- reason
        test_output$warning_message <- NULL
   
        test_output$plot_data_df <- sub_df_complete
        test_output$n_cat_used <- n_cat
        test_output$n_num_used <- n_num
        test_output$cat_cols_used <- cat_cols_in_scope
        test_output$num_cols_used <- num_cols_in_scope

        
        
    
        if (actual_method == "chi2") {
   
          if (n_cat < 2 || n_num > 0) stop("Chí-kvadrát test vyžaduje alespoň 2 kategoriální proměnné a žádné numerické.")
          cat_col1 <- cat_cols_in_scope[1]
          cat_col2 <- cat_cols_in_scope[2]
          if (n_complete < 5) stop("Nedostatek dat pro chí-kvadrát test.")
          cont_table <- table(sub_df_complete[[cat_col1]], sub_df_complete[[cat_col2]])
          expected_freq <- tryCatch(suppressWarnings(chisq.test(cont_table))$expected, error = function(e) NULL)
          if (!is.null(expected_freq) && any(expected_freq < 5)) {
            test_output$warning_message <- "Některé očekávané frekvence jsou < 5. Výsledky chí-kvadrát testu mohou být nepřesné. Zvažte Fisherův test."
          }
          chi_test <- suppressWarnings(chisq.test(cont_table, correct = FALSE))
          test_output$test_name <- "Chí-kvadrát (χ²)"
          test_output$statistic <- chi_test$statistic
          test_output$p_value <- chi_test$p.value
          test_output$degrees_freedom <- chi_test$parameter
          test_output$statistic_name <- "χ²"
  
          test_output$contingency_table_long <- as.data.frame(cont_table)

          test_output$contingency_table <- test_output$contingency_table_long %>%
            tidyr::pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
            dplyr::rename(!!cat_col1 := Var1)
          
        } else if (actual_method == "fisher") {

          if (n_cat != 2 || n_num > 0) stop("Fisherův test vyžaduje přesně 2 kategoriální proměnné.")
          cat_col1 <- cat_cols_in_scope[1]
          cat_col2 <- cat_cols_in_scope[2]
          cont_table <- table(sub_df_complete[[cat_col1]], sub_df_complete[[cat_col2]])
          fisher_test <- fisher.test(cont_table)
          test_output$test_name <- "Fisherův přesný test"
          test_output$p_value <- fisher_test$p.value
          if(all(dim(cont_table) == 2)) {
            test_output$statistic <- fisher_test$estimate
            test_output$statistic_name <- "Odds Ratio"
          }
  
          test_output$contingency_table_long <- as.data.frame(cont_table)
  
          test_output$contingency_table <- test_output$contingency_table_long %>%
            tidyr::pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
            dplyr::rename(!!cat_col1 := Var1)
          
        } else if (actual_method == "anova") {
     
          if (n_cat < 1 || n_num < 1) stop("ANOVA vyžaduje alespoň 1 kategoriální a 1 numerickou proměnnou.")
          cat_cols_selected <- cat_cols_in_scope
          num_cols_selected <- num_cols_in_scope
          results_anova <- list()
          for(c_col in cat_cols_selected) {
            if(length(levels(sub_df_complete[[c_col]])) < 2) next
            for(n_col in num_cols_selected) {
              formula_str <- paste0("`", n_col, "` ~ `", c_col, "`")
              aov_fit <- tryCatch(aov(as.formula(formula_str), data = sub_df_complete), error = function(e) NULL)
              if (!is.null(aov_fit)) {
                summary_aov <- summary(aov_fit)
                f_value <- summary_aov[[1]]$`F value`[1]
                p_value <- summary_aov[[1]]$`Pr(>F)`[1]
                df1 <- summary_aov[[1]]$Df[1]
                df2 <- summary_aov[[1]]$Df[2]
                results_anova[[length(results_anova) + 1]] <- list(
                  cat_col = c_col,
                  num_col = n_col,
                  statistic = f_value,
                  p_value = p_value,
                  df1 = df1,
                  df2 = df2
                )
              }
            }
          }
          if(length(results_anova) == 0) stop("Nepodařilo se provést ANOVA (možná nedostatek skupin?).")
          test_output$test_name <- "ANOVA"
          test_output$statistic_name <- "F"
          test_output$anova_results <- bind_rows(results_anova)
          
        } else if (actual_method == "kruskal") {
 
          if (n_cat == 1 && n_num >= 1) {
            cat_col_name <- cat_cols_in_scope[1]
            num_cols_selected <- num_cols_in_scope
            if(length(levels(sub_df_complete[[cat_col_name]])) < 2) stop("Kategoriální proměnná musí mít alespoň 2 úrovně pro Kruskal-Wallis.")
            results_kruskal <- list()
            for(n_col in num_cols_selected) {
              formula_str <- paste0("`", n_col, "` ~ `", cat_col_name, "`")
              kruskal_test <- tryCatch(kruskal.test(as.formula(formula_str), data = sub_df_complete), error = function(e) NULL)
              if (!is.null(kruskal_test)) {
                results_kruskal[[length(results_kruskal) + 1]] <- list(
                  cat_col = cat_col_name,
                  num_col = n_col,
                  statistic = kruskal_test$statistic,
                  p_value = kruskal_test$p.value,
                  df = kruskal_test$parameter 
                )
              }
            }
            if(length(results_kruskal) == 0) stop("Nepodařilo se provést Kruskal-Wallis.")
            test_output$test_name <- "Kruskal-Wallis (podle skupin)"
            test_output$statistic_name <- "Kruskal-Wallis χ²"
            test_output$anova_results <- bind_rows(results_kruskal) 
          } else {
            stop("Kruskal-Wallis test vyžaduje 1 kategoriální (s min. 2 úrovněmi) a 1+ numerické.")
          }
          
        } else if (actual_method == "t.test") {
 
          if (n_num == 2 && n_cat == 0) {
            num1_col <- num_cols_in_scope[1]
            num2_col <- num_cols_in_scope[2]
            num1 <- sub_df_complete[[num1_col]]
            num2 <- sub_df_complete[[num2_col]]
            t_test <- t.test(num1, num2, paired = paired)
            test_output$test_name <- paste("Studentův t-test", if(paired) "(párový)" else "(nepárový)")
            test_output$statistic <- t_test$statistic
            test_output$p_value <- t_test$p.value
            test_output$degrees_freedom <- t_test$parameter
            test_output$statistic_name <- "t"
          } else if (n_num == 1 && n_cat == 1) {
            cat_col_name <- cat_cols_in_scope[1]
            num_col_name <- num_cols_in_scope[1]
            factor_var <- sub_df_complete[[cat_col_name]]
            if (length(levels(factor_var)) != 2) {
              stop("Pro t-test s kategoriální proměnnou musí mít tato proměnná přesně 2 úrovně.")
            }
            if (paired) stop("Párový t-test nelze použít pro porovnání skupin definovaných kategoriální proměnnou.")
            formula_str <- paste0("`", num_col_name, "` ~ `", cat_col_name, "`")
            levene_p <- tryCatch(car::leveneTest(as.formula(formula_str), data = sub_df_complete)$`Pr(>F)`[1], error=function(e) 0)
            var_equal <- levene_p > 0.05
            t_test <- t.test(as.formula(formula_str), data = sub_df_complete, paired = FALSE, var.equal = var_equal)
            test_output$test_name <- paste("Studentův t-test (nezávislé skupiny)", if(!var_equal) "(Welchův - různé rozptyly)")
            test_output$statistic <- t_test$statistic
            test_output$p_value <- t_test$p.value
            test_output$degrees_freedom <- t_test$parameter
            test_output$statistic_name <- "t"
          } else {
            stop("Studentův t-test vyžaduje buď 2 numerické proměnné, nebo 1 numerickou a 1 kategoriální (s 2 úrovněmi).")
          }
          
        } else if (actual_method == "wilcoxon") {
   
          if (n_num != 2 || n_cat > 0) stop("Wilcoxonův test vyžaduje přesně 2 numerické proměnné.")
          if (!paired) stop("Pro nepárová data použijte Mann-Whitney U test místo Wilcoxonova.")
          num1 <- sub_df_complete[[num_cols_in_scope[1]]]
          num2 <- sub_df_complete[[num_cols_in_scope[2]]]
          wilcox_test <- suppressWarnings(wilcox.test(num1, num2, paired = TRUE, exact = FALSE, correct = TRUE))
          test_output$test_name <- "Wilcoxonův párový test"
          test_output$statistic <- wilcox_test$statistic
          test_output$p_value <- wilcox_test$p.value
          test_output$statistic_name <- "V"
          
        } else if (actual_method == "mannwhitney") {

          if (n_num == 2 && n_cat == 0) {
            if (paired) stop("Pro párová data použijte Wilcoxonův test místo Mann-Whitney U.")
            num1_col <- num_cols_in_scope[1]
            num2_col <- num_cols_in_scope[2]
            num1 <- sub_df_complete[[num1_col]]
            num2 <- sub_df_complete[[num2_col]]
            wilcox_test <- suppressWarnings(wilcox.test(num1, num2, paired = FALSE, exact = FALSE, correct = TRUE))
            test_output$test_name <- "Mann-Whitney U test (nepárový Wilcoxon)"
            test_output$statistic <- wilcox_test$statistic
            test_output$p_value <- wilcox_test$p.value
            test_output$statistic_name <- "W"
          } else if (n_num == 1 && n_cat == 1) {
            cat_col_name <- cat_cols_in_scope[1]
            num_col_name <- num_cols_in_scope[1]
            factor_var <- sub_df_complete[[cat_col_name]]
            if (length(levels(factor_var)) != 2) {
              stop("Pro Mann-Whitney test s kategoriální proměnnou musí mít tato proměnná přesně 2 úrovně.")
            }
            if (paired) stop("Párový Mann-Whitney test neexistuje (použijte nepárový nebo Wilcoxonův pro párová data).")
            formula_str <- paste0("`", num_col_name, "` ~ `", cat_col_name, "`")
            wilcox_test <- suppressWarnings(wilcox.test(as.formula(formula_str), data = sub_df_complete, paired = FALSE, exact = FALSE, correct = TRUE))
            test_output$test_name <- "Mann-Whitney U test (nezávislé skupiny)"
            test_output$statistic <- wilcox_test$statistic
            test_output$p_value <- wilcox_test$p.value
            test_output$statistic_name <- "W"
          } else {
            stop("Mann-Whitney U test vyžaduje buď 2 numerické proměnné (nepárové), nebo 1 numerickou a 1 kategoriální (s 2 úrovněmi).")
          }
          
        } else {
          stop(paste("Neznámá nebo nepodporovaná metoda:", actual_method))
        }
        
        cat("DEBUG DEPENDENCY: Assigning results directly to rv...\n") 
        rv$results <- test_output
        rv$analysis_error <- NULL
        cat("DEBUG DEPENDENCY: Reactive values assigned.\n") 
        print(str(rv$results, max.level=2)) 
        return("Success")
        
      }, error = function(e) {
        cat("--- ERROR captured by tryCatch (Dependency) ---\n"); print(e)
        rv$analysis_error <- paste("Chyba:", e$message)
        rv$results <- NULL
        cat("DEBUG DEPENDENCY: rv$analysis_error set to:", rv$analysis_error, "\n") 
        return("Error")
      }) 
      
      rv$is_analyzing <- FALSE
      cat("DEBUG DEPENDENCY: rv$is_analyzing set to FALSE. Status:", analysis_status, "\n") 
      cat("--- run_dependency_analysis END ---\n") # DEBUG
    }) # konec observeEvent
    
    # --- Výstup pro podmíněné zobrazení ---
    output$showDependencyResults <- reactive({
   .
      !is.null(rv$results) && is.null(rv$analysis_error)
    })
    outputOptions(output, 'showDependencyResults', suspendWhenHidden = FALSE)
    
    # --- Přímé renderování výstupů ---
    
    # Chyba analýzy
    output$analysis_error_ui <- renderUI({

      error_html <- if (!is.null(rv$analysis_error)) {
        tags$div(class = "alert alert-danger", role = "alert",
                 tags$strong("Chyba analýzy: "), rv$analysis_error)
      } else { NULL }
      
      warning_html <- if (!is.null(rv$results$warning_message)) {
        tags$div(class = "alert alert-warning", role = "alert",
                 tags$strong("Varování: "), rv$results$warning_message)
      } else { NULL }
      
      tagList(error_html, warning_html)
    })
    
    # Shrnutí testu
    output$test_summary_ui <- renderUI({
  
      req(rv$results)
      res <- rv$results
      tagList(
        p(tags$strong("Použitý test:"), tags$span(style="font-weight:bold; color:#007bff;", res$test_name %||% "N/A")),
        if (nzchar(res$reason %||% "")) p(tags$em(res$reason, style="font-size: small; color: grey;")),
        if (!is.null(res$p_value) && is.null(res$anova_results)) {
          tags$div(class='mt-2 pt-2 border-top',
                   p(tags$strong("p-hodnota: "),
                     tags$span(style=if(!is.na(res$p_value) && res$p_value < 0.05) "font-weight:bold; color:green;" else "", formatPValueR(res$p_value)),
                     if(!is.na(res$p_value) && res$p_value < 0.05) tags$span(icon("check"), "Statisticky významné (p < 0.05)", style="font-size:small; color:green; margin-left:5px;")
                     else if(!is.na(res$p_value)) tags$span(icon("times"), "Není statisticky významné (p ≥ 0.05)", style="font-size:small; color:grey; margin-left:5px;")
                   ),
                   if(!is.null(res$statistic)) p(tags$strong(paste0(res$statistic_name %||% "Test. statistika", ": ")),
                                                 formatStatR(res$statistic)
                   ),
                   if(!is.null(res$degrees_freedom)) p(tags$strong("Stupně volnosti: "), res$degrees_freedom) 
          )
        }
      )
    })
    
    # Detailní výsledky (tabulky apod.)
    output$results_detail_ui <- renderUI({

      req(rv$results)
      res <- rv$results
      
      ui_elements <- list()
      
      if (!is.null(res$contingency_table)) {
        ui_elements[[length(ui_elements) + 1]] <- h6("Kontingenční tabulka:")
        ui_elements[[length(ui_elements) + 1]] <- DTOutput(ns("contingency_table_dt"))
      }
      
      if (!is.null(res$anova_results)) {
        ui_elements[[length(ui_elements) + 1]] <- h6("Výsledky podle kombinací proměnných:")
        ui_elements[[length(ui_elements) + 1]] <- DTOutput(ns("anova_table_dt"))
      }
      
      if (length(ui_elements) == 0 && is.null(rv$analysis_error)) { # Zobrazí se jen pokud není chyba a nejsou ani detaily
        return(p(em("Pro tento test nejsou k dispozici žádné detailní tabulkové výsledky.")))
      } else {
        return(tagList(ui_elements))
      }
    })
    
    # Renderování DT pro kontingenční tabulku
    output$contingency_table_dt <- renderDT({

      req(rv$results$contingency_table)
      datatable(rv$results$contingency_table,
                rownames = FALSE,
                options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, scrollX = TRUE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Czech.json')))
    }, server = FALSE)
    
    # Renderování DT pro ANOVA/Kruskal výsledky
    output$anova_table_dt <- renderDT({

      req(rv$results$anova_results)
      stat_name <- rv$results$statistic_name %||% "Statistika"
      
      df_anova_prep <- rv$results$anova_results %>%
        dplyr::mutate(
          Statistika_formatted = sapply(statistic, formatStatR),
          p_hodnota_formatted = sapply(p_value, formatPValueR),
          St_volnosti_formatted = if ("df" %in% names(.)) as.character(round(df, 2)) else if ("df1" %in% names(.)) paste(round(df1, 0), round(df2, 0), sep=", ") else "-",
          Vyzn_formatted = ifelse(p_value < 0.05, '<span style="color:green;" title="Statisticky významné"><i class="fa fa-check"></i> Ano</span>', '<span style="color:grey;" title="Není statisticky významné"><i class="fa fa-times"></i> Ne</span>')
        )
      
      current_names <- names(df_anova_prep)
      current_names[current_names == "Statistika_formatted"] <- stat_name
      names(df_anova_prep) <- current_names
      
      df_anova_display <- df_anova_prep %>%
        dplyr::select(
          `Kat. proměnná` = cat_col,
          `Num. proměnná` = num_col,
          all_of(stat_name),
          `St. volnosti` = St_volnosti_formatted,
          `p-hodnota` = p_hodnota_formatted,
          Významné = Vyzn_formatted
        )
      
      datatable(df_anova_display,
                rownames = FALSE, escape = FALSE,
                options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE,
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Czech.json')))
      # --- KONEC ZMĚNY ---
    }, server = FALSE)
    

    output$dependency_plot <- renderPlotly({
      req(rv$results, rv$results$plot_data_df)
      res <- rv$results
      plot_df <- res$plot_data_df
      
      n_cat <- res$n_cat_used
      n_num <- res$n_num_used
      cat_cols <- res$cat_cols_used
      num_cols <- res$num_cols_used
      
      fig <- plot_ly()
      
      tryCatch({
        if (n_cat == 1 && n_num == 1) {

          cat_col_name <- cat_cols[1]
          num_col_name <- num_cols[1]
          fig <- plot_ly(data = plot_df, y = ~get(num_col_name), color = ~get(cat_col_name), type = "box",
                         boxpoints = "outliers") %>% # Show outliers
            layout(title = paste("Distribuce", num_col_name, "podle", cat_col_name),
                   yaxis = list(title = num_col_name),
                   xaxis = list(title = cat_col_name, categoryorder = "array", categoryarray = levels(plot_df[[cat_col_name]])), # Zajistí správné pořadí osy x
                   showlegend = TRUE) # Show legend for colors
          
        } else if (n_cat >= 2 && n_num == 0) {
          # Heatmapa pro kontingenční tabulku 
          cat_col1 <- cat_cols[1]
          cat_col2 <- cat_cols[2] # Používáme první dvě pro graf
          cont_table_wide <- res$contingency_table # Široký formát z DT
          
          # Připravíme data pro heatmapu
          mat <- as.matrix(cont_table_wide[, -1]) # Odstraníme první sloupec s názvy řádků
          rownames(mat) <- cont_table_wide[[1]]
          
          fig <- plot_ly(z = mat, x = colnames(mat), y = rownames(mat), type = "heatmap",
                         colorscale = "Viridis", # Nebo jiná škála
                         colorbar = list(title = "Četnost")) %>%
            layout(title = paste("Kontingenční tabulka:", cat_col1, "vs", cat_col2),
                   xaxis = list(title = cat_col2, tickangle = -45),
                   yaxis = list(title = cat_col1, automargin = TRUE),
                   margin = list(l = 100, b = 100)) # Upravit okraje podle potřeby
          
        } else if (n_cat == 0 && n_num == 2) {
          # Box ploty vedle sebe pro 2 numerické proměnné
          num_col1 <- num_cols[1]
          num_col2 <- num_cols[2]
          
          # Připravíme data do dlouhého formátu
          plot_df_long <- plot_df %>%
            tidyr::pivot_longer(cols = all_of(num_cols), names_to = "Proměnná", values_to = "Hodnota")
          
          fig <- plot_ly(data = plot_df_long, y = ~Hodnota, color = ~Proměnná, type = "box",
                         boxpoints = "outliers") %>%
            layout(title = paste("Porovnání distribucí:", num_col1, "vs", num_col2),
                   yaxis = list(title = "Hodnota"),
                   xaxis = list(title = "Proměnná"),
                   showlegend = TRUE)
          
        } else {
          # Případ, pro který nemáme specifický graf
          fig <- fig %>% layout(title = "Vizualizace není pro tuto kombinaci proměnných dostupná",
                                xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
                                yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE),
                                annotations = list(text = "Vyberte 1 kat + 1 num, 2 kat, nebo 2 num proměnné.",
                                                   showarrow=FALSE, font=list(size=14)))
        }
        
        return(fig)
        
      }, error = function(e) {
        cat("--- ERROR during Plot rendering (Dependency) ---\n"); print(e)
        return(plot_ly() %>% layout(title = "Chyba při vykreslování grafu",
                                    annotations = list(text = paste("Chyba:", e$message), showarrow=FALSE)))
      })
    })
    # --- KONEC ZMĚNY ---
    
    
    # --- AI Interpretace ---
    output$ai_error_ui <- renderUI({

      if (!is.null(rv$ai_error)) { tags$div(class = "alert alert-warning", role = "alert", tags$strong("Chyba AI: "), rv$ai_error, actionButton(ns("run_interpretation"), "Zkusit znovu", icon = icon("sync"), class = "btn btn-warning btn-xs pull-right")) } else {NULL}
    })
    output$ai_interpretation_ui <- renderUI({
 
      req(!is.null(rv$results) && is.null(rv$analysis_error))
      if(!rv$is_interpreting && is.null(rv$ai_interpretation) && is.null(rv$ai_error)) { actionButton(ns("run_interpretation"), "Interpretovat", icon = icon("lightbulb"), class = "btn-info") }
      else if(rv$is_interpreting) { p(icon("spinner", class = "fa-spin"), " AI pracuje...") }
      else if(!is.null(rv$ai_interpretation)) { wellPanel(style="background-color: #e9f5ff;", tags$b("Interpretace:"), tags$p(style="white-space: pre-wrap;", rv$ai_interpretation), actionButton(ns("reset_interpretation"), "Skrýt / Nová", icon = icon("sync"), class = "btn-link btn-sm")) }
      else { NULL }
    })
    
    # AI Volání
    observeEvent(input$run_interpretation, {

      cat("\n--- run_dependency_interpretation START ---\n"); 
      req(rv$results);
      res <- rv$results
      rv$is_interpreting <- TRUE; rv$ai_interpretation <- NULL; rv$ai_error <- NULL;
      cat("DEBUG DEPENDENCY: AI flags set (interpreting=TRUE, results=NULL)\n"); 
      
      cat("DEBUG DEPENDENCY: Preparing payload for AI.\n");
      payload <- list(
        analysis_type = "dependency_test",
        test_name = res$test_name,
        columns_involved = res$columns,
        paired_data = res$input_paired, # Posíláme, zda bylo zaškrtnuto
        p_value = res$p_value, # Může být NULL
        statistic = res$statistic, # Může být NULL
        statistic_name = res$statistic_name, # Název statisitky (např. Odds Ratio)
        degrees_freedom = res$degrees_freedom %||% res$df %||% (if(!is.null(res$df1)) paste(res$df1, res$df2, sep=",") else NULL), # Může být NULL nebo z anova/kruskal
        # Posíláme jen info, zda existuje kontingenční tabulka
        has_contingency_table = !is.null(res$contingency_table),
        # Posíláme ANOVA/Kruskal výsledky, pokud existují
        # Zaokrouhlení pro přehlednost v payloadu
        anova_results = if (!is.null(res$anova_results)) {
          lapply(1:nrow(res$anova_results), function(i) as.list(res$anova_results[i,])) %>%
            lapply(function(row) list(
              cat_col = row$cat_col,
              num_col = row$num_col,
              statistic = round(row$statistic, 3),
              p_value = round(row$p_value, 5)
            ))
        } else { NULL },
        warning_message = res$warning_message 
      )
      cat("DEBUG DEPENDENCY: AI Payload prepared:\n"); print(str(payload, max.level = 2));
      
      api_key <- Sys.getenv("OPENROUTER_API_KEY", "NA");
      if (api_key == "NA" || nchar(api_key) < 10) { cat("DEBUG DEPENDENCY: Missing API Key\n"); rv$ai_error <- "API klíč (OPENROUTER_API_KEY) není nastaven."; rv$is_interpreting <- FALSE; return() }# DEBUG
      cat("DEBUG DEPENDENCY: API Key found. Preparing API call.\n"); 
      
      system_prompt <- paste(
        "Jsi AI asistent specializující se na analýzu dat...", 
        "Zaměř se na:",
        "1. **Použitý test:** Uveď název provedeného testu ({payload$test_name}).",
        "2. **Hlavní výsledek (p-hodnota):** Vysvětli, co znamená výsledná p-hodnota (pokud existuje JEDNA hlavní p-hodnota).",
        "   - p < 0.05: Statisticky významný výsledek (závislost/rozdíl).",
        "   - p >= 0.05: Není statisticky významný výsledek.",
        "3. **Detailnější výsledky:**",
        "   - **Kontingenční tabulka:** Zmiň, pokud existuje.",
        "   - **ANOVA/Kruskal:** Pokud jsou, shrň výsledky pro jednotlivé kombinace (p-hodnoty).",
        "   - **Testová statistika:** Zmiň hodnotu, pokud je relevantní.",
        "4. **Kontext (párová data):** Zmiň, pokud relevantní.",
        "5. **Varování:** Zmiň {payload$warning_message}, pokud existuje.",
        "6. **Omezení:** Nezaměňovat stat. významnost za praktickou, pozor na kauzalitu.",
        "Pravidla:", "- Čeština, stručnost, srozumitelnost, odstavce.",
        sep = "\n"
      )
      
      user_prompt_parts <- list(
        paste0("Provedl jsem analýzu typu '", payload$analysis_type, "' s následujícími výsledky:"),
        paste0("- Použitý test: ", payload$test_name),
        paste0("- Zahrnuté sloupce: ", paste(payload$columns_involved, collapse=", ")),
        paste0("- Byla data označena jako párová? ", ifelse(isTRUE(payload$paired_data), "Ano", "Ne"))
      )
      if(!is.null(payload$p_value) && is.null(payload$anova_results)) user_prompt_parts[[length(user_prompt_parts)+1]] <- paste0("- Hlavní p-hodnota: ", formatPValueR(payload$p_value))
      if(!is.null(payload$statistic) && is.null(payload$anova_results)) user_prompt_parts[[length(user_prompt_parts)+1]] <- paste0("- Hlavní testová statistika (", payload$statistic_name %||% "hodnota","): ", formatStatR(payload$statistic))
      if(!is.null(payload$degrees_freedom) && is.null(payload$anova_results)) user_prompt_parts[[length(user_prompt_parts)+1]] <- paste0("- Stupně volnosti: ", payload$degrees_freedom)
      if(isTRUE(payload$has_contingency_table)) user_prompt_parts[[length(user_prompt_parts)+1]] <- "- Byla vygenerována kontingenční tabulka."
      if(!is.null(payload$anova_results)) {
        user_prompt_parts[[length(user_prompt_parts)+1]] <- "\n- Výsledky pro jednotlivé kombinace (ANOVA/Kruskal):"
        for(ar in payload$anova_results) {
          user_prompt_parts[[length(user_prompt_parts)+1]] <- paste0("  - ", ar$cat_col, " vs ", ar$num_col, ": p=", formatPValueR(ar$p_value), ", stat=", formatStatR(ar$statistic))
        }
      }
      if(!is.null(payload$warning_message)) user_prompt_parts[[length(user_prompt_parts)+1]] <- paste0("\n- Varování z analýzy: ", payload$warning_message)
      user_prompt_parts[[length(user_prompt_parts)+1]] <- "\nProsím, interpretuj tyto výsledky."
      full_user_prompt <- paste(user_prompt_parts, collapse = "\n")
      cat("DEBUG DEPENDENCY: User Prompt for AI:\n", full_user_prompt, "\n"); 
      
      api_error_msg <- NULL
      response <- tryCatch({
        POST(url = "https://openrouter.ai/api/v1/chat/completions",
             add_headers(Authorization = paste("Bearer", api_key), `Content-Type` = "application/json"),
             body = toJSON(list(model = "deepseek/deepseek-chat-v3-0324:free",
                                messages = list(list(role = "system", content = system_prompt),
                                                list(role = "user", content = full_user_prompt)),
                                max_tokens = 800
             ), auto_unbox = TRUE, na = "null"),
             encode = "json",
             timeout(60))
      }, error = function(e) { cat("--- ERROR API call ---\n"); print(e); api_error_msg <<- paste("Chyba spojení s AI API:", e$message); return(NULL) })
      
      cat("DEBUG DEPENDENCY: AI API call finished.\n");
      
      isolate({

        if (!is.null(api_error_msg)) { cat("DEBUG DEPENDENCY: AI Error from tryCatch:", api_error_msg, "\n"); rv$ai_error <- api_error_msg }
        else if (is.null(response)) { cat("DEBUG DEPENDENCY: API call returned NULL unexpectedly.\n"); rv$ai_error <- "Neočekávaná chyba API (NULL response)." }
        else {
          status <- status_code(response); cat("DEBUG DEPENDENCY: AI API Status:", status, "\n");
          if (status >= 300) {
            err_content <- httr::content(response, "text", encoding="UTF-8")
            err_details <- tryCatch(fromJSON(err_content)$error$message, error=function(e) substr(err_content, 1, 300))
            rv$ai_error <- paste("Chyba API (", status, "): ", err_details)
            cat("DEBUG DEPENDENCY: Parsed AI Error:", rv$ai_error,"\n")
            cat("DEBUG DEPENDENCY: Full error response content:\n", err_content, "\n")
          } else {
            content <- tryCatch(httr::content(response, "parsed"), error = function(e) NULL)
            if (is.null(content)) {
              cat("DEBUG DEPENDENCY: Failed to parse JSON response\n")
              rv$ai_error <- "Nepodařilo se zpracovat odpověď z AI (neplatný JSON)."
              rv$ai_interpretation <- NULL
              cat("DEBUG DEPENDENCY: Raw response text:\n", httr::content(response, "text", encoding="UTF-8"), "\n")
            } else {
              interpretation_text <- content$choices[[1]]$message$content
              if (!is.null(interpretation_text) && nchar(trimws(interpretation_text)) > 0) {
                cat("DEBUG DEPENDENCY: AI Success\n")
                rv$ai_interpretation <- trimws(interpretation_text)
                rv$ai_error <- NULL
              } else {
                cat("DEBUG DEPENDENCY: AI empty or invalid response\n")
                rv$ai_error <- "AI nevrátila platnou interpretaci (prázdná odpověď)."
                rv$ai_interpretation <- NULL
                cat("DEBUG DEPENDENCY: Received content:\n"); print(content)
              }
            }
          }
        }
        rv$is_interpreting <- FALSE;
        cat("DEBUG DEPENDENCY: is_interpreting set to FALSE.\n");
      }) 
      cat("--- run_dependency_interpretation END ---\n\n") 
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
 
    observeEvent(input$reset_interpretation, {

      cat("DEBUG DEPENDENCY: Reset AI\n"); rv$ai_interpretation <- NULL; rv$ai_error <- NULL; rv$is_interpreting <- FALSE
    })
    
  }) # konec moduleServer
}