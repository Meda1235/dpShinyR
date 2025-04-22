
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

# --- Helper Functions ---
# Funkce pro formátování p-hodnoty
formatPValueR <- function(pValue) {
  if (!is.numeric(pValue) || is.na(pValue)) return('-')
  # Použijeme vědecký zápis pro velmi malé hodnoty
  if (pValue < 0.001 && pValue != 0) return(format(pValue, scientific = TRUE, digits = 2))
  # Pro nulu vrátíme explicitně "0.000" nebo "< 0.001"
  if (pValue == 0) return("< 0.001") # Nebo "0.000"
  # Jinak zaokrouhlíme na 3 desetinná místa
  return(format(round(pValue, 3), nsmall = 3))
}

# Funkce pro formátování statistiky
formatStatR <- function(stat) {
  if (!is.numeric(stat) || is.na(stat)) return('-')
  # Zaokrouhlíme na 3 desetinná místa
  return(format(round(stat, 3), nsmall = 3))
}

# --- UI Funkce Modulu ---
comparisonUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Porovnání Skupin"),
    p("Porovnejte hodnoty numerických proměnných mezi skupinami definovanými kategoriální proměnnou (nezávislé skupiny) nebo porovnejte dvě numerické proměnné v rámci stejných subjektů/párů (párový test)."),
    fluidRow(
      # --- Vstupní část ---
      column(4,
             wellPanel(
               tags$b("1. Typ porovnání"),
               checkboxInput(ns("paired"), "Párový test (porovnání dvou měření u stejných subjektů)", value = FALSE),
               tags$hr(),
               
               tags$b("2. Výběr proměnných"),
               # Dynamické UI podle typu testu
               uiOutput(ns("variable_selection_ui")),
               
               tags$hr(),
               tags$b("3. Výběr metody"),
               # Dynamický výběr metody podle typu testu
               uiOutput(ns("method_selection_ui")),
               
               tags$hr(),
               actionButton(ns("run_analysis"), "Spustit Analýzu", icon = icon("play"), class = "btn-primary"),
               uiOutput(ns("analysis_loading_ui")) # Indikátor načítání
             )
      ), 
      
      # --- Výstupní část ---
      column(8,
             h4("Výsledky Analýzy"),
             uiOutput(ns("analysis_error_ui")), # Zobrazení chyb analýzy
             conditionalPanel(
               condition = "output.showComparisonResults === true", # Podmínka pro zobrazení výsledků
               ns = ns,
               tagList(
                 # Shrnutí provedeného testu
                 wellPanel(style = "background-color: #f8f9fa;", uiOutput(ns("test_summary_ui"))),
                 # Detailní tabulka výsledků
                 tags$h5("Detailní výsledky testů"),
                 DTOutput(ns("results_table_dt")) %>% withSpinner(type = 4, color="#0dc5c1"),
                 # Grafy
                 tags$h5("Vizualizace distribucí"),
                 uiOutput(ns("plots_ui")) %>% withSpinner(type = 4, color="#0dc5c1"),
                 # AI Interpretace
                 tags$hr(), tags$h5("AI Interpretace"),
                 uiOutput(ns("ai_error_ui")), # Chyby AI
                 uiOutput(ns("ai_interpretation_ui")) # Text interpretace
               ) # konec tagList
             ) # konec conditionalPanel
      ) # konec column 8
    ) # konec fluidRow
  ) # konec tagList
}


# --- Server Funkce Modulu ---
comparisonServer <- function(id, reactive_data, reactive_col_types) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reaktivní hodnoty modulu ---
    rv <- reactiveValues(
      results = NULL,             # Uloží strukturované výsledky analýzy
      analysis_error = NULL,      # Uloží chybovou zprávu z analýzy
      is_analyzing = FALSE,       # Příznak běžící analýzy
      ai_interpretation = NULL,   # Uloží text interpretace od AI
      ai_error = NULL,            # Uloží chybovou zprávu od AI
      is_interpreting = FALSE,    # Příznak běžící AI interpretace
      plot_data_list = NULL,      # Seznam data.frame pro generování grafů
      is_paired = FALSE           # Příznak, zda byla poslední analýza párová
    )
    
    # --- Dynamické UI pro výběr proměnných (podle input$paired) ---
    output$variable_selection_ui <- renderUI({
      req(reactive_col_types())
      col_info <- reactive_col_types()
      numeric_cols <- col_info$Column[col_info$DetectedType == "Numeric"]
      categorical_cols <- col_info$Column[col_info$DetectedType %in% c("Categorical", "Factor", "Character")]
      
      if (input$paired) {
        # --- UI pro PÁROVÝ test ---
        # Potřebujeme 2 numerické a 1 volitelný ID sloupec
        tagList(
          selectInput(ns("num_col_1"), "První numerická proměnná:", choices = c("Vyberte..."="", numeric_cols), selected = isolate(input$num_col_1) %||% ""),
          selectInput(ns("num_col_2"), "Druhá numerická proměnná:", choices = c("Vyberte..."="", numeric_cols), selected = isolate(input$num_col_2) %||% ""),
          selectInput(ns("id_col"), "Kategoriální proměnná (ID subjektu/páru - volitelné):", choices = c("Není zadáno"="", categorical_cols), selected = isolate(input$id_col) %||% "")
        )
      } else {
        # --- UI pro NEZÁVISLÉ skupiny ---
        # Potřebujeme 1 kategoriální a 1+ numerickou
        if(length(categorical_cols) == 0) {
          return(p("Nebyly nalezeny žádné vhodné kategoriální proměnné pro definování skupin.", class="text-danger"))
        }
        tagList(
          selectInput(ns("target_col"), "Kategoriální proměnná (Skupiny):",
                      choices = c("Vyberte..." = "", setNames(categorical_cols, categorical_cols)),
                      selected = isolate(input$target_col) %||% ""),
          # Zvlášť UI pro výběr numerických sloupců, až po výběru target_col
          uiOutput(ns("feature_select_ui_independent"))
        )
      }
    })
    
    # Zvlášť UI pro výběr numerických (jen pro nezávislé)
    output$feature_select_ui_independent <- renderUI({
      req(!input$paired, input$target_col)
      target_selected <- input$target_col
      req(nchar(target_selected) > 0)
      
      col_info <- reactive_col_types()
      # Nabídnout jen numerické sloupce, které NEJSOU cílová proměnná
      numeric_cols <- col_info$Column[col_info$DetectedType == "Numeric" & col_info$Column != target_selected]
      
      if(length(numeric_cols) == 0) {
        return(p("Nebyly nalezeny žádné vhodné numerické proměnné k porovnání (jiné než grupovací).", class="text-danger"))
      }
      # Použijeme checkboxGroupInput pro výběr více proměnných
      checkboxGroupInput(ns("feature_cols"), label = "Numerické proměnné k porovnání (min. 1):",
                         choices = setNames(numeric_cols, numeric_cols),
                         selected = isolate(intersect(input$feature_cols, numeric_cols))) # Zachová výběr, pokud existoval
    })
    
    # --- Dynamické UI pro výběr metody (podle input$paired) ---
    output$method_selection_ui <- renderUI({
      if (input$paired) {
        # Metody relevantní pro párový test
        selectInput(ns("method"), "Metoda párového porovnání:",
                    choices = c("Automaticky (doporučeno)" = "auto",
                                "Párový t-test" = "paired.t.test",
                                "Wilcoxonův párový test" = "wilcox.paired"),
                    selected = "auto")
      } else {
        # Metody relevantní pro nezávislé skupiny
        selectInput(ns("method"), "Metoda porovnání (nezávislé skupiny):",
                    choices = c("Automaticky (doporučeno)" = "auto",
                                "ANOVA" = "anova",
                                "Kruskal-Wallis" = "kruskal",
                                "t-test (Studentův/Welchův)" = "t.test", 
                                "Mann-Whitney U test" = "mannwhitney"),
                    selected = isolate(input$method) %||% "auto") 
      }
    })
    
    # --- Indikátor načítání ---
    output$analysis_loading_ui <- renderUI({
      if (rv$is_analyzing) { tags$div(style="display: inline-block; margin-left: 15px;", icon("spinner", class = "fa-spin", style="color: #007bff;")) } else { NULL }
    })
    
    # --- Reset výsledků při změně relevantních vstupů ---
    observeEvent(c(input$paired, input$target_col, input$feature_cols, input$num_col_1, input$num_col_2, input$id_col, input$method), {
      input_changed <- FALSE
      # Pokud ještě nejsou výsledky nebo se změnil typ testu (párový/nepárový)
      if (is.null(rv$results) || !identical(input$paired, isolate(rv$is_paired))) {
        input_changed <- TRUE
      } else {
        # Kontrola změny metody
        if (!identical(input$method, isolate(rv$results$input_method %||% input$method))) input_changed <- TRUE
        # Kontrola změny sloupců podle typu testu
        if (input$paired) {
          if (!identical(input$num_col_1, isolate(rv$results$feature_columns[1] %||% input$num_col_1))) input_changed <- TRUE
          if (!identical(input$num_col_2, isolate(rv$results$feature_columns[2] %||% input$num_col_2))) input_changed <- TRUE
          if (!identical(input$id_col, isolate(rv$results$target_column %||% input$id_col))) input_changed <- TRUE
        } else {
          if (!identical(input$target_col, isolate(rv$results$target_column %||% input$target_col))) input_changed <- TRUE
          if (!identical(sort(input$feature_cols), sort(isolate(rv$results$feature_columns %||% input$feature_cols)))) input_changed <- TRUE
        }
      }
      
      # Pokud došlo ke změně, resetujeme vše
      if(input_changed) {
        cat("DEBUG COMPARISON: Input changed, resetting results.\n")
        rv$results <- NULL
        rv$analysis_error <- NULL
        rv$ai_interpretation <- NULL
        rv$ai_error <- NULL
        rv$plot_data_list <- NULL
        rv$is_paired <- input$paired 
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    
    # --- Spuštění Analýzy (ROZVĚTVENO PODLE PÁROVÉHO/NEZÁVISLÉHO) ---
    observeEvent(input$run_analysis, {
      cat("\n--- run_comparison_analysis START (Paired:", input$paired, ") ---\n")
      # Reset stavů před analýzou
      rv$analysis_error <- NULL; rv$results <- NULL; rv$ai_interpretation <- NULL; rv$ai_error <- NULL; rv$plot_data_list <- NULL
      rv$is_analyzing <- TRUE
      rv$is_paired <- input$paired # Uložíme aktuální typ
      
      # Získání potřebných vstupů
      method <- input$method
      req(reactive_data(), reactive_col_types())
      data_in <- reactive_data()
      col_types_df <- reactive_col_types()
      
     
      analysis_status <- tryCatch({
        
        if (input$paired) {
  
          num_col_1 <- input$num_col_1
          num_col_2 <- input$num_col_2
          id_col <- input$id_col # Může být ""
          
          # Validace vstupů pro párový test
          if (is.null(num_col_1) || nchar(num_col_1) == 0 || is.null(num_col_2) || nchar(num_col_2) == 0) {
            stop("Vyberte obě numerické proměnné pro párový test.")
          }
          if (num_col_1 == num_col_2) {
            stop("Vybrané numerické proměnné musí být rozdílné.")
          }
          # ID sloupec nesmí být stejný jako numerické
          if (!is.null(id_col) && nchar(id_col) > 0 && (id_col == num_col_1 || id_col == num_col_2)) {
            stop("ID sloupec nesmí být stejný jako porovnávané numerické sloupce.")
          }
          
          selected_cols_paired <- c(num_col_1, num_col_2)
          # Přidáme ID sloupec, jen pokud byl vybrán
          if (!is.null(id_col) && nchar(id_col) > 0) {
            selected_cols_paired <- c(selected_cols_paired, id_col)
          }
          
          # Příprava datového rámce
          sub_df <- data_in %>% dplyr::select(all_of(selected_cols_paired))
          # Konverze numerických sloupců s ošetřením chyb
          sub_df <- sub_df %>% dplyr::mutate(across(all_of(c(num_col_1, num_col_2)), ~suppressWarnings(as.numeric(as.character(.)))))
          # Pokud byl ID sloupec, převedeme na faktor
          if (!is.null(id_col) && nchar(id_col) > 0) {
            sub_df <- sub_df %>% dplyr::mutate(!!sym(id_col) := as.factor(!!sym(id_col)))
          }
          
          # Odstranění řádků s NA v numerických sloupcích
          sub_df_complete <- sub_df %>% filter(!is.na(!!sym(num_col_1)) & !is.na(!!sym(num_col_2)))
          n_complete <- nrow(sub_df_complete)
          
          # Kontrola dostatku dat
          if (n_complete < 3) {
            stop(paste("Nedostatek kompletních párů pozorování pro analýzu (nalezeno:", n_complete, ", minimum 3)."))
          }
          
          # Výpočet rozdílů
          differences <- sub_df_complete[[num_col_1]] - sub_df_complete[[num_col_2]]
          
          # Výběr testu (auto nebo manuální)
          actual_method_paired <- method
          reason_paired <- ""
          normality_p <- NA # Pro uložení p-hodnoty normality
          
          if (method == "auto") {
            # Test normality rozdílů
            shapiro_result <- NULL
            if(length(differences) >= 3 && length(unique(differences)) > 1) { # Podmínky pro shapiro.test
              tryCatch({
                shapiro_result <- shapiro.test(differences)
                normality_p <- shapiro_result$p.value
              }, error = function(e) {
                cat("Shapiro test pro rozdíly selhal:", e$message, "\n")
                normality_p <<- 0 # Považujeme za nenormální, pokud test selže
              })
            } else {
              normality_p <- 0 # Málo dat nebo konstantní -> neparametrický
              reason_paired <- "Málo dat/unikátních hodnot pro test normality rozdílů. "
            }
            
            if (!is.na(normality_p) && normality_p > 0.05) {
              actual_method_paired <- "paired.t.test"
              reason_paired <- paste0(reason_paired, "Auto (rozdíly normální, Shapiro p=", round(normality_p, 3), ") -> Párový t-test")
            } else {
              actual_method_paired <- "wilcox.paired"
              reason_paired <- paste0(reason_paired, "Auto (rozdíly nenormální, Shapiro p=", round(normality_p, 3), ") -> Wilcoxonův párový test")
            }
            cat(paste("DEBUG PAIRED: Auto ->", actual_method_paired, "\n"))
          } else {
            reason_paired <- "Manuální výběr"
          }
          
          # Provedení testu
          test_result <- NULL
          current_test_output <- list(
            target_column = if(!is.null(id_col) && nchar(id_col) > 0) id_col else "N/A (Implicitní páry)",
            feature_columns = c(num_col_1, num_col_2),
            input_method = method,
            test_name = NULL, # Bude nastaveno níže
            reason = reason_paired,
            statistic = NA_real_, p_value = NA_real_,
            statistic_name = NA_character_, degrees_freedom = NA_character_,
            error_message = NULL, warning_message = NULL
          )
          
          tryCatch({
            if (actual_method_paired == "paired.t.test") {
              test_result <- t.test(sub_df_complete[[num_col_1]], sub_df_complete[[num_col_2]], paired = TRUE)
              current_test_output$statistic <- test_result$statistic; current_test_output$p_value <- test_result$p.value; current_test_output$degrees_freedom <- test_result$parameter; current_test_output$statistic_name <- "t"; current_test_output$test_name <- "Párový t-test"
            } else if (actual_method_paired == "wilcox.paired") {
              if (all(differences == 0)) { # Ošetření nulových rozdílů
                current_test_output$p_value <- 1.0 # Nulový rozdíl není statisticky významný
                current_test_output$warning_message <- "Všechny rozdíly jsou nulové."
              } else if (length(unique(differences)) == 1 && length(differences) > 1) { # Ošetření konstantních nenulových rozdílů
                current_test_output$warning_message <- "Všechny rozdíly jsou stejné (nenulové)."
                # Můžeme provést test, ale p-hodnota bude extrémní
                test_result <- suppressWarnings(wilcox.test(sub_df_complete[[num_col_1]], sub_df_complete[[num_col_2]], paired = TRUE, exact = FALSE, correct = TRUE))
                current_test_output$statistic <- test_result$statistic; current_test_output$p_value <- test_result$p.value; current_test_output$statistic_name <- "V"
              } else {
                test_result <- suppressWarnings(wilcox.test(sub_df_complete[[num_col_1]], sub_df_complete[[num_col_2]], paired = TRUE, exact = FALSE, correct = TRUE)) # exact=FALSE pro případ vazeb
                current_test_output$statistic <- test_result$statistic; current_test_output$p_value <- test_result$p.value; current_test_output$statistic_name <- "V" # Wilcoxon signed-rank test statistic
              }
              current_test_output$test_name <- "Wilcoxonův párový test"
            } else {
              stop(paste("Neznámá párová metoda:", actual_method_paired))
            }
          }, error = function(e_inner) {
            cat(paste("--- ERROR PAIRED TEST:", e_inner$message, "\n"))
            current_test_output$error_message <- e_inner$message
            current_test_output$p_value <- NA # Nastavíme NA, pokud test selže
          })
          
          # Uložení výsledků pro párový test
          rv$results <- list(
            type = "paired",
            target_column = current_test_output$target_column,
            feature_columns = current_test_output$feature_columns,
            input_method = method,
            num_levels = NA, # Nerelevantní
            tests = list(paired_result = current_test_output) # Výsledek je jen jeden
          )
          
          # Příprava dat pro grafy
          plot_data_paired <- sub_df_complete %>%
            mutate(Difference = !!sym(num_col_1) - !!sym(num_col_2)) %>%
            mutate(.row_id = row_number()) # Přidáme ID řádku pro propojení
          
          # Pokud máme ID sloupec, použijeme ho, jinak použijeme .row_id
          id_col_for_plot <- if(!is.null(id_col) && nchar(id_col) > 0) id_col else ".row_id"
          plot_data_paired <- plot_data_paired %>%
            rename(ID_Internal = !!sym(id_col_for_plot)) %>%
            mutate(ID_Internal = factor(ID_Internal)) # Převedeme ID na faktor pro graf
          
          rv$plot_data_list <- list(
            difference_plot = plot_data_paired %>% dplyr::select(Difference),
            paired_plot = plot_data_paired %>%
              tidyr::pivot_longer(cols = all_of(c(num_col_1, num_col_2)), names_to = "Variable", values_to = "Value") %>%
              dplyr::select(ID_Internal, Variable, Value)
          )
          
        } else {
 
          target_col <- input$target_col
          feature_cols <- input$feature_cols
          
          # Validace
          if (is.null(target_col) || nchar(target_col) == 0) stop("Vyberte kategoriální proměnnou.")
          if (is.null(feature_cols) || length(feature_cols) == 0) stop("Vyberte alespoň jednu numerickou proměnnou.")
          
          selected_cols_all <- c(target_col, feature_cols)
          sub_df <- data_in %>% dplyr::select(all_of(selected_cols_all))
          sub_df <- sub_df %>% dplyr::mutate(across(all_of(feature_cols), ~suppressWarnings(as.numeric(as.character(.)))))
          sub_df <- sub_df %>% dplyr::mutate(!!sym(target_col) := as.factor(!!sym(target_col)))
          sub_df_complete <- na.omit(sub_df)
          n_complete <- nrow(sub_df_complete)
          target_levels <- levels(sub_df_complete[[target_col]])
          num_levels <- length(target_levels)
          
          if (num_levels < 2) stop(paste("Kategoriální proměnná", shQuote(target_col), "musí mít alespoň 2 úrovně."))
          # Zvýšíme požadavek na data pro robustnější testy předpokladů
          if (n_complete < num_levels * 5) { # Např. 5 na skupinu
            stop(paste("Nedostatek kompletních pozorování pro robustní analýzu (nalezeno:", n_complete, ")."))
          }
          
          results_list <- list()
          plot_data_list_temp <- list()
          
          for(feature in feature_cols) {
            cat(paste("DEBUG INDEPENDENT: Analyzing feature:", feature, "\n"))
            current_test_output <- list(
              target_column = target_col, feature_column = feature, input_method = method,
              test_name = NULL, reason = "", statistic = NA_real_, p_value = NA_real_,
              statistic_name = NA_character_, degrees_freedom = NA_character_,
              error_message = NULL, warning_message = NULL
            )
            
            actual_method_feature <- method
            reason_feature <- if(method == "auto") "" else "Manuální výběr. " # Přidána mezera
            
            # Získání dat pro tuto feature a rozdělení do skupin
            feature_data <- sub_df_complete %>% dplyr::select(all_of(c(target_col, feature)))
            groups_data <- split(feature_data[[feature]], feature_data[[target_col]])
            

            if (method == "auto") {
              min_group_size_for_tests <- 3
              can_test_assumptions <- all(sapply(groups_data, length) >= min_group_size_for_tests)
              
              # --- Levene Test ---
              levene_p <- NA_real_
              levene_performed <- FALSE
              if (can_test_assumptions && num_levels >= 2) { # Levene má smysl pro 2+ skupiny
                tryCatch({
                  # Použijeme formuli pro jistotu
                  levene_formula <- as.formula(paste0("`", feature, "` ~ `", target_col, "`"))
                  levene_result <- car::leveneTest(levene_formula, data = feature_data)
                  levene_p <- levene_result$`Pr(>F)`[1]
                  levene_performed <- TRUE
                }, error = function(e) {
                  cat(paste("Levene test selhal pro", feature, ":", e$message, "\n"))
                  current_test_output$warning_message <- paste(current_test_output$warning_message %||% "", "Levene test selhal.")
                  levene_p <<- NA_real_ # Zajistíme, že je NA, pokud selže
                })
              }
              homogeneity_ok <- !is.na(levene_p) && levene_p > 0.05
              reason_feature <- paste0(reason_feature, if(levene_performed) paste0("Levene p=", round(levene_p, 3), ". ") else "Levene N/A. ")
              
              # --- Shapiro-Wilk Test ---
              # Pro K=2 testujeme normalitu v každé skupině
              # Pro K>2 testujeme normalitu celkových dat (jako v Pythonu)
              normality_ok <- FALSE
              shapiro_performed <- FALSE
              shapiro_details <- ""
              
              if (num_levels == 2) {
                if (can_test_assumptions) {
                  p_vals <- sapply(groups_data, function(g) if(length(g) >= 3 && length(unique(g)) > 1) tryCatch(shapiro.test(g)$p.value, error=function(e) 0) else 0)
                  normality_ok <- all(p_vals > 0.05)
                  shapiro_performed <- TRUE
                  shapiro_details <- paste0("Shapiro p=(", paste(round(p_vals, 3), collapse=", "), "). ")
                } else {
                  shapiro_details <- "Shapiro N/A (málo dat ve skupinách). "
                }
              } else { # K > 2
                if (n_complete >= 3 && length(unique(feature_data[[feature]])) > 1) {
                  shapiro_p_overall <- tryCatch(shapiro.test(feature_data[[feature]])$p.value, error = function(e) 0)
                  normality_ok <- shapiro_p_overall > 0.05
                  shapiro_performed <- TRUE
                  shapiro_details <- paste0("Shapiro(celk.) p=", round(shapiro_p_overall, 3), ". ")
                } else {
                  shapiro_details <- "Shapiro N/A (málo dat/unik. hodnot). "
                }
              }
              reason_feature <- paste0(reason_feature, shapiro_details)
              
              # --- Výběr testu ---
              if (num_levels == 2) {
                if (normality_ok && homogeneity_ok) { actual_method_feature <- "t.test.equalvar"; reason_feature <- paste0(reason_feature, "-> t-test (shodné rozpt.)") }
                else if (normality_ok && !homogeneity_ok) { actual_method_feature <- "t.test.welch"; reason_feature <- paste0(reason_feature, "-> Welchův t-test") }
                else { actual_method_feature <- "mannwhitney"; reason_feature <- paste0(reason_feature, "-> Mann-Whitney U") }
              } else { # K > 2
                if (normality_ok && homogeneity_ok) { actual_method_feature <- "anova"; reason_feature <- paste0(reason_feature, "-> ANOVA") }
                else { actual_method_feature <- "kruskal"; reason_feature <- paste0(reason_feature, "-> Kruskal-Wallis") }
              }
              # ----- Konec Auto režimu -----
            } else {
              # --- Manuální režim ---
              reason_feature <- "Manuální výběr. "
              # Validace vhodnosti metody pro počet skupin
              if(num_levels == 2 && actual_method_feature %in% c("anova", "kruskal")) {
                current_test_output$warning_message <- paste("Metoda", actual_method_feature, "je obvykle pro >2 skupiny.")
              } else if(num_levels > 2 && actual_method_feature %in% c("t.test", "mannwhitney")) {
                stop(paste("Metoda", actual_method_feature, "je pro 2 skupiny, ale máte", num_levels, "."))
              }
              # Při manuálním t.test provedeme interní výběr Student/Welch
              if(actual_method_feature == "t.test") {
                levene_p_manual <- tryCatch(car::leveneTest(as.formula(paste0("`", feature, "` ~ `", target_col, "`")), data = feature_data)$`Pr(>F)`[1], error=function(e) NA_real_)
                # Pokud Levene selže nebo p<=0.05, použijeme Welch
                if (is.na(levene_p_manual) || levene_p_manual <= 0.05) {
                  actual_method_feature <- "t.test.welch" 
                  reason_feature <- paste0(reason_feature, " Rozptyly nehomogenní (nebo test selhal) -> Welchův t-test.")
                } else {
                  actual_method_feature <- "t.test.equalvar"
                  reason_feature <- paste0(reason_feature, " Rozptyly homogenní -> Studentův t-test.")
                }
              }
            } # --- Konec Manuálního režimu ---
            
            current_test_output$reason <- reason_feature
            # Uložíme interní kód metody pro provedení, ale název pro zobrazení nastavíme níže
            final_method_code <- actual_method_feature
            
            # --- Provedení konkrétního testu ---
            formula_str <- paste0("`", feature, "` ~ `", target_col, "`")
            test_result <- NULL
            tryCatch({
              if (final_method_code == "anova") {
                test_result <- summary(aov(as.formula(formula_str), data = feature_data))
                current_test_output$statistic <- test_result[[1]]$`F value`[1]; current_test_output$p_value <- test_result[[1]]$`Pr(>F)`[1]; current_test_output$degrees_freedom <- paste(test_result[[1]]$Df[1], test_result[[1]]$Df[2], sep=","); current_test_output$statistic_name <- "F"; current_test_output$test_name <- "ANOVA"
              } else if (final_method_code == "kruskal") {
                test_result <- kruskal.test(as.formula(formula_str), data = feature_data)
                current_test_output$statistic <- test_result$statistic; current_test_output$p_value <- test_result$p.value; current_test_output$degrees_freedom <- test_result$parameter; current_test_output$statistic_name <- "Kruskal-Wallis χ²"; current_test_output$test_name <- "Kruskal-Wallis test"
              } else if (final_method_code == "t.test.equalvar") { # t-test se shodnými rozptyly
                test_result <- t.test(as.formula(formula_str), data = feature_data, paired = FALSE, var.equal = TRUE)
                current_test_output$statistic <- test_result$statistic; current_test_output$p_value <- test_result$p.value; current_test_output$degrees_freedom <- test_result$parameter; current_test_output$statistic_name <- "t"; current_test_output$test_name <- "Studentův t-test"
              } else if (final_method_code == "t.test.welch") { # Welchův t-test
                test_result <- t.test(as.formula(formula_str), data = feature_data, paired = FALSE, var.equal = FALSE)
                current_test_output$statistic <- test_result$statistic; current_test_output$p_value <- test_result$p.value; current_test_output$degrees_freedom <- test_result$parameter; current_test_output$statistic_name <- "t"; current_test_output$test_name <- "Welchův t-test"
              } else if (final_method_code == "mannwhitney") {
                # Ověření, že máme skutečně 2 skupiny pro wilcox.test
                if(num_levels != 2) stop("Mann-Whitney test lze použít pouze pro 2 skupiny.")
                test_result <- suppressWarnings(wilcox.test(as.formula(formula_str), data = feature_data, paired = FALSE, exact = FALSE, correct = TRUE))
                current_test_output$statistic <- test_result$statistic; current_test_output$p_value <- test_result$p.value; current_test_output$statistic_name <- "W"; current_test_output$test_name <- "Mann-Whitney U test"
              } else {
                stop(paste("Neznámá finální metoda:", final_method_code))
              }
            }, error = function(e_inner) {
              cat(paste("--- ERROR executing test for feature", feature, ":", e_inner$message, "\n"))
              current_test_output$error_message <- e_inner$message
              current_test_output$p_value <- NA # Test selhal
            })
            
            results_list[[feature]] <- current_test_output
            
            # Data pro graf (nezávislé skupiny)
            plot_data_list_temp[[feature]] <- feature_data %>%
              dplyr::rename(Group = !!sym(target_col), Value = !!sym(feature))
            
          } # Konec for cyklu přes features (nezávislé)
          
          # Uložení výsledků (nezávislé)
          if(length(results_list) == 0) stop("Nepodařilo se provést žádnou analýzu pro vybrané proměnné.")
          rv$results <- list(
            type = "independent",
            target_column = target_col, feature_columns = feature_cols,
            input_method = method, num_levels = num_levels,
            tests = results_list
          )
          rv$plot_data_list <- plot_data_list_temp
        } # Konec else pro nezávislé skupiny
        
        # Pokud vše proběhlo, analysis_error je NULL
        rv$analysis_error <- NULL
        cat("DEBUG COMPARISON: Analysis finished successfully.\n")
        return("Success")
        
      }, error = function(e) {
        # Zachycení jakékoli chyby z vnitřku (stop() nebo jiné)
        cat("--- ERROR captured by tryCatch (Comparison) ---\n"); print(e)
        rv$analysis_error <- paste("Chyba:", e$message)
        rv$results <- NULL # Resetujeme výsledky při chybě
        rv$plot_data_list <- NULL
        cat("DEBUG COMPARISON: rv$analysis_error set to:", rv$analysis_error, "\n")
        return("Error")
      }) # konec tryCatch
      
      rv$is_analyzing <- FALSE # Ukončení indikátoru načítání
      cat("DEBUG COMPARISON: rv$is_analyzing set to FALSE. Status:", analysis_status, "\n")
      cat("--- run_comparison_analysis END ---\n")
    }) # konec observeEvent(input$run_analysis)
    
    # --- Výstup pro podmíněné zobrazení výsledků ---
    output$showComparisonResults <- reactive({
      !is.null(rv$results) && is.null(rv$analysis_error)
    })
    outputOptions(output, 'showComparisonResults', suspendWhenHidden = FALSE)
    
    
    # --- Přímé renderování výstupů (UI elementy) ---
    
    # Zobrazení chyb
    output$analysis_error_ui <- renderUI({
      if (!is.null(rv$analysis_error)) {
        # Zobrazíme hlavní chybu, která přerušila analýzu
        tags$div(class = "alert alert-danger", role = "alert", tags$strong("Chyba analýzy: "), rv$analysis_error)
      } else if (!is.null(rv$results) && length(rv$results$tests) > 0) {
        # Pokud analýza proběhla, zobrazíme případné dílčí chyby/varování z testů
        warnings_errors <- list()
        test_list_to_check <- rv$results$tests
        
        for(test_name_key in names(test_list_to_check)) {
          test_res <- test_list_to_check[[test_name_key]]
          # Získáme správný název proměnné/porovnání
          feature_label <- if(rv$is_paired) paste(rv$results$feature_columns, collapse=" vs ") else test_res$feature_column %||% test_name_key
          
          if(!is.null(test_res$error_message)) {
            warnings_errors[[length(warnings_errors) + 1]] <- tags$div(class = "alert alert-danger mt-1", role = "alert", style="font-size: 0.9em;", tags$strong(paste("Chyba pro", feature_label, ":")), test_res$error_message)
          } else if (!is.null(test_res$warning_message)) {
            warnings_errors[[length(warnings_errors) + 1]] <- tags$div(class = "alert alert-warning mt-1", role = "alert", style="font-size: 0.9em;", tags$strong(paste("Varování pro", feature_label, ":")), test_res$warning_message)
          }
        }
        if (length(warnings_errors) > 0) tagList(warnings_errors) else NULL
      } else {
        NULL # Nic k zobrazení
      }
    })
    
    # Shrnutí provedeného testu
    output$test_summary_ui <- renderUI({
      req(rv$results)
      res <- rv$results
      if (res$type == "paired") {
        tagList(
          p(tags$strong("Typ testu:"), "Párový"),
          p(tags$strong("Porovnávané proměnné:"), tags$code(paste(res$feature_columns, collapse=" vs. "))),
          p(tags$strong("ID Sloupec:"), tags$code(res$target_column %||% "Nezadán")),
          p(tags$strong("Požadovaná metoda:"), tags$code(res$input_method))
        )
      } else { # Independent
        tagList(
          p(tags$strong("Typ testu:"), "Nezávislé skupiny"),
          p(tags$strong("Kategoriální proměnná (Skupiny):"), tags$code(res$target_column)),
          p(tags$strong("Počet skupin:"), res$num_levels),
          p(tags$strong("Porovnávané numerické proměnné:"), tags$code(paste(res$feature_columns, collapse=", "))),
          p(tags$strong("Požadovaná metoda:"), tags$code(res$input_method))
        )
      }
    })
    
    # Detailní výsledky (tabulka)
    output$results_table_dt <- renderDT({
      req(rv$results$tests)
      tests_list <- rv$results$tests
      
      if (rv$is_paired) {
        # Tabulka pro párový test
        res <- tests_list$paired_result
        results_df <- data.frame(
          `Porovnání` = paste(rv$results$feature_columns, collapse=" vs "),
          `Použitý test` = res$test_name %||% "-",
          `Důvod (Auto)` = res$reason %||% "-",
          `Statistika` = formatStatR(res$statistic),
          `Náz. Stat.` = res$statistic_name %||% "-",
          `St. volnosti` = res$degrees_freedom %||% "-",
          `p-hodnota` = formatPValueR(res$p_value),
          `Významné` = if(is.na(res$p_value)) "-" else if(res$p_value < 0.05) '<span style="color:green;" title="Statisticky významné (p < 0.05)"><i class="fa fa-check"></i> Ano</span>' else '<span style="color:grey;" title="Není statisticky významné (p >= 0.05)"><i class="fa fa-times"></i> Ne</span>',
          check.names = FALSE
        )
      } else {
        # Tabulka pro nezávislé testy
        results_df <- lapply(names(tests_list), function(feature_name) {
          res <- tests_list[[feature_name]]
          data.frame(
            `Numerická proměnná` = feature_name,
            `Použitý test` = res$test_name %||% "-",
            `Důvod (Auto)` = res$reason %||% "-",
            `Statistika` = formatStatR(res$statistic),
            `Náz. Stat.` = res$statistic_name %||% "-",
            `St. volnosti` = res$degrees_freedom %||% "-",
            `p-hodnota` = formatPValueR(res$p_value),
            `Významné` = if(is.na(res$p_value)) "-" else if(res$p_value < 0.05) '<span style="color:green;" title="Statisticky významné (p < 0.05)"><i class="fa fa-check"></i> Ano</span>' else '<span style="color:grey;" title="Není statisticky významné (p >= 0.05)"><i class="fa fa-times"></i> Ne</span>',
            check.names = FALSE
          )
        }) %>% bind_rows()
      }
      
      datatable(results_df,
                rownames = FALSE, escape = FALSE, # escape=FALSE umožňuje HTML v buňkách
                options = list(dom = 't', # Zobrazí jen tabulku (bez vyhledávání, stránkování atd.)
                               paging = FALSE,
                               searching = FALSE,
                               ordering = TRUE, # Povolíme řazení podle sloupců
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Czech.json')))
    }, server = FALSE) # server=FALSE je OK pro malé tabulky
    
    
    # --- Dynamické UI pro grafy ---
    output$plots_ui <- renderUI({
      req(rv$plot_data_list)
      plot_outputs <- list()
      
      if (rv$is_paired) {
        # UI pro párové grafy
        plot_outputs[[1]] <- fluidRow(
          column(6, h6("Distribuce rozdílů"), plotlyOutput(ns("plot_difference"))),
          column(6, h6("Propojené páry"), plotlyOutput(ns("plot_paired_lines")))
        )
      } else {
        # UI pro nezávislé grafy (boxploty)
        plot_names <- names(rv$plot_data_list)
        plot_plotly_outputs <- lapply(plot_names, function(name) {
          plotlyOutput(ns(paste0("plot_", name)))
        })
        # Rozdělení do řádků po dvou grafech
        num_plots <- length(plot_plotly_outputs)
        for (i in seq(1, num_plots, by = 2)) {
          col1 <- column(6, plot_plotly_outputs[[i]])
          col2 <- if ((i + 1) <= num_plots) column(6, plot_plotly_outputs[[i+1]]) else column(6) # Prázdný sloupec, pokud je lichý počet
          plot_outputs[[length(plot_outputs) + 1]] <- fluidRow(col1, col2)
        }
      }
      # Vrátí seznam UI elementů (řádků s grafy)
      do.call(tagList, plot_outputs)
    })
    
    # --- Generování jednotlivých grafů ---
    observe({
      req(rv$plot_data_list)
      plot_data_names <- names(rv$plot_data_list)
      
      if (rv$is_paired) {
        # --- Grafy pro párový test ---
        # 1. Boxplot/Histogram rozdílů
        output$plot_difference <- renderPlotly({
          req(rv$plot_data_list$difference_plot)
          plot_df_diff <- rv$plot_data_list$difference_plot
         
          p <- plot_ly(data = plot_df_diff, y = ~Difference, type = "box",
                       name="Rozdíl", boxpoints = "all", jitter = 0.3, pointpos = 0) %>%
            layout(title = paste("Distribuce rozdílů (", rv$results$feature_columns[1], " - ", rv$results$feature_columns[2], ")"),
                   yaxis = list(title = "Rozdíl", zeroline=TRUE),
                   xaxis = list(title = ""),
                   showlegend = FALSE)
          print(p)
        })
        
        # 2. Propojený graf párů
        output$plot_paired_lines <- renderPlotly({
          req(rv$plot_data_list$paired_plot)
          plot_df_paired <- rv$plot_data_list$paired_plot
          id_var_name <- names(plot_df_paired)[1] # Získáme název ID sloupce
          p <- plot_ly(data = plot_df_paired, x = ~Variable, y = ~Value,
                       # Barva podle ID, ale skryjeme legendu pokud je moc ID
                       color = ~ID_Internal,
                       type = 'scatter', mode = 'lines+markers',
                       # Text pro hover
                       text = ~paste("ID:", ID_Internal, "<br>Hodnota:", round(Value, 3)),
                       hoverinfo = 'text') %>%
            layout(title = "Hodnoty pro jednotlivé páry/subjekty",
                   xaxis = list(title = "Měření/Proměnná"),
                   yaxis = list(title = "Hodnota"),
                   showlegend = length(unique(plot_df_paired$ID_Internal)) <= 15) # Zobrazit legendu jen pro málo ID
          print(p)
        })
        
      } else {
        # --- Grafy pro nezávislé skupiny (boxploty) ---
        lapply(plot_data_names, function(feature_name) {
          output[[paste0("plot_", feature_name)]] <- renderPlotly({
            plot_df_single <- rv$plot_data_list[[feature_name]]
            target_name <- rv$results$target_column # Skutečný název grupovací proměnné
            
            tryCatch({
              p <- plot_ly(data = plot_df_single, y = ~Value, color = ~Group, type = "box",
                           boxpoints = "all", jitter = 0.3, pointpos = 0, name = ~Group) %>% # Přidáno name, boxpoints a jitter
                layout(title = paste("Distribuce", feature_name, "podle", target_name),
                       yaxis = list(title = feature_name),
                       xaxis = list(title = target_name),
                       boxmode = 'group', # Seskupí boxy podle barvy
                       showlegend = TRUE) # Zobrazí legendu
              print(p)
            }, error = function(e) {
              cat(paste("--- ERROR plotting feature", feature_name, ":", e$message, "\n"))
              plot_ly() %>% layout(title = paste("Chyba grafu pro", feature_name),
                                   annotations = list(text = paste("Chyba:", e$message), showarrow=FALSE))
            })
          })
        })
      }
    })
    
    
    # --- AI Interpretace ---
    # UI a reset zůstávají stejné
    output$ai_error_ui <- renderUI({ if (!is.null(rv$ai_error)) { tags$div(class = "alert alert-warning", role = "alert", tags$strong("Chyba AI: "), rv$ai_error, actionButton(ns("run_interpretation"), "Zkusit znovu", icon = icon("sync"), class = "btn btn-warning btn-xs pull-right")) } else {NULL} })
    output$ai_interpretation_ui <- renderUI({ req(!is.null(rv$results) && is.null(rv$analysis_error)); if(!rv$is_interpreting && is.null(rv$ai_interpretation) && is.null(rv$ai_error)) { actionButton(ns("run_interpretation"), "Interpretovat", icon = icon("lightbulb"), class = "btn-info") } else if(rv$is_interpreting) { p(icon("spinner", class = "fa-spin"), " AI pracuje...") } else if(!is.null(rv$ai_interpretation)) { wellPanel(style="background-color: #e9f5ff;", tags$b("Interpretace:"), tags$p(style="white-space: pre-wrap;", rv$ai_interpretation), actionButton(ns("reset_interpretation"), "Skrýt / Nová", icon = icon("sync"), class = "btn-link btn-sm")) } else { NULL } })
    observeEvent(input$reset_interpretation, { cat("DEBUG COMPARISON: Reset AI\n"); rv$ai_interpretation <- NULL; rv$ai_error <- NULL; rv$is_interpreting <- FALSE })
    
    # AI Volání (upraveno pro rozlišení typů)
    observeEvent(input$run_interpretation, {
      cat("\n--- run_comparison_interpretation START (Paired:", rv$is_paired, ") ---\n");
      req(rv$results$tests);
      res <- rv$results # Pro snazší přístup
      rv$is_interpreting <- TRUE; rv$ai_interpretation <- NULL; rv$ai_error <- NULL;
      
      # --- Příprava payloadu a promptů pro AI ---
      if (res$type == "paired") {
        test_res <- res$tests$paired_result
        payload <- list(
          analysis_type = "paired_comparison",
          compared_variables = res$feature_columns,
          id_variable = res$target_column, # Může být "N/A (Implicitní páry)"
          test_used = test_res$test_name %||% res$input_method,
          p_value = test_res$p_value, statistic = test_res$statistic, statistic_name = test_res$statistic_name,
          significant = if(is.na(test_res$p_value)) NA else test_res$p_value < 0.05,
          warning = test_res$warning_message, error = test_res$error_message
        )
        # System Prompt pro párový test
        system_prompt <- paste(
          "Jsi AI asistent specializující se na analýzu dat. Uživatel provedl párový test porovnávající dvě měření ('", payload$compared_variables[1], "' a '", payload$compared_variables[2], "') u stejných subjektů/párů",
          if(payload$id_variable != "N/A (Implicitní páry)") paste0("(identifikovaných pomocí '", payload$id_variable, "').") else ".",
          "\n\nTvým úkolem je shrnout a interpretovat výsledek tohoto testu v češtině, jednoduchým a srozumitelným jazykem.\n",
          "1. Uveď porovnávané proměnné a použitý test: Např. 'Pro porovnání ", payload$compared_variables[1], " a ", payload$compared_variables[2], " byl použit ", payload$test_used, "...'\n",
          "2. Interpretuj p-hodnotu:\n",
          "   - Pokud p < 0.05: Zdůrazni, že byl nalezen STATISTICKY VÝZNAMNÝ ROZDÍL mezi hodnotami těchto dvou proměnných.\n",
          "   - Pokud p >= 0.05: Uveď, že NEBYL NALEZEN statisticky významný rozdíl.\n",
          "   - Pokud p je NA nebo chyba: Zmiň, že test selhal nebo neproběhl.\n",
          "3. Zmínka o statistice (volitelné): Můžeš krátce zmínit hodnotu (", payload$statistic_name %||% "stat", ": ", formatStatR(payload$statistic), ").\n",
          "4. Varování/Chyba: Pokud existuje (", payload$warning %||% payload$error %||% "žádné", "), zmiň ho.\n",
          "\nDůležité: Zdůrazni, že statistická významnost ≠ praktická významnost a nelze usuzovat na kauzalitu.\n",
          "Pravidla: Odpovídej v češtině, stručně, věcně, formátuj do odstavců.",
          sep = ""
        )
        # User Prompt pro párový test
        p_val_str <- if(is.null(payload$p_value) || is.na(payload$p_value)) "N/A" else formatPValueR(payload$p_value)
        stat_str <- if(is.null(payload$statistic) || is.na(payload$statistic)) "" else paste0(", ", payload$statistic_name %||% "stat", "=", formatStatR(payload$statistic))
        warn_err_str <- if(!is.null(payload$error)) paste(" (Chyba:", payload$error, ")") else if(!is.null(payload$warning)) paste(" (Varování:", payload$warning, ")") else ""
        full_user_prompt <- paste(
          "Provedl jsem párový test.",
          paste0("- Porovnávané proměnné: '", payload$compared_variables[1], "' vs '", payload$compared_variables[2], "'"),
          paste0("- Použitý test: '", payload$test_used, "'"),
          paste0("- Výsledek: p=", p_val_str, stat_str, warn_err_str),
          "\nProsím, interpretuj tento výsledek.",
          collapse = "\n"
        )
        
      } else { # Nezávislé skupiny
        tests_summary_for_ai <- lapply(names(res$tests), function(feature_name){ test_res <- res$tests[[feature_name]]; list(feature = feature_name, test_used = test_res$test_name %||% res$input_method, p_value = test_res$p_value, statistic = test_res$statistic, statistic_name = test_res$statistic_name, significant = if(is.na(test_res$p_value)) NA else test_res$p_value < 0.05, warning = test_res$warning_message, error = test_res$error_message ) })
        payload <- list( analysis_type = "group_comparison", target_variable = res$target_column, number_of_groups = res$num_levels, compared_features = res$feature_columns, results_per_feature = tests_summary_for_ai )
        
        # System Prompt pro nezávislé skupiny (původní)
        system_prompt <- paste( "Jsi AI asistent specializující se na analýzu dat. Uživatel provedl porovnání skupin definovaných kategoriální proměnnou ('", payload$target_variable, "', ", payload$number_of_groups, " skupiny) pro několik numerických proměnných.", "\n\nTvým úkolem je shrnout a interpretovat výsledky testů pro každou numerickou proměnnou v češtině, jednoduchým a srozumitelným jazykem.\n", "Pro každou porovnávanou numerickou proměnnou ('feature'):", "1. Uveď proměnnou a použitý test.", "2. Interpretuj p-hodnotu:", "   - p < 0.05: STATISTICKY VÝZNAMNÝ ROZDÍL mezi skupinami.", "   - p >= 0.05: NEBYL NALEZEN statisticky významný rozdíl.", "   - p je NA/chyba: Test selhal/neproběhl.", "3. Zmínka o statistice (volitelné).", "4. Varování/Chyba: Pokud existuje, zmiň ho.", "\nCelkové shrnutí: Stručně shrň, kde byly nalezeny významné rozdíly.", "\nDůležité: Zdůrazni, že stat. významnost ≠ praktická významnost a nelze usuzovat na kauzalitu.", "\nPravidla: Odpovídej v češtině, stručně, věcně, pro každou proměnnou zvlášť + souhrn, formátuj do odstavců.", sep = "\n" )
        # User Prompt pro nezávislé skupiny (původní)
        user_prompt_parts <- list( paste0("Provedl jsem analýzu typu '", payload$analysis_type, "'."), paste0("- Cílová proměnná (skupiny): '", payload$target_variable, "' (Počet skupin: ", payload$number_of_groups, ")"), paste0("- Porovnávané numerické proměnné: ", paste(payload$compared_features, collapse=", ")), "\n- Výsledky pro jednotlivé proměnné:" )
        for(feat_res in payload$results_per_feature) { p_val_str <- if(is.null(feat_res$p_value) || is.na(feat_res$p_value)) "N/A" else formatPValueR(feat_res$p_value); stat_str <- if(is.null(feat_res$statistic) || is.na(feat_res$statistic)) "" else paste0(", ", feat_res$statistic_name %||% "stat", "=", formatStatR(feat_res$statistic)); warn_err_str <- if(!is.null(feat_res$error)) paste(" (Chyba:", feat_res$error, ")") else if(!is.null(feat_res$warning)) paste(" (Varování:", feat_res$warning, ")") else ""; user_prompt_parts[[length(user_prompt_parts)+1]] <- paste0("  - ", feat_res$feature, ": Test='", feat_res$test_used, "', p=", p_val_str, stat_str, warn_err_str) }
        user_prompt_parts[[length(user_prompt_parts)+1]] <- "\nProsím, interpretuj tyto výsledky."
        full_user_prompt <- paste(user_prompt_parts, collapse = "\n")
      }
      
      cat("DEBUG COMPARISON: User Prompt for AI:\n", full_user_prompt, "\n");
      
      # --- API Volání a zpracování odpovědi (beze změny) ---
      api_key <- Sys.getenv("OPENROUTER_API_KEY", "NA"); if (api_key == "NA" || nchar(api_key) < 10) { cat("DEBUG COMPARISON: Missing API Key\n"); rv$ai_error <- "API klíč (OPENROUTER_API_KEY) není nastaven."; rv$is_interpreting <- FALSE; return() }
      api_error_msg <- NULL
      response <- tryCatch({ POST(url = "https://openrouter.ai/api/v1/chat/completions", add_headers(Authorization = paste("Bearer", api_key), `Content-Type` = "application/json"), body = toJSON(list(model = "deepseek/deepseek-chat-v3-0324:free", messages = list(list(role = "system", content = system_prompt), list(role = "user", content = full_user_prompt)), max_tokens = 1000 ), auto_unbox = TRUE, na = "null"), encode = "json", timeout(90)) }, error = function(e) { cat("--- ERROR API call ---\n"); print(e); api_error_msg <<- paste("Chyba spojení s AI API:", e$message); return(NULL) })
      
      cat("DEBUG COMPARISON: AI API call finished.\n");
      
      isolate({
        if (!is.null(api_error_msg)) { cat("DEBUG COMPARISON: AI Error from tryCatch:", api_error_msg, "\n"); rv$ai_error <- api_error_msg }
        else if (is.null(response)) { cat("DEBUG COMPARISON: API call returned NULL unexpectedly.\n"); rv$ai_error <- "Neočekávaná chyba API (NULL response)." }
        else { status <- status_code(response); cat("DEBUG COMPARISON: AI API Status:", status, "\n"); if (status >= 300) { err_content <- httr::content(response, "text", encoding="UTF-8"); err_details <- tryCatch(fromJSON(err_content)$error$message, error=function(e) substr(err_content, 1, 300)); rv$ai_error <- paste("Chyba API (", status, "): ", err_details); cat("DEBUG COMPARISON: Parsed AI Error:", rv$ai_error,"\n") }
        else { content <- tryCatch(httr::content(response, "parsed"), error = function(e) NULL); if (is.null(content)) { cat("DEBUG COMPARISON: Failed to parse JSON response\n"); rv$ai_error <- "Nepodařilo se zpracovat odpověď z AI (neplatný JSON)."; rv$ai_interpretation <- NULL; cat("DEBUG COMPARISON: Raw response text:\n", httr::content(response, "text", encoding="UTF-8"), "\n") }
        else { interpretation_text <- content$choices[[1]]$message$content; if (!is.null(interpretation_text) && nchar(trimws(interpretation_text)) > 0) { cat("DEBUG COMPARISON: AI Success\n"); rv$ai_interpretation <- trimws(interpretation_text); rv$ai_error <- NULL }
        else { cat("DEBUG COMPARISON: AI empty or invalid response\n"); rv$ai_error <- "AI nevrátila platnou interpretaci."; rv$ai_interpretation <- NULL; print(content) } } } }
        rv$is_interpreting <- FALSE; cat("DEBUG COMPARISON: is_interpreting set to FALSE.\n");
      }) # Konec isolate()
      cat("--- run_comparison_interpretation END ---\n\n")
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
  }) # konec moduleServer
}