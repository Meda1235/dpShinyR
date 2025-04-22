# --- app.R ---
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(plotly)
library(stats)
library(MASS) # boxcox
library(nortest) # ad.test, lillie.test (alternatives to shapiro)
library(cluster) # silhouette, pam
library(factoextra) # fviz_cluster, fviz_pca_biplot
library(caret) # createDataPartition, preProcess, confusionMatrix, varImp
library(randomForest)
library(rpart)
library(e1071) # naiveBayes
library(class) # knn
library(glmnet) # Ridge, Lasso, ElasticNet
library(car) # leveneTest
library(nnet) # multinom
# library(dbscan) # Load if needed

# Helper function for Modes
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# --- UI Definition ---
ui <- fluidPage(
  theme = shinytheme("cerulean"), # Choose a theme
  titlePanel("Interactive Data Analysis Tool (R Shiny Version)"),
  
  navbarPage(
    "Analysis Steps",
    id = "main_nav",
    
    # 1. Data Upload and Overview Tab
    tabPanel("1. Data",
             sidebarLayout(
               sidebarPanel(
                 h4("Upload Data"),
                 fileInput("file1", "Choose CSV or Excel File",
                           accept = c(".csv", ".xlsx", ".xls")),
                 checkboxInput("header", "File Contains Header", TRUE),
                 conditionalPanel(
                   condition = "input.file1 && input.file1.name.endsWith('.csv')",
                   selectInput("sep", "CSV Separator", c(Comma=",", Semicolon=";", Tab="\t"), ",")
                 ),
                 conditionalPanel(
                   condition = "input.file1 && (input.file1.name.endsWith('.xlsx') || input.file1.name.endsWith('.xls'))",
                   numericInput("sheet_index", "Excel Sheet Index", 1, min = 1)
                 ),
                 hr(),
                 h4("Analyze Columns"),
                 actionButton("analyze_cols_btn", "Analyze Column Types", icon("cogs")),
                 hr(),
                 h4("Update Column Type"),
                 uiOutput("col_to_update_ui"),
                 selectInput("new_col_type", "New Type", choices = c("Categorical", "Numeric")),
                 actionButton("update_type_btn", "Update Type", icon("edit"))
               ),
               mainPanel(
                 h4("Data Preview"),
                 DTOutput("data_table_raw"),
                 hr(),
                 h4("Column Analysis Results"),
                 DTOutput("col_analysis_table")
               )
             )
    ), # End Tab 1: Data
    
    # 2. Preprocessing Tab
    tabPanel("2. Preprocessing",
             tabsetPanel(
               # 2a. Missing Values
               tabPanel("Missing Values",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Handle Missing Values"),
                            uiOutput("missing_strategy_ui"),
                            actionButton("fill_missing_btn", "Apply Strategies", icon("magic")),
                            hr(),
                            actionButton("undo_fill_btn", "Undo Last Fill", icon("undo"), disabled = TRUE) # Added Undo
                          ),
                          mainPanel(
                            h4("Data After Handling Missing Values"),
                            verbatimTextOutput("missing_summary"),
                            DTOutput("data_table_missing")
                          )
                        )
               ),
               # 2b. Outliers
               tabPanel("Outliers",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Detect Outliers (Z-score > 3)"),
                            actionButton("detect_outliers_btn", "Detect", icon("search")),
                            hr(),
                            h4("Handle Outliers"),
                            selectInput("outlier_col_select", "Select Column (or ALL)", choices = c("ALL")),
                            selectInput("outlier_method", "Method",
                                        choices = c("Remove Rows" = "remove",
                                                    "Replace with Mean" = "replace_mean",
                                                    "Replace with Median" = "replace_median",
                                                    "Clip at 3 SD" = "clip",
                                                    "Replace with Custom" = "replace_custom")),
                            conditionalPanel(
                              condition = "input.outlier_method == 'replace_custom'",
                              numericInput("outlier_custom_val", "Custom Value", 0)
                            ),
                            actionButton("handle_outliers_btn", "Apply Handling", icon("wrench"))
                          ),
                          mainPanel(
                            h4("Outlier Detection Summary"),
                            DTOutput("outlier_summary_table"),
                            hr(),
                            h4("Data After Handling Outliers"),
                            DTOutput("data_table_outlier")
                          )
                        )
               ),
               # 2c. Normality & Transformation
               tabPanel("Normality & Transformation",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Check Normality"),
                            selectInput("normality_test_method", "Preferred Test",
                                        choices = c("Auto (Shapiro < 50, KS >= 50)" = "auto",
                                                    "Shapiro-Wilk" = "shapiro",
                                                    "Kolmogorov-Smirnov (vs Normal)" = "ks")),
                            actionButton("check_normality_btn", "Check Normality", icon("chart-bar")),
                            hr(),
                            h4("Transform Column"),
                            uiOutput("transform_col_ui"),
                            selectInput("transform_method", "Method",
                                        choices = c("Log (ln)" = "log",
                                                    "Square Root" = "sqrt",
                                                    "Box-Cox" = "boxcox")),
                            actionButton("transform_col_btn", "Apply Transformation", icon("exchange-alt"))
                          ),
                          mainPanel(
                            h4("Normality Test Results"),
                            DTOutput("normality_results_table"),
                            hr(),
                            h4("Data After Transformation"),
                            DTOutput("data_table_transformed")
                          )
                        )
               )
             ) # End tabsetPanel
    ), # End Tab 2: Preprocessing
    
    # 3. Relationships Tab
    tabPanel("3. Relationships",
             tabsetPanel(
               # 3a. Correlation
               tabPanel("Correlation",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Correlation Analysis"),
                            uiOutput("corr_vars_ui"), # Select multiple numeric vars
                            selectInput("corr_method", "Method",
                                        choices = c("Auto (based on normality)" = "auto",
                                                    "Pearson" = "pearson",
                                                    "Spearman" = "spearman",
                                                    "Kendall" = "kendall")),
                            actionButton("run_corr_btn", "Calculate Correlation", icon("link"))
                          ),
                          mainPanel(
                            h4("Correlation Results"),
                            verbatimTextOutput("corr_reason_output"),
                            h5("Correlation Matrix"),
                            DTOutput("corr_matrix_table"),
                            h5("Detailed Results"),
                            DTOutput("corr_details_table"),
                            h5("Scatter Plot (if 2 variables selected)"),
                            plotlyOutput("corr_scatter_plot")
                          )
                        )
               ),
               # 3b. Dependency Test
               tabPanel("Dependency Test",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Test of Dependency"),
                            uiOutput("dep_vars_ui"), # Select 2 variables (numeric or categorical)
                            selectInput("dep_method", "Method",
                                        choices = c("Auto" = "auto",
                                                    "Chi-squared (Cat vs Cat)" = "chi2",
                                                    "Fisher's Exact (2x2 Cat)" = "fisher",
                                                    "ANOVA (Cat vs Num)" = "anova",
                                                    "Kruskal-Wallis (Cat vs Num, non-normal)" = "kruskal",
                                                    "Wilcoxon Paired (Num vs Num, paired)" = "wilcoxon",
                                                    "Mann-Whitney U (Num vs Num, independent)" = "mannwhitney"
                                        )),
                            checkboxInput("dep_paired", "Data are Paired? (for Wilcoxon)", FALSE), # Checkbox for paired option
                            actionButton("run_dep_btn", "Run Test", icon("vials"))
                          ),
                          mainPanel(
                            h4("Dependency Test Results"),
                            verbatimTextOutput("dep_test_output"),
                            h5("Contingency Table (for Chi2/Fisher)"),
                            DTOutput("dep_contingency_table") # Changed from verbatimTextOutput
                          )
                        )
               ),
               # 3c. Regression
               tabPanel("Regression",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Regression Analysis"),
                            uiOutput("reg_y_var_ui"), # Select 1 dependent variable
                            uiOutput("reg_x_vars_ui"), # Select 1+ independent variables
                            selectInput("reg_method", "Method",
                                        choices = c("Auto" = "auto",
                                                    "Linear (OLS)" = "ols",
                                                    "Ridge" = "ridge",
                                                    "Lasso" = "lasso",
                                                    "Elastic Net" = "elasticnet",
                                                    "Logistic (Binary)" = "logistic",
                                                    "Logistic (Multinomial)" = "multinomial")),
                            sliderInput("reg_test_size", "Test Set Size", min = 0.1, max = 0.5, value = 0.2, step = 0.05),
                            numericInput("reg_seed", "Random Seed", 42),
                            actionButton("run_reg_btn", "Run Regression", icon("chart-line"))
                          ),
                          mainPanel(
                            h4("Regression Results"),
                            verbatimTextOutput("reg_summary_output"),
                            h5("Coefficients"),
                            DTOutput("reg_coeffs_table"),
                            h5("Model Performance"),
                            DTOutput("reg_metrics_table"),
                            h5("Residual Plot (Predicted vs Residuals)"),
                            plotlyOutput("reg_residual_plot"),
                            h5("Actual vs Predicted Plot (Test Set)"),
                            plotlyOutput("reg_actual_pred_plot") # Added plot
                          )
                        )
               )
             ) # End tabsetPanel
    ), # End Tab 3: Relationships
    
    # 4. Group Comparison Tab
    tabPanel("4. Group Comparison",
             sidebarLayout(
               sidebarPanel(
                 h4("Compare Groups"),
                 uiOutput("group_num_vars_ui"), # Select 1+ numeric variables
                 uiOutput("group_cat_var_ui"), # Select 1 categorical variable (grouping)
                 checkboxInput("group_paired", "Data are Paired?", FALSE),
                 conditionalPanel(
                   condition = "input.group_paired == true",
                   helpText("Paired requires exactly 2 numeric variables selected.")
                 ),
                 actionButton("run_group_comp_btn", "Compare Groups", icon("users"))
               ),
               mainPanel(
                 h4("Group Comparison Results"),
                 verbatimTextOutput("group_comp_output") # Display results here
               )
             )
    ), # End Tab 4: Group Comparison
    
    
    # 5. Unsupervised Learning Tab
    tabPanel("5. Unsupervised",
             tabsetPanel(
               # 5a. Clustering
               tabPanel("Clustering",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Cluster Analysis"),
                            uiOutput("cluster_vars_ui"), # Select 2+ numeric variables
                            selectInput("cluster_algo", "Algorithm",
                                        choices = c("Auto" = "auto",
                                                    "K-Means" = "kmeans",
                                                    "Hierarchical" = "hierarchical",
                                                    "DBSCAN" = "dbscan")),
                            conditionalPanel(
                              condition = "input.cluster_algo == 'kmeans' || input.cluster_algo == 'hierarchical'",
                              numericInput("num_clusters", "Number of Clusters (k)", 3, min = 2)
                            ),
                            conditionalPanel(
                              condition = "input.cluster_algo == 'auto' && input.cluster_algo != 'dbscan'", # Allow k suggestion in auto if not DBSCAN
                              helpText("Enter desired k, or leave blank for auto-detection (K-Means/Hierarchical).")
                            ),
                            selectInput("cluster_dist", "Distance Metric (for Hierarchical/DBSCAN)",
                                        choices = c("Euclidean" = "euclidean",
                                                    "Manhattan" = "manhattan",
                                                    "Cosine (Needs package like 'lsa')" = "cosine")), # Note dependency
                            checkboxInput("cluster_standardize", "Standardize Data", TRUE),
                            actionButton("run_cluster_btn", "Run Clustering", icon("project-diagram"))
                          ),
                          mainPanel(
                            h4("Clustering Results"),
                            verbatimTextOutput("cluster_summary_output"),
                            h5("Cluster Visualization (PCA - First 2 Components)"),
                            plotlyOutput("cluster_pca_plot"),
                            h5("PCA Biplot (Variable Contributions)"),
                            plotOutput("cluster_pca_biplot"), # Use base plot for biplot initially
                            h5("Cluster Summary Statistics"),
                            DTOutput("cluster_stats_table"),
                            h5("Silhouette Score (if applicable)"),
                            verbatimTextOutput("cluster_silhouette_output")
                          )
                        )
               )
               # Add Factor Analysis Panel here if needed later
             ) # End tabsetPanel
    ), # End Tab 5: Unsupervised
    
    # 6. Supervised Learning Tab
    tabPanel("6. Supervised",
             tabsetPanel(
               # 6a. Classification
               tabPanel("Classification",
                        sidebarLayout(
                          sidebarPanel(
                            h4("Classification Analysis"),
                            uiOutput("classif_features_ui"), # Select features
                            uiOutput("classif_target_ui"),   # Select target
                            selectInput("classif_algo", "Algorithm",
                                        choices = c("Auto" = "auto",
                                                    "Logistic Regression" = "logistic_regression",
                                                    "K-Nearest Neighbors (KNN)" = "knn",
                                                    "Decision Tree" = "decision_tree",
                                                    "Random Forest" = "random_forest",
                                                    "Naive Bayes" = "naive_bayes")),
                            checkboxInput("classif_standardize", "Standardize Numeric Features", TRUE),
                            sliderInput("classif_test_size", "Test Set Size", min = 0.1, max = 0.5, value = 0.25, step = 0.05),
                            conditionalPanel(
                              condition = "input.classif_algo == 'knn' || input.classif_algo == 'auto'",
                              numericInput("classif_knn_k", "KNN: Number of Neighbors (k)", 5, min = 1)
                            ),
                            numericInput("classif_seed", "Random Seed", 42),
                            actionButton("run_classif_btn", "Run Classification", icon("tasks"))
                          ),
                          mainPanel(
                            h4("Classification Results"),
                            verbatimTextOutput("classif_summary_output"),
                            h5("Performance Metrics (Test Set)"),
                            DTOutput("classif_metrics_table"),
                            h5("Confusion Matrix (Test Set)"),
                            verbatimTextOutput("classif_conf_matrix_output"), # Use verbatim for base table output
                            plotOutput("classif_conf_matrix_plot"), # Use caret's plot
                            h5("Feature Importance (if available)"),
                            DTOutput("classif_importance_table")
                          )
                        )
               )
               # Add other supervised tasks like Regression (if not covered elsewhere)
             ) # End tabsetPanel
    ) # End Tab 6: Supervised
    
    
  ) # End navbarPage
) # End fluidPage

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Reactive values to store data and results
  rv <- reactiveValues(
    data = NULL,                 # Original uploaded data
    data_processed = NULL,       # Data after preprocessing steps
    col_types = NULL,            # Column types analysis results
    normality = NULL,            # Normality test results
    outlier_summary = NULL,      # Outlier detection results
    last_filled_mask = NULL,     # Mask of cells filled in the last fill_missing op
    previous_data_state = NULL   # Store data before fill_missing for undo
  )
  
  # --- 1. Data Loading and Initial Analysis ---
  
  # Load data when file is uploaded
  data_raw <- reactive({
    req(input$file1)
    inFile <- input$file1
    fpath <- inFile$datapath
    ext <- tools::file_ext(fpath)
    
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(fpath, header = input$header, sep = input$sep, stringsAsFactors = FALSE, na.strings=c("","NA","NaN", "NULL", "None"))
      } else if (ext %in% c("xlsx", "xls")) {
        read_excel(fpath, sheet = input$sheet_index, col_names = input$header, na=c("","NA","NaN", "NULL", "None")) %>%
          mutate(across(everything(), as.character)) # Read everything as character initially for flexibility
      } else {
        stop("Unsupported file type.")
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
      return(NULL)
    })
    
    if(!is.null(df)) {
      # Replace empty strings with NA after loading
      df[df == ""] <- NA
    }
    return(df)
  })
  
  # Update reactive values when raw data changes
  observe({
    df_raw <- data_raw()
    if (!is.null(df_raw) && nrow(df_raw) > 0 && ncol(df_raw) > 0) {
      # Attempt to convert columns that look numeric
      df_typed <- df_raw %>%
        mutate(across(everything(), ~type.convert(as.character(.), as.is = TRUE, na.strings=c("","NA","NaN", "NULL", "None"))))
      
      rv$data <- df_typed
      rv$data_processed <- df_typed # Initialize processed data
      rv$col_types <- NULL # Reset analysis
      rv$normality <- NULL
      rv$outlier_summary <- NULL
      rv$last_filled_mask <- NULL
      rv$previous_data_state <- NULL
      # Update choices for UI elements
      update_select_inputs()
      # Reset undo button state
      updateActionButton(session, "undo_fill_btn", disabled = TRUE)
      showNotification("Data loaded successfully.", type = "message")
    } else {
      # Clear data if loading failed or file removed
      rv$data <- NULL
      rv$data_processed <- NULL
      rv$col_types <- NULL
      rv$normality <- NULL
      rv$outlier_summary <- NULL
      rv$last_filled_mask <- NULL
      rv$previous_data_state <- NULL
      # Optionally clear UI choices here too
      update_select_inputs(clear = TRUE)
      updateActionButton(session, "undo_fill_btn", disabled = TRUE)
      if (!is.null(input$file1)) { # Only show error if a file was selected but failed
        # Error message shown in data_raw() tryCatch
      }
    }
  })
  
  
  # Display raw data table
  output$data_table_raw <- renderDT({
    req(rv$data)
    datatable(rv$data, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # Function to update common select inputs
  update_select_inputs <- function(clear = FALSE) {
    data_to_use <- rv$data_processed # Use potentially processed data for choices
    if (is.null(data_to_use) || clear) {
      cols <- character(0)
      num_cols <- character(0)
      cat_cols <- character(0)
    } else {
      cols <- names(data_to_use)
      # Use rv$col_types if available for better type distinction
      if (!is.null(rv$col_types)) {
        num_cols <- rv$col_types %>% filter(Type == "Numeric") %>% pull(Column)
        cat_cols <- rv$col_types %>% filter(Type == "Categorical") %>% pull(Column)
        # Ensure cols exist in current data
        num_cols <- intersect(num_cols, cols)
        cat_cols <- intersect(cat_cols, cols)
        # Fallback if col_types is inconsistent
        if (length(num_cols) + length(cat_cols) != length(cols)) {
          num_cols <- names(data_to_use)[sapply(data_to_use, is.numeric)]
          cat_cols <- names(data_to_use)[!sapply(data_to_use, is.numeric)]
        }
        
      } else {
        num_cols <- names(data_to_use)[sapply(data_to_use, is.numeric)]
        cat_cols <- names(data_to_use)[!sapply(data_to_use, is.numeric)]
      }
    }
    
    # Update UIs (Use safe_update_selectInput if needed)
    updateSelectInput(session, "outlier_col_select", choices = c("ALL", cols), selected = "ALL")
    updateSelectInput(session, "dep_paired", selected = FALSE) # Reset paired checkbox
    
    # Use uiOutput rendering functions now for dynamic choices
    output$col_to_update_ui <- renderUI({ selectInput("col_to_update", "Column to Update", choices = cols) })
    output$transform_col_ui <- renderUI({ selectInput("transform_col", "Column to Transform", choices = num_cols) })
    output$corr_vars_ui <- renderUI({ selectInput("corr_vars", "Select 2+ Numeric Variables", choices = num_cols, multiple = TRUE) })
    output$dep_vars_ui <- renderUI({ selectInput("dep_vars", "Select 2 Variables", choices = cols, multiple = TRUE, selected = NULL) }) # Allow selecting 2 of any type initially
    output$reg_y_var_ui <- renderUI({ selectInput("reg_y_var", "Dependent Variable (Y)", choices = cols) })
    output$reg_x_vars_ui <- renderUI({ selectInput("reg_x_vars", "Independent Variables (X)", choices = cols, multiple = TRUE) })
    output$group_num_vars_ui <- renderUI({ selectInput("group_num_vars", "Numeric Variable(s)", choices = num_cols, multiple = TRUE) })
    output$group_cat_var_ui <- renderUI({ selectInput("group_cat_var", "Categorical Variable (Group)", choices = cat_cols) })
    output$cluster_vars_ui <- renderUI({ selectInput("cluster_vars", "Select 2+ Numeric Variables", choices = num_cols, multiple = TRUE) })
    output$classif_features_ui <- renderUI({ selectInput("classif_features", "Feature Columns (X)", choices = cols, multiple = TRUE) })
    output$classif_target_ui <- renderUI({ selectInput("classif_target", "Target Column (Y - Categorical)", choices = cat_cols) })
    
    
    # Dynamic UI for missing value strategies
    output$missing_strategy_ui <- renderUI({
      req(rv$data_processed)
      col_names <- names(rv$data_processed)
      missing_info <- sapply(col_names, function(col) {
        sum(is.na(rv$data_processed[[col]]))
      })
      cols_with_missing <- names(missing_info[missing_info > 0])
      
      if (length(cols_with_missing) == 0) {
        return(p("No missing values detected in the current dataset."))
      }
      
      lapply(cols_with_missing, function(col) {
        current_type <- if (!is.null(rv$col_types)) {
          rv$col_types$Type[rv$col_types$Column == col]
        } else if (is.numeric(rv$data_processed[[col]])) {
          "Numeric"
        } else {
          "Categorical"
        }
        
        # Define choices based on column type
        choices <- c(
          "Drop Rows with Missing" = "drop",
          "Forward Fill (Previous Value)" = "ffill",
          "Backward Fill (Next Value)" = "bfill",
          "Replace with Mode" = "mode" # Always applicable
        )
        if (current_type == "Numeric") {
          choices <- c(choices,
                       "Replace with Mean" = "mean",
                       "Replace with Median" = "median",
                       "Replace with Zero" = "zero",
                       "Linear Interpolation" = "interpolate"
          )
        }
        # Reorder choices alphabetically by label maybe?
        choices <- choices[order(names(choices))]
        choices <- c("Do Nothing" = "none", choices) # Add 'Do Nothing'
        
        
        selectInput(inputId = paste0("strat_", col),
                    label = paste0("Strategy for '", col, "' (", missing_info[col], " missing)"),
                    choices = choices,
                    selected = "none") # Default to 'Do Nothing'
      })
    })
  }
  
  
  # Analyze columns button
  observeEvent(input$analyze_cols_btn, {
    req(rv$data)
    df <- rv$data
    
    if (is.null(df) || ncol(df) == 0) {
      showNotification("No data loaded to analyze.", type = "warning")
      return()
    }
    
    tryCatch({
      total_rows <- nrow(df)
      col_analysis <- lapply(names(df), function(col_name) {
        col_data <- df[[col_name]]
        non_na_data <- col_data[!is.na(col_data)]
        num_unique <- length(unique(non_na_data))
        missing_pct <- sum(is.na(col_data)) / total_rows * 100
        
        # Heuristic for type detection (similar to Python version)
        is_potentially_numeric <- suppressWarnings(!any(is.na(as.numeric(as.character(non_na_data)))))
        
        # Threshold based on dataset size
        if (total_rows < 200) {
          category_threshold <- 8
        } else if (total_rows < 1000) {
          category_threshold <- 10
        } else {
          category_threshold <- 15
        }
        
        col_type <- "Categorical" # Default
        if (is_potentially_numeric) {
          # Check if it *actually* converted to numeric well
          converted_numeric <- suppressWarnings(as.numeric(as.character(col_data)))
          if (!any(is.na(converted_numeric[ !is.na(col_data) ] ))) { # If non-NA original values converted without new NAs
            if (num_unique > category_threshold) {
              col_type <- "Numeric"
            } else {
              # If few unique values but looks numeric, could still be treated as numeric (e.g., rating 1-5)
              # Let's keep it numeric if it looks numeric and has > 2 unique values, otherwise category
              if (num_unique > 2) {
                col_type <- "Numeric" # Treat ordinal-like as numeric initially
              } else {
                col_type <- "Categorical" # Binary 0/1 better as Cat usually
              }
            }
            # Ensure the main data reflects this numeric conversion attempt if successful
            # This might be too aggressive - let's just report the type for now
            # rv$data[[col_name]] <<- converted_numeric # Be careful with modifying rv$data here
          } else {
            col_type <- "Categorical" # Failed numeric conversion for some values
          }
          
        } else {
          col_type <- "Categorical"
        }
        # Check if it's logical/boolean - treat as Categorical
        if (is.logical(col_data)) {
          col_type <- "Categorical"
        }
        
        
        return(data.frame(
          Column = col_name,
          Type = col_type,
          UniqueValues = num_unique,
          MissingPercent = sprintf("%.2f%%", missing_pct),
          stringsAsFactors = FALSE
        ))
      })
      
      rv$col_types <- bind_rows(col_analysis)
      # Update select inputs based on new types
      update_select_inputs()
      showNotification("Column analysis complete.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error during column analysis:", e$message), type = "error", duration = 10)
      rv$col_types <- NULL
    })
  })
  
  
  # Display column analysis results
  output$col_analysis_table <- renderDT({
    req(rv$col_types)
    datatable(rv$col_types, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # Update column type button
  observeEvent(input$update_type_btn, {
    req(rv$data_processed, rv$col_types, input$col_to_update, input$new_col_type)
    col_name <- input$col_to_update
    new_type <- input$new_col_type
    
    if (!col_name %in% names(rv$data_processed)) {
      showNotification("Selected column not found in the data.", type = "error")
      return()
    }
    
    current_data <- rv$data_processed
    col_index <- which(names(current_data) == col_name)
    
    tryCatch({
      if (new_type == "Numeric") {
        # Attempt conversion to numeric
        converted_col <- suppressWarnings(as.numeric(as.character(current_data[[col_name]])))
        if (any(is.na(converted_col) & !is.na(current_data[[col_name]]))) {
          showNotification(paste("Warning: Some values in column '", col_name, "' could not be converted to numeric and became NA."), type = "warning", duration = 8)
        }
        current_data[[col_name]] <- converted_col
      } else { # Categorical
        # Attempt conversion to factor (or character)
        current_data[[col_name]] <- as.factor(current_data[[col_name]])
        # Alternatively keep as character: current_data[[col_name]] <- as.character(current_data[[col_name]])
      }
      
      # Update the main data store
      rv$data_processed <- current_data
      
      # Update the column type table
      type_row_index <- which(rv$col_types$Column == col_name)
      if (length(type_row_index) > 0) {
        rv$col_types$Type[type_row_index] <- new_type
      } else {
        # This shouldn't happen if analyze was run, but handle defensively
        new_row <- data.frame(Column=col_name, Type=new_type, UniqueValues=NA, MissingPercent=NA, stringsAsFactors = FALSE)
        rv$col_types <- bind_rows(rv$col_types, new_row)
      }
      
      
      # Update UI elements that depend on column types
      update_select_inputs()
      showNotification(paste("Column '", col_name, "' type updated to", new_type, "and data converted."), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error updating column type:", e$message), type = "error", duration = 10)
      # Revert if failed? Maybe too complex, just notify.
    })
  })
  
  
  # --- 2. Preprocessing ---
  
  # --- 2a. Missing Values ---
  observeEvent(input$fill_missing_btn, {
    req(rv$data_processed)
    data_to_process <- rv$data_processed
    # Store the state *before* applying changes for potential undo
    rv$previous_data_state <- data_to_process
    
    # Collect strategies from dynamically generated inputs
    strategies <- list()
    col_names <- names(data_to_process)
    missing_info <- sapply(col_names, function(col) sum(is.na(data_to_process[[col]])))
    cols_with_missing <- names(missing_info[missing_info > 0])
    
    if (length(cols_with_missing) == 0) {
      showNotification("No missing values to handle.", type = "info")
      return()
    }
    
    for (col in cols_with_missing) {
      input_id <- paste0("strat_", col)
      if (!is.null(input[[input_id]]) && input[[input_id]] != "none") {
        strategies[[col]] <- input[[input_id]]
      }
    }
    
    
    if (length(strategies) == 0) {
      showNotification("No filling strategies selected.", type = "info")
      return()
    }
    
    
    # Create a mask to track changes for this operation
    current_filled_mask <- matrix(FALSE, nrow = nrow(data_to_process), ncol = ncol(data_to_process))
    colnames(current_filled_mask) <- names(data_to_process)
    
    
    tryCatch({
      rows_to_drop_mask <- rep(FALSE, nrow(data_to_process)) # Track rows marked for dropping
      
      for (col in names(strategies)) {
        method <- strategies[[col]]
        col_idx <- which(names(data_to_process) == col)
        original_na_mask <- is.na(data_to_process[[col]]) # Mask of NAs *before* filling this column
        
        
        if (method == "drop") {
          rows_to_drop_mask <- rows_to_drop_mask | original_na_mask
          # Don't modify the column data directly here, handle drops at the end
          next # Move to next strategy
        }
        
        value_to_fill <- NULL
        temp_col <- data_to_process[[col]] # Work on a temporary copy
        
        
        if (method == "mean") {
          if(is.numeric(temp_col)) value_to_fill <- mean(temp_col, na.rm = TRUE)
        } else if (method == "median") {
          if(is.numeric(temp_col)) value_to_fill <- median(temp_col, na.rm = TRUE)
        } else if (method == "mode") {
          value_to_fill <- get_mode(temp_col)
        } else if (method == "zero") {
          value_to_fill <- 0
        } else if (method == "ffill") {
          temp_col <- zoo::na.locf(temp_col, na.rm = FALSE)
          # Handle leading NAs if any remain
          temp_col <- zoo::na.locf(temp_col, na.rm = FALSE, fromLast = TRUE)
        } else if (method == "bfill") {
          temp_col <- zoo::na.locf(temp_col, na.rm = FALSE, fromLast = TRUE)
          # Handle trailing NAs if any remain
          temp_col <- zoo::na.locf(temp_col, na.rm = FALSE)
        } else if (method == "interpolate") {
          if(is.numeric(temp_col)) {
            # Ensure it's numeric before interpolating
            temp_col_num <- suppressWarnings(as.numeric(as.character(temp_col)))
            if(!any(is.na(temp_col_num) & !is.na(temp_col))) { # Check if conversion worked ok
              temp_col <- zoo::na.approx(temp_col_num, na.rm = FALSE)
              # Handle potential NAs at ends left by na.approx
              if(any(is.na(temp_col))) temp_col <- zoo::na.locf(temp_col, na.rm=FALSE)
              if(any(is.na(temp_col))) temp_col <- zoo::na.locf(temp_col, na.rm=FALSE, fromLast=TRUE)
            } else {
              warning(paste("Cannot interpolate non-numeric or problematic column:", col))
              next # Skip this column for interpolation
            }
            
          } else {
            warning(paste("Interpolation skipped for non-numeric column:", col))
            next
          }
        }
        
        # Apply fill value if calculated
        if (!is.null(value_to_fill)) {
          temp_col[is.na(temp_col)] <- value_to_fill
        }
        
        # Update the column in the main data frame for processing
        data_to_process[[col]] <- temp_col
        
        # Mark cells that were originally NA and are now filled in the mask
        current_filled_mask[original_na_mask, col_idx] <- TRUE
        
      } # End loop over strategies
      
      # Apply row drops if any 'drop' strategy was used
      if (any(rows_to_drop_mask)) {
        original_row_count <- nrow(data_to_process)
        data_to_process <- data_to_process[!rows_to_drop_mask, , drop = FALSE]
        # We need to adjust the mask as well
        current_filled_mask <- current_filled_mask[!rows_to_drop_mask, , drop = FALSE]
        dropped_count <- original_row_count - nrow(data_to_process)
        showNotification(paste("Dropped", dropped_count, "rows due to missing values based on selected 'Drop Rows' strategies."), type = "info", duration = 6)
      }
      
      
      # Update the reactive value
      rv$data_processed <- data_to_process
      rv$last_filled_mask <- current_filled_mask # Store the mask for potential undo
      updateActionButton(session, "undo_fill_btn", disabled = FALSE) # Enable undo
      update_select_inputs() # Update UI choices if data changed shape/type
      showNotification("Missing value strategies applied.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error handling missing values:", e$message), type = "error", duration = 10)
      # Revert to previous state on error?
      rv$data_processed <- rv$previous_data_state # Revert on error
      rv$last_filled_mask <- NULL # Clear mask on error
      rv$previous_data_state <- NULL
      updateActionButton(session, "undo_fill_btn", disabled = TRUE)
    })
  })
  
  # Undo Last Fill Button
  observeEvent(input$undo_fill_btn, {
    if (!is.null(rv$previous_data_state)) {
      rv$data_processed <- rv$previous_data_state
      rv$last_filled_mask <- NULL # Clear the mask as the operation is undone
      rv$previous_data_state <- NULL # Clear the stored state
      updateActionButton(session, "undo_fill_btn", disabled = TRUE) # Disable undo again
      update_select_inputs() # Update UI
      showNotification("Last missing value fill operation undone.", type = "message")
    } else {
      showNotification("No previous state to undo to.", type = "warning")
    }
  })
  
  
  # Display summary after missing value handling
  output$missing_summary <- renderPrint({
    req(rv$data_processed)
    df <- rv$data_processed
    cat("Data dimensions after handling missing values:", paste(dim(df), collapse = " x "), "\n\n")
    missing_info <- sapply(df, function(col) sum(is.na(col)))
    cat("Missing values per column:\n")
    print(missing_info[missing_info >= 0]) # Print even if zero
  })
  
  # Display data table after missing value handling
  output$data_table_missing <- renderDT({
    req(rv$data_processed)
    datatable(rv$data_processed, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # --- 2b. Outliers ---
  observeEvent(input$detect_outliers_btn, {
    req(rv$data_processed)
    df <- rv$data_processed
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(num_cols) == 0) {
      showNotification("No numeric columns found to detect outliers.", type = "warning")
      rv$outlier_summary <- NULL
      return()
    }
    
    summary_list <- list()
    all_values_list <- list() # To store raw values for plotting later if needed
    
    withProgress(message = 'Detecting outliers...', value = 0, {
      n_cols <- length(num_cols)
      for (i in seq_along(num_cols)) {
        col_name <- num_cols[i]
        incProgress(1/n_cols, detail = paste("Processing", col_name))
        
        series <- df[[col_name]]
        # Ensure it's truly numeric for calculations
        series_num <- suppressWarnings(as.numeric(as.character(series)))
        series_num <- series_num[!is.na(series_num)] # Drop NA for Z-score calculation
        
        if (length(series_num) < 3) { # Need at least 3 points for sd
          summary_list[[col_name]] <- data.frame(Column=col_name, Count=0, Percent=0, Mean=NA, StdDev=NA, NumPoints=length(series_num), stringsAsFactors = FALSE)
          all_values_list[[col_name]] <- numeric(0) # Store empty numeric vector
          next
        }
        
        mean_val <- mean(series_num, na.rm = TRUE)
        sd_val <- sd(series_num, na.rm = TRUE)
        
        outlier_count <- 0
        outlier_pct <- 0
        outliers_values <- numeric(0) # Store actual outlier values
        
        if (!is.na(sd_val) && sd_val > 0) {
          z_scores <- (series_num - mean_val) / sd_val
          outlier_mask <- abs(z_scores) > 3
          outlier_count <- sum(outlier_mask, na.rm = TRUE)
          outlier_pct <- (outlier_count / length(series_num)) * 100
          outliers_values <- series_num[outlier_mask]
        } else {
          # Handle case with zero standard deviation or insufficient data
          sd_val <- ifelse(is.na(sd_val), NA, sd_val) # Ensure sd_val is NA if calc failed
        }
        
        
        summary_list[[col_name]] <- data.frame(
          Column = col_name,
          Count = outlier_count,
          Percent = round(outlier_pct, 2),
          Mean = round(mean_val, 3),
          StdDev = round(sd_val, 3),
          NumPoints = length(series_num),
          stringsAsFactors = FALSE
        )
        all_values_list[[col_name]] <- series_num # Store all non-NA numeric values
      } # End loop
    }) # End withProgress
    
    rv$outlier_summary <- bind_rows(summary_list)
    # Store all_values_list somewhere if needed for plots, e.g., rv$outlier_plot_data <- all_values_list
    showNotification("Outlier detection complete.", type = "message")
  })
  
  # Display outlier summary table
  output$outlier_summary_table <- renderDT({
    req(rv$outlier_summary)
    datatable(rv$outlier_summary, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # Handle outliers button
  observeEvent(input$handle_outliers_btn, {
    req(rv$data_processed, input$outlier_col_select, input$outlier_method)
    df <- rv$data_processed
    col_to_handle <- input$outlier_col_select
    method <- input$outlier_method
    custom_val <- input$outlier_custom_val
    
    cols_affected <- character(0)
    original_row_count <- nrow(df)
    
    handle_single_col <- function(col_name, dataf) {
      series <- dataf[[col_name]]
      series_num <- suppressWarnings(as.numeric(as.character(series)))
      na_mask_original <- is.na(series_num) # Store original NA positions
      
      series_clean <- series_num[!na_mask_original]
      
      if (length(series_clean) < 3) return(dataf) # Not enough data
      
      mean_val <- mean(series_clean, na.rm = TRUE)
      sd_val <- sd(series_clean, na.rm = TRUE)
      modified_col <- series_num # Work on a copy
      
      if (!is.na(sd_val) && sd_val > 0) {
        z_scores <- (series_clean - mean_val) / sd_val
        outlier_mask_clean <- abs(z_scores) > 3 # Mask for non-NA values
        
        # Map the outlier mask back to the original series length including NAs
        outlier_mask_full <- rep(FALSE, length(series_num))
        outlier_mask_full[!na_mask_original] <- outlier_mask_clean
        
        num_outliers <- sum(outlier_mask_full)
        if (num_outliers == 0) return(dataf) # No outliers found
        
        
        if (method == "remove") {
          # Signal to remove rows - handled outside this function if col_to_handle == "ALL"
          # If handling single column, filter here
          if(col_to_handle != "ALL") {
            return(dataf[!outlier_mask_full, , drop = FALSE])
          } else {
            # Return the mask of rows to KEEP for the "ALL" case
            return(!outlier_mask_full)
          }
        } else if (method == "replace_mean") {
          modified_col[outlier_mask_full] <- mean_val
        } else if (method == "replace_median") {
          median_val <- median(series_clean, na.rm = TRUE)
          modified_col[outlier_mask_full] <- median_val
        } else if (method == "clip") {
          lower_limit <- mean_val - 3 * sd_val
          upper_limit <- mean_val + 3 * sd_val
          modified_col <- pmax(pmin(modified_col, upper_limit, na.rm=TRUE), lower_limit, na.rm=TRUE)
        } else if (method == "replace_custom") {
          if (is.numeric(custom_val)) {
            modified_col[outlier_mask_full] <- custom_val
          } else {
            stop("Custom replacement value must be numeric.")
          }
        }
      }
      
      # If not removing rows, update the column in the dataframe
      if (method != "remove" || col_to_handle == "ALL") { # For ALL+remove, we return mask
        dataf[[col_name]] <- modified_col
        return(dataf)
      }
      return(dataf) # Should have returned filtered df or mask by now
    }
    
    
    tryCatch({
      if (col_to_handle == "ALL") {
        num_cols <- names(df)[sapply(df, is.numeric)]
        if (method == "remove") {
          # Combine masks: keep a row only if it's NOT an outlier in ANY numeric column
          keep_mask <- rep(TRUE, nrow(df))
          for (col in num_cols) {
            col_keep_mask <- handle_single_col(col, df) # This returns a logical vector for keep
            if(is.logical(col_keep_mask) && length(col_keep_mask) == nrow(df)) {
              keep_mask <- keep_mask & col_keep_mask
            } else {
              # Handle case where handle_single_col didn't return a mask (e.g. no outliers)
              # It should still have the same number of rows if no outliers were found
              # This part might need refinement depending on exact return value of handle_single_col
            }
          }
          df <- df[keep_mask, , drop = FALSE]
          cols_affected <- num_cols # Mark all numeric cols as potentially affected
          
        } else {
          # Apply modification to each numeric column
          for (col in num_cols) {
            df <- handle_single_col(col, df)
          }
          cols_affected <- num_cols
        }
      } else {
        # Handle single column
        if (col_to_handle %in% names(df) && is.numeric(df[[col_to_handle]])) {
          df <- handle_single_col(col_to_handle, df)
          cols_affected <- col_to_handle
        } else {
          stop(paste("Selected column '", col_to_handle, "' is not numeric or not found."))
        }
      }
      
      rows_removed <- original_row_count - nrow(df)
      rv$data_processed <- df
      update_select_inputs()
      msg <- paste("Outlier handling method '", method, "' applied")
      if (length(cols_affected) > 0) {
        msg <- paste(msg, "to column(s):", paste(cols_affected, collapse=", "))
      }
      if (rows_removed > 0) {
        msg <- paste(msg, "-", rows_removed, "rows removed.")
      }
      showNotification(msg, type = "message")
      # Re-run outlier detection? Maybe optionally.
      rv$outlier_summary <- NULL # Clear old summary
      
      
    }, error = function(e) {
      showNotification(paste("Error handling outliers:", e$message), type = "error", duration = 10)
    })
  })
  
  # Display data table after outlier handling
  output$data_table_outlier <- renderDT({
    # Show the current state of data_processed, which reflects outlier handling
    req(rv$data_processed)
    datatable(rv$data_processed, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  
  # --- 2c. Normality & Transformation ---
  observeEvent(input$check_normality_btn, {
    req(rv$data_processed)
    df <- rv$data_processed
    preferred_test <- input$normality_test_method
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(num_cols) == 0) {
      showNotification("No numeric columns found to check normality.", type = "warning")
      rv$normality <- NULL
      return()
    }
    
    results_list <- list()
    withProgress(message = 'Checking normality...', value = 0, {
      n_cols <- length(num_cols)
      for (i in seq_along(num_cols)) {
        col_name <- num_cols[i]
        incProgress(1/n_cols, detail = paste("Processing", col_name))
        
        series <- df[[col_name]]
        series_num <- suppressWarnings(as.numeric(as.character(series)))
        original_na_mask <- is.na(series_num)
        series_clean <- series_num[!original_na_mask]
        
        total_count <- length(series_num)
        missing_count <- sum(original_na_mask)
        has_missing <- missing_count > 0
        
        
        test_used <- ""
        p_value <- NA
        is_normal <- NA
        warning_msg <- ""
        stat_value <- NA # Store statistic value
        
        if (length(series_clean) < 3) {
          warning_msg <- "Insufficient data (less than 3 non-NA values)."
        } else {
          # Outlier detection (simple Z-score > 3)
          outlier_ratio <- tryCatch({
            z_scores <- scale(series_clean)
            mean(abs(z_scores) > 3, na.rm = TRUE)
          }, warning = function(w) { NA }, error = function(e) { NA }) # Handle cases like sd=0
          
          has_many_outliers <- !is.na(outlier_ratio) && outlier_ratio > 0.05
          
          test_choice <- preferred_test
          if (preferred_test == "auto") {
            test_choice <- if (length(series_clean) < 50) "shapiro" else "ks"
          }
          
          test_result <- tryCatch({
            if (test_choice == "shapiro") {
              if (length(series_clean) >= 3 && length(series_clean) <= 5000) { # Shapiro limit
                test <- shapiro.test(series_clean)
                test_used <- "Shapiro-Wilk"
                list(statistic = test$statistic, p.value = test$p.value, method = "Shapiro-Wilk")
              } else {
                list(statistic=NA, p.value=NA, method="Shapiro-Wilk (Skipped: N outside [3, 5000])")
              }
            } else if (test_choice == "ks") {
              # KS test vs standard normal requires standardization
              if(sd(series_clean, na.rm=TRUE) > 0) {
                standardized_series <- scale(series_clean)
                test <- ks.test(standardized_series, "pnorm") # Test against standard normal CDF
                test_used <- "Kolmogorov-Smirnov (vs Normal)"
                list(statistic = test$statistic, p.value = test$p.value, method = "Kolmogorov-Smirnov (vs Normal)")
              } else {
                list(statistic=NA, p.value=NA, method="Kolmogorov-Smirnov (Skipped: SD=0)")
              }
              
            } else {
              list(statistic=NA, p.value=NA, method="Unknown Test")
            }
          }, error = function(e) {
            list(statistic=NA, p.value=NA, method=paste("Test Error:", e$message))
          })
          
          stat_value <- test_result$statistic
          p_value <- test_result$p.value
          test_used <- test_result$method
          is_normal <- !is.na(p_value) && p_value > 0.05
          
          
          note_parts <- c()
          if (test_choice == "shapiro" && preferred_test == "auto") note_parts <- c(note_parts, "Auto: Shapiro (N<50)")
          if (test_choice == "ks" && preferred_test == "auto") note_parts <- c(note_parts, "Auto: K-S (N>=50)")
          if (has_many_outliers) note_parts <- c(note_parts, "High outlier ratio (>5%)")
          if (has_missing) note_parts <- c(note_parts, paste0(missing_count, " missing values (", sprintf("%.1f%%", missing_count/total_count*100), ")"))
          
          warning_msg <- paste(note_parts, collapse = "; ")
          if (warning_msg == "") warning_msg <- "-" # Placeholder if no warnings
          
        } # End else (length >= 3)
        
        results_list[[col_name]] <- data.frame(
          Column = col_name,
          Test = test_used,
          Statistic = round(stat_value, 4),
          P_Value = format(p_value, scientific = FALSE, digits = 4), # Format p-value nicely
          Is_Normal = is_normal,
          Warning_Notes = warning_msg,
          stringsAsFactors = FALSE
        )
        
      } # End loop over columns
    }) # End withProgress
    
    rv$normality <- bind_rows(results_list)
    showNotification("Normality checks complete.", type = "message")
  })
  
  # Display normality results table
  output$normality_results_table <- renderDT({
    req(rv$normality)
    datatable(rv$normality,
              options = list(scrollX = TRUE, pageLength = 10,
                             columnDefs = list(
                               list(className = 'dt-center', targets = c(4)) # Center align 'Is_Normal'
                             )),
              rownames = FALSE,
              # Add conditional formatting for Is_Normal column
              callback = JS(
                "table.on('draw.dt', function() {",
                "  $('td:nth-child(5)', table.table().body()).each(function() {", # 5th column (Is_Normal)
                "    if ($(this).text().trim() == 'TRUE') {",
                "      $(this).css('color', 'green').css('font-weight', 'bold');",
                "    } else if ($(this).text().trim() == 'FALSE') {",
                "      $(this).css('color', 'red').css('font-weight', 'bold');",
                "    }",
                "  });",
                "});"
              ))
  })
  
  # Apply transformation button
  observeEvent(input$transform_col_btn, {
    req(rv$data_processed, input$transform_col, input$transform_method)
    df <- rv$data_processed
    col_name <- input$transform_col
    method <- input$transform_method
    
    if (!col_name %in% names(df)) {
      showNotification("Column not found.", type = "error")
      return()
    }
    if (!is.numeric(df[[col_name]])) {
      showNotification("Selected column must be numeric for transformation.", type = "error")
      return()
    }
    
    
    tryCatch({
      original_series <- df[[col_name]]
      transformed_series <- original_series # Initialize with original
      
      if (method == "log") {
        if (any(original_series <= 0, na.rm = TRUE)) {
          stop("Log transformation requires all values to be positive.")
        }
        transformed_series <- log(original_series)
      } else if (method == "sqrt") {
        if (any(original_series < 0, na.rm = TRUE)) {
          stop("Square root transformation requires non-negative values.")
        }
        transformed_series <- sqrt(original_series)
      } else if (method == "boxcox") {
        # Box-Cox in R's MASS package typically finds lambda and can apply it.
        # It requires positive values.
        if (any(original_series <= 0, na.rm = TRUE)) {
          stop("Box-Cox transformation requires all values to be positive.")
        }
        # Fit Box-Cox to find lambda
        non_na_series <- original_series[!is.na(original_series)]
        if(length(non_na_series) < 3) {
          stop("Not enough non-NA data points for Box-Cox.")
        }
        
        # Need a simple formula, like value ~ 1
        # Using MASS::boxcox directly plots, we want the transformation.
        # Let's use forecast::BoxCox which is simpler to apply
        if (!requireNamespace("forecast", quietly = TRUE)) {
          stop("Package 'forecast' needed for easy Box-Cox application. Please install it.")
        }
        
        bc_lambda <- forecast::BoxCox.lambda(non_na_series, method = "guerrero") # Or "loglik"
        if(is.na(bc_lambda)) {
          stop("Could not determine Box-Cox lambda automatically.")
        }
        transformed_series <- forecast::BoxCox(original_series, lambda = bc_lambda)
        showNotification(paste("Applied Box-Cox with automatically determined lambda =", round(bc_lambda, 3)), type = "info")
        
        
      } else {
        stop("Unknown transformation method.")
      }
      
      # Update the dataframe
      df[[col_name]] <- transformed_series
      rv$data_processed <- df
      # Clear normality results as they are now invalid
      rv$normality <- NULL
      update_select_inputs() # In case type changed implicitly (unlikely here)
      showNotification(paste("Column '", col_name, "' transformed using", method, "method."), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error during transformation:", e$message), type = "error", duration = 10)
    })
  })
  
  # Display data table after transformation
  output$data_table_transformed <- renderDT({
    # Show the current state of data_processed, which reflects transformations
    req(rv$data_processed)
    datatable(rv$data_processed, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # --- 3. Relationships ---
  
  # --- 3a. Correlation ---
  correlation_results <- eventReactive(input$run_corr_btn, {
    req(rv$data_processed, input$corr_vars, input$corr_method)
    df <- rv$data_processed
    selected_cols <- input$corr_vars
    method <- input$corr_method
    
    if (length(selected_cols) < 2) {
      showNotification("Please select at least two numeric variables.", type = "warning")
      return(NULL)
    }
    
    # Ensure selected columns are numeric
    sub_df <- df %>% select(all_of(selected_cols))
    is_numeric_check <- sapply(sub_df, is.numeric)
    if (!all(is_numeric_check)) {
      non_numeric_cols <- names(is_numeric_check[!is_numeric_check])
      showNotification(paste("Non-numeric columns selected:", paste(non_numeric_cols, collapse=", "), ". Please select only numeric columns."), type = "error")
      return(NULL)
    }
    
    # Handle missing data - use pairwise complete observations
    sub_df_complete <- sub_df # Will use pairwise complete in cor/cor.test
    
    used_method <- method
    reason <- ""
    normality_data <- rv$normality # Fetch pre-calculated normality if available
    
    if (method == "auto") {
      use_pearson <- TRUE
      reason_detail <- c()
      if (!is.null(normality_data)) {
        all_selected_normal <- TRUE
        for(col in selected_cols) {
          norm_status <- normality_data %>% filter(Column == col) %>% pull(Is_Normal)
          if (length(norm_status) == 0 || !isTRUE(norm_status)) { # If not found or FALSE
            all_selected_normal <- FALSE
            reason_detail <- c(reason_detail, paste0(col, ": ", ifelse(length(norm_status)==0, "Normality unknown", "Not normal")))
          } else {
            reason_detail <- c(reason_detail, paste0(col, ": Normal"))
          }
        }
        if (all_selected_normal) {
          used_method <- "pearson"
          reason <- "Auto method: All selected variables appear normally distributed -> Pearson selected."
        } else {
          used_method <- "spearman"
          reason <- paste("Auto method: Not all variables appear normal -> Spearman selected. Details:", paste(reason_detail, collapse=", "))
        }
      } else {
        # Fallback if normality not checked: use Spearman
        used_method <- "spearman"
        reason <- "Auto method: Normality status not available -> Spearman selected as a robust default."
      }
    } else {
      reason <- paste("Method selected by user:", used_method)
    }
    
    
    if (!used_method %in% c("pearson", "spearman", "kendall")) {
      showNotification("Invalid correlation method selected.", type = "error")
      return(NULL)
    }
    
    
    results_list <- list()
    corr_matrix <- matrix(NA, nrow=length(selected_cols), ncol=length(selected_cols),
                          dimnames=list(selected_cols, selected_cols))
    p_matrix <- matrix(NA, nrow=length(selected_cols), ncol=length(selected_cols),
                       dimnames=list(selected_cols, selected_cols))
    
    tryCatch({
      for (i in 1:length(selected_cols)) {
        for (j in i:length(selected_cols)) {
          col1_name <- selected_cols[i]
          col2_name <- selected_cols[j]
          
          if (i == j) {
            corr_matrix[i, j] <- 1.0
            p_matrix[i, j] <- 0.0
            results_list[[paste(col1_name, col2_name, sep="-")]] <- list(
              Var1 = col1_name, Var2 = col2_name, Correlation = 1.0, P_Value = 0.0, R_Squared = 1.0, CI_Low = NA, CI_High = NA, Strength = "Perfect", Significant = TRUE, N_Pairs = sum(complete.cases(sub_df[, c(col1_name, col2_name)]))
            )
          } else {
            test_res <- cor.test(sub_df[[col1_name]], sub_df[[col2_name]],
                                 method = used_method, exact = FALSE) # exact=FALSE for Kendall/Spearman with ties
            
            corr_val <- test_res$estimate
            p_val <- test_res$p.value
            n_pairs <- sum(complete.cases(sub_df[, c(col1_name, col2_name)])) # N for CI
            
            
            corr_matrix[i, j] <- corr_val
            corr_matrix[j, i] <- corr_val
            p_matrix[i, j] <- p_val
            p_matrix[j, i] <- p_val
            
            # Fisher Z transformation for CI (approximates well for Pearson, less so for others)
            # Only calculate if possible (n > 3 and |r| < 1)
            ci_low <- NA
            ci_high <- NA
            if (used_method == "pearson" && n_pairs > 3 && abs(corr_val) < 1) {
              z <- 0.5 * log((1 + corr_val) / (1 - corr_val))
              se <- 1 / sqrt(n_pairs - 3)
              alpha <- 0.05
              z_crit <- qnorm(1 - alpha / 2)
              z_low <- z - z_crit * se
              z_high <- z + z_crit * se
              ci_low <- tanh(z_low)
              ci_high <- tanh(z_high)
            }
            
            
            # Interpretation helper
            interpret_strength <- function(r) {
              abs_r <- abs(r)
              if (is.na(abs_r)) return("NA")
              if (abs_r < 0.1) return("Trivial")
              else if (abs_r < 0.3) return("Weak")
              else if (abs_r < 0.5) return("Moderate")
              else if (abs_r < 0.7) return("Strong")
              else return("Very Strong")
            }
            
            results_list[[paste(col1_name, col2_name, sep="-")]] <- list(
              Var1 = col1_name,
              Var2 = col2_name,
              Correlation = corr_val,
              P_Value = p_val,
              R_Squared = corr_val^2,
              CI_Low = ci_low,
              CI_High = ci_high,
              Strength = interpret_strength(corr_val),
              Significant = p_val < 0.05,
              N_Pairs = n_pairs
            )
          }
        }
      }
      
      results_df <- bind_rows(lapply(results_list, as.data.frame.list)) %>%
        filter(Var1 != Var2) %>% # Remove self-correlations for the detailed table
        select(Var1, Var2, Correlation, P_Value, R_Squared, CI_Low, CI_High, Strength, Significant, N_Pairs) %>%
        arrange(Var1, Var2)
      
      # Format numbers for display
      results_df <- results_df %>%
        mutate(across(where(is.numeric) & !is.integer & !is.logical, ~round(., 4))) %>%
        mutate(P_Value = format.pval(P_Value, digits = 3, eps = 0.001))
      
      corr_matrix_display <- round(corr_matrix, 3)
      
      
      return(list(
        method = used_method,
        reason = reason,
        matrix = corr_matrix_display,
        p_matrix = p_matrix, # Keep for potential coloring?
        details = results_df,
        scatter_data = if(length(selected_cols) == 2) sub_df else NULL,
        scatter_labels = if(length(selected_cols) == 2) selected_cols else NULL
      ))
      
    }, error = function(e) {
      showNotification(paste("Error calculating correlation:", e$message), type = "error", duration = 10)
      return(NULL)
    })
  })
  
  # Display correlation results
  output$corr_reason_output <- renderText({
    res <- correlation_results()
    req(res)
    paste("Method Used:", res$method, "\nReason:", res$reason)
  })
  
  output$corr_matrix_table <- renderDT({
    res <- correlation_results()
    req(res$matrix)
    datatable(res$matrix, options = list(scrollX = TRUE, pageLength = ncol(res$matrix)), rownames = TRUE)
  })
  
  output$corr_details_table <- renderDT({
    res <- correlation_results()
    req(res$details)
    datatable(res$details,
              options = list(scrollX = TRUE, pageLength = 10,
                             columnDefs = list(
                               list(className = 'dt-center', targets = c(8)) # Center align 'Significant'
                             )),
              rownames = FALSE,
              callback = JS( # Color Significant column
                "table.on('draw.dt', function() {",
                "  $('td:nth-child(9)', table.table().body()).each(function() {", # 9th column (Significant)
                "    if ($(this).text().trim() == 'TRUE') {",
                "      $(this).css('color', 'green').css('font-weight', 'bold');",
                "    } else if ($(this).text().trim() == 'FALSE') {",
                "      $(this).css('color', 'red').css('font-weight', 'bold');",
                "    }",
                "  });",
                "});"
              ))
  })
  
  
  output$corr_scatter_plot <- renderPlotly({
    res <- correlation_results()
    req(res$scatter_data)
    df_scatter <- res$scatter_data
    labels <- res$scatter_labels
    
    p <- ggplot(df_scatter, aes_string(x = safe_name(labels[1]), y = safe_name(labels[2]))) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", linetype="dashed") + # Add linear trend line
      labs(
        title = paste("Scatter Plot:", labels[1], "vs", labels[2]),
        subtitle = paste(res$method, "correlation:", round(res$matrix[1,2], 3)), # Get corr value from matrix
        x = labels[1],
        y = labels[2]
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # --- 3b. Dependency Test ---
  dependency_test_result <- eventReactive(input$run_dep_btn, {
    req(rv$data_processed, input$dep_vars, input$dep_method)
    df <- rv$data_processed
    selected_cols <- input$dep_vars
    method <- input$dep_method
    is_paired <- input$dep_paired # Get paired status
    
    if (length(selected_cols) != 2) {
      showNotification("Please select exactly two variables.", type = "warning")
      return(NULL)
    }
    
    # Get types from rv$col_types if available, otherwise guess
    col_types <- NULL
    if (!is.null(rv$col_types)) {
      col_info <- rv$col_types %>% filter(Column %in% selected_cols)
      if (nrow(col_info) == 2) {
        col_types <- setNames(col_info$Type, col_info$Column)
      }
    }
    if (is.null(col_types)) { # Fallback type detection
      col_types <- setNames(sapply(df[selected_cols], function(c) if(is.numeric(c)) "Numeric" else "Categorical"), selected_cols)
    }
    
    
    col1_name <- selected_cols[1]
    col2_name <- selected_cols[2]
    type1 <- col_types[col1_name]
    type2 <- col_types[col2_name]
    
    # Prepare data subset, coerce numeric, drop NAs
    sub_df <- df %>% select(all_of(selected_cols))
    tryCatch({
      if(type1 == "Numeric") sub_df[[col1_name]] <- as.numeric(as.character(sub_df[[col1_name]]))
      if(type2 == "Numeric") sub_df[[col2_name]] <- as.numeric(as.character(sub_df[[col2_name]]))
      # Drop rows with NA in *either* selected column
      sub_df <- sub_df %>% drop_na(all_of(selected_cols))
    }, error = function(e) {
      showNotification(paste("Error preparing data for test:", e$message), type="error")
      return(NULL)
    })
    
    
    if (nrow(sub_df) < 5) { # Arbitrary small N check
      showNotification("Too few non-missing data points for testing.", type="warning")
      return(NULL)
    }
    
    
    test_name <- method
    reason <- ""
    results <- list(test = "N/A", statistic = NA, p_value = NA, details = "Test not run.", contingency_table = NULL)
    
    # --- Auto Method Logic ---
    if (method == "auto") {
      if (type1 == "Categorical" && type2 == "Categorical") {
        test_name <- "chi2"
        reason <- "Auto: Both variables are categorical -> Chi-squared Test."
      } else if (type1 == "Categorical" && type2 == "Numeric") {
        test_name <- "anova" # Or Kruskal-Wallis depending on normality? Let's default to ANOVA for auto.
        reason <- "Auto: One categorical, one numeric -> ANOVA (or Kruskal-Wallis)."
      } else if (type1 == "Numeric" && type2 == "Categorical") {
        test_name <- "anova"
        reason <- "Auto: One numeric, one categorical -> ANOVA (or Kruskal-Wallis)."
      } else if (type1 == "Numeric" && type2 == "Numeric") {
        # Dependency for numeric vs numeric often means correlation or regression
        reason <- "Auto: Both variables are numeric. Suggest using Correlation or Regression analysis instead of dependency tests like Chi2/ANOVA."
        showNotification(reason, type="info", duration=8)
        return(NULL) # Stop here for Num-Num in auto dependency test
      } else {
        test_name <- "unknown"
        reason <- "Auto: Could not determine appropriate test for the variable types."
      }
    } else {
      reason <- paste("User selected method:", method)
    }
    
    # --- Run Selected Test ---
    tryCatch({
      if (test_name == "chi2") {
        if (type1 != "Categorical" || type2 != "Categorical") stop("Chi-squared requires two categorical variables.")
        cont_table <- table(sub_df[[col1_name]], sub_df[[col2_name]])
        if(any(dim(cont_table) < 2)) stop("Contingency table has dimension < 2.")
        # Check for expected frequencies < 5
        test_res <- chisq.test(cont_table)
        expected_freq <- test_res$expected
        if (any(expected_freq < 5)) {
          reason <- paste(reason, "Warning: Some expected frequencies < 5, Chi-squared may be inaccurate. Consider Fisher's Exact Test if applicable.")
          showNotification("Warning: Low expected frequencies in Chi-squared test.", type="warning")
        }
        
        results <- list(
          test = "Chi-squared Test", statistic = test_res$statistic, p_value = test_res$p.value,
          details = paste("df =", test_res$parameter, reason),
          contingency_table = as.data.frame.matrix(cont_table) # Convert table to df for DT
        )
      } else if (test_name == "fisher") {
        if (type1 != "Categorical" || type2 != "Categorical") stop("Fisher's Exact Test requires two categorical variables.")
        cont_table <- table(sub_df[[col1_name]], sub_df[[col2_name]])
        if (!all(dim(cont_table) == c(2, 2))) {
          stop("Fisher's Exact Test requires a 2x2 contingency table.")
        }
        test_res <- fisher.test(cont_table)
        results <- list(
          test = "Fisher's Exact Test", statistic = test_res$estimate, # Odds Ratio
          p_value = test_res$p.value,
          details = paste("Odds Ratio CI:", round(test_res$conf.int[1],3), "-", round(test_res$conf.int[2],3), reason),
          contingency_table = as.data.frame.matrix(cont_table)
        )
      } else if (test_name %in% c("anova", "kruskal")) {
        # Identify which is numeric and which is categorical
        num_col <- if(type1=="Numeric") col1_name else if(type2=="Numeric") col2_name else NULL
        cat_col <- if(type1=="Categorical") col1_name else if(type2=="Categorical") col2_name else NULL
        if(is.null(num_col) || is.null(cat_col)) stop("ANOVA/Kruskal-Wallis requires one numeric and one categorical variable.")
        
        formula <- as.formula(paste0("`", safe_name(num_col), "` ~ `", safe_name(cat_col), "`")) # Use backticks for safe names
        
        # Check normality of numeric var within groups (optional but good practice)
        # Check variance homogeneity (Levene's test)
        levene_res <- tryCatch(car::leveneTest(formula, data = sub_df), error = function(e) NULL)
        variances_equal <- !is.null(levene_res) && levene_res$`Pr(>F)`[1] > 0.05
        levene_p <- if(!is.null(levene_res)) levene_res$`Pr(>F)`[1] else NA
        
        reason_detail <- paste0("Levene's Test p=", format.pval(levene_p, digits=3), " (Variances ", ifelse(variances_equal, "Equal", "Unequal"), "). ")
        
        
        if (test_name == "anova") {
          if (!variances_equal) {
            reason_detail <- paste0(reason_detail, "Warning: Variances unequal, Welch's ANOVA might be more appropriate (using standard ANOVA).")
            showNotification("Warning: Unequal variances detected. Standard ANOVA results may be less reliable.", type="warning")
          }
          # Standard ANOVA using aov()
          aov_res <- aov(formula, data = sub_df)
          summary_aov <- summary(aov_res)
          f_stat <- summary_aov[[1]]$`F value`[1]
          p_val <- summary_aov[[1]]$`Pr(>F)`[1]
          df_num <- summary_aov[[1]]$Df[1]
          df_den <- summary_aov[[1]]$Df[2]
          results <- list(
            test = "ANOVA", statistic = f_stat, p_value = p_val,
            details = paste("F(", df_num, ",", df_den, ") stat. ", reason_detail, reason)
          )
        } else { # Kruskal-Wallis
          kruskal_res <- kruskal.test(formula, data = sub_df)
          results <- list(
            test = "Kruskal-Wallis Test", statistic = kruskal_res$statistic, p_value = kruskal_res$p.value,
            details = paste("Chi-squared approx. df =", kruskal_res$parameter, reason_detail, reason)
          )
        }
        
      } else if (test_name == "wilcoxon") {
        if (type1 != "Numeric" || type2 != "Numeric") stop("Wilcoxon test requires two numeric variables.")
        if (!is_paired) stop("Wilcoxon test requires 'Data are Paired?' checkbox to be checked.")
        test_res <- wilcox.test(sub_df[[col1_name]], sub_df[[col2_name]], paired = TRUE)
        results <- list(
          test = "Wilcoxon Signed-Rank Test (Paired)", statistic = test_res$statistic, p_value = test_res$p.value,
          details = paste(reason, "(Paired samples assumed)")
        )
        
      } else if (test_name == "mannwhitney") {
        if (type1 != "Numeric" || type2 != "Numeric") stop("Mann-Whitney U test requires two numeric variables.")
        if (is_paired) stop("Mann-Whitney U test requires 'Data are Paired?' checkbox to be unchecked (independent samples).")
        test_res <- wilcox.test(sub_df[[col1_name]], sub_df[[col2_name]], paired = FALSE) # paired=FALSE gives Mann-Whitney
        results <- list(
          test = "Mann-Whitney U Test (Wilcoxon Rank-Sum)", statistic = test_res$statistic, p_value = test_res$p.value,
          details = paste(reason, "(Independent samples assumed)")
        )
      } else {
        stop(paste("Test '", test_name, "' is not implemented or invalid for the data types.", reason))
      }
      
      return(results)
      
    }, error = function(e) {
      showNotification(paste("Error running dependency test:", e$message), type = "error", duration = 10)
      return(list(test = "Error", statistic = NA, p_value = NA, details = e$message, contingency_table = NULL)) # Return error state
    })
  })
  
  # Display dependency test results
  output$dep_test_output <- renderPrint({
    res <- dependency_test_result()
    req(res)
    cat("Test Run:", res$test, "\n")
    cat("Statistic:", round(res$statistic, 4), "\n")
    cat("P-value:", format.pval(res$p_value, digits = 4, eps = 0.001), "\n")
    cat("Details:", res$details, "\n")
    cat("Significant (p < 0.05):", !is.na(res$p_value) && res$p_value < 0.05, "\n")
  })
  
  # Display contingency table if available
  output$dep_contingency_table <- renderDT({
    res <- dependency_test_result()
    req(res$contingency_table)
    # Ensure rownames are displayed as a column
    cont_table_df <- res$contingency_table
    if (!is.null(rownames(cont_table_df))) {
      cont_table_df <- cbind(Category = rownames(cont_table_df), cont_table_df)
      rownames(cont_table_df) <- NULL
    }
    datatable(cont_table_df, options = list(scrollX = TRUE, pageLength = 5), rownames = FALSE)
  })
  
  
  # --- 3c. Regression ---
  regression_results <- eventReactive(input$run_reg_btn, {
    req(rv$data_processed, input$reg_y_var, input$reg_x_vars, input$reg_method)
    df <- rv$data_processed
    y_var <- input$reg_y_var
    x_vars <- input$reg_x_vars
    method <- input$reg_method
    test_size <- input$reg_test_size
    seed <- input$reg_seed
    
    if (y_var %in% x_vars) {
      showNotification("Dependent variable cannot be included in independent variables.", type = "error")
      return(NULL)
    }
    if (length(x_vars) < 1) {
      showNotification("Please select at least one independent variable.", type = "error")
      return(NULL)
    }
    
    # Prepare data: select columns, handle missing (drop rows with any NA in selected vars)
    selected_cols <- c(y_var, x_vars)
    sub_df <- df %>%
      select(all_of(selected_cols)) %>%
      drop_na()
    
    if (nrow(sub_df) < 10) { # Need sufficient data
      showNotification("Too few complete cases for regression analysis.", type = "warning")
      return(NULL)
    }
    
    # Determine Y type
    y_data <- sub_df[[y_var]]
    y_unique_count <- n_distinct(y_data)
    y_is_numeric <- is.numeric(y_data)
    # Simple binary check (assumes 0/1 or two distinct values convertible to factor)
    y_is_binary <- FALSE
    y_is_multiclass <- FALSE
    
    if (!y_is_numeric) { # If not numeric, treat as factor/categorical
      y_data_factor <- as.factor(y_data)
      y_unique_count <- n_distinct(y_data_factor) # Recalc unique count for factor levels
      if (y_unique_count == 2) {
        y_is_binary <- TRUE
        # Ensure Y is factor with appropriate levels for logistic
        sub_df[[y_var]] <- factor(y_data_factor) # Convert in the dataframe
      } else if (y_unique_count > 2 && y_unique_count <= 15) { # Threshold for multinomial
        y_is_multiclass <- TRUE
        sub_df[[y_var]] <- factor(y_data_factor) # Convert in the dataframe
      } else if (y_unique_count > 15) {
        showNotification("Target variable has too many (>15) unique non-numeric values. Cannot perform classification/multinomial regression.", type="error")
        return(NULL)
      } else { # Only 1 unique value
        showNotification("Target variable has only one unique value. Regression not possible.", type="error")
        return(NULL)
      }
    } else {
      # If numeric, check if it looks like binary/multiclass based on unique values
      if (y_unique_count == 2) {
        # Could potentially be treated as binary logistic
        y_is_binary <- TRUE
        # Convert to factor for logistic model
        sub_df[[y_var]] <- factor(y_data)
        warning("Numeric target variable has only 2 unique values. Treating as binary classification. Convert explicitly to factor if intended.")
        showNotification("Numeric Y has 2 unique values; treated as binary.", type="warning")
      } else if (y_unique_count > 2 && y_unique_count <= 15 && all(y_data == floor(y_data), na.rm = TRUE)) {
        # Looks like integer class labels
        y_is_multiclass <- TRUE
        sub_df[[y_var]] <- factor(y_data)
        warning("Numeric integer target variable has few unique values (<=15). Treating as multinomial classification. Convert explicitly to factor if intended.")
        showNotification("Numeric integer Y has few unique values; treated as multinomial.", type="warning")
      }
      # Otherwise, it's treated as continuous numeric (y_is_numeric = TRUE)
    }
    
    selected_method <- method
    reason <- ""
    model_object <- NULL
    
    # --- Auto Method Logic for Regression ---
    if (method == "auto") {
      if (y_is_binary) {
        selected_method <- "logistic"
        reason <- "Auto: Target is binary -> Logistic Regression selected."
      } else if (y_is_multiclass) {
        selected_method <- "multinomial"
        reason <- "Auto: Target is categorical (3-15 levels) -> Multinomial Logistic Regression selected."
      } else if (y_is_numeric) {
        # Check for high correlation among predictors (multicollinearity)
        numeric_x_vars <- x_vars[sapply(sub_df[x_vars], is.numeric)]
        high_corr <- FALSE
        if (length(numeric_x_vars) >= 2) {
          cor_matrix <- cor(sub_df[numeric_x_vars], use = "pairwise.complete.obs")
          diag(cor_matrix) <- 0 # Ignore self-correlation
          if (any(abs(cor_matrix) > 0.8, na.rm = TRUE)) {
            high_corr <- TRUE
          }
        }
        
        # Check N vs P (samples vs predictors)
        n_samples <- nrow(sub_df)
        n_features <- length(x_vars) # Consider effective features after dummification? Simpler for now.
        
        if (n_features >= n_samples * 0.8) { # Heuristic: many predictors relative to samples
          selected_method <- "ridge"
          reason <- "Auto: High number of predictors relative to samples -> Ridge Regression selected."
        } else if (high_corr) {
          selected_method <- "ridge" # Or ElasticNet? Ridge is simpler default.
          reason <- "Auto: High correlation detected among numeric predictors -> Ridge Regression selected."
        } else {
          selected_method <- "ols"
          reason <- "Auto: Target is numeric, no obvious issues -> Linear Regression (OLS) selected."
        }
      } else {
        # Should have been caught earlier (e.g., >15 non-numeric levels)
        showNotification("Auto: Cannot determine suitable regression type for the target variable.", type="error")
        return(NULL)
      }
    } else {
      # Validate user choice against data type
      if (selected_method %in% c("logistic", "multinomial") && !y_is_binary && !y_is_multiclass) {
        stop("Logistic/Multinomial regression requires a categorical target (or numeric with few unique values treated as such).")
      }
      if (selected_method %in% c("ols", "ridge", "lasso", "elasticnet") && !y_is_numeric) {
        stop("Linear regression methods (OLS, Ridge, Lasso, ElasticNet) require a numeric target variable.")
      }
      if (selected_method == "logistic" && y_is_multiclass) {
        stop("Logistic regression selected, but target has >2 levels. Use Multinomial Logistic Regression instead.")
      }
      if (selected_method == "multinomial" && y_is_binary) {
        # This could work, but logistic is more standard. Allow but warn?
        warning("Multinomial regression selected for a binary target. Standard Logistic Regression is usually preferred.")
        showNotification("Warning: Multinomial selected for binary target.", type="warning")
      }
      reason <- paste("User selected method:", selected_method)
    }
    
    
    # --- Split Data ---
    set.seed(seed)
    # Stratify for classification tasks if possible
    stratify_col <- NULL
    if (y_is_binary || y_is_multiclass) {
      stratify_col <- y_var
    }
    # Using base R for splitting
    train_indices <- sample(1:nrow(sub_df), size = floor((1 - test_size) * nrow(sub_df)))
    # caret::createDataPartition is better for stratification
    # train_indices <- createDataPartition(sub_df[[y_var]], p = 1 - test_size, list = FALSE, times = 1)
    
    train_data <- sub_df[train_indices, ]
    test_data <- sub_df[-train_indices, ]
    
    if(nrow(train_data) == 0 || nrow(test_data) == 0) {
      stop("Data splitting resulted in empty train or test set. Adjust test size or check data.")
    }
    
    # --- Prepare Formula ---
    # Use backticks for safe column names
    formula <- as.formula(paste0("`", safe_name(y_var), "` ~ .")) # '.' means all other columns (x_vars)
    
    results_bundle <- list() # To store outputs
    
    # --- Train Model ---
    tryCatch({
      model_fit <- NULL
      model_summary <- NULL
      predict_args <- list() # Arguments for predict() can vary
      
      if (selected_method == "ols") {
        model_fit <- lm(formula, data = train_data)
        model_summary <- summary(model_fit)
        predict_args <- list(type = "response") # Default for lm
      } else if (selected_method == "logistic") {
        model_fit <- glm(formula, data = train_data, family = binomial(link = "logit"))
        model_summary <- summary(model_fit)
        predict_args <- list(type = "response") # Gives probabilities
      } else if (selected_method == "multinomial") {
        # nnet::multinom is common
        model_fit <- nnet::multinom(formula, data = train_data, trace = FALSE) # trace=FALSE supresses optimization output
        model_summary <- summary(model_fit) # Summary is different for multinom
        predict_args <- list(type = "class") # Directly predicts class label
      } else if (selected_method %in% c("ridge", "lasso", "elasticnet")) {
        # Use glmnet - requires matrix input for X, vector for Y
        x_train_matrix <- model.matrix(formula, data = train_data)[, -1] # [-1] removes intercept
        y_train_vector <- train_data[[y_var]]
        x_test_matrix <- model.matrix(formula, data = test_data)[, -1]
        
        alpha_val <- switch(selected_method, "ridge" = 0, "lasso" = 1, "elasticnet" = 0.5) # Alpha for elastic net (0=ridge, 1=lasso)
        
        # Need to find optimal lambda (regularization strength) using cross-validation
        cv_fit <- cv.glmnet(x_train_matrix, y_train_vector, alpha = alpha_val, family = ifelse(y_is_numeric, "gaussian", "binomial")) # Add family for logistic ridge/lasso if needed
        best_lambda <- cv_fit$lambda.min
        
        model_fit <- glmnet(x_train_matrix, y_train_vector, alpha = alpha_val, lambda = best_lambda, family = ifelse(y_is_numeric, "gaussian", "binomial"))
        model_summary <- list(lambda = best_lambda, alpha = alpha_val) # Store params
        predict_args <- list(newx = x_test_matrix, s = best_lambda, type = ifelse(y_is_numeric, "response", "class")) # 's' specifies lambda
        
      } else {
        stop(paste("Model training not implemented for:", selected_method))
      }
      
      # --- Predict on Test Set ---
      y_test_actual <- test_data[[y_var]]
      if (selected_method %in% c("ridge", "lasso", "elasticnet")) {
        y_pred <- predict(model_fit, newx = x_test_matrix, s = best_lambda, type = predict_args$type)
        if (predict_args$type == "response" && !y_is_numeric) { # glmnet response for binomial gives probs
          y_pred_class <- ifelse(y_pred > 0.5, levels(y_test_actual)[2], levels(y_test_actual)[1])
          y_pred_class <- factor(y_pred_class, levels = levels(y_test_actual)) # Ensure factor levels match
        } else if (predict_args$type == "class"){
          # For multinomial or binomial with type='class'
          y_pred_class <- factor(y_pred[,1], levels=levels(y_test_actual)) # glmnet predict class returns matrix
        } else {
          # Numeric prediction
          y_pred_numeric <- y_pred[,1] # glmnet predict response returns matrix
        }
        
        
      } else {
        # Standard predict for lm, glm, multinom
        y_pred <- predict(model_fit, newdata = test_data, type = predict_args$type)
        # Convert predictions if necessary (e.g., probabilities to classes)
        if (selected_method == "logistic") {
          predicted_probs <- y_pred
          y_pred_class <- ifelse(predicted_probs > 0.5, levels(y_test_actual)[2], levels(y_test_actual)[1])
          y_pred_class <- factor(y_pred_class, levels = levels(y_test_actual))
        } else if (selected_method == "multinomial") {
          y_pred_class <- y_pred # Already predicted class by multinom with type='class'
        } else {
          # Numeric prediction (OLS)
          y_pred_numeric <- y_pred
        }
      }
      
      
      # --- Evaluate ---
      metrics <- list()
      coefficients_df <- NULL
      residuals_data <- NULL
      actual_pred_data <- NULL
      
      # Coefficients extraction
      if (!is.null(model_fit)) {
        if (selected_method %in% c("ridge", "lasso", "elasticnet")) {
          coeffs <- coef(model_fit, s = best_lambda)
          coefficients_df <- data.frame(
            Feature = rownames(coeffs),
            Coefficient = as.numeric(coeffs[,1]),
            stringsAsFactors = FALSE
          )
          # CI calculation is complex for regularized regression, skip for now.
        } else if (selected_method == "multinomial") {
          # Coefficients are per class (relative to baseline)
          coeffs <- coef(model_fit)
          if (is.vector(coeffs)) { # Binary case handled by multinom?
            coefficients_df <- data.frame(Feature=names(coeffs), Coefficient=coeffs)
          } else { # Matrix for >2 classes
            # Present coefficients for each class vs baseline
            coeffs_df_list <- lapply(rownames(coeffs), function(class_level) {
              data.frame(
                Feature = colnames(coeffs),
                Coefficient = coeffs[class_level, ],
                Class = class_level,
                stringsAsFactors = FALSE
              )
            })
            coefficients_df <- bind_rows(coeffs_df_list)
          }
        } else { # OLS, Logistic
          coeffs <- coef(model_summary) # Summary includes Std. Error, t/z value, p-value
          coefficients_df <- data.frame(
            Feature = rownames(coeffs),
            Coefficient = coeffs[, "Estimate"],
            StdError = coeffs[, "Std. Error"],
            T_or_Z_value = coeffs[, ifelse("t value" %in% colnames(coeffs), "t value", "z value")],
            P_value = coeffs[, "Pr(>|t|)", drop=FALSE] %||% coeffs[, "Pr(>|z|)", drop=FALSE], # Handle both t and z test outputs
            stringsAsFactors = FALSE
          )
          # Calculate CIs for OLS/Logistic
          conf_ints <- tryCatch(confint(model_fit), error = function(e) NULL)
          if (!is.null(conf_ints)) {
            colnames(conf_ints) <- c("CI_Low", "CI_High")
            coefficients_df <- cbind(coefficients_df, conf_ints[match(coefficients_df$Feature, rownames(conf_ints)), ])
          }
        }
        rownames(coefficients_df) <- NULL # Clean up rownames
        coefficients_df <- coefficients_df %>% mutate(across(where(is.numeric), ~round(., 5))) # Round numeric cols
      }
      
      
      if (y_is_numeric && !y_is_binary && !y_is_multiclass) { # Numeric Regression Evaluation
        mse <- mean((y_test_actual - y_pred_numeric)^2)
        rmse <- sqrt(mse)
        mae <- mean(abs(y_test_actual - y_pred_numeric))
        r2 <- ifelse(var(y_test_actual) == 0, NA, 1 - sum((y_test_actual - y_pred_numeric)^2) / sum((y_test_actual - mean(y_test_actual))^2)) # Manual R^2 calculation robust to var=0
        n_test <- length(y_test_actual)
        p_features <- length(x_vars) # Number of predictors
        if(n_test > p_features + 1 && !is.na(r2)) {
          adj_r2 <- 1 - ((1 - r2) * (n_test - 1) / (n_test - p_features - 1))
        } else {
          adj_r2 <- NA
        }
        
        
        metrics <- data.frame(
          Metric = c("Mean Squared Error (MSE)", "Root Mean Squared Error (RMSE)", "Mean Absolute Error (MAE)", "R-squared", "Adjusted R-squared"),
          Value = c(mse, rmse, mae, r2, adj_r2)
        ) %>% mutate(Value = round(Value, 4))
        
        # Residuals
        residuals_data <- data.frame(Predicted = y_pred_numeric, Residuals = y_test_actual - y_pred_numeric)
        actual_pred_data <- data.frame(Actual = y_test_actual, Predicted = y_pred_numeric)
        
        
      } else { # Classification Evaluation
        # Ensure y_pred_class is defined and is a factor with same levels as y_test_actual
        if (!exists("y_pred_class") || is.null(y_pred_class)) {
          stop("Prediction variable 'y_pred_class' not generated correctly.")
        }
        if (!is.factor(y_test_actual)) y_test_actual <- factor(y_test_actual)
        if (!is.factor(y_pred_class)) y_pred_class <- factor(y_pred_class, levels = levels(y_test_actual)) # Ensure levels match
        
        
        # Handle cases where prediction might not have all levels
        common_levels <- intersect(levels(y_test_actual), levels(y_pred_class))
        if(length(common_levels) < length(levels(y_test_actual))) {
          warning("Predicted classes do not cover all actual classes. Metrics might be affected.")
          # Ensure levels match for confusionMatrix
          levels(y_pred_class) <- levels(y_test_actual)
        }
        
        
        # Use caret::confusionMatrix for comprehensive metrics
        cm <- tryCatch(confusionMatrix(data = y_pred_class, reference = y_test_actual),
                       error = function(e) { print(e); NULL } ) # Handle errors gracefully
        
        
        if (!is.null(cm)) {
          accuracy <- cm$overall["Accuracy"]
          metrics_by_class <- as.data.frame(cm$byClass)
          # Add overall metrics
          metrics_overall <- data.frame(
            Metric = names(cm$overall),
            Value = cm$overall,
            row.names = NULL
          )
          # Combine and format
          metrics <- bind_rows(
            metrics_overall %>% mutate(Class = "Overall"),
            metrics_by_class %>% tibble::rownames_to_column("Metric") %>% pivot_longer(cols=-Metric, names_to="Class", values_to="Value")
          ) %>% select(Class, Metric, Value) %>% mutate(Value = round(Value, 4))
          
          conf_matrix_table <- as.data.frame.matrix(cm$table)
          
          
        } else {
          # Basic metrics if confusionMatrix fails
          accuracy <- mean(y_pred_class == y_test_actual, na.rm=TRUE)
          metrics <- data.frame(Metric = "Accuracy", Value = round(accuracy, 4), Class="Overall")
          conf_matrix_table <- table(Actual = y_test_actual, Predicted = y_pred_class)
          showNotification("Could not generate detailed classification metrics using caret.", type="warning")
        }
        
        
        results_bundle$confusion_matrix <- conf_matrix_table
        results_bundle$confusion_matrix_object <- cm # Store caret object for plotting
      }
      
      
      results_bundle$model_type = selected_method
      results_bundle$reason = reason
      results_bundle$coefficients = coefficients_df
      results_bundle$metrics = metrics
      results_bundle$residuals_data = residuals_data
      results_bundle$actual_pred_data = actual_pred_data
      results_bundle$model_summary_text <- capture.output(print(model_summary)) # Capture summary text
      
      
      return(results_bundle)
      
    }, error = function(e) {
      showNotification(paste("Error during regression:", e$message), type = "error", duration = 10)
      return(list(error = e$message)) # Return error state
    })
    
  })
  
  # Display Regression Results
  output$reg_summary_output <- renderPrint({
    res <- regression_results()
    req(res)
    if(!is.null(res$error)) {
      cat("Error:", res$error)
    } else {
      cat("--- Regression Summary ---\n")
      cat("Method Used:", res$model_type, "\n")
      cat("Reason:", res$reason, "\n\n")
      # Print captured summary output if available
      if (!is.null(res$model_summary_text)) {
        cat("Model Summary Details:\n")
        cat(res$model_summary_text, sep = "\n")
        cat("\n")
      }
      # Print metrics summary
      if (!is.null(res$metrics)) {
        cat("--- Performance Metrics (Test Set) ---\n")
        # Format metrics table nicely
        metrics_df <- res$metrics
        if ("Class" %in% names(metrics_df)) { # Classification metrics
          print(metrics_df %>% pivot_wider(names_from=Class, values_from=Value), row.names = FALSE)
        } else { # Regression metrics
          print(metrics_df, row.names = FALSE)
        }
        
        cat("\n")
      }
      # Print Confusion Matrix if it exists
      if(!is.null(res$confusion_matrix)) {
        cat("--- Confusion Matrix (Test Set) ---\n")
        print(res$confusion_matrix)
        cat("\n")
      }
      
    }
  })
  
  output$reg_coeffs_table <- renderDT({
    res <- regression_results()
    req(res$coefficients)
    datatable(res$coefficients, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$reg_metrics_table <- renderDT({
    res <- regression_results()
    req(res$metrics)
    metrics_df <- res$metrics
    # If classification, might need pivoting for better DT display
    if("Class" %in% names(metrics_df)) {
      metrics_display <- metrics_df %>%
        pivot_wider(names_from = Class, values_from = Value)
    } else {
      metrics_display <- metrics_df
    }
    datatable(metrics_display, options = list(scrollX = TRUE, searching = FALSE, paging = FALSE, info=FALSE), rownames = FALSE)
  })
  
  output$reg_residual_plot <- renderPlotly({
    res <- regression_results()
    req(res$residuals_data)
    df_resid <- res$residuals_data
    
    p <- ggplot(df_resid, aes(x = Predicted, y = Residuals)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(se = FALSE, method = "loess", color = "blue") + # Loess smooth line
      labs(title = "Residuals vs Predicted Values", x = "Predicted Values", y = "Residuals") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$reg_actual_pred_plot <- renderPlotly({
    res <- regression_results()
    req(res$actual_pred_data)
    df_ap <- res$actual_pred_data
    
    p <- ggplot(df_ap, aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") + # Line y=x
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "Actual vs Predicted Values (Test Set)", x = "Actual Values", y = "Predicted Values") +
      coord_fixed() + # Ensure 1:1 aspect ratio if possible
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$classif_conf_matrix_output <- renderPrint({
    res <- regression_results() # Use regression results for classification too
    req(res$confusion_matrix)
    cat("Confusion Matrix (Test Set):\n")
    print(res$confusion_matrix)
  })
  
  output$classif_conf_matrix_plot <- renderPlot({
    res <- regression_results() # Reuse
    req(res$confusion_matrix_object) # Need the caret object
    cm_obj <- res$confusion_matrix_object
    # Use base R plotting for the table
    fourfoldplot(cm_obj$table, color = c("#CC6666", "#99CC99"),
                 conf.level = 0, margin = 1, main = "Confusion Matrix")
    # Or use ggplot if preferred:
    # plot_data <- as.data.frame(cm_obj$table)
    # ggplot(plot_data, aes(x=Reference, y=Prediction, fill=Freq)) +
    # geom_tile() + geom_text(aes(label=Freq)) + scale_fill_gradient(low="white", high="steelblue")
  })
  
  
  # --- 4. Group Comparison ---
  group_comparison_results <- eventReactive(input$run_group_comp_btn, {
    req(rv$data_processed, input$group_num_vars, input$group_cat_var)
    df <- rv$data_processed
    num_vars <- input$group_num_vars
    cat_var <- input$group_cat_var
    is_paired <- input$group_paired
    normality_info <- rv$normality # Use pre-calculated normality
    
    results_list <- list()
    
    if (is_paired) {
      # --- Paired Analysis ---
      if (length(num_vars) != 2) {
        return(list(error = "Paired analysis requires exactly two numeric variables."))
      }
      col1 <- num_vars[1]
      col2 <- num_vars[2]
      
      # Prepare data: select the 3 columns, coerce numeric, drop NAs across the pair + group
      paired_df <- df %>%
        select(all_of(c(col1, col2, cat_var))) %>%
        mutate(across(all_of(c(col1, col2)), ~as.numeric(as.character(.)))) %>%
        drop_na()
      
      if (nrow(paired_df) < 3) {
        return(list(error = "Insufficient complete paired data for analysis."))
      }
      
      # Calculate differences
      paired_df <- paired_df %>% mutate(Difference = .[[col1]] - .[[col2]])
      
      # Check normality of differences (using Shapiro-Wilk as it's common for paired diffs)
      diff_norm_test <- tryCatch(shapiro.test(paired_df$Difference), error=function(e) NULL)
      diff_is_normal <- !is.null(diff_norm_test) && diff_norm_test$p.value > 0.05
      diff_norm_p <- if(!is.null(diff_norm_test)) diff_norm_test$p.value else NA
      
      
      test_name <- ""
      test_stat <- NA
      p_value <- NA
      note <- paste0("Normality of Differences (Shapiro p=", format.pval(diff_norm_p, digits=3), "): ", ifelse(diff_is_normal, "Normal", "Not Normal"), ".")
      
      
      tryCatch({
        if (diff_is_normal) {
          test_res <- t.test(paired_df[[col1]], paired_df[[col2]], paired = TRUE)
          test_name <- "Paired t-test"
          test_stat <- test_res$statistic
          p_value <- test_res$p.value
          note <- paste(note, "Used Paired t-test.")
        } else {
          test_res <- wilcox.test(paired_df[[col1]], paired_df[[col2]], paired = TRUE)
          test_name <- "Wilcoxon Signed-Rank Test (Paired)"
          test_stat <- test_res$statistic
          p_value <- test_res$p.value
          note <- paste(note, "Used Wilcoxon Signed-Rank test.")
        }
        
        results_list[[paste(col1, "vs", col2)]] <- list(
          Comparison = paste(col1, "vs", col2),
          Grouping = cat_var, # Might analyze overall paired difference or within groups? Let's do overall diff first.
          Test = test_name,
          Statistic = test_stat,
          P_Value = p_value,
          Note = note,
          Significant = p_value < 0.05
        )
      }, error = function(e) {
        results_list[[paste(col1, "vs", col2)]] <- list(Comparison = paste(col1, "vs", col2), Test = "Error", Note = e$message)
      })
      
      
    } else {
      # --- Independent Groups Analysis ---
      for (num_var in num_vars) {
        # Prepare data for this specific comparison
        comp_df <- df %>%
          select(all_of(c(num_var, cat_var))) %>%
          mutate(across(all_of(num_var), ~as.numeric(as.character(.)))) %>%
          drop_na()
        
        if (nrow(comp_df) < 3) {
          results_list[[num_var]] <- list(Comparison = num_var, Grouping=cat_var, Test = "Skipped", Note = "Insufficient complete data.")
          next
        }
        
        # Ensure categorical variable is factor
        comp_df[[cat_var]] <- as.factor(comp_df[[cat_var]])
        num_groups <- nlevels(comp_df[[cat_var]])
        
        if (num_groups < 2) {
          results_list[[num_var]] <- list(Comparison = num_var, Grouping=cat_var, Test = "Skipped", Note = "Grouping variable has less than 2 levels.")
          next
        }
        
        # Check Normality (use stored result if available)
        is_normal <- FALSE
        if (!is.null(normality_info)) {
          norm_status <- normality_info %>% filter(Column == num_var) %>% pull(Is_Normal)
          if (length(norm_status) > 0) is_normal <- isTRUE(norm_status[1])
        }
        # Can also check normality within groups, more rigorous but complex UI
        
        # Check Variance Homogeneity using Levene's Test
        formula <- as.formula(paste0("`", safe_name(num_var), "` ~ `", safe_name(cat_var), "`"))
        levene_res <- tryCatch(car::leveneTest(formula, data = comp_df), error = function(e) NULL)
        variances_equal <- !is.null(levene_res) && levene_res$`Pr(>F)`[1] > 0.05
        levene_p <- if(!is.null(levene_res)) levene_res$`Pr(>F)`[1] else NA
        var_note <- paste0("Levene Test p=", format.pval(levene_p, digits=3), "(Vars ", ifelse(variances_equal, "Equal", "Unequal"), "). ")
        
        
        test_name <- ""
        test_stat <- NA
        p_value <- NA
        reason <- ""
        
        tryCatch({
          if (num_groups == 2) {
            # --- Two Group Comparison ---
            if (is_normal && variances_equal) {
              test_res <- t.test(formula, data = comp_df, var.equal = TRUE)
              test_name <- "Independent Samples t-test"
              reason <- "Normal data, Equal variances"
            } else if (is_normal && !variances_equal) {
              test_res <- t.test(formula, data = comp_df, var.equal = FALSE) # Welch's t-test
              test_name <- "Welch's t-test"
              reason <- "Normal data, Unequal variances"
            } else { # Not normal
              test_res <- wilcox.test(formula, data = comp_df, paired = FALSE) # Mann-Whitney U
              test_name <- "Mann-Whitney U test"
              reason <- "Non-normal data"
            }
            test_stat <- test_res$statistic
            p_value <- test_res$p.value
            
          } else {
            # --- More Than Two Groups Comparison ---
            if (is_normal && variances_equal) {
              aov_res <- aov(formula, data = comp_df)
              summary_aov <- summary(aov_res)
              test_name <- "ANOVA"
              test_stat <- summary_aov[[1]]$`F value`[1]
              p_value <- summary_aov[[1]]$`Pr(>F)`[1]
              reason <- "Normal data, Equal variances"
            } else { # Not normal or unequal variances
              test_res <- kruskal.test(formula, data = comp_df)
              test_name <- "Kruskal-Wallis Test"
              test_stat <- test_res$statistic
              p_value <- test_res$p.value
              reason <- "Non-normal data or Unequal variances"
            }
          }
          
          results_list[[num_var]] <- list(
            Comparison = num_var,
            Grouping = cat_var,
            Test = test_name,
            Statistic = test_stat,
            P_Value = p_value,
            Note = paste0(var_note, reason),
            Significant = p_value < 0.05
          )
          
        }, error = function(e) {
          results_list[[num_var]] <- list(Comparison = num_var, Grouping=cat_var, Test = "Error", Note = e$message)
        })
        
      } # End loop over numeric variables
    } # End independent groups analysis
    
    
    return(bind_rows(lapply(results_list, as.data.frame.list)))
    
  })
  
  # Display Group Comparison Results
  output$group_comp_output <- renderPrint({
    res <- group_comparison_results()
    req(res)
    if("error" %in% names(res)) {
      cat("Error:", res$error)
    } else {
      print(res %>% mutate(across(where(is.numeric), ~round(., 4))), row.names=FALSE)
    }
  })
  
  
  # --- 5. Unsupervised: Clustering ---
  cluster_results <- eventReactive(input$run_cluster_btn, {
    req(rv$data_processed, input$cluster_vars, input$cluster_algo)
    df <- rv$data_processed
    selected_cols <- input$cluster_vars
    algo <- input$cluster_algo
    num_k <- input$num_clusters
    dist_metric <- input$cluster_dist
    standardize <- input$cluster_standardize
    
    if (length(selected_cols) < 2) {
      showNotification("Please select at least two numeric variables for clustering.", type = "warning")
      return(NULL)
    }
    
    # Prepare data: select numeric, handle NA (drop rows), standardize
    data_for_clustering <- df %>%
      select(all_of(selected_cols)) %>%
      mutate(across(everything(), ~as.numeric(as.character(.)))) %>%
      drop_na()
    
    if (nrow(data_for_clustering) < 3) {
      showNotification("Insufficient complete data for clustering.", type = "warning")
      return(NULL)
    }
    
    data_scaled <- data_for_clustering
    if (standardize) {
      data_scaled <- scale(data_for_clustering)
      if (any(is.na(data_scaled))) { # Check if scaling produced NAs (e.g., sd=0)
        showNotification("Warning: Standardization produced NAs (possibly due to zero variance columns). Clustering might fail.", type="warning")
        # Optionally remove columns with zero variance before scaling
        col_vars <- apply(data_for_clustering, 2, var, na.rm = TRUE)
        cols_to_keep <- names(col_vars[col_vars > 1e-9]) # Keep cols with variance > near zero
        if(length(cols_to_keep) < length(selected_cols)) {
          showNotification(paste("Removed columns with zero variance:", paste(setdiff(selected_cols, cols_to_keep), collapse=", ")), type="info")
          selected_cols <- cols_to_keep
          if (length(selected_cols) < 2) stop("Not enough columns left after removing zero variance ones.")
          data_for_clustering <- data_for_clustering[, selected_cols, drop=FALSE]
          data_scaled <- scale(data_for_clustering)
        }
        if (any(is.na(data_scaled))) stop("Scaling still produced NAs. Cannot proceed.") # Final check
      }
    }
    
    
    used_algorithm <- algo
    reason <- ""
    n_samples <- nrow(data_scaled)
    n_features <- ncol(data_scaled)
    cluster_model <- NULL
    labels <- NULL
    silhouette_score_val <- NA
    optimal_k <- NULL
    
    
    # --- Auto Algorithm Selection ---
    if (algo == "auto") {
      if (n_samples < 500 && n_samples > 2) { # Hierarchical good for small N
        used_algorithm <- "hierarchical"
        reason <- "Auto: Small dataset (N < 500) -> Hierarchical clustering suggested."
      } else { # Default to KMeans for larger datasets
        used_algorithm <- "kmeans"
        reason <- "Auto: Larger dataset (N >= 500) -> K-Means suggested."
      }
      # DBSCAN is density-based, harder to auto-select well without parameter knowledge.
    }
    
    
    # --- Determine Number of Clusters (k) if needed ---
    k_to_use <- num_k
    if (used_algorithm == "kmeans" || used_algorithm == "hierarchical") {
      if (is.null(num_k) || is.na(num_k) || num_k < 2) {
        # Auto-detect k using silhouette score (can be slow!)
        reason <- paste(reason, "Attempting to find optimal k...")
        showNotification("Finding optimal k (may take time)...", type="message")
        
        max_k <- min(10, floor(n_samples / 2)) # Limit max k
        if (max_k < 2) stop("Cannot automatically determine k with less than 4 data points.")
        
        # Use pam (k-medoids) for silhouette as it's often more robust than kmeans result itself
        # Or use fviz_nbclust
        nbclust_res <- tryCatch(
          factoextra::fviz_nbclust(data_scaled, FUNcluster = kmeans, method = "silhouette", k.max = max_k) +
            labs(subtitle = "Silhouette method") ,
          error = function(e) { print(e); NULL }
        )
        # Extract optimal k from nbclust plot data usually
        optimal_k <- tryCatch({
          sil_data <- nbclust_res$data
          sil_data$clusters[which.max(sil_data$y)]
        }, error = function(e) { NULL })
        
        
        if (!is.null(optimal_k) && is.numeric(optimal_k) && optimal_k >= 2) {
          k_to_use <- as.integer(optimal_k)
          reason <- paste(reason, paste0("Optimal k found: ", k_to_use, "."))
          showNotification(paste("Optimal k found:", k_to_use), type="info")
        } else {
          k_to_use <- 3 # Fallback k
          reason <- paste(reason, "Could not determine optimal k automatically, using default k=3.")
          showNotification("Could not determine optimal k, using k=3.", type="warning")
        }
      }
    }
    
    
    # --- Run Clustering Algorithm ---
    cluster_results_list <- list()
    tryCatch({
      if (used_algorithm == "kmeans") {
        if (k_to_use >= n_samples) stop("k cannot be greater than or equal to the number of samples for K-Means.")
        set.seed(123) # for reproducibility
        kmeans_res <- kmeans(data_scaled, centers = k_to_use, nstart = 25)
        labels <- kmeans_res$cluster
        cluster_model <- kmeans_res # Store the model object
      } else if (used_algorithm == "hierarchical") {
        # Need to compute distance matrix first
        dist_matrix <- dist(data_scaled, method = dist_metric)
        # Linkage method: 'ward.D2' is common for Euclidean, 'average'/'complete' for others
        linkage_method <- if (dist_metric == "euclidean") "ward.D2" else "average"
        hclust_res <- hclust(dist_matrix, method = linkage_method)
        labels <- cutree(hclust_res, k = k_to_use)
        cluster_model <- hclust_res # Store hclust object
      } else if (used_algorithm == "dbscan") {
        # DBSCAN requires parameters eps and MinPts - hard to auto-tune
        # Using default parameters from dbscan package, often needs tuning!
        if (!requireNamespace("dbscan", quietly = TRUE)) stop("Package 'dbscan' is required.")
        dbscan_res <- dbscan::dbscan(data_scaled, eps = 0.5, minPts = 5) # Example parameters
        labels <- dbscan_res$cluster # 0 represents noise points
        cluster_model <- dbscan_res
        reason <- paste(reason, "Used default DBSCAN params (eps=0.5, minPts=5) - may need tuning.")
        showNotification("Using default DBSCAN parameters, results may vary.", type="warning")
      } else {
        stop(paste("Unknown clustering algorithm:", used_algorithm))
      }
      
      
      # Calculate Silhouette Score (if labels exist and > 1 cluster)
      unique_labels <- unique(labels)
      num_unique_clusters <- length(unique_labels)
      # Exclude noise label (0) from cluster count for silhouette
      if (any(labels == 0)) num_unique_clusters <- num_unique_clusters -1
      
      if (num_unique_clusters > 1 && num_unique_clusters < n_samples) {
        # Exclude noise points (label 0) from silhouette calculation if present
        non_noise_indices <- which(labels != 0)
        if (length(non_noise_indices) > 1 && length(unique(labels[non_noise_indices])) > 1) {
          sil <- silhouette(labels[non_noise_indices], dist(data_scaled[non_noise_indices, ], method = dist_metric))
          silhouette_score_val <- mean(sil[, "sil_width"])
        } else {
          # Handle case where only noise or one cluster remains after removing noise
          silhouette_score_val <- NA
        }
      } else {
        silhouette_score_val <- NA # Not applicable for 1 cluster or all noise
      }
      
      
      # --- PCA for Visualization ---
      pca_res <- NULL
      pca_data <- NULL
      pca_biplot_obj <- NULL
      if (ncol(data_scaled) >= 2) {
        pca_res <- prcomp(data_scaled, center = FALSE, scale. = FALSE) # Already scaled
        pca_data <- data.frame(pca_res$x[, 1:2]) # Get first 2 PCs
        names(pca_data) <- c("PC1", "PC2")
        pca_data$Cluster <- as.factor(labels) # Add cluster labels
        pca_data$Label <- rownames(data_scaled) # Add labels if needed
        
        
        # Create biplot object using factoextra
        pca_biplot_obj <- tryCatch(
          fviz_pca_biplot(pca_res, repel = TRUE,
                          geom.ind = "point", # show points only (or "text")
                          col.ind = pca_data$Cluster, # color by cluster
                          palette = "jco",
                          addEllipses = TRUE, ellipse.level=0.95,
                          legend.title = "Clusters") + theme_minimal(),
          error = function(e) { print(e); NULL }
        )
      }
      
      
      # --- Cluster Summary Stats ---
      cluster_summary <- data_for_clustering %>%
        mutate(Cluster = labels) %>%
        group_by(Cluster) %>%
        summarise(across(everything(), list(mean = mean, sd = sd, min = min, max = max, n = ~n())), .groups = 'drop') %>%
        mutate(across(where(is.numeric), ~round(., 3)))
      
      # Prepare output bundle
      cluster_results_list <- list(
        algorithm = used_algorithm,
        reason = reason,
        k_used = if(exists("k_to_use")) k_to_use else NA,
        num_clusters_found = length(unique(labels[labels!=0])), # Exclude noise
        num_noise_points = sum(labels == 0),
        standardized = standardize,
        distance_metric = dist_metric,
        silhouette_score = silhouette_score_val,
        labels = labels,
        cluster_summary = cluster_summary,
        pca_data = pca_data,
        pca_biplot = pca_biplot_obj # Store the plot object
      )
      
    }, error = function(e) {
      showNotification(paste("Error during clustering:", e$message), type = "error", duration = 10)
      return(list(error = e$message))
    })
    
    return(cluster_results_list)
  })
  
  # Display Clustering Results
  output$cluster_summary_output <- renderPrint({
    res <- cluster_results()
    req(res)
    if (!is.null(res$error)) {
      cat("Error:", res$error)
    } else {
      cat("--- Clustering Summary ---\n")
      cat("Algorithm Used:", res$algorithm, "\n")
      cat("Reason/Notes:", res$reason, "\n")
      if (!is.na(res$k_used)) cat("K value used:", res$k_used, "\n")
      cat("Number of Clusters Found:", res$num_clusters_found, "\n")
      if (res$num_noise_points > 0) cat("Number of Noise Points (Label 0):", res$num_noise_points, "\n")
      cat("Data Standardized:", res$standardized, "\n")
      cat("Distance Metric Used (if applicable):", res$distance_metric, "\n")
      cat("Average Silhouette Score:", ifelse(is.na(res$silhouette_score), "N/A", round(res$silhouette_score, 4)), "\n")
    }
  })
  
  output$cluster_pca_plot <- renderPlotly({
    res <- cluster_results()
    req(res$pca_data)
    pca_plot_data <- res$pca_data
    
    p <- ggplot(pca_plot_data, aes(x = PC1, y = PC2, color = Cluster, text = paste("Label:", Label, "<br>Cluster:", Cluster))) +
      geom_point(alpha = 0.7) +
      labs(title = "Cluster Visualization (First 2 PCA Components)",
           x = "Principal Component 1", y = "Principal Component 2") +
      theme_minimal() +
      scale_color_viridis_d() # Use a nice color scale
    
    # Add ellipses using stat_ellipse if clusters are factors and > 3 points per cluster
    # Check cluster sizes
    cluster_counts <- table(pca_plot_data$Cluster)
    valid_clusters_for_ellipse <- names(cluster_counts[cluster_counts >= 3])
    if(length(valid_clusters_for_ellipse) > 0) {
      pca_plot_data_filtered <- pca_plot_data %>% filter(Cluster %in% valid_clusters_for_ellipse)
      if(nrow(pca_plot_data_filtered) > 0) {
        p <- p + stat_ellipse(data=pca_plot_data_filtered, aes(group = Cluster, color = Cluster), type = "t", level = 0.95, linetype = 2, alpha=0.5)
      }
    }
    
    
    ggplotly(p, tooltip = "text")
  })
  
  output$cluster_pca_biplot <- renderPlot({
    res <- cluster_results()
    req(res$pca_biplot)
    print(res$pca_biplot) # Print the factoextra plot object
  })
  
  output$cluster_stats_table <- renderDT({
    res <- cluster_results()
    req(res$cluster_summary)
    datatable(res$cluster_summary, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$cluster_silhouette_output <- renderPrint({
    res <- cluster_results()
    req(res)
    if (!is.null(res$error)) return()
    cat("Average Silhouette Score:", ifelse(is.na(res$silhouette_score), "N/A", round(res$silhouette_score, 4)), "\n")
    cat("(Higher is generally better. Score ranges from -1 to 1.)\n")
  })
  
  
  # --- 6. Supervised: Classification ---
  classification_results <- eventReactive(input$run_classif_btn, {
    req(rv$data_processed, input$classif_features, input$classif_target, input$classif_algo)
    df <- rv$data_processed
    feature_cols <- input$classif_features
    target_col <- input$classif_target
    algo <- input$classif_algo
    standardize_num <- input$classif_standardize
    test_size <- input$classif_test_size
    knn_k <- input$classif_knn_k
    seed <- input$classif_seed
    
    if (target_col %in% feature_cols) {
      showNotification("Target variable cannot be included in feature columns.", type = "error")
      return(NULL)
    }
    if (length(feature_cols) < 1) {
      showNotification("Please select at least one feature column.", type = "error")
      return(NULL)
    }
    
    # --- Prepare Data ---
    selected_cols <- c(target_col, feature_cols)
    class_df <- df %>%
      select(all_of(selected_cols)) %>%
      drop_na() # Listwise deletion for simplicity
    
    if (nrow(class_df) < 10) {
      showNotification("Insufficient complete data for classification.", type = "warning")
      return(NULL)
    }
    
    # Ensure target is a factor
    class_df[[target_col]] <- as.factor(class_df[[target_col]])
    if (nlevels(class_df[[target_col]]) < 2) {
      showNotification("Target variable must have at least 2 distinct levels for classification.", type = "error")
      return(NULL)
    }
    
    # Identify numeric and categorical features *within the selected features*
    numeric_features <- feature_cols[sapply(class_df[feature_cols], is.numeric)]
    categorical_features <- feature_cols[!sapply(class_df[feature_cols], is.numeric)]
    
    
    # --- Auto Algorithm Selection (Simple version) ---
    used_algorithm <- algo
    reason <- ""
    if (algo == "auto") {
      n_samples <- nrow(class_df)
      if (n_samples < 1000 && length(categorical_features) == 0) {
        used_algorithm <- "knn"
        reason <- "Auto: Small dataset, no categoricals -> KNN suggested."
      } else if (length(categorical_features) > 0) {
        used_algorithm <- "random_forest"
        reason <- "Auto: Dataset includes categorical features -> Random Forest suggested (handles mixed types well)."
      } else {
        used_algorithm <- "logistic_regression" # Only makes sense for binary? Needs refinement.
        # Let's default to Random Forest if unsure in auto
        used_algorithm <- "random_forest"
        reason <- "Auto: Defaulting to Random Forest for general purpose."
        if(nlevels(class_df[[target_col]]) == 2) {
          used_algorithm <- "logistic_regression"
          reason <- "Auto: Binary target -> Logistic Regression suggested."
        }
        
      }
      showNotification(paste("Auto selected algorithm:", used_algorithm), type="info")
    }
    
    
    # --- Train/Test Split ---
    set.seed(seed)
    train_indices <- createDataPartition(class_df[[target_col]], p = 1 - test_size, list = FALSE, times = 1)
    train_data <- class_df[train_indices, ]
    test_data <- class_df[-train_indices, ]
    
    
    # --- Preprocessing (using caret::preProcess for simplicity) ---
    preprocess_params <- NULL
    if (standardize_num && length(numeric_features) > 0) {
      # Define methods for preprocessing
      methods_to_apply <- c("center", "scale") # Standardize numeric features
      
      # Need to handle categorical features: dummy variables (one-hot encoding)
      # caret's train function handles this via the formula interface usually.
      # But if applying preProcess manually, we need care.
      # Let's rely on caret's train or specific model functions to handle factors.
      # If standardizing only:
      preProcValues <- preProcess(train_data[, numeric_features, drop=FALSE], method = methods_to_apply)
      train_data_processed <- predict(preProcValues, train_data)
      test_data_processed <- predict(preProcValues, test_data)
      preprocess_params <- preProcValues # Store params if needed
      reason <- paste(reason, "Numeric features standardized (center, scale).")
      
    } else {
      # No explicit preprocessing step here, rely on model function
      train_data_processed <- train_data
      test_data_processed <- test_data
      reason <- paste(reason, "No standardization applied.")
    }
    
    
    # --- Model Training ---
    model_fit <- NULL
    fit_time <- system.time({ # Time the training
      tryCatch({
        formula <- as.formula(paste0("`", safe_name(target_col), "` ~ ."))
        
        # Using caret::train for a unified interface where possible
        ctrl <- trainControl(method = "none") # No resampling, just fit once
        
        if (used_algorithm == "logistic_regression") {
          if (nlevels(train_data_processed[[target_col]]) != 2) stop("Logistic Regression requires a binary target.")
          model_fit <- train(formula, data = train_data_processed, method = "glm", family = "binomial", trControl = ctrl)
        } else if (used_algorithm == "knn") {
          # Tune grid for k
          tuneGrid <- expand.grid(k = knn_k)
          # Need to ensure preprocessing is handled correctly if not done before.
          # caret's train handles preprocessing if specified in preProcess argument or within trainControl
          preProcDef <- NULL
          if(standardize_num && length(numeric_features)>0) preProcDef <- c("center", "scale")
          
          model_fit <- train(formula, data = train_data, # Use original data if train handles preproc
                             method = "knn",
                             trControl = trainControl(method="none", preProcOptions = if(!is.null(preProcDef)) list(method=preProcDef) else NULL),
                             tuneGrid = tuneGrid)
          reason <- paste(reason, "KNN requires standardized data (handled by train).")
          
        } else if (used_algorithm == "decision_tree") {
          model_fit <- train(formula, data = train_data_processed, method = "rpart", trControl = ctrl, tuneLength=1) # Simple tree
        } else if (used_algorithm == "random_forest") {
          # RF handles factors directly
          model_fit <- train(formula, data = train_data_processed, method = "rf", trControl = ctrl, tuneLength = 1, importance = TRUE) # importance=TRUE for feature importance
        } else if (used_algorithm == "naive_bayes") {
          # Naive Bayes (e.g., e1071)
          # Needs numeric features typically (Gaussian NB). Factors are handled differently.
          # caret's 'naive_bayes' method might handle mixed types better.
          model_fit <- train(formula, data = train_data_processed, method = "naive_bayes", trControl = ctrl)
          reason <- paste(reason, "Naive Bayes assumption: Numeric features are Gaussian, categorical independent.")
          if(standardize_num && length(numeric_features)>0 && !is.null(preProcValues)) {
            # If standardized, Gaussian assumption might be more reasonable
          } else if (length(numeric_features)>0) {
            showNotification("Warning: Naive Bayes applied to potentially non-normal numeric features.", type="warning")
          }
          
        } else {
          stop(paste("Classification algorithm", used_algorithm, "not implemented."))
        }
        
      }, error = function(e) {
        showNotification(paste("Error training classification model:", e$message), type = "error", duration=10)
        model_fit <<- NULL # Ensure model_fit is NULL on error
      })
    }) # End system.time
    
    
    # --- Evaluation ---
    results_bundle <- list(algorithm = used_algorithm, reason = reason, train_time = fit_time[["elapsed"]])
    if (!is.null(model_fit)) {
      tryCatch({
        # Predict on test set
        predictions <- predict(model_fit, newdata = test_data_processed)
        
        # Ensure prediction levels match actual levels
        actual_levels <- levels(test_data_processed[[target_col]])
        if(!is.factor(predictions)) predictions <- factor(predictions, levels = actual_levels)
        else levels(predictions) <- actual_levels # Force levels
        
        
        # Confusion Matrix and Metrics using caret
        cm <- confusionMatrix(data = predictions, reference = test_data_processed[[target_col]])
        
        results_bundle$confusion_matrix <- cm$table
        results_bundle$metrics_overall <- cm$overall
        # Handle possibility of only 2 classes for byClass metrics
        results_bundle$metrics_by_class <- tryCatch(cm$byClass, error=function(e) NULL)
        
        # Feature Importance (if available, e.g., from Random Forest)
        imp <- tryCatch(varImp(model_fit, scale = FALSE), error = function(e) NULL)
        if (!is.null(imp)) {
          imp_df <- imp$importance
          imp_df <- data.frame(Feature = rownames(imp_df), Importance = imp_df$Overall) %>%
            arrange(desc(Importance)) %>%
            mutate(Importance = round(Importance, 4))
          results_bundle$feature_importance <- imp_df
        } else {
          results_bundle$feature_importance <- NULL
        }
        
      }, error = function(e) {
        showNotification(paste("Error during classification evaluation:", e$message), type="error", duration=10)
        results_bundle$error_evaluation <- e$message
      })
    } else {
      results_bundle$error_training <- "Model training failed."
    }
    
    return(results_bundle)
    
  })
  
  # Display Classification Results
  output$classif_summary_output <- renderPrint({
    res <- classification_results()
    req(res)
    cat("--- Classification Summary ---\n")
    cat("Algorithm Used:", res$algorithm, "\n")
    cat("Reason/Notes:", res$reason, "\n")
    cat("Training Time (sec):", round(res$train_time, 2), "\n\n")
    
    if (!is.null(res$error_training)) {
      cat("Training Error:", res$error_training, "\n")
      return()
    }
    if (!is.null(res$error_evaluation)) {
      cat("Evaluation Error:", res$error_evaluation, "\n")
    }
    
    if (!is.null(res$metrics_overall)) {
      cat("--- Overall Performance Metrics (Test Set) ---\n")
      print(round(res$metrics_overall, 4))
      cat("\n")
    }
    if (!is.null(res$metrics_by_class)) {
      cat("--- Metrics By Class (Test Set) ---\n")
      print(round(res$metrics_by_class, 4))
      cat("\n")
    }
    
  })
  
  output$classif_metrics_table <- renderDT({
    res <- classification_results()
    req(res)
    metrics_list <- list()
    if (!is.null(res$metrics_overall)) {
      metrics_list[["Overall"]] <- data.frame(Metric = names(res$metrics_overall), Value = res$metrics_overall, stringsAsFactors = FALSE)
    }
    if (!is.null(res$metrics_by_class)) {
      # Check if it's a matrix (multiple classes) or vector (binary)
      if(is.matrix(res$metrics_by_class)) {
        by_class_df <- as.data.frame(res$metrics_by_class) %>%
          tibble::rownames_to_column("Metric") %>%
          pivot_longer(-Metric, names_to="Class", values_to="Value")
      } else { # Vector for binary case
        by_class_df <- data.frame(Metric = names(res$metrics_by_class), Value = res$metrics_by_class, Class="Class1", stringsAsFactors = FALSE)
        # Try to get the actual class name? Difficult without target levels here.
      }
      metrics_list[["By Class"]] <- by_class_df
    }
    
    if(length(metrics_list) > 0) {
      # Combine or display separately? Combine might be messy. Let's show Overall.
      metrics_display <- metrics_list[["Overall"]] %>% mutate(Value = round(Value, 4))
      datatable(metrics_display, options = list(scrollX = TRUE, paging=FALSE, searching=FALSE, info=FALSE), rownames = FALSE)
    } else {
      # Return empty table if no metrics
      datatable(data.frame(Message=character()), options=list(info=FALSE))
    }
    
  })
  
  
  output$classif_conf_matrix_output <- renderPrint({
    res <- classification_results()
    req(res$confusion_matrix)
    cat("--- Confusion Matrix (Test Set) ---\n")
    print(res$confusion_matrix)
  })
  
  # Placeholder for caret's confusion matrix plot (similar to regression one)
  output$classif_conf_matrix_plot <- renderPlot({
    res <- classification_results()
    req(res$confusion_matrix) # Need the table at least
    cm_table <- res$confusion_matrix
    # Basic heatmap plot
    plot_data <- as.data.frame(cm_table)
    ggplot(plot_data, aes(x=Reference, y=Prediction, fill=Freq)) +
      geom_tile(color = "white") + # Add white borders
      geom_text(aes(label=Freq), vjust = 1) +
      scale_fill_gradient(low="lightblue", high="darkblue") +
      labs(title="Confusion Matrix", x="Actual Class", y="Predicted Class") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x labels if long
  })
  
  output$classif_importance_table <- renderDT({
    res <- classification_results()
    req(res$feature_importance)
    datatable(res$feature_importance, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  
  # --- Helper Functions ---
  safe_name <- function(name) {
    # Ensure column names are safe for formulas/ggplot aes_string
    return(paste0("`", gsub("`", "\\`", name), "`"))
  }
  
  
  # Use `%||%` infix operator for fallback (like Python's `or`)
  `%||%` <- function(a, b) {
    if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
  }
  
} 
# --- Run the App ---
shinyApp(ui = ui, server = server)
