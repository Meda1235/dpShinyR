
library(dplyr) 
library(tidyr)
library(readr)
library(MASS) 


# --- Heuristická analýza ---
run_heuristic_analysis <- function(df) {

  print("--- Running Heuristic Analysis ---")
  print(paste("Input dimensions:", nrow(df), "x", ncol(df)))
  print("Summary of data entering heuristic analysis:")
  print(summary(df))
  if (is.null(df) || ncol(df) == 0) {
    warning("Heuristic analysis received NULL or empty data frame.")
    return(NULL)
  }
  
  n_cols <- ncol(df)
  col_analysis_list <- vector("list", n_cols)
  
  for (i in 1:n_cols) {
    col_name <- names(df)[i]
    col_data <- df[[i]]
    

    total_rows <- length(col_data)
    missing_values <- sum(is.na(col_data)) 
    missing_pct <- if(total_rows > 0) missing_values / total_rows * 100 else 0
    non_na_data <- col_data[!is.na(col_data)]
    n_non_na <- length(non_na_data)
    num_unique <- n_distinct(non_na_data)
    initial_class <- class(col_data)[1]

    detected_type <- "Categorical" 
    
    # 1. Případ: Jasně numerický podle R třídy
    if (initial_class %in% c("numeric", "integer", "double")) {
      detected_type <- "Numeric"
      # Zpětná kontrola: Numerický s málo unikátními hodnotami -> Kategorický
      if (n_non_na > 0) {
        category_threshold <- case_when(total_rows < 200 ~ 8, total_rows < 1000 ~ 12, TRUE ~ 20)
        if (num_unique <= category_threshold) {
          detected_type <- "Categorical"
        }
      }
      # 2. Případ: Logický -> Kategorický
    } else if (initial_class == "logical") {
      detected_type <- "Categorical" # Zůstává kategorický
      
      # 3. Případ: Ostatní (character, factor, Date, atd.) - zkusíme zjistit, zda nejsou ve skutečnosti numerické
    } else if (initial_class %in% c("character", "factor", "Date", "POSIXct", "POSIXlt")) {
      # Výchozí pro tuto skupinu je Categorical, ale ověříme obsah
      detected_type <- "Categorical" # Zůstáváme u Categorical, pokud se neprokáže opak
      
      if (n_non_na > 0) {
        # Testujeme, jaká část ne-NA hodnot vypadá jako číslo
        # Použijeme as.character pro jistotu (kvůli faktorům atd.)
        test_char_data <- as.character(non_na_data)
        # *** Použijeme parse_double pro lepší detekci s ohledem na locale ***
        suppressWarnings(numeric_test <- readr::parse_double(test_char_data, na = c("", "NA", "NaN", "NULL", "None", "#N/A"), locale = readr::default_locale())) # Použijeme výchozí locale zde
        proportion_numeric <- sum(!is.na(numeric_test)) / n_non_na
        
        # Pokud většina vypadá jako čísla, změníme na Numeric
        if (proportion_numeric > 0.95) {
          detected_type <- "Numeric"
          # Znovu zkontrolujeme nízkou kardinalitu pro tyto detekované numerické
          if(n_non_na > 0) { # Check again n_non_na > 0 before calculating threshold
            category_threshold <- case_when(total_rows < 200 ~ 8, total_rows < 1000 ~ 12, TRUE ~ 20)
            if (num_unique <= category_threshold) {
              detected_type <- "Categorical"
            }
          }
        }
       
      }
      
    }

    
    col_analysis_list[[i]] <- data.frame(
      Column = col_name,
      DetectedType = detected_type, 
      UniqueValues = num_unique,
      MissingValues = missing_values,
      MissingPercent = sprintf("%.2f%%", missing_pct), 
      OriginalRClass = initial_class,
      AISuggestion = NA_character_, 
      stringsAsFactors = FALSE
    )
  }
  
  analysis_df <- bind_rows(col_analysis_list)
  print("--- Heuristic Analysis Complete ---")
  print("Summary of analysis results:")
  print(analysis_df[, c("Column", "DetectedType", "MissingValues", "OriginalRClass")]) 
  return(analysis_df)
}


# --- Imputace ---

impute_missing_values <- function(df, imputation_config, col_types_info, dec_separator = ".") {
  

  calculate_mode <- function(x, na.rm = TRUE) {
 
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    if (length(x) == 0) return(NA)
    ux <- unique(x)
    tab <- tabulate(match(x, ux))
    mode_val <- ux[which.max(tab)]
    return(mode_val)
  }
  
  if (is.null(df) || nrow(df) == 0 || is.null(imputation_config) || length(imputation_config) == 0 || is.null(col_types_info)) {
    warning("Invalid input to impute_missing_values.")
    return(list(data = df, summary = list(cols_imputed = character(0), rows_removed = 0, cols_skipped = names(imputation_config), message = "Invalid input")))
  }
  
  data_modified <- df
  original_row_count <- nrow(data_modified)
  cols_to_impute <- names(imputation_config)
  cols_imputed_success <- character(0)
  cols_skipped_info <- character(0)
  cols_for_row_removal <- character(0)
  

  current_locale <- locale(decimal_mark = dec_separator) 
  
  # 1. Fáze: Standardní imputace
  for (col_name in cols_to_impute) {

    selected_method <- imputation_config[[col_name]]
    
    if (selected_method == "remove_row") {
      if (col_name %in% names(data_modified)) {
        cols_for_row_removal <- c(cols_for_row_removal, col_name)
      } else {
        warning(paste("Column '", col_name, "' specified for row removal not found in data."), call. = FALSE)
        cols_skipped_info <- c(cols_skipped_info, paste0(col_name, " (not found)"))
      }
      next # Zpracuje se ve fázi 2
    }
    
   
    if (!col_name %in% names(data_modified)) {
      warning(paste("Column '", col_name, "' specified for imputation not found."), call. = FALSE)
      cols_skipped_info <- c(cols_skipped_info, paste0(col_name, " (not found)"))
      next
    }
    if (!any(is.na(data_modified[[col_name]]))) {
      cols_skipped_info <- c(cols_skipped_info, paste0(col_name, " (no NA)"))
      next
    }
    
   
    col_info <- col_types_info %>% filter(Column == col_name) 
    if (nrow(col_info) == 0) {
      warning(paste("Type information missing for column '", col_name, "'. Skipping imputation."), call. = FALSE)
      cols_skipped_info <- c(cols_skipped_info, paste0(col_name, " (type info missing)"))
      next
    }
    if (!all(c("DetectedType", "OriginalRClass") %in% names(col_info))) {
      warning(paste("DetectedType or OriginalRClass missing in type info for column '", col_name, "'. Skipping."), call. = FALSE)
      cols_skipped_info <- c(cols_skipped_info, paste0(col_name, " (type info incomplete)"))
      next
    }
    detected_type <- col_info$DetectedType[1]
    original_class <- col_info$OriginalRClass[1] # Původní R třída z načtení/konverze
    
    current_col_data <- data_modified[[col_name]] # Data sloupce PŘED imputací
    imputed_value <- NA
    valid_method = TRUE
    
    # Výpočet/stanovení imputované hodnoty
    tryCatch({

      numeric_col_for_calc <- NULL
      if (detected_type == "Numeric") {
        if (is.numeric(current_col_data)) {
          numeric_col_for_calc <- current_col_data
        } else {
          # Pokus o konverzi POUZE pro výpočet, pokud je detekován jako numerický, ale uložen jinak
          numeric_col_for_calc <- suppressWarnings(parse_double(as.character(current_col_data), na = c("", "NA", "NaN", "NULL", "None", "#N/A"), locale = current_locale))
          if (all(is.na(numeric_col_for_calc[!is.na(current_col_data)]))) { # Zkontrolujeme, zda konverze nevrátila jen NA
            warning(paste("Column '", col_name, "' detected as Numeric, but conversion failed for calculation. Skipping mean/median."), call. = FALSE)
            numeric_col_for_calc <- NULL # Resetujeme, pokud konverze selhala
          }
        }
      }
      
      if (selected_method == "mean") {
        if (detected_type == "Numeric" && !is.null(numeric_col_for_calc)) {
          imputed_value <- mean(numeric_col_for_calc, na.rm = TRUE) # Použijeme konvertovaná data
          if (is.nan(imputed_value)) stop("Mean calculation resulted in NaN (all NA or conversion failed?)")
        } else {
          warning(paste("Method 'mean' skipped for column '", col_name, "'. DetectedType is not Numeric or conversion failed."), call. = FALSE)
          valid_method = FALSE
        }
      } else if (selected_method == "median") {
        if (detected_type == "Numeric" && !is.null(numeric_col_for_calc)) {
          imputed_value <- median(numeric_col_for_calc, na.rm = TRUE) # Použijeme konvertovaná data
          if (is.na(imputed_value)) stop("Median calculation resulted in NA (all NA or conversion failed?)")
        } else {
          warning(paste("Method 'median' skipped for column '", col_name, "'. DetectedType is not Numeric or conversion failed."), call. = FALSE)
          valid_method = FALSE
        }
      } else if (selected_method == "mode") {
        # Modus lze počítat na jakémkoli typu (vrátí hodnotu původního typu)
        imputed_value <- calculate_mode(current_col_data, na.rm = TRUE)
        if (is.na(imputed_value) && length(current_col_data[!is.na(current_col_data)]) > 0) { 
          warning(paste("Mode calculation returned NA for column '", col_name,"'. Data might be all NA or only unique values."), call.=FALSE)
        } else if (is.na(imputed_value)) {
          stop("Mode calculation failed (likely all NA)")
        }
      } else if (selected_method == "zero") {
        if (detected_type == "Numeric") {
          imputed_value <- 0
        } else {
          warning(paste("Method 'zero' is only suitable for Numeric columns. Skipping for '", col_name, "'."), call. = FALSE)
          valid_method = FALSE
        }
      } else if (selected_method == "constant_unknown") {
        # Vhodné pro kategorické (včetně textu, faktoru)
        if (detected_type == "Categorical") {
          imputed_value <- "Unknown"
        } else {
          warning(paste("Method 'constant_unknown' is intended for Categorical columns. Applying to '", col_name, "' (Numeric) may lead to type issues."), call. = FALSE)
          imputed_value <- "Unknown"
        }
      } else if (selected_method == "constant_true") {
        # Vhodné primárně pro logické, ale i pro jiné, pokud je to záměr
        if (original_class == "logical") {
          imputed_value <- TRUE
        } else {
          warning(paste("Applying 'constant_true' to non-logical column '", col_name, "'. Ensure this is intended."), call. = FALSE)
          imputed_value <- TRUE # Může změnit typ sloupce
        }
      } else if (selected_method == "constant_false") {
        if (original_class == "logical") {
          imputed_value <- FALSE
        } else {
          warning(paste("Applying 'constant_false' to non-logical column '", col_name, "'. Ensure this is intended."), call. = FALSE)
          imputed_value <- FALSE # Může změnit typ sloupce
        }
      } else {
        warning(paste("Unknown imputation method '", selected_method, "' for column '", col_name, "'."), call. = FALSE)
        valid_method = FALSE
      }
      
      # Zpracování výsledku výpočtu (pokud metoda byla validní)
      if (!valid_method) {
        cols_skipped_info <- c(cols_skipped_info, paste0(col_name, " (invalid method: ", selected_method, " for type ", detected_type, ")"))
      } else if (is.na(imputed_value) && !(selected_method %in% c("mode") && all(is.na(current_col_data)))) {
        warning(paste("Failed to calculate imputation value for column '", col_name, "' with method '", selected_method, "'. Skipping."), call. = FALSE)
        cols_skipped_info <- c(cols_skipped_info, paste0(col_name, " (calculation failed)"))
      } else {
        # Aplikace imputované hodnoty
        na_indices <- is.na(current_col_data)
        target_class <- class(current_col_data)[1] 
        compatible_value <- NA 
        
        # Pokus o konverzi imputované hodnoty na cílový typ sloupce
        final_impute_val <- tryCatch({

          if (target_class == "factor") {
            imputed_value_char <- as.character(imputed_value)
            current_levels <- levels(current_col_data)
            if (!imputed_value_char %in% current_levels) {
              imputed_value_char 
            } else {
              factor(imputed_value_char, levels = current_levels)
            }
          } else if (target_class == "Date") {
            methods::as(imputed_value, target_class) 
          } else if (target_class %in% c("numeric", "integer", "double") && is.numeric(imputed_value)) {
            if(target_class == "integer") as.integer(imputed_value) else as.double(imputed_value)
          } else if (target_class == "logical" && is.logical(imputed_value)) {
            imputed_value 
          } else if (target_class == "character") {
            as.character(imputed_value) 
          }
          else {
            methods::as(imputed_value, target_class)
          }
        }, warning = function(w) {
          warning(paste("Type conversion warning during imputation for column '", col_name,"': ", w$message), call.=FALSE)
          imputed_value 
        }, error = function(e) {
          warning(paste("Type conversion error during imputation for column '", col_name,"'. Using raw value. Error: ", e$message), call. = FALSE)
          imputed_value 
        })
        
        
   
        tryCatch({

          if (is.factor(data_modified[[col_name]]) && is.character(final_impute_val) && !final_impute_val %in% levels(data_modified[[col_name]])) {
            new_levels <- c(levels(data_modified[[col_name]]), final_impute_val)
            data_modified <- data_modified %>%
              mutate(!!sym(col_name) := factor(
                ifelse(is.na(!!sym(col_name)), final_impute_val, as.character(!!sym(col_name))),
                levels = new_levels)
              )
          } else {
            if(class(data_modified[[col_name]]) != class(final_impute_val) && !is.factor(data_modified[[col_name]])) {
              final_impute_val_converted <- tryCatch(methods::as(final_impute_val, class(data_modified[[col_name]])),
                                                     error = function(e) final_impute_val) 
              if(class(data_modified[[col_name]]) != class(final_impute_val_converted)) {
                warning(paste("Imputing column '", col_name, "' (", class(data_modified[[col_name]]), ") with value of type '", class(final_impute_val_converted),"'. Column type might change."), call.=FALSE)
              }
              final_impute_val <- final_impute_val_converted
            }
            
            data_modified <- data_modified %>%
              mutate(!!sym(col_name) := coalesce(!!sym(col_name), final_impute_val))
          }
          cols_imputed_success <- c(cols_imputed_success, col_name)
          
        }, error = function(e) {
          warning(paste("Error applying imputation to column '", col_name, "' in the data frame: ", e$message), call. = FALSE)
          cols_skipped_info <- c(cols_skipped_info, paste0(col_name, " (apply error)"))
        }) 
        
      } 
      
    }, error = function(e) {
      # Chyba při VÝPOČTU imputované hodnoty
      warning(paste("Error calculating imputation value for column '", col_name, "' with method '", selected_method, "': ", e$message), call. = FALSE)
      cols_skipped_info <- c(cols_skipped_info, paste0(col_name, " (calculation error: ", e$message, ")"))
    }) 
    
  } 
  
  # 2. Fáze: Odstranění řádků
  rows_removed_count = 0
  if (length(cols_for_row_removal) > 0) {
    rows_before_removal <- nrow(data_modified)
    valid_cols_for_removal <- intersect(cols_for_row_removal, names(data_modified))
    if(length(valid_cols_for_removal) > 0) {
   
      data_modified <- data_modified %>%
        tidyr::drop_na(any_of(valid_cols_for_removal))
      rows_removed_count <- rows_before_removal - nrow(data_modified)
      cols_imputed_success <- unique(c(cols_imputed_success, valid_cols_for_removal))
    } else {
      warning("No valid columns found for row removal.", call. = FALSE)
    }
  }
  
  final_message <- paste("Imputation attempted. Success:", length(cols_imputed_success),
                         "Skipped/Error:", length(cols_skipped_info),
                         "Rows removed:", rows_removed_count)
  
  summary_info <- list(
    cols_imputed = cols_imputed_success,
    rows_removed = rows_removed_count,
    cols_skipped = cols_skipped_info,
    message = final_message
  )
  
  return(list(data = data_modified, summary = summary_info))
}