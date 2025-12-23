library(dplyr)

#' 
#' @param data Input dataset (data_25, data_28, data_30, data_33, data_35, or data_40)
#' @param remove_age_through_from_features Whether to exclude age_through from modeling features
#'                                        (TRUE by default since it's constant within each dataset)
#' @return List containing corrected data and feature columns for modeling
create_modeling_features <- function(data, remove_age_through_from_features = TRUE) {

  modeling_data <- data %>%
    select(-any_of(c(
      "OPS",               
      "offensive_value",   
      "career_achievement", 
      "efficiency_score"   
    )))
  
  if(remove_age_through_from_features) {
    feature_columns <- modeling_data %>%
      select(where(is.numeric)) %>%
      select(-any_of(c("age_through", "inducted_num"))) %>%
      names()
  } else {
    feature_columns <- modeling_data %>%
      select(where(is.numeric)) %>%
      select(-any_of(c("inducted_num"))) %>%
      names()
  }
  
  return(list(
    data = modeling_data,
    feature_columns = feature_columns
  ))
}

#' @param age_datasets (data_25, data_28, data_30, data_33, data_35, or data_40)
#' @return 
apply_corrected_feature_engineering <- function(age_datasets) {
  
  cat("=== APPLYING CORRECTED FEATURE ENGINEERING ===\n\n")
  
  corrected_datasets <- list()
  
  for(age_name in names(age_datasets)) {
    cat(sprintf("Processing %s...\n", age_name))
    
    original_data <- age_datasets[[age_name]]
    
    result <- create_modeling_features(original_data, remove_age_through_from_features = TRUE)
    corrected_datasets[[age_name]] <- result
    
    feature_matrix <- result$data[, result$feature_columns, drop = FALSE] %>% na.omit()
    
    feature_vars <- sapply(feature_matrix, var, na.rm = TRUE)
    zero_var_count <- sum(feature_vars == 0 | is.na(feature_vars))
    
    if(zero_var_count == 0) {
      tryCatch({
        test_matrix <- as.matrix(feature_matrix)
        cond_num <- kappa(test_matrix, exact = FALSE)
        rank_result <- qr(test_matrix)$rank
        
        viable <- cond_num < 1e12 && rank_result == ncol(test_matrix)
        
        cat(sprintf("  ✅ %s: %d features, condition=%.2e, viable=%s\n",
                    age_name, ncol(feature_matrix), cond_num, viable))
        
      }, error = function(e) {
        cat(sprintf("  ❌ %s: Matrix test failed\n", age_name))
      })
    } else {
      cat(sprintf("  ❌ %s: %d zero-variance features remain\n", age_name, zero_var_count))
    }
  }
  
  cat("\n=== CORRECTED FEATURE ENGINEERING COMPLETE ===\n")
  return(corrected_datasets)
}