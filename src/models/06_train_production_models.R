# =============================================================================
# PRODUCTION HOF PREDICTION MODELS
# =============================================================================
# This script:
#   1. Trains final models on FULL data (train + holdout) for each age snapshot
#   2. Saves production-ready models
#   3. Provides prediction functions for new players
#   4. Extracts feature importance/coefficients
# =============================================================================
# EDIT LINE 44

library(tidyverse)
library(caret)
library(glmnet)
library(pROC)
library(patchwork)
library(showtext)

set.seed(1992)

best_configs <- list(
  data_25 = list(
    model = "glmnet",
    sampling = "up",
    alpha = 0.75,
    lambda = 0.00379269,
    threshold = 0.4505347
  ),
  data_28 = list(
    model = 'glmnet',
    sampling = 'up',
    alpha = 0.25,
    lambda = 0.01274275,
    threshold = 0.4828578
  ),
  data_30 = list(
    model = "glmnet",
    sampling = "up",
    alpha = 0.25,
    lambda = 0.006951928,
    threshold = 0.5104107
  ),
  data_33 = list(
    model = "glmnet",
    sampling = "smote",
    alpha = 0.0,
    lambda = 0.483293,
    threshold = 0.5099826
  ),
  data_35 = list(
    model = "glmnet",
    sampling = "smote",
    alpha = 1.0,
    lambda = 0.002069138,
    threshold = 0.5087926
  ),
  data_40 = list(
    model = 'glmnet',
    sampling = 'up',
    alpha = 0.75,
    lambda = 0.006951928,
    threshold = 0.50836493
  )
)
# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading datasets...\n")

data_25 <- readRDS("data/processed/data_25.rds")
data_28 <- readRDS("data/processed/data_28.rds")
data_30 <- readRDS("data/processed/data_30.rds")
data_33 <- readRDS("data/processed/data_33.rds")
data_35 <- readRDS("data/processed/data_35.rds")
data_40 <- readRDS("data/processed/data_40.rds")

non_features <- c('playerID', 'age_through', 'Years', 'finalYear', 'nameFirst', 
                  'nameLast', 'POS', 'Era', 'minyear_peak', 'maxyear_peak', 
                  'inducted', 'inducted_num')

datasets <- list(
  data_25 = data_25, 
  data_28 = data_28,
  data_30 = data_30, 
  data_33 = data_33, 
  data_35 = data_35,
  data_40 = data_40
)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

simple_smote <- function(data, target_col, k = 5) {
  minority_class <- names(sort(table(data[[target_col]])))[1]
  majority_class <- names(sort(table(data[[target_col]])))[2]
  
  minority_data <- data[data[[target_col]] == minority_class, ]
  majority_data <- data[data[[target_col]] == majority_class, ]
  
  n_minority <- nrow(minority_data)
  n_majority <- nrow(majority_data)
  n_synthetic <- n_majority - n_minority
  
  if (n_synthetic <= 0) return(data)
  
  numeric_cols <- sapply(minority_data, is.numeric)
  numeric_cols[target_col] <- FALSE
  
  if (sum(numeric_cols) == 0) return(data)
  
  numeric_features <- minority_data[, numeric_cols, drop = FALSE]
  
  synthetic_samples <- list()
  
  for (i in 1:n_synthetic) {
    sample_idx <- sample(nrow(minority_data), 1)
    sample_row <- numeric_features[sample_idx, , drop = FALSE]
    neighbor_idx <- sample(nrow(minority_data), min(k, nrow(minority_data)))
    neighbor_row <- numeric_features[sample(neighbor_idx, 1), , drop = FALSE]
    
    alpha <- runif(1, 0, 1)
    synthetic_row <- alpha * sample_row + (1 - alpha) * neighbor_row
    
    full_synthetic <- minority_data[sample_idx, , drop = FALSE]
    full_synthetic[, numeric_cols] <- synthetic_row
    
    synthetic_samples[[i]] <- full_synthetic
  }
  
  synthetic_df <- do.call(rbind, synthetic_samples)
  return(rbind(majority_data, minority_data, synthetic_df))
}

apply_sampling <- function(data, method) {
  if (method == "none") return(data)
  
  set.seed(1992)
  
  if (method == "up") {
    minority_class <- names(sort(table(data$inducted)))[1]
    majority_class <- names(sort(table(data$inducted)))[2]
    minority_data <- data[data$inducted == minority_class, ]
    majority_data <- data[data$inducted == majority_class, ]
    n_to_add <- nrow(majority_data) - nrow(minority_data)
    oversampled <- minority_data[sample(nrow(minority_data), n_to_add, replace = TRUE), ]
    return(rbind(majority_data, minority_data, oversampled))
  }
  
  if (method == "smote") {
    return(simple_smote(data, "inducted"))
  }
  
  if (method == "down") {
    minority_class <- names(sort(table(data$inducted)))[1]
    majority_class <- names(sort(table(data$inducted)))[2]
    minority_data <- data[data$inducted == minority_class, ]
    majority_data <- data[data$inducted == majority_class, ]
    undersampled <- majority_data[sample(nrow(majority_data), nrow(minority_data)), ]
    return(rbind(undersampled, minority_data))
  }
  
  return(data)
}

convert_to_numeric <- function(df, features) {
  df_out <- df
  
  for (feat in features) {
    if (is.factor(df_out[[feat]])) {
      df_out[[feat]] <- as.numeric(as.character(df_out[[feat]]))
    } else if (is.character(df_out[[feat]])) {
      df_out[[feat]] <- as.numeric(df_out[[feat]])
    } else if (is.logical(df_out[[feat]])) {
      df_out[[feat]] <- as.numeric(df_out[[feat]])
    }
  }
  
  return(df_out)
}

# =============================================================================
# TRAIN PRODUCTION MODELS
# =============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("TRAINING PRODUCTION MODELS ON FULL DATA\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

production_models <- list()
preprocessing_params <- list()
feature_importance <- list()

for (name in names(datasets)) {
  cat("\n", "-" %>% rep(50) %>% paste(collapse = ""), "\n", sep = "")
  cat("Training:", name, "\n")
  cat("-" %>% rep(50) %>% paste(collapse = ""), "\n")
  
  config <- best_configs[[name]]
  data <- datasets[[name]]
  features <- setdiff(colnames(data), non_features)
  
  model_data <- data %>%
    dplyr::select(all_of(features), inducted) %>%
    na.omit()
  
  model_data <- convert_to_numeric(model_data, features)
  
  non_numeric <- features[!sapply(model_data[, features], is.numeric)]
  if (length(non_numeric) > 0) {
    cat("WARNING: Non-numeric features found:", paste(non_numeric, collapse = ", "), "\n")
    cat("Removing these features...\n")
    features <- setdiff(features, non_numeric)
    model_data <- model_data %>% dplyr::select(all_of(features), inducted)
  }
  
  feature_vars <- sapply(model_data[, features], var, na.rm = TRUE)
  zero_var_features <- names(feature_vars[feature_vars == 0 | is.na(feature_vars)])
  if (length(zero_var_features) > 0) {
    cat("Removing zero-variance features:", paste(zero_var_features, collapse = ", "), "\n")
    features <- setdiff(features, zero_var_features)
    model_data <- model_data %>% dplyr::select(all_of(features), inducted)
  }
  
  cat("Full dataset size:", nrow(model_data), "\n")
  cat("Number of features:", length(features), "\n")
  cat("Class distribution - Y:", sum(model_data$inducted == "Y"), 
      "N:", sum(model_data$inducted == "N"), "\n")
  
  sampled_data <- apply_sampling(model_data, config$sampling)
  cat("After", config$sampling, "sampling:", nrow(sampled_data), "\n")
  
  sampled_data <- convert_to_numeric(sampled_data, features)

  X_train <- sampled_data[, features, drop = FALSE]
  
  for (col in features) {
    if (any(is.na(X_train[[col]]))) {
      X_train[[col]][is.na(X_train[[col]])] <- mean(X_train[[col]], na.rm = TRUE)
    }
  }
  
  preproc <- preProcess(X_train, method = c("center", "scale"))
  preprocessing_params[[name]] <- preproc
  
  X_scaled <- predict(preproc, X_train)
  y <- factor(sampled_data$inducted, levels = c("Y", "N"))
  
  X_matrix <- as.matrix(X_scaled)
  
  if (!is.numeric(X_matrix)) {
    cat("ERROR: Matrix is not numeric. Checking columns...\n")
    for (col in colnames(X_scaled)) {
      cat("  ", col, ":", class(X_scaled[[col]]), "\n")
    }
    stop("Cannot proceed with non-numeric data")
  }
  
  set.seed(1992)
  
  final_model <- glmnet(
    x = X_matrix,
    y = y,
    family = "binomial",
    alpha = config$alpha,
    lambda = config$lambda
  )
  
  production_models[[name]] <- list(
    model = final_model,
    preprocess = preproc,
    features = features,
    config = config,
    threshold = config$threshold
  )
  
  coefs <- coef(final_model, s = config$lambda)
  coef_df <- data.frame(
    feature = rownames(coefs),
    coefficient = as.numeric(coefs)
  ) %>%
    filter(feature != "(Intercept)", coefficient != 0) %>%
    arrange(desc(abs(coefficient)))
  
  feature_importance[[name]] <- coef_df
  
  cat("\nTop 10 features by |coefficient|:\n")
  print(head(coef_df, 10))
  
  raw_train_probs <- predict(final_model, newx = X_matrix, 
                             s = config$lambda, type = "response")[,1]
  train_probs <- 1 - raw_train_probs  
  train_preds <- factor(ifelse(train_probs >= config$threshold, "Y", "N"), 
                        levels = c("Y", "N"))
  cm <- confusionMatrix(train_preds, y, positive = "Y")
  cat("\nTraining performance check:\n")
  cat("  Sensitivity:", round(cm$byClass["Sensitivity"], 4), "\n")
  cat("  Specificity:", round(cm$byClass["Specificity"], 4), "\n")
  cat("  Balanced Acc:", round((cm$byClass["Sensitivity"] + cm$byClass["Specificity"])/2, 4), "\n")
}

# =============================================================================
# SAVE PRODUCTION MODELS
# =============================================================================

cat("\n\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("SAVING PRODUCTION MODELS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

saveRDS(production_models, "models/production_models.rds")
saveRDS(feature_importance, "models/feature_importance.rds")

cat("\nSaved: models/production_models.rds\n")
cat("Saved: models/feature_importance.rds\n")

# =============================================================================
# PREDICTION FUNCTION
# =============================================================================

#' @param player_data
#' @param age
#' @param models
#' @return 
predict_hof <- function(player_data, age, models = NULL) {
  if (is.null(models)) {
    models <- readRDS("models/production_models.rds")
  }
  
  model_name <- paste0("data_", age)
  
  if (!model_name %in% names(models)) {
    available_ages <- as.numeric(gsub("data_", "", names(models)))
    closest_age <- available_ages[which.min(abs(available_ages - age))]
    model_name <- paste0("data_", closest_age)
    warning(paste("Age", age, "not available. Using closest age:", closest_age))
  }
  
  model_obj <- models[[model_name]]
  
  missing_features <- setdiff(model_obj$features, colnames(player_data))
  if (length(missing_features) > 0) {
    stop(paste("Missing features:", paste(missing_features, collapse = ", ")))
  }
  
  X <- player_data[, model_obj$features, drop = FALSE]
  
  for (feat in model_obj$features) {
    if (is.factor(X[[feat]])) {
      X[[feat]] <- as.numeric(as.character(X[[feat]]))
    } else if (is.character(X[[feat]])) {
      X[[feat]] <- as.numeric(X[[feat]])
    } else if (is.logical(X[[feat]])) {
      X[[feat]] <- as.numeric(X[[feat]])
    }
  }
  
  for (feat in model_obj$features) {
    if (any(is.na(X[[feat]]))) {
      X[[feat]][is.na(X[[feat]])] <- 0
    }
  }
  
  X_scaled <- predict(model_obj$preprocess, X)
  
  X_matrix <- as.matrix(X_scaled)
  storage.mode(X_matrix) <- "numeric"
  
  raw_prob <- predict(model_obj$model, 
                      newx = X_matrix, 
                      s = model_obj$config$lambda, 
                      type = "response")[1,1]
  prob <- 1 - raw_prob
  
  prediction <- ifelse(prob >= model_obj$threshold, "Y", "N")
  
  return(list(
    probability = prob,
    prediction = prediction,
    threshold = model_obj$threshold,
    age_model_used = as.numeric(gsub("data_", "", model_name)),
    confidence = ifelse(prediction == "Y", prob, 1 - prob)
  ))
}

dump("predict_hof", file = "models/predict_hof_function.R")
cat("Saved: models/predict_hof_function.R\n")


# =============================================================================
# SUMMARY TABLE
# =============================================================================

cat("\n\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("PRODUCTION MODEL SUMMARY\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

results_df <- read.csv('models/rigorous_tuning_results.csv')
summary_df <- map_dfr(names(best_configs), function(name) {
  config <- best_configs[[name]]
  age <- as.numeric(gsub("data_", "", name))
  
  # Get CV metrics from results_df for this dataset/config combination
  cv_metrics <- results_df %>%
    filter(dataset == name,
           model == config$model,
           sampling == config$sampling) %>%
    summarise(
      CV_BalancedAcc = max(balanced_acc),
      CV_ROC = max(roc)
    )
  
  tibble(
    Age = age,
    Model = config$model,
    Sampling = config$sampling,
    Alpha = config$alpha,
    Lambda = config$lambda,
    Threshold = config$threshold,
    CV_BalancedAcc = cv_metrics$CV_BalancedAcc,
    CV_ROC = cv_metrics$CV_ROC
  )
}) %>%
  arrange(Age)

print(summary_df)

write.csv(summary_df, "models/production_model_summary.csv", row.names = FALSE)

cat("\n\nProduction models ready for deployment!\n")
cat("\nUsage example:\n")
cat("  models <- readRDS('models/production_models.rds')\n")
cat("  source('models/predict_hof_function.R')\n")
cat("  result <- predict_hof(player_data, age = 33, models = models)\n")
cat("  print(result$probability)\n")

# ==============================================================================
# HOF Model Performance Visualization for LinkedIn
# Part 3: Modeling & Evaluation
# ==============================================================================

if ("package:pROC" %in% search()) detach("package:pROC", unload = TRUE)

primary_blue <- "#418FDE"
primary_red <- "#C8102E"
dark_bg <- "#1a1a2e"
card_bg <- "#16213e"
text_color <- "#eaeaea"
muted_text <- "#a0a0a0"
grid_color <- "#2a2a4a"



age_performance <- results_df %>%
  group_by(dataset) %>%
  summarise(
    roc_auc = mean(roc, na.rm = TRUE),
    balanced_acc = mean(balanced_acc, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(age = as.numeric(gsub("data_", "", dataset))) %>%
  filter(!is.na(age)) %>%
  arrange(age) %>%
  select(age, roc_auc, balanced_acc)

model_comparison <- results_df %>%
  group_by(model) %>%
  summarise(balanced_acc = max(balanced_acc, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(balanced_acc), is.finite(balanced_acc)) %>%
  arrange(balanced_acc) %>%
  mutate(
    is_winner = balanced_acc == max(balanced_acc),
    model = factor(model, levels = model)  # keeps the sorted order
  )

theme_hof_light <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "#f8f9fa", color = NA),
      panel.grid.major = element_line(color = "#e0e0e0", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text = element_text(color = "#333333"),
      plot.title = element_text(color = "#1a1a1a", size = 14, face = "bold", 
                                 hjust = 0.5, margin = ggplot2::margin(b = 10)),
      plot.subtitle = element_text(color = "#666666", size = 10, hjust = 0.5,
                                    margin = ggplot2::margin(b = 15)),
      axis.title = element_text(color = "#666666", size = 10),
      axis.text = element_text(color = "#666666", size = 9),
      legend.position = "none",
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )
}

p1 <- ggplot(age_performance, aes(x = .data$age, y = .data$roc_auc)) +
  geom_area(fill = primary_blue, alpha = 0.15) +
  geom_line(color = primary_blue, linewidth = 1.5) +
  geom_point(color = primary_blue, size = 4) +
  geom_point(color = "white", size = 2) +
  geom_text(aes(label = sprintf("%.3f", .data$roc_auc)), 
            vjust = -1.5, color = "#333333", size = 3.5, fontface = "bold") +
  scale_x_continuous(
    breaks = c(25, 28, 30, 33, 35, 40),
    labels = c("Age 25", "Age 28", "Age 30", "Age 33", "Age 35", "Age 40")
  ) +
  scale_y_continuous(
    limits = c(0.90, 1.00),
    breaks = seq(0.90, 1.0, 0.02),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  labs(
    title = "Prediction Improves with Age",
    subtitle = "ROC-AUC increases as careers progress",
    x = NULL,
    y = "ROC-AUC"
  ) +
  theme_hof_light()

p2 <- ggplot(model_comparison, aes(x = .data$model, y = .data$balanced_acc, fill = .data$is_winner)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", .data$balanced_acc * 100)),
            hjust = -0.2, color = "#333333", size = 3.5, fontface = "bold") +
  coord_flip(ylim = c(0.87, 0.96)) +
  scale_fill_manual(values = c("TRUE" = primary_red, "FALSE" = primary_blue)) +
  scale_y_continuous(
    breaks = seq(0.87, 0.95, 0.02),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Model Comparison (Age 33)",
    subtitle = "Regularized regression beats complex models",
    x = NULL,
    y = "Balanced Accuracy"
  ) +
  theme_hof_light()

combined_light <- p1 + p2 +
  plot_annotation(
    title = "Age-Specific Model Performance",
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(color = "#1a1a1a", size = 16, face = "bold", 
                                 hjust = 0.5, margin = ggplot2::margin(b = 5))
    )
  )

ggsave(
  "output/hof_model_performance_light.png",
  plot = combined_light,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)

cat("✓ Light version saved to hof_model_performance_light.png\n")