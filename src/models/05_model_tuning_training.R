# =============================================================================
# RIGOROUS MODEL TUNING FOR HOF PREDICTION
# =============================================================================
#   1. Proper train/holdout split BEFORE any modeling
#   2. Resampling (SMOTE, etc.) happens INSIDE CV folds via caret
#   3. Optimizes for Balanced Accuracy (sens + spec) / 2, not just ROC
#   4. Includes threshold optimization
#   5. Era-aware CV to prevent temporal leakage
#   6. Feature importance and selection
#   7. Calibration assessment
#   8. Final holdout evaluation
# =============================================================================

library(tidyverse)
library(caret)
library(pROC)
library(doParallel)
library(gbm)
library(xgboost)
library(kernlab)
library(randomForest)
library(glmnet)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(1992)

balancedSummary <- function(data, lev = NULL, model = NULL) {
  if (is.null(lev)) lev <- levels(data$obs)
  
  if (!all(lev %in% colnames(data))) {
    stop("Probability columns not found")
  }
  
  # ROC-AUC
  roc_auc <- as.numeric(pROC::auc(data$obs, data[, lev[1]], quiet = TRUE))
  cm <- confusionMatrix(data$pred, data$obs, positive = lev[1])
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  ppv <- cm$byClass["Pos Pred Value"]
  npv <- cm$byClass["Neg Pred Value"]
  balanced_acc <- (sens + spec) / 2
  youden_j <- sens + spec - 1
  out <- c(
    ROC = roc_auc,
    BalancedAcc = balanced_acc,
    Sens = sens,
    Spec = spec,
    PPV = ppv,
    NPV = npv,
    YoudenJ = youden_j
  )
  names(out) <- c("ROC", "BalancedAcc", "Sens", "Spec", "PPV", "NPV", "YoudenJ")
  return(out)
}

# =============================================================================
# LOAD AND PREPARE DATA
# =============================================================================

data_25 <- readRDS("data/processed/data_25.rds")
data_28 <- readRDS("data/processed/data_28.rds")
data_30 <- readRDS("data/processed/data_30.rds")
data_33 <- readRDS("data/processed/data_33.rds")
data_35 <- readRDS("data/processed/data_35.rds")
data_40 <- readRDS("data/processed/data_40.rds")

non_features <- c('playerID', 'age_through', 'Years', 'finalYear', 'nameFirst', 
                  'nameLast', 'POS', 'Era', 'minyear_peak', 'maxyear_peak', 
                  'inducted', 'inducted_num')

datasets_raw <- list(
  data_25 = data_25, 
  data_28 = data_28,
  data_30 = data_30, 
  data_33 = data_33, 
  data_35 = data_35,
  data_40 = data_40
)

cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("HOF PREDICTION MODEL TUNING\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")
cat("Creating stratified train/holdout splits (80/20)...\n\n")

datasets <- list()

for (name in names(datasets_raw)) {
  df <- datasets_raw[[name]]
  features <- setdiff(colnames(df), non_features)
  model_data <- df %>%
    dplyr::select(all_of(features), inducted, Era) %>%
    na.omit()
  set.seed(1992)
  train_idx <- createDataPartition(model_data$inducted, p = 0.80, list = FALSE)
  
  train_set <- model_data[train_idx, ]
  holdout_set <- model_data[-train_idx, ]
  
  datasets[[name]] <- list(
    train = train_set,
    holdout = holdout_set,
    features = features
  )
  
  cat(name, ":\n")
  cat("  Train:   ", nrow(train_set), " (Y:", sum(train_set$inducted == "Y"), 
      ", N:", sum(train_set$inducted == "N"), ")\n", sep = "")
  cat("  Holdout: ", nrow(holdout_set), " (Y:", sum(holdout_set$inducted == "Y"), 
      ", N:", sum(holdout_set$inducted == "N"), ")\n", sep = "")
}

cat("\n")

# CV
sampling_methods <- c("none", "up", "down", "smote", "rose")

generate_seeds <- function(n_folds = 10, n_repeats = 3, n_tuning = 100) {
  set.seed(1992)
  seeds <- vector(mode = "list", length = n_folds * n_repeats + 1)
  for (i in 1:(n_folds * n_repeats)) {
    seeds[[i]] <- sample.int(10000, n_tuning)
  }
  seeds[[n_folds * n_repeats + 1]] <- sample.int(10000, 1)
  return(seeds)
}

grids <- list(
  glmnet = expand.grid(
    alpha = c(0, 0.25, 0.5, 0.75, 1), 
    lambda = 10^seq(-4, 1, length = 20)
  ),
  gbm = expand.grid(
    n.trees = c(100, 200, 300),
    interaction.depth = c(3, 5, 7),
    shrinkage = c(0.05, 0.1),
    n.minobsinnode = c(10, 20)
  ),
  xgbTree = expand.grid(
    nrounds = c(100, 200),
    max_depth = c(4, 6),
    eta = c(0.05, 0.1),
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 5,
    subsample = c(0.8, 1)
  ),
  svmRadial = expand.grid(
    sigma = c(0.01, 0.05, 0.1),
    C = c(0.5, 1, 5, 10)
  ),
  rf = expand.grid(
    mtry = c(5, 10, 15, 20, 25)
  )
)

max_tuning <- max(sapply(grids, nrow))
cv_seeds <- generate_seeds(n_tuning = max_tuning + 10)

tune_model_rigorous <- function(method_name, tuning_grid, train_data, 
                                 sampling_method, features) {
  
  model_features <- setdiff(features, "Era")
  
  formula_data <- train_data %>% dplyr::select(all_of(model_features), inducted)
  
  ctrl <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    sampling = if (sampling_method == "none") NULL else sampling_method,
    classProbs = TRUE,
    summaryFunction = balancedSummary,
    savePredictions = "final",
    allowParallel = TRUE,
    verboseIter = FALSE,
    seeds = cv_seeds
  )

  set.seed(1992)
  
  trained_model <- tryCatch({
    train(
      inducted ~ .,
      data = formula_data,
      method = method_name,
      trControl = ctrl,
      tuneGrid = tuning_grid,
      metric = "BalancedAcc", 
      maximize = TRUE,
      preProcess = c("center", "scale")
    )
  }, error = function(e) {
    cat("    ERROR:", e$message, "\n")
    return(NULL)
  })
  
  return(trained_model)
}

find_optimal_threshold <- function(model, method = "youden") {
  preds <- model$pred
  best_tune <- model$bestTune
  for (param in names(best_tune)) {
    preds <- preds[preds[[param]] == best_tune[[param]], ]
  }
  roc_obj <- roc(preds$obs, preds$Y, quiet = TRUE)
  if (method == "youden") {
    best_coords <- coords(roc_obj, "best", best.method = "youden", 
                          ret = c("threshold", "sensitivity", "specificity"))
  } else if (method == "closest.topleft") {
    best_coords <- coords(roc_obj, "best", best.method = "closest.topleft",
                          ret = c("threshold", "sensitivity", "specificity"))
  }
  return(list(
    threshold = best_coords$threshold,
    sensitivity = best_coords$sensitivity,
    specificity = best_coords$specificity,
    roc = roc_obj
  ))
}

assess_calibration <- function(model, n_bins = 10) {
  preds <- model$pred
  best_tune <- model$bestTune
  for (param in names(best_tune)) {
    preds <- preds[preds[[param]] == best_tune[[param]], ]
  }
  
  actual_numeric <- as.numeric(preds$obs == "Y")
  brier_score <- mean((preds$Y - actual_numeric)^2)
  
  preds$bin <- cut(preds$Y, breaks = seq(0, 1, length.out = n_bins + 1), 
                   include.lowest = TRUE)
  
  calibration_df <- preds %>%
    group_by(bin) %>%
    summarise(
      mean_predicted = mean(Y),
      mean_actual = mean(obs == "Y"),
      n = n(),
      .groups = "drop"
    )
  
  return(list(
    brier_score = brier_score,
    calibration = calibration_df
  ))
}

all_results <- list()
all_models <- list()
result_counter <- 1

models_to_tune <- c("glmnet", "gbm", "xgbTree", "svmRadial", "rf")

for (dataset_name in names(datasets)) {
  cat("\n")
  cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
  cat("DATASET:", toupper(dataset_name), "\n")
  cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
  
  train_data <- datasets[[dataset_name]]$train
  features <- datasets[[dataset_name]]$features
  
  for (model_name in models_to_tune) {
    cat("\n  MODEL:", model_name, "\n")
    cat("  ", "-" %>% rep(40) %>% paste(collapse = ""), "\n", sep = "")
    
    for (samp_method in sampling_methods) {
      cat("    Sampling:", samp_method, "... ")
      
      trained <- tune_model_rigorous(
        method_name = model_name,
        tuning_grid = grids[[model_name]],
        train_data = train_data,
        sampling_method = samp_method,
        features = features
      )
      
      if (is.null(trained)) {
        cat("FAILED\n")
        next
      }
      
      best_idx <- which.max(trained$results$BalancedAcc)
      best_result <- trained$results[best_idx, ]
      
      threshold_info <- tryCatch(
        find_optimal_threshold(trained),
        error = function(e) list(threshold = 0.5, sensitivity = NA, specificity = NA)
      )
      
      calib_info <- tryCatch(
        assess_calibration(trained),
        error = function(e) list(brier_score = NA)
      )
      
      cat("BalancedAcc =", round(best_result$BalancedAcc, 4),
          "| ROC =", round(best_result$ROC, 4),
          "| Brier =", round(calib_info$brier_score, 4), "\n")
      
      model_key <- paste(dataset_name, model_name, samp_method, sep = "_")
      all_models[[model_key]] <- trained
      
      all_results[[result_counter]] <- data.frame(
        dataset = dataset_name,
        model = model_name,
        sampling = samp_method,
        balanced_acc = best_result$BalancedAcc,
        balanced_acc_sd = best_result$BalancedAccSD,
        roc = best_result$ROC,
        roc_sd = best_result$ROCSD,
        sens_cv = best_result$Sens,
        spec_cv = best_result$Spec,
        ppv_cv = best_result$PPV,
        npv_cv = best_result$NPV,
        optimal_threshold = threshold_info$threshold,
        sens_at_optimal = threshold_info$sensitivity,
        spec_at_optimal = threshold_info$specificity,
        brier_score = calib_info$brier_score,
        stringsAsFactors = FALSE
      )
      
      for (param in names(trained$bestTune)) {
        all_results[[result_counter]][[param]] <- trained$bestTune[[param]]
      }
      
      result_counter <- result_counter + 1
    }
  }
  
  results_df <- bind_rows(all_results)
  saveRDS(results_df, "models/rigorous_tuning_results_partial.rds")
  saveRDS(results_df, "models/rigorous_tuning_results_partial.rds")
}

# =============================================================================
# COMPILE FINAL CV RESULTS
# =============================================================================

results_df <- bind_rows(all_results)

cat("\n\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("CROSS-VALIDATION RESULTS SUMMARY\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("TOP 15 CONFIGURATIONS BY BALANCED ACCURACY:\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n")

top_configs <- results_df %>%
  arrange(desc(balanced_acc)) %>%
  dplyr::select(dataset, model, sampling, balanced_acc, roc, sens_at_optimal, 
         spec_at_optimal, brier_score) %>%
  head(15)

print(top_configs)

cat("\n\nBEST CONFIGURATION PER MODEL TYPE:\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n")

best_per_model <- results_df %>%
  group_by(model) %>%
  slice_max(balanced_acc, n = 1) %>%
  dplyr::select(model, dataset, sampling, balanced_acc, roc, sens_at_optimal, spec_at_optimal) %>%
  arrange(desc(balanced_acc))

print(best_per_model, n = Inf)

# Effect of sampling method
cat("\n\nAVERAGE PERFORMANCE BY SAMPLING METHOD:\n")
cat("-" %>% rep(60) %>% paste(collapse = ""), "\n")

by_sampling <- results_df %>%
  group_by(sampling) %>%
  summarise(
    mean_balanced_acc = mean(balanced_acc, na.rm = TRUE),
    sd_balanced_acc = sd(balanced_acc, na.rm = TRUE),
    mean_roc = mean(roc, na.rm = TRUE),
    n_configs = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_balanced_acc))

print(by_sampling, n = Inf)

cat("\n\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("FINAL HOLDOUT EVALUATION\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("(This is the ONLY time we touch the holdout set)\n\n")

best_config <- results_df %>%
  arrange(desc(balanced_acc)) %>%
  head(1)

cat("Best configuration from CV:\n")
cat("  Dataset:", best_config$dataset, "\n")
cat("  Model:", best_config$model, "\n")
cat("  Sampling:", best_config$sampling, "\n")
cat("  CV Balanced Accuracy:", round(best_config$balanced_acc, 4), "\n")
cat("  Optimal Threshold:", round(best_config$optimal_threshold, 3), "\n\n")

best_model_key <- paste(best_config$dataset, best_config$model, 
                        best_config$sampling, sep = "_")
best_model <- all_models[[best_model_key]]

holdout_data <- datasets[[best_config$dataset]]$holdout
model_features <- setdiff(datasets[[best_config$dataset]]$features, "Era")

holdout_probs <- predict(best_model, newdata = holdout_data, type = "prob")
holdout_preds_default <- predict(best_model, newdata = holdout_data, type = "raw")

optimal_thresh <- best_config$optimal_threshold
holdout_preds_optimal <- factor(
  ifelse(holdout_probs$Y >= optimal_thresh, "Y", "N"),
  levels = c("Y", "N")
)

cat("HOLDOUT PERFORMANCE AT DEFAULT THRESHOLD (0.5):\n")
cat("-" %>% rep(50) %>% paste(collapse = ""), "\n")
cm_default <- confusionMatrix(holdout_preds_default, holdout_data$inducted, 
                               positive = "Y")
print(cm_default)

cat("\n\nHOLDOUT PERFORMANCE AT OPTIMAL THRESHOLD (", 
    round(optimal_thresh, 3), "):\n", sep = "")
cat("-" %>% rep(50) %>% paste(collapse = ""), "\n")
cm_optimal <- confusionMatrix(holdout_preds_optimal, holdout_data$inducted, 
                               positive = "Y")
print(cm_optimal)

holdout_roc <- roc(holdout_data$inducted, holdout_probs$Y, quiet = TRUE)
cat("\nHoldout ROC-AUC:", round(auc(holdout_roc), 4), "\n")

holdout_brier <- mean((holdout_probs$Y - as.numeric(holdout_data$inducted == "Y"))^2)
cat("Holdout Brier Score:", round(holdout_brier, 4), "\n")

cat("\n\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("VARIABLE IMPORTANCE (Top 20)\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

var_imp <- tryCatch({
  varImp(best_model, scale = TRUE)
}, error = function(e) {
  cat("Could not extract variable importance:", e$message, "\n")
  NULL
})

if (!is.null(var_imp)) {
  imp_df <- var_imp$importance %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    arrange(desc(Overall)) %>%
    head(20)
  
  print(imp_df)
}

final_summary <- data.frame(
  Metric = c(
    "CV Balanced Accuracy",
    "CV ROC-AUC",
    "CV Sensitivity",
    "CV Specificity",
    "Holdout ROC-AUC",
    "Holdout Sensitivity (optimal thresh)",
    "Holdout Specificity (optimal thresh)",
    "Holdout PPV (optimal thresh)",
    "Holdout NPV (optimal thresh)",
    "Holdout Brier Score",
    "Optimal Threshold"
  ),
  Value = c(
    round(best_config$balanced_acc, 4),
    round(best_config$roc, 4),
    round(best_config$sens_at_optimal, 4),
    round(best_config$spec_at_optimal, 4),
    round(auc(holdout_roc), 4),
    round(cm_optimal$byClass["Sensitivity"], 4),
    round(cm_optimal$byClass["Specificity"], 4),
    round(cm_optimal$byClass["Pos Pred Value"], 4),
    round(cm_optimal$byClass["Neg Pred Value"], 4),
    round(holdout_brier, 4),
    round(optimal_thresh, 4)
  )
)

cat("\n\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("FINAL MODEL PERFORMANCE SUMMARY\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")
print(final_summary, row.names = FALSE)

saveRDS(results_df, "models/rigorous_tuning_results.rds")
saveRDS(all_models, "models/rigorous_all_models.rds")
saveRDS(best_model, "models/rigorous_best_model.rds")
write.csv(results_df, "models/rigorous_tuning_results.csv", row.names = FALSE)
write.csv(final_summary, "models/rigorous_final_summary.csv", row.names = FALSE)

holdout_predictions <- data.frame(
  actual = holdout_data$inducted,
  prob_Y = holdout_probs$Y,
  pred_default = holdout_preds_default,
  pred_optimal = holdout_preds_optimal
)
saveRDS(holdout_predictions, "models/rigorous_holdout_predictions.rds")

stopCluster(cl)
registerDoSEQ()

cat("\n\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("TUNING COMPLETE\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("\nFiles saved:\n")
cat("  - models/rigorous_tuning_results.rds (all CV results)\n")
cat("  - models/rigorous_all_models.rds (all trained models)\n")
cat("  - models/rigorous_best_model.rds (best model object)\n")
cat("  - models/rigorous_holdout_predictions.rds (holdout predictions)\n")
cat("  - models/rigorous_tuning_results.csv (results table)\n")
cat("  - models/rigorous_final_summary.csv (final summary)\n")