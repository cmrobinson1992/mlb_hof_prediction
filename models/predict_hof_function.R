predict_hof <-
function(player_data, age, models = NULL) {
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
