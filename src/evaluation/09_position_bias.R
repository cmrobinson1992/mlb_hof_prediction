#####################################
# CHI-SQUARE TEST
#####################################
data_final <- readRDS("data/processed/data_final_features.rds")

position_summary <- data_final %>%
  group_by(POS) %>%
  summarise(
    total_players = n_distinct(playerID),
    hof_players = n_distinct(playerID[inducted == "Y"]),
    total_seasons = n(),
    .groups = "drop"
  ) %>%
  mutate(
    hof_rate = hof_players / total_players,
    pct_of_players = total_players / sum(total_players) * 100
  ) %>%
  arrange(desc(total_players))

print("Position Summary:")
print(position_summary)

contingency_data <- data_final %>%
  distinct(playerID, .keep_all = TRUE) %>%  
  dplyr::select(playerID, POS, inducted) %>%
  count(POS, inducted) %>%
  pivot_wider(names_from = inducted, values_from = n, values_fill = 0)

names(data)[grepl("induct", names(data), ignore.case = TRUE)]

contingency_data <- data_final %>%
  distinct(playerID, .keep_all = TRUE) %>%  
  count(POS, inducted) %>%
  pivot_wider(names_from = inducted, values_from = n, values_fill = 0) %>%
  rename(Non_HOF = N, HOF = Y)

print("Contingency Table (Players by Position and HOF Status):")
print(contingency_data)

contingency_matrix <- contingency_data %>%
  column_to_rownames("POS") %>%
  as.matrix()

print("\nContingency Matrix:")
print(contingency_matrix)

position_totals <- rowSums(contingency_matrix)
hof_totals <- colSums(contingency_matrix)
grand_total <- sum(contingency_matrix)

print("\nSample Size Summary:")
print("Total unique players: {grand_total}")
print("Hall of Fame players: {hof_totals['HOF']}")
print("Non-Hall of Fame players: {hof_totals['Non_HOF']}")
print("Overall HOF rate: {round(hof_totals['HOF']/grand_total * 100, 2)}%")


position_totals <- rowSums(contingency_matrix)
hof_totals <- colSums(contingency_matrix)
grand_total <- sum(contingency_matrix)

cat("\nSample Size Summary:\n")
cat(paste("Total unique players:", grand_total, "\n"))
cat(paste("Hall of Fame players:", hof_totals['HOF'], "\n"))
cat(paste("Non-Hall of Fame players:", hof_totals['Non_HOF'], "\n"))
cat(paste("Overall HOF rate:", round(hof_totals['HOF']/grand_total * 100, 2), "%\n"))

chi_test <- chisq.test(contingency_matrix)
print("\n=== CHI-SQUARED TEST RESULTS ===")
print(chi_test)

position_analysis <- data.frame(
  Position = rownames(contingency_matrix),
  HOF_Count = contingency_matrix[, "HOF"],
  Total_Players = position_totals,
  HOF_Rate = round(contingency_matrix[, "HOF"] / position_totals * 100, 2),
  Expected_HOF = round(chi_test$expected[, "HOF"], 2),
  Observed_minus_Expected = round(contingency_matrix[, "HOF"] - chi_test$expected[, "HOF"], 2),
  Standardized_Residual = round(chi_test$stdres[, "HOF"], 2)
)

print("\n=== POSITION-SPECIFIC ANALYSIS ===")
print(position_analysis)

cat("\n=== PROFESSIONAL STATISTICAL ASSESSMENT ===\n")
cat(paste("Chi-squared statistic:", round(chi_test$statistic, 4), "\n"))
cat(paste("Degrees of freedom:", chi_test$parameter, "\n"))
cat(paste("P-value:", format(chi_test$p.value, scientific = TRUE, digits = 4), "\n"))
cat(paste("Effect size (Cramér's V):", round(sqrt(chi_test$statistic / (grand_total * (min(dim(contingency_matrix)) - 1))), 4), "\n"))

cat("=== ADDITIONAL STATISTICAL ANALYSES ===\n\n")

cat("1. POST-HOC PAIRWISE COMPARISONS (Bonferroni corrected):\n")
positions <- rownames(contingency_matrix)
n_comparisons <- choose(length(positions), 2)
alpha_adjusted <- 0.05 / n_comparisons

cat(paste("Number of pairwise comparisons:", n_comparisons, "\n"))
cat(paste("Bonferroni-adjusted alpha level:", round(alpha_adjusted, 6), "\n\n"))

pairwise_comparisons <- NULL
for(i in 1:(length(positions)-1)) {
  for(j in (i+1):length(positions)) {
    pos1 <- positions[i]
    pos2 <- positions[j]
    
    table_2x2 <- matrix(c(
      contingency_matrix[pos1, "HOF"], contingency_matrix[pos1, "Non_HOF"],
      contingency_matrix[pos2, "HOF"], contingency_matrix[pos2, "Non_HOF"]
    ), nrow = 2, byrow = TRUE)
    
    fisher_result <- fisher.test(table_2x2)
    
    pairwise_comparisons <- rbind(pairwise_comparisons, data.frame(
      Comparison = paste(pos1, "vs", pos2),
      P_value = round(fisher_result$p.value, 6),
      Significant_Bonferroni = fisher_result$p.value < alpha_adjusted,
      Odds_Ratio = round(fisher_result$estimate, 3),
      OR_CI_Lower = round(fisher_result$conf.int[1], 3),
      OR_CI_Upper = round(fisher_result$conf.int[2], 3)
    ))
  }
}

print(pairwise_comparisons[order(pairwise_comparisons$P_value), ])

cat("\n2. 95% CONFIDENCE INTERVALS FOR HOF RATES BY POSITION:\n")
ci_results <- data.frame(
  Position = rownames(contingency_matrix),
  HOF_Count = contingency_matrix[, "HOF"],
  Total_Players = position_totals,
  HOF_Rate_Percent = round(contingency_matrix[, "HOF"] / position_totals * 100, 2)
)

for(i in 1:nrow(ci_results)) {
  pos <- ci_results$Position[i]
  binom_test <- binom.test(contingency_matrix[pos, "HOF"], position_totals[pos])
  ci_results$CI_Lower[i] <- round(binom_test$conf.int[1] * 100, 2)
  ci_results$CI_Upper[i] <- round(binom_test$conf.int[2] * 100, 2)
}

ci_results$CI_Width <- ci_results$CI_Upper - ci_results$CI_Lower
ci_results <- ci_results[order(-ci_results$HOF_Rate_Percent), ]

print(ci_results)

cat("\n3. EFFECT SIZE INTERPRETATION:\n")
cramers_v <- sqrt(chi_test$statistic / (grand_total * (min(dim(contingency_matrix)) - 1)))
cat(paste("Cramér's V =", round(cramers_v, 4), "\n"))

if(cramers_v < 0.1) {
  effect_interpretation <- "negligible effect size"
} else if(cramers_v < 0.3) {
  effect_interpretation <- "small effect size"
} else if(cramers_v < 0.5) {
  effect_interpretation <- "medium effect size"
} else {
  effect_interpretation <- "large effect size"
}

cat(paste("Interpretation:", effect_interpretation, "\n"))

#####################################
# COUNTERFACTUALS
#####################################
production_models <- readRDS("models/production_models.rds")

model_obj <- production_models$data_35
features <- model_obj$features

create_corrected_counterfactuals <- function(data, models, target_age = 35, n_players = 6) {
  
  cat("=== CORRECTED COUNTERFACTUAL ANALYSIS (Age", target_age, ") ===\n")
  
  model_name <- paste0("data_", target_age)
  model_obj <- models[[model_name]]
  model <- model_obj$model
  features <- model_obj$features
  
  age_data <- data %>% 
    filter(age_through == target_age, !is.na(POS))
  
  set.seed(1992)
  age_data <- age_data %>% 
    slice_sample(n = n_players)
 
  cat("Analyzing", nrow(age_data), "players\n\n")
  
  positions <- c("1B", "2B", "3B", "SS", "OF", "C")
  
  results <- list()
  
  for (i in 1:nrow(age_data)) {
    player <- age_data[i, ]
    original_pos <- as.character(player$POS)
    
    cat("Processing:", player$nameFirst, player$nameLast, "(", original_pos, ")\n")
    
    for (new_pos in positions) {
      cf_player <- player
      
      cf_player$pos_difficulty <- case_when(
        new_pos == "C" ~ 5,    
        new_pos == "SS" ~ 4,     
        new_pos == "3B" ~ 3,    
        new_pos == "2B" ~ 2,     
        new_pos == "1B" ~ 1,     
        new_pos == "OF" ~ 3,     
        TRUE ~ 3
      )
      
      pos_bonus <- case_when(
        new_pos == "C" ~ 0.050,  
        new_pos == "SS" ~ 0.030, 
        new_pos == "2B" ~ 0.020,  
        new_pos == "3B" ~ 0.010,  
        new_pos == "1B" ~ -0.020,
        new_pos == "OF" ~ 0.000, 
        TRUE ~ 0.000
      )
      cf_player$pos_adj_ops <- player$pos_adj_ops + pos_bonus
      
      X <- cf_player[, features, drop = FALSE]
      
      for (feat in features) {
        if (feat %in% names(X)) {
          if (is.factor(X[[feat]])) {
            X[[feat]] <- as.numeric(as.character(X[[feat]]))
          } else if (is.character(X[[feat]])) {
            X[[feat]] <- as.numeric(X[[feat]])
          } else if (is.logical(X[[feat]])) {
            X[[feat]] <- as.numeric(X[[feat]])
          }
        }
      }
      
      X[is.na(X)] <- 0
      
      X_scaled <- predict(model_obj$preprocess, X)
      
      X_matrix <- as.matrix(X_scaled)
      storage.mode(X_matrix) <- "numeric"
      
      raw_prob <- predict(model_obj$model, 
                          newx = X_matrix, 
                          s = model_obj$config$lambda, 
                          type = "response")[1,1]
      
      correct_prob <- 1 - raw_prob  # THE KEY FIX!
      
      results[[length(results) + 1]] <- data.frame(
        player = paste(player$nameFirst, player$nameLast),
        playerID = player$playerID,
        age = target_age,
        original_position = original_pos,
        counterfactual_position = new_pos,
        hof_probability = round(correct_prob, 4),
        is_inducted = as.character(player$inducted),
        is_counterfactual = (new_pos != original_pos),
        stringsAsFactors = FALSE
      )
    }
  }
  
  result_df <- do.call(rbind, results)
  
  result_df <- result_df %>%
    group_by(playerID) %>%
    mutate(
      original_prob = hof_probability[original_position == counterfactual_position],
      prob_change = hof_probability - original_prob,
      prob_change_pct = round(100 * prob_change / original_prob, 1)
    ) %>%
    ungroup()
  
  return(result_df)
}

expanded_counterfactuals <- create_corrected_counterfactuals(data_final, production_models, 35, n_players = 50)
player_list <- expanded_counterfactuals %>%
  filter(!is_counterfactual) %>%
  dplyr::select(player, original_position, hof_probability, is_inducted) %>%
  arrange(desc(hof_probability))

print(player_list, n = 50)

generate_counterfactuals_any_size <- function(n_players = 100) {
  cat("Generating counterfactuals for", n_players, "players...\n")
  
  # Get available players
  available_players <- data_final %>% 
    filter(age_through == 35, !is.na(POS)) %>%
    nrow()
  
  actual_n <- min(n_players, available_players)
  cat("Analyzing", actual_n, "out of", available_players, "available players\n")
  
  cf_result <- create_corrected_counterfactuals(data_final, production_models, 35, actual_n)
  
  cat("Result: ", nrow(cf_result), "rows (", n_distinct(cf_result$playerID), "players × 6 positions)\n")
  
  return(cf_result)
}

cat("=== COUNTERFACTUAL ANALYSIS OPTIONS ===\n\n")

cat("OPTION 1: Medium sample (100 players = 600 rows)\n")
counterfactuals_100 <- generate_counterfactuals_any_size(100)

cat("\nOPTION 2: Large sample (200 players = 1200 rows)\n")
counterfactuals_200 <- generate_counterfactuals_any_size(600)

cat("\nOPTION 3: Full dataset (all 1232 players = 7392 rows)\n")
cat("This would take a few minutes. Do you want the full analysis?\n")

cat("\n=== WORKING WITH 200-PLAYER SAMPLE ===\n")
cat("Dimensions:", dim(counterfactuals_200), "\n")

hof_players_200 <- counterfactuals_200 %>%
  filter(is_counterfactual == FALSE, is_inducted == "Y") %>%
  arrange(desc(hof_probability))

cat("Hall of Fame players in 200-player sample:\n")
print(hof_players_200[, c("player", "original_position", "hof_probability")], n = 15)

borderline_200 <- counterfactuals_200 %>%
  filter(is_counterfactual == FALSE, hof_probability >= 0.4, hof_probability <= 0.8) %>%
  arrange(desc(hof_probability))

cat("\nBorderline candidates (40-80% probability) in sample:\n")
print(borderline_200[, c("player", "original_position", "hof_probability", "is_inducted")])

cat("\nThis larger sample gives you much more comprehensive counterfactual analysis!\n")
cat("The dataframe 'counterfactuals_200' now contains", nrow(counterfactuals_200), "rows.\n")

chart_data <- counterfactuals_200 %>% 
  dplyr::filter(is_counterfactual == TRUE, !is.na(prob_change))

p_prob_change <- ggplot(data = chart_data) +
  geom_point(aes(x = original_prob, y = prob_change, color = counterfactual_position),
             alpha = 0.7, size = 4) +
  geom_hline(yintercept = 0, color = "#a0a0a0", linetype = "dashed") +
  scale_color_manual(values = c("1B" = "#418FDE", "2B" = "#7CB9E8", 
                               "3B" = "#FFD700", "SS" = "#FF6B6B", 
                               "OF" = "#a0a0a0", "C" = "#C8102E")) +
  labs(title = "Counterfactual Analysis: Position Changes & HOF Probability",
       x = "Original HOF Probability", 
       y = "Change in HOF Probability",
       color = "Counterfactual Position" ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12), 
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )

print(p_prob_change)

ggsave(
  "output/position_prob_change.png",
  plot = p_prob_change,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)