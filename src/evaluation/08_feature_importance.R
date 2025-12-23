library(ggplot2)
library(dplyr)

production_model <- readRDS("models/production_models.rds")
final_model <- production_model$data_35$model
final_model_coefs <- coef(final_model)[-1, 1]  
final_model_names <- rownames(coef(final_model))[-1] 

corrected_viz_data <- data.frame(
  feature = final_model_names,
  coefficient = as.numeric(final_model_coefs)
) %>%
  mutate(
    induction_coefficient = -coefficient,  
    effect_type = ifelse(induction_coefficient > 0, "Increases HOF Probability", "Decreases HOF Probability")
  ) %>%
  arrange(desc(abs(induction_coefficient)))

top_positive_effects <- corrected_viz_data[corrected_viz_data$effect_type == "Increases HOF Probability", ][1:10, ]
all_negative_effects <- corrected_viz_data[corrected_viz_data$effect_type == "Decreases HOF Probability", ]

comparison_data <- rbind(top_positive_effects, all_negative_effects) %>%
  mutate(
    feature_short = case_when(
      feature == "career_achievement" ~ "Career Achievement",
      feature == "pos_adj_ops" ~ "Pos-Adj OPS", 
      feature == "award_share" ~ "Award Share",
      feature == "neutralized_H" ~ "Neutralized Hits",
      feature == "pos_difficulty" ~ "Position Difficulty",
      feature == "peak_performance_ratio" ~ "Peak Performance Ratio",
      feature == "PEDSusp" ~ "PED Suspension",
      feature == "PEDMitchell" ~ "Mitchell Report",
      feature == "so_per_ab" ~ "Strikeouts per AB",
      feature == "era_num" ~ "Era Number",
      TRUE ~ feature
    )
  ) %>%
  arrange(desc(induction_coefficient)) %>%
  mutate(importance = row_number()) %>%
  filter(importance <= 10 | importance >= (max(importance) - 5))

p_comparison <- ggplot(comparison_data, aes(x = reorder(feature_short, induction_coefficient), 
                                           y = induction_coefficient, 
                                           fill = effect_type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("Increases HOF Probability" = "#418FDE", 
               "Decreases HOF Probability" = "#C8102E"),
    name = "Effect on HOF Induction"
  ) +
  labs(
    title = "Hall of Fame Induction: Most Influential Features",
    subtitle = "Model coefficients showing strongest positive and negative effects on induction probability",
    x = "Feature",
    y = "Effect Strength on HOF Induction",
    caption = "Based on logistic regression model with 83.4% deviance explained"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    plot.caption = element_text(size = 12, color = "gray60"),
    axis.text.y = element_text(size = 12, face = "bold"),  # Bold feature names
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, color = "gray50") 
 # annotate("text", x = 35, y = 0.6, label = "Increases\nHOF Probability", 
 #          color = "#418FDE", size = 4.5, fontface = "bold") +  # Moved up and right for readability
 # annotate("text", x = 8, y = -0.08, label = "Decreases\nHOF Probability", 
 #          color = "#C8102E", size = 4.5, fontface = "bold")

print(p_comparison)

ggsave(
  "output/feature_importance.png",
  plot = p_comparison,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)

confounding_adjusted_data <- corrected_viz_data %>%
  filter(
    feature %in% c(
      "award_share",           
      "pos_adj_ops",          
      "career_achievement",   
      "era_adj_ops",          
      "PEDSusp",              
      "PEDMitchell",          
      "peak_performance_ratio",
      "pos_difficulty",       
      "efficiency_score",     
      "games_per_year",       
      "awards_per_year"       
    )
  ) %>%
  arrange(desc(abs(induction_coefficient))) %>%
  mutate(
    feature_clean = case_when(
      feature == "award_share" ~ "Award Share",
      feature == "pos_adj_ops" ~ "Position-Adjusted OPS", 
      feature == "career_achievement" ~ "Career Achievement Score",
      feature == "era_adj_ops" ~ "Era-Adjusted OPS",
      feature == "PEDSusp" ~ "PED Suspension",
      feature == "PEDMitchell" ~ "Mitchell Report PED",
      feature == "peak_performance_ratio" ~ "Peak Performance Ratio",
      feature == "pos_difficulty" ~ "Position Difficulty",
      feature == "efficiency_score" ~ "Efficiency Score",
      feature == "games_per_year" ~ "Games per Year",
      feature == "awards_per_year" ~ "Awards per Year",
      TRUE ~ feature
    ),
    effect_type = ifelse(induction_coefficient > 0, "Increases HOF Probability", "Decreases HOF Probability")
  )

print("Confounding-adjusted features:")
print(confounding_adjusted_data[c("feature_clean", "induction_coefficient", "effect_type")])

p_confounding_adjusted <- ggplot(confounding_adjusted_data, 
                                aes(x = reorder(feature_clean, induction_coefficient), 
                                    y = induction_coefficient, 
                                    fill = effect_type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("Increases HOF Probability" = "#418FDE", 
               "Decreases HOF Probability" = "#C8102E"),
    name = "Effect on HOF Induction"
  ) +
  labs(
    title = "Hall of Fame Induction: Independent Feature Effects",
    subtitle = "Key factors adjusted for confounding - focusing on distinct predictive dimensions",
    x = "Feature",
    y = "Effect Strength on HOF Induction",
    caption = "Features selected to minimize multicollinearity and highlight distinct causal pathways"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12, color = "gray60"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, color = "gray50") +
  annotate("text", x = 8, y = 0.6, label = "Increases\nHOF Probability", 
           color = "#418FDE", size = 4.5, fontface = "bold") +
  annotate("text", x = 2, y = -0.05, label = "Decreases\nHOF Probability", 
           color = "#C8102E", size = 4.5, fontface = "bold")

print(p_confounding_adjusted)

non_zero_features_data <- corrected_viz_data %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(induction_coefficient)))

print("Non-zero coefficient features:")
print(non_zero_features_data[c("feature", "induction_coefficient")])

confounding_adjusted_data_v2 <- corrected_viz_data %>%
  filter(
    feature %in% c(
      "award_share",       
      "pos_adj_ops",          
      "career_achievement",  
      "IBB",   
      "SH",      
      "SB",                
      "era_adj_hr", 
      "PEDSusp",         
      "PEDMitchell",      
      "offensive_value",    
      "neutralized_H",    
      "CS"                
    )
  ) %>%
  arrange(desc(abs(induction_coefficient))) %>%
  mutate(
    feature_clean = case_when(
      feature == "award_share" ~ "Award Share",
      feature == "pos_adj_ops" ~ "Position-Adjusted OPS", 
      feature == "career_achievement" ~ "Career Achievement",
      feature == "IBB" ~ "Intentional Walks",
      feature == "SH" ~ "Sacrifice Hits",
      feature == "SB" ~ "Stolen Bases",
      feature == "era_adj_hr" ~ "Era-Adjusted SHs",
      feature == "PEDSusp" ~ "PED Suspension",
      feature == "PEDMitchell" ~ "Mitchell Report PED",
      feature == "offensive_value" ~ "Offensive Value",
      feature == "neutralized_H" ~ "Context-Adjusted Hits",
      feature == "CS" ~ "Caught Stealing",
      TRUE ~ feature
    ),
    effect_type = ifelse(induction_coefficient > 0, "Increases HOF Probability", "Decreases HOF Probability")
  )

p_confounding_adjusted_v2 <- ggplot(confounding_adjusted_data_v2, 
                                   aes(x = reorder(feature_clean, induction_coefficient), 
                                       y = induction_coefficient, 
                                       fill = effect_type)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("Increases HOF Probability" = "#418FDE", 
               "Decreases HOF Probability" = "#C8102E"),
    name = "Effect on HOF Induction"
  ) +
  labs(
    title = "Hall of Fame Induction: Key Independent Factors",
    subtitle = "Distinct predictive dimensions adjusted for multicollinearity and confounding",
    x = "Feature",
    y = "Effect Strength on HOF Induction",
    caption = "Selected features represent unique causal pathways: awards, performance, character, baserunning"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(size = 12, color = "gray60"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.7, color = "gray50") +
  annotate("text", x = 9, y = 0.6, label = "Increases\nHOF Probability", 
           color = "#418FDE", size = 4.5, fontface = "bold") +
  annotate("text", x = 2, y = -0.2, label = "Decreases\nHOF Probability", 
           color = "#C8102E", size = 4.5, fontface = "bold")

print(p_confounding_adjusted_v2)

ggsave(
  "output/feature_importance_confounding.png",
  plot = p_confounding_adjusted_v2,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)


##################################
# Sacrifice Hits Analysis
##################################

all_predictions <- read.csv('data/predictions/all_predictions.csv')

sh_by_era <- all_predictions %>%
  filter(!is.na(era_num) & !is.na(SH)) %>%
  group_by(era_num, Era) %>%
  summarise(
    players = n(),
    avg_sh = mean(SH, na.rm = TRUE),
    median_sh = median(SH, na.rm = TRUE),
    max_sh = max(SH, na.rm = TRUE),
    players_with_sh = sum(SH > 0, na.rm = TRUE),
    pct_with_sh = round(100 * players_with_sh / players, 1),
    .groups = "drop"
  ) %>%
  arrange(era_num)

hof_by_era <- all_predictions %>%
  filter(!is.na(era_num) & !is.na(inducted)) %>%
  group_by(era_num, Era) %>%
  summarise(
    total_players = n(),
    hof_players = sum(inducted == "Y", na.rm = TRUE),
    hof_rate = round(100 * hof_players / total_players, 1),
    avg_sh_hof = mean(SH[inducted == "Y"], na.rm = TRUE),
    avg_sh_non_hof = mean(SH[inducted != "Y"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(era_num)

era_viz_data <- hof_by_era %>%
  mutate(
    sh_advantage = avg_sh_hof - avg_sh_non_hof,
    sh_ratio = avg_sh_hof / avg_sh_non_hof,
    era_name = case_when(
      era_num == 1 ~ "Dead Ball\n(-1919)",
      era_num == 2 ~ "Live Ball\n(1920-1941)", 
      era_num == 3 ~ "Integration\n(1942-1960)",
      era_num == 4 ~ "Expansion\n(1961-1976)",
      era_num == 5 ~ "Free Agency\n(1977-1993)",
      era_num == 6 ~ "Steroid\n(1994-2005)",
      era_num == 7 ~ "Sabermetric\n(2006-)",
      TRUE ~ as.character(Era)
    )
  )

p_sh_era <- ggplot(era_viz_data, aes(x = era_num)) +
  geom_col(data = sh_by_era, aes(y = avg_sh/2), fill = "#E8E8E8", alpha = 0.7, width = 0.6) +
  geom_line(aes(y = sh_advantage), color = "#418FDE", size = 2, group = 1) +
  geom_point(aes(y = sh_advantage), color = "#418FDE", size = 4) +
  scale_x_continuous(breaks = 1:7, labels = era_viz_data$era_name) +
  labs(
    title = "The Declining Importance of Sacrifice Hits in HOF Voting",
    subtitle = "Gray bars: Average sacrifice hits per era | Blue line: HOF advantage in sacrifice hits",
    x = "Baseball Era",
    y = "Sacrifice Hits",
    caption = "HOF players had 182+ more sacrifice hits in Dead Ball era vs. essentially no advantage in Sabermetric era"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 12, color = "gray60"),
    axis.text.x = element_text(size = 12, angle = 0),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  ) +
  annotate("text", x = 1.2, y = 160, label = "Dead Ball Era:\n+182.5 SH advantage", 
           color = "#418FDE", size = 5, fontface = "bold", hjust = -0.25) +
  annotate("text", x = 6.8, y = -10, label = "Modern Era:\nNo SH advantage", 
           color = "#418FDE", size = 5, fontface = "bold", hjust = 1)

print(p_sh_era)

sh_confound_data <- all_predictions %>%
  filter(!is.na(era_num) & !is.na(SH) & !is.na(inducted)) %>%
  group_by(era_num, inducted) %>%
  summarise(
    avg_sh = mean(SH, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    status = ifelse(inducted == "Y", "HOF Players", "Non-HOF Players"),
    era_name = case_when(
      era_num == 1 ~ "Dead Ball",
      era_num == 2 ~ "Live Ball", 
      era_num == 3 ~ "Integration",
      era_num == 4 ~ "Expansion",
      era_num == 5 ~ "Free Agency",
      era_num == 6 ~ "Steroid",
      era_num == 7 ~ "Sabermetric",
      TRUE ~ paste("Era", era_num)
    )
  )

p_confound <- ggplot(sh_confound_data, aes(x = era_num, y = avg_sh, color = status)) +
  geom_line(size = 2, alpha = 0.8) +
  geom_point(size = 4) +
  scale_color_manual(values = c("HOF Players" = "#418FDE", "Non-HOF Players" = "#C8102E")) +
  scale_x_continuous(breaks = 1:7, 
                     labels = c("Dead Ball\n(-1919)", "Live Ball\n(1920-41)", "Integration\n(1942-60)",
                               "Expansion\n(1961-76)", "Free Agency\n(1977-93)", "Steroid\n(1994-05)", "Sabermetric\n(2006-)")) +
  labs(
    title = "The Sacrifice Hits Confounding Effect",
    subtitle = "HOF advantage in sacrifice hits disappears in modern eras - positive model coefficient is historical artifact",
    x = "Baseball Era", 
    y = "Average Career Sacrifice Hits",
    color = "Player Type",
    caption = "Early eras: Large HOF advantage. Modern eras: No advantage. Model trained on all eras shows false positive effect."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 12, color = "gray60"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12), 
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  ) +
  annotate("segment", x = 2, y = 150, xend = 6, yend = 30, 
           arrow = arrow(length = unit(0.3, "cm")), 
           color = "gray50", linetype = "dashed", alpha = 0.7)

print(p_confound)

ggsave(
  "output/sh_confounding.png",
  plot = p_confound,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)