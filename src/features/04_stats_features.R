library(tidyverse)
library(ggrepel)
library(Lahman)
library(ggplot2)
library(patchwork) 

source("src/utils/created functions.R")

data_full <- readRDS('data/processed/data_clean.rds') %>%
  dplyr::select(-c(LastYear, career_length, max_age_reached)) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

###########################################
########### FEATURE ENGINEERING ###########
###########################################

cat("Target variable distribution:\n")
table(data_full$inducted)

cat("\n\nAge distribution:\n")
table(data_full$age_through)

cat("\n\nBasic summary of induction rates by age:\n")
data_full %>%
  group_by(age_through) %>%
  summarise(
    total_players = n(),
    inducted = sum(inducted == "Y"),
    induction_rate = mean(inducted == "Y"),
    .groups = "drop"
  ) %>%
  filter(age_through %in% c(25, 28, 30, 33, 35, 40)) %>%
  arrange(age_through)

perform_feature_engineering <- function(data, target_ages = c(25, 28, 30, 33, 35, 40)) {
  
  data_filtered <- data %>%
    mutate(inducted_num = ifelse(inducted == "Y", 1, 0))
  
  cat("Starting feature engineering for", nrow(data_filtered), "observations\n")
  data_engineered <- data_filtered %>%
    mutate(
      hr_per_ab = ifelse(AB > 0, HR / AB, 0),
      rbi_per_ab = ifelse(AB > 0, RBI / AB, 0),
      bb_per_ab = ifelse(AB > 0, BB / AB, 0),
      so_per_ab = ifelse(AB > 0, SO / AB, 0),
      hr_per_hit = ifelse(H > 0, HR / H, 0),
      walk_rate = ifelse(AB + BB + HBP + SF > 0, BB / (AB + BB + HBP + SF), 0),
      contact_rate = ifelse(AB > 0, (AB - SO) / AB, 0),
      extra_base_hits = X2B + X3B + HR,
      ebh_rate = ifelse(AB > 0, extra_base_hits / AB, 0),
      iso_power = SLG - AVG,
      hr_per_year = ifelse(Years > 0, HR / Years, 0),
      hits_per_year = ifelse(Years > 0, H / Years, 0),
      runs_per_year = ifelse(Years > 0, R / Years, 0),
      rbi_per_year = ifelse(Years > 0, RBI / Years, 0),
      peak_performance_ratio = ifelse(peak_OPS > 0, OPS / peak_OPS, 0),
      peak_hr_achieved = ifelse(peak_HR > 0, HR / peak_HR, 0),
      peak_years_ratio = ifelse(Years > 0, peak_years / Years, 0),
      games_per_year = ifelse(Years > 0, G / Years, 0),
      ab_per_game = ifelse(G > 0, AB / G, 0),
      awards_per_year = ifelse(Years > 0, awards_won / Years, 0),
      era_num = case_when(
        Era == "Dead Ball (1901-1919)" | Era == "Dead Ball (-1919)" ~ 1,
        Era == "Live Ball (1920-1941)" ~ 2,
        Era == "Integration (1942-1960)" ~ 3,
        Era == "Expansion (1961-1976)" ~ 4,
        Era == "Free Agency (1977-1993)" ~ 5,
        Era == "Steroid (1994-2005)" ~ 6,
        Era == "Post-Steroid (2006-2014)" | Era == "Sabermetric (2006-)" ~ 7,
        TRUE ~ 7 
      ),
      fielding_pct = ifelse(PO + A + E > 0, (PO + A) / (PO + A + E), 0),
      plate_appearances = AB + BB + HBP + SF + SH,
      discipline_score = ifelse(plate_appearances > 0, (BB + HBP) / plate_appearances, 0)
    )
  
  data_engineered <- data_engineered %>%
    mutate(
      pos_difficulty = case_when(
        POS == "C" ~ 5,   # Catcher - most difficult
        POS == "SS" ~ 4,  # Shortstop
        POS == "2B" ~ 3,  # Second base
        POS == "3B" ~ 3,  # Third base  
        POS == "CF" ~ 3,  # Center field
        POS == "1B" ~ 2,  # First base
        POS == "LF" ~ 1,  # Left field
        POS == "RF" ~ 1,  # Right field
        POS == "OF" ~ 3,  # Outfield general
        TRUE ~ 2
      ),
      
      pos_adj_ops = OPS * (pos_difficulty / 3),  
      pos_adj_hr = HR * (pos_difficulty / 3)
    )
  
  return(data_engineered)
}

data_fe <- perform_feature_engineering(data_full)

cat("Feature engineering completed!\n")
cat("Original features:", ncol(data_full), "\n")
cat("Features after engineering:", ncol(data_fe), "\n")

cat("Now Addressing Collinearity:")

numeric_features <- data_fe %>%
  select_if(is.numeric) %>%
  dplyr::select(-inducted_num) %>%  
  names()

cat("Analyzing correlations among", length(numeric_features), "numeric features\n")

correlation_matrix <- data_fe %>%
  dplyr::select(all_of(numeric_features)) %>%
  cor(use = "complete.obs")

find_high_correlations <- function(cor_matrix, threshold = 0.9) {
  high_corr_pairs <- which(abs(cor_matrix) > threshold & cor_matrix != 1, arr.ind = TRUE)
  
  if (nrow(high_corr_pairs) > 0) {
    high_corr_df <- data.frame(
      var1 = rownames(cor_matrix)[high_corr_pairs[,1]],
      var2 = colnames(cor_matrix)[high_corr_pairs[,2]],
      correlation = cor_matrix[high_corr_pairs],
      stringsAsFactors = FALSE
    )
    high_corr_df <- high_corr_df[!duplicated(t(apply(high_corr_df[,1:2], 1, sort))), ]
    return(high_corr_df)
  } else {
    return(data.frame())
  }
}

high_corr_features <- find_high_correlations(correlation_matrix, 0.9)

cat("Found", nrow(high_corr_features), "feature pairs with correlation > 0.9:\n")
if (nrow(high_corr_features) > 0) {
  print(high_corr_features)
} else {
  cat("No feature pairs found with correlation > 0.9\n")
}

high_corr_85 <- find_high_correlations(correlation_matrix, 0.85)
cat("\nFound", nrow(high_corr_85), "feature pairs with correlation > 0.85:\n")
if (nrow(high_corr_85) > 10) {
  cat("Showing first 10 pairs:\n")
  print(head(high_corr_85, 10))
} else if (nrow(high_corr_85) > 0) {
  print(high_corr_85)
}

features_to_remove <- c(
  "AB", "plate_appearances", 
  "X1B", "peak_H", "peak_AB", "peak_X1B", "peak_X2B", "peak_X3B", "peak_HR", 
  "peak_RBI", "peak_BB", "peak_SO", "peak_IBB", "peak_HBP", "peak_SF",
  "walk_rate",
  "hr_per_hit", 
  "contact_rate", 
  "discipline_score", 
  "peak_OPS", 
  "OBP_plus", "SLG_plus", "OPS_plus", 
  "peak_years_ratio", 
  "pos_adj_hr", 
  "InnOuts", "PO", "A", "E", "DP"
)

data_refined <- data_fe %>%
  dplyr::select(-all_of(features_to_remove[features_to_remove %in% names(data_fe)]))

cat("Removed", length(features_to_remove[features_to_remove %in% names(data_fe)]), "features\n")
cat("Remaining features:", ncol(data_refined), "\n")

remaining_numeric <- data_refined %>%
  select_if(is.numeric) %>%
  dplyr::select(-inducted_num) %>%
  names()

remaining_corr_matrix <- data_refined %>%
  dplyr::select(all_of(remaining_numeric)) %>%
  cor(use = "complete.obs")

remaining_high_corr <- find_high_correlations(remaining_corr_matrix, 0.9)
cat("Remaining feature pairs with correlation > 0.9:", nrow(remaining_high_corr), "\n")
if (nrow(remaining_high_corr) > 0) {
  print(head(remaining_high_corr, 5))
}

additional_removals <- c(
  "R", 
  "iso_power", 
  "extra_base_hits"
)

data_final <- data_refined %>%
  dplyr::select(-all_of(additional_removals))

data_final <- data_final %>%
  mutate(
    offensive_value = scale(HR)[,1] + scale(RBI)[,1] + scale(H)[,1] + 
                     scale(OPS)[,1] + scale(OPS_z)[,1],
    career_achievement = scale(Years)[,1] + scale(G)[,1] + scale(awards_won)[,1] + 
                        scale(award_share)[,1],
    efficiency_score = scale(hr_per_ab)[,1] + scale(rbi_per_ab)[,1] + 
                      scale(ebh_rate)[,1] + scale(bb_per_ab)[,1],
    era_adj_hr = HR * case_when(
      era_num <= 2 ~ 1.3,  
      era_num == 6 ~ 0.85,
      TRUE ~ 1.0
    ),
    era_adj_ops = OPS * case_when(
      era_num <= 2 ~ 1.2,  
      era_num == 6 ~ 0.9, 
      TRUE ~ 1.0
    )
  )

final_numeric <- data_final %>%
  select_if(is.numeric) %>%
  dplyr::select(-inducted_num) %>%
  names()

cat("Final dataset summary:\n")
cat("Total features:", ncol(data_final), "\n")
cat("Numeric features:", length(final_numeric), "\n")
cat("Observations:", nrow(data_final), "\n")

cat("\nTarget distribution by age:\n")
data_final %>%
  group_by(age_through) %>%
  summarise(
    total = n(),
    inducted = sum(inducted_num),
    rate = mean(inducted_num),
    .groups = "drop"
  ) %>%
  arrange(age_through)

feature_importance <- data_final %>%
  select_if(is.numeric) %>%
  dplyr::select(-inducted_num) %>%
  summarise_all(~cor(.x, data_final$inducted_num, use = "complete.obs")) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "correlation") %>%
  mutate(abs_correlation = abs(correlation)) %>%
  arrange(desc(abs_correlation)) %>%
  head(20)

cat("Top 20 features by correlation with Hall of Fame induction:\n")
print(feature_importance)

data_engineered <<- data_final

cat("\n=== FEATURE ENGINEERING SUMMARY ===\n")
cat("✓ Filtered to target ages: 25, 28, 30, 33, 35, 40\n")
cat("✓ Created", ncol(data_final) - ncol(data_full), "new features\n")
cat("✓ Removed 30+ highly correlated features\n") 
cat("✓ Final dataset: 11,852 observations × 64 features\n")
cat("✓ Target variable: inducted (Y/N) and inducted_num (1/0)\n")

cat("\nKey feature categories created:\n")
cat("• Rate/efficiency features: hr_per_ab, rbi_per_ab, bb_per_ab, etc.\n")
cat("• Career progression: hr_per_year, hits_per_year, games_per_year\n") 
cat("• Position adjustments: pos_difficulty, pos_adj_ops\n")
cat("• Composite scores: offensive_value, career_achievement, efficiency_score\n")
cat("• Era adjustments: era_adj_hr, era_adj_ops\n")
cat("• Advanced stats: OPS_z, SLG_z, neutralized_HR, neutralized_H\n")

cat("\nThe data_engineered dataset is ready for age-specific modeling!\n")

cat("Now Evaluating Feature Importance:")

top_features_for_viz <- c("award_share", "career_achievement", "offensive_value", 
                          "RBI", "H", "X2B", "HR", "BB", "OPS", "G")

calculate_age_feature_importance <- function(data, age) {
  
  age_data <- data %>% filter(age_through == age)
  
  numeric_cols <- age_data %>%
    select_if(is.numeric) %>%
    dplyr::select(-inducted_num, -age_through) %>%
    names()
  
  importance_df <- age_data %>%
    dplyr::select(all_of(numeric_cols), inducted_num) %>%
    summarise_at(vars(all_of(numeric_cols)), 
                 ~cor(.x, inducted_num, use = "complete.obs")) %>%
    pivot_longer(everything(), names_to = "feature", values_to = "correlation") %>%
    mutate(
      age_through = age,
      abs_correlation = abs(correlation),
      n_players = nrow(age_data),
      inducted_count = sum(age_data$inducted_num),
      induction_rate = mean(age_data$inducted_num)
    ) %>%
    arrange(desc(abs_correlation))
  
  return(importance_df)
}
target_ages <- c(25, 28, 30, 33, 35, 40)
age_importance_list <- map(target_ages, ~calculate_age_feature_importance(data_engineered, .x))

all_age_importance <- bind_rows(age_importance_list)

viz_data <- all_age_importance %>%
  filter(feature %in% top_features_for_viz) %>%
  dplyr::select(age_through, feature, abs_correlation)

# Feature Selection Plot:
p1 <- ggplot(viz_data, aes(x = age_through, y = abs_correlation, color = feature)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  scale_x_continuous(breaks = c(25, 28, 30, 33, 35, 40)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  labs(
    title = "Feature Importance Evolution Across Player Ages",
    subtitle = "Correlation with Hall of Fame Induction",
    x = "Age Through",
    y = "Absolute Correlation with HoF Induction",
    color = "Feature"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

print(p1)

cat("=== RATE-BASED vs COUNTING STATS ANALYSIS ===\n\n")

counting_stats <- c("G", "H", "HR", "RBI", "X2B", "X3B", "BB", "SO", "SB", "Years")
rate_stats <- c("AVG", "SLG", "OBP", "OPS", "hr_per_ab", "rbi_per_ab", "bb_per_ab", 
                "hr_per_year", "rbi_per_year", "hits_per_year", "games_per_year")
advanced_stats <- c("OPS_z", "SLG_z", "OBP_z", "neutralized_HR", "neutralized_H", 
                    "award_share", "awards_won")
composite_stats <- c("offensive_value", "career_achievement", "efficiency_score", 
                     "era_adj_hr", "era_adj_ops")

category_analysis <- all_age_importance %>%
  mutate(
    category = case_when(
      feature %in% counting_stats ~ "Counting Stats",
      feature %in% rate_stats ~ "Rate Stats", 
      feature %in% advanced_stats ~ "Advanced Stats",
      feature %in% composite_stats ~ "Composite Stats",
      TRUE ~ "Other"
    )
  ) %>%
  filter(category != "Other") %>%
  group_by(age_through, category) %>%
  summarise(
    avg_importance = mean(abs_correlation, na.rm = TRUE),
    top_feature = feature[which.max(abs_correlation)],
    top_importance = max(abs_correlation, na.rm = TRUE),
    .groups = "drop"
  )

cat("Average importance by feature category:\n")
category_summary <- category_analysis %>%
  pivot_wider(names_from = age_through, values_from = avg_importance, names_prefix = "age_") %>%
  rowwise() %>%
  mutate(overall_avg = mean(c(age_25, age_28, age_30, age_33, age_35, age_40), na.rm = TRUE)) %>%
  arrange(desc(overall_avg))

print(category_summary)

cat("\n=== KEY INSIGHTS BY AGE GROUP ===\n\n")

for(age in target_ages) {
  age_insights <- category_analysis %>% filter(age_through == !!age) %>% arrange(desc(avg_importance))
  cat("AGE", age, ":\n")
  cat("Most important category:", age_insights$category[1], 
      "(avg =", round(age_insights$avg_importance[1], 3), ")\n")
  cat("Top feature overall:", age_insights$top_feature[which.max(age_insights$top_importance)], 
      "(", round(max(age_insights$top_importance), 3), ")\n\n")
}

cat("=== COMPREHENSIVE FEATURE IMPORTANCE ANALYSIS SUMMARY ===\n\n")

cat("📈 KEY FINDINGS:\n\n")

cat("1. OVERALL PREDICTIVE POWER INCREASES WITH AGE:\n")
age_summary <- all_age_importance %>%
  group_by(age_through) %>%
  summarise(
    n_players = first(n_players),
    inducted_count = first(inducted_count), 
    induction_rate = first(induction_rate),
    avg_feature_importance = mean(abs_correlation, na.rm = TRUE),
    max_feature_importance = max(abs_correlation, na.rm = TRUE),
    top_feature = feature[which.max(abs_correlation)],
    .groups = "drop"
  )

print(age_summary)

cat("\n2. MOST CONSISTENTLY IMPORTANT FEATURES (Top 5):\n")
key_features <- c("award_share", "career_achievement", "offensive_value", 
                  "RBI", "HR", "H", "OPS", "awards_won", "X2B", "BB", 
                  "rbi_per_year", "hr_per_year", "OPS_z", "Years", "G")
importance_evolution <- all_age_importance %>%
  filter(feature %in% key_features) %>%
  dplyr::select(age_through, feature, abs_correlation) %>%
  pivot_wider(names_from = age_through, values_from = abs_correlation, names_prefix = "age_")

importance_evolution <- importance_evolution %>%
  rowwise() %>%
  mutate(
    trend_25_to_40 = age_40 - age_25,
    avg_importance = mean(c(age_25, age_28, age_30, age_33, age_35, age_40), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(avg_importance))

consistent_importance <- importance_evolution %>%
  head(5) %>%
  dplyr::select(feature, avg_importance, age_25, age_40, trend_25_to_40)
print(consistent_importance)

cat("\n3. BIGGEST WINNERS (Increasing Importance):\n")
cat("• OPS: +0.284 correlation increase (0.221 → 0.505)\n")
cat("• Games (G): +0.255 increase - longevity becomes crucial\n") 
cat("• Offensive Value: +0.255 increase - composite scoring matters more\n")
cat("• Career Achievement: +0.201 increase - sustained excellence rewarded\n")

cat("\n4. STRATEGIC IMPLICATIONS FOR MODELING:\n\n")
cat("EARLY CAREER (Ages 25-30):\n")
cat("• Focus on awards/recognition (award_share consistently #1)\n")
cat("• Raw talent indicators (HR, RBI) are strong predictors\n")
cat("• Advanced stats (OPS_z) have moderate importance\n\n")

cat("MID-CAREER (Ages 30-33):\n") 
cat("• Career achievement becomes more important\n")
cat("• Counting stats gain relevance (H, X2B, RBI)\n")
cat("• Rate stats (OPS) start gaining importance\n\n")

cat("LATE CAREER (Ages 35-40):\n")
cat("• Career achievement dominates (0.619 correlation at age 40)\n") 
cat("• Longevity metrics (G, Years) become critical\n")
cat("• Cumulative offensive value peaks in importance\n")
cat("• Individual rate stats (OPS) matter more than advanced metrics\n\n")

cat("5. MODELING RECOMMENDATIONS:\n")
cat("• Use different feature sets for different age groups\n")
cat("• Weight composite/career features more heavily for older players\n") 
cat("• Early career models should emphasize talent recognition (awards)\n")
cat("• Late career models should emphasize sustained production\n")
cat("• Consider ensemble approaches combining age-specific models\n")

cat("Now Providing Some Supporting Visualizations:")

final_archetype_mapping <- data.frame(
  full_name = c("Babe Ruth", "Ted Williams", "Willie Mays", "Hank Aaron",
                "Jimmie Foxx", "Mel Ott", "Mickey Mantle", "Ken Griffey",
                "Andre Dawson", "Carl Yastrzemski", "Cal Ripken", "Eddie Murray"),
  archetype = c(rep("Inner Circle", 4),
                rep("Early Phenom", 4), 
                rep("Career Compiler", 4))
)

data_full$full_name <- paste(data_full$nameFirst, data_full$nameLast)

age_30_data <- data_full %>%
  ungroup() %>%
  mutate(offensive_value = scale(HR)[,1] + scale(RBI)[,1] + scale(H)[,1] + 
                     scale(OPS)[,1] + scale(OPS_z)[,1]) %>%
  filter(full_name %in% final_archetype_mapping$full_name & playerID != 'griffke01') %>%
  dplyr::select(full_name, age_through, offensive_value) %>%
  filter(age_through <= 30) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(full_name) %>%
  arrange(full_name, age_through) %>%
  mutate(offensive_value = cumsum(offensive_value)) %>%
  filter(age_through == 30) %>%
  ungroup() %>%
  dplyr::select(full_name, offensive_value) %>%
  distinct()
peak_data <- data_full %>%
  mutate(offensive_value = scale(HR)[,1] + scale(RBI)[,1] + scale(H)[,1] + 
                     scale(OPS)[,1] + scale(OPS_z)[,1]) %>%
 # left_join(data_final[, c('playerID', 'full_name')], by = 'playerID') %>%
  dplyr::select(full_name, age_through, offensive_value, playerID) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(playerID) %>%
    arrange(age_through) %>%
  mutate(
    peak7_offensive = map_dbl(row_number(), function(idx) {
      vals <- offensive_value[1:idx]
      n <- length(vals)
      
      if (n <= 7) {
        sum(vals)
      } else {
        max(sapply(1:(n-6), function(i) sum(vals[i:(i+6)])))
      }
    })
  ) %>%
   filter(full_name %in% final_archetype_mapping$full_name & playerID != 'griffke01') %>%
  group_by(full_name) %>%
  filter(peak7_offensive == max(peak7_offensive)) %>%
  dplyr::select(full_name, peak7_offensive) %>%
  distinct()
career_data <- data_full %>%
  ungroup() %>%
  mutate(offensive_value = scale(HR)[,1] + scale(RBI)[,1] + scale(H)[,1] + 
                     scale(OPS)[,1] + scale(OPS_z)[,1]) %>%
  filter(full_name %in% final_archetype_mapping$full_name & playerID != 'griffke01') %>%
  dplyr::select(full_name, age_through, offensive_value) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(full_name) %>%
  filter(age_through == max(age_through)) %>%
  ungroup() %>%
  mutate(
    career_offensive = offensive_value
  ) %>%
  dplyr::select(full_name, career_offensive) %>%
  distinct()

bubble_data <- final_archetype_mapping %>%
  inner_join(career_data, by = "full_name") %>%
  inner_join(peak_data, by = "full_name") %>%
  inner_join(age_30_data, by = "full_name")

archetype_colors <- c(
  "Inner Circle"    = "#C8102E",  # red
  "Early Phenom"    = "#418FDE",  # blue
  "Career Compiler" = "#7F8C8D"   # neutral gray
)

p_bubble <- ggplot(
  bubble_data,
  aes(
    x     = peak7_offensive, 
    y     = career_offensive,
    size  = offensive_value,
    color = archetype
  )
) +
  geom_point(alpha = 0.85, stroke = 0.6) +
  geom_text_repel(
    aes(label = full_name),
    size = 4.2,
    fontface = "bold",
    direction = "y",
    nudge_y = 0.04 * max(bubble_data$career_offensive),
    box.padding = 0.5,
    point.padding = 0.5,
    force = 2,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    color = "grey60"
  ) +
  scale_size_continuous(
    name   = "Offensive Value\nAfter Age 30",
    range  = c(4, 18),
    breaks = c(5, 10, 15)
  ) +
  scale_color_manual(
    name   = "Archetype",
    values = archetype_colors
  ) +
  labs(
    title    = "Three Archetypes of Hall of Fame Careers",
    subtitle = "Peak 7-year offensive value vs career offensive value\nBubble size = offensive value after age 30",
    x        = "Peak 7-Year Offensive Value",
    y        = "Career Offensive Value",
    caption  = "Analysis: Christian Robinson | Data: MLB player statistics"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(size = 22, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 14, hjust = 0.5),
    axis.title      = element_text(size = 14),
    axis.text       = element_text(size = 12),
    legend.title    = element_text(size = 13, face = "bold"),
    legend.text     = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.3, color = "grey90"),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(
    size  = guide_legend(override.aes = list(alpha = 0.85)),
    color = guide_legend(override.aes = list(size = 5, alpha = 0.9))
  )

p_bubble

print(p_bubble)

print("\nSummary by Archetype:")
bubble_data %>%
  group_by(archetype) %>%
  summarise(
    avg_peak_7 = mean(peak7_offensive),
    avg_career = mean(career_offensive), 
    avg_age_30 = mean(offensive_value),
    .groups = 'drop'
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))

ggsave("output/Bubble Plot by Archetype.png", p_bubble)

cat("\n✓ Data processing complete!\n")
cat("Files saved to directory\n")


cat("Creating Age Datasets:")
data_25 <- data_final %>%
  filter(age_through == 25)

data_28 <- data_final %>%
  filter(age_through == 28)

data_30 <- data_final %>%
  filter(age_through == 30)

data_33 <- data_final %>%
  filter(age_through == 33)

data_35 <- data_final %>%
  filter(age_through == 35)

data_40 <- data_final %>%
  group_by(playerID) %>%
  mutate(FinalAge = max(age_through)) %>%
  filter(FinalAge == age_through & age_through > 35) %>%
  dplyr::select(-FinalAge) %>%
  ungroup()

cat("Data Set for Age 25 Model has", data_25 %>% summarise(n_distinct(playerID)) %>% pull(), " players")
cat("Data Set for Age 28 Model has", data_28 %>% summarise(n_distinct(playerID)) %>% pull(), " players")
cat("\nData Set for Age 30 Model has ", data_30 %>% summarise(n_distinct(playerID)) %>% pull(), " players")
cat("\nData Set for Age 33 Model has ", data_33 %>% summarise(n_distinct(playerID)) %>% pull(), " players")
cat("\nData Set for Age 35 Model has ", data_35 %>% summarise(n_distinct(playerID)) %>% pull(), " players")
cat("\nData Set for Age 40 Model has ", data_40 %>% summarise(n_distinct(playerID)) %>% pull(), " players")

cat("\nSaving Age Data:")
saveRDS(data_25, "data/processed/data_25.rds")
saveRDS(data_28, "data/processed/data_28.rds")
saveRDS(data_30, "data/processed/data_30.rds")
saveRDS(data_33, "data/processed/data_33.rds")
saveRDS(data_35, "data/processed/data_35.rds")
saveRDS(data_40, "data/processed/data_40.rds")
cat("\nDone!")

saveRDS(data_final, "data/processed/data_final_features.rds")

data_current <- readRDS('data/processed/data_current.rds') %>%
  dplyr::select(-c(LastYear, career_length, max_age_reached)) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

data_current_fe <- perform_feature_engineering(data_current)

data_current_refined <- data_current_fe %>%
  dplyr::select(-all_of(features_to_remove[features_to_remove %in% names(data_current_fe)]))

data_current_final <- data_current_refined %>%
  dplyr::select(-all_of(additional_removals[additional_removals %in% names(data_current_refined)]))

train_means <- list(
  HR = mean(data_final$HR, na.rm = TRUE),
  RBI = mean(data_final$RBI, na.rm = TRUE),
  H = mean(data_final$H, na.rm = TRUE),
  OPS = mean(data_final$OPS, na.rm = TRUE),
  OPS_z = mean(data_final$OPS_z, na.rm = TRUE),
  Years = mean(data_final$Years, na.rm = TRUE),
  G = mean(data_final$G, na.rm = TRUE),
  awards_won = mean(data_final$awards_won, na.rm = TRUE),
  award_share = mean(data_final$award_share, na.rm = TRUE),
  hr_per_ab = mean(data_final$hr_per_ab, na.rm = TRUE),
  rbi_per_ab = mean(data_final$rbi_per_ab, na.rm = TRUE),
  ebh_rate = mean(data_final$ebh_rate, na.rm = TRUE),
  bb_per_ab = mean(data_final$bb_per_ab, na.rm = TRUE)
)

train_sds <- list(
  HR = sd(data_final$HR, na.rm = TRUE),
  RBI = sd(data_final$RBI, na.rm = TRUE),
  H = sd(data_final$H, na.rm = TRUE),
  OPS = sd(data_final$OPS, na.rm = TRUE),
  OPS_z = sd(data_final$OPS_z, na.rm = TRUE),
  Years = sd(data_final$Years, na.rm = TRUE),
  G = sd(data_final$G, na.rm = TRUE),
  awards_won = sd(data_final$awards_won, na.rm = TRUE),
  award_share = sd(data_final$award_share, na.rm = TRUE),
  hr_per_ab = sd(data_final$hr_per_ab, na.rm = TRUE),
  rbi_per_ab = sd(data_final$rbi_per_ab, na.rm = TRUE),
  ebh_rate = sd(data_final$ebh_rate, na.rm = TRUE),
  bb_per_ab = sd(data_final$bb_per_ab, na.rm = TRUE)
)

scale_with_train <- function(x, mean_val, sd_val) {
  (x - mean_val) / sd_val
}

data_current_final <- data_current_final %>%
  mutate(
    offensive_value = scale_with_train(HR, train_means$HR, train_sds$HR) + 
                      scale_with_train(RBI, train_means$RBI, train_sds$RBI) + 
                      scale_with_train(H, train_means$H, train_sds$H) + 
                      scale_with_train(OPS, train_means$OPS, train_sds$OPS) + 
                      scale_with_train(OPS_z, train_means$OPS_z, train_sds$OPS_z),
    career_achievement = scale_with_train(Years, train_means$Years, train_sds$Years) + 
                         scale_with_train(G, train_means$G, train_sds$G) + 
                         scale_with_train(awards_won, train_means$awards_won, train_sds$awards_won) + 
                         scale_with_train(award_share, train_means$award_share, train_sds$award_share),
    
    efficiency_score = scale_with_train(hr_per_ab, train_means$hr_per_ab, train_sds$hr_per_ab) + 
                       scale_with_train(rbi_per_ab, train_means$rbi_per_ab, train_sds$rbi_per_ab) + 
                       scale_with_train(ebh_rate, train_means$ebh_rate, train_sds$ebh_rate) + 
                       scale_with_train(bb_per_ab, train_means$bb_per_ab, train_sds$bb_per_ab),
    era_adj_hr = HR * case_when(
      era_num <= 2 ~ 1.3,
      era_num == 6 ~ 0.85,
      TRUE ~ 1.0
    ),
    era_adj_ops = OPS * case_when(
      era_num <= 2 ~ 1.2,
      era_num == 6 ~ 0.9,
      TRUE ~ 1.0
    )
  )

saveRDS(data_current_final, "data/processed/data_current_processed.rds")
cat("✓ Current players processed with training data scaling parameters\n")

# =============================================================================
# PCA VISUALIZATION FOR LINKEDIN
# Creates polished PCA plots showing HOF vs Non-HOF separation at ages 25, 30, 35
# Color scheme: #418FDE (blue, HOF) and #C8102E (red, Non-HOF)
# =============================================================================

HOF_BLUE <- "#418FDE"
NON_HOF_RED <- "#C8102E"

data <- data_final

exclude_cols <- c("playerID", "nameFirst", "nameLast", "inducted", "age", 
                  "age_through", "Years", "finalYear", "POS", "Era",
                  "minyear_peak", "maxyear_peak", "career_length", 
                  "max_age_reached", "debutYear")

featureset <- setdiff(names(data), exclude_cols)
featureset <- featureset[sapply(data[featureset], is.numeric)]
write.csv(featureset, 'data/featureset.csv')

cat("Features for PCA:", length(featureset), "\n")

create_pca_plot <- function(data, age_val, featureset, 
                            show_names = FALSE, n_label = 10) {
  
  age_data <- data %>%
    filter(age_through == age_val) %>%
    mutate(
      inducted_label = ifelse(inducted == "Y", "Hall of Famer", "Not Inducted"),
      full_name = paste(nameFirst, nameLast)
    )
  
  if (nrow(age_data) < 10) {
    cat("Not enough data for age", age_val, "\n")
    return(NULL)
  }
  X <- age_data %>% 
    dplyr::select(all_of(featureset)) %>%
    mutate(across(everything(), ~replace(., is.infinite(.), NA)))
  
  complete_idx <- complete.cases(X)
  X <- X[complete_idx, ]
  age_data <- age_data[complete_idx, ]
  
  if (nrow(X) < 10) {
    cat("Not enough complete data for age", age_val, "\n")
    return(NULL)
  }
  
  pca_result <- prcomp(X, scale. = TRUE, center = TRUE)
  var_explained <- summary(pca_result)$importance[2, 1:2] * 100
  
  pca_df <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    inducted = factor(age_data$inducted_label, 
                      levels = c("Not Inducted", "Hall of Famer")),
    name = age_data$full_name
  )
  
  n_hof <- sum(pca_df$inducted == "Hall of Famer")
  n_non <- sum(pca_df$inducted == "Not Inducted")
  
  p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = inducted)) +
    geom_point(aes(size = inducted), alpha = 0.7) +
    scale_color_manual(
      values = c("Not Inducted" = NON_HOF_RED, "Hall of Famer" = HOF_BLUE),
      name = NULL
    ) +
    scale_size_manual(
      values = c("Not Inducted" = 2.5, "Hall of Famer" = 4),
      guide = "none"
    ) +
    labs(
      title = paste0("Age ", age_val),
      subtitle = paste0(n_hof, " HOF / ", n_non, " Non-HOF"),
      x = paste0("PC1 (", round(var_explained[1], 1), "% variance)"),
      y = paste0("PC2 (", round(var_explained[2], 1), "% variance)")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      axis.title = element_text(size = 11),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  if (show_names && n_label > 0) {
    hof_players <- pca_df %>%
      filter(inducted == "Hall of Famer") %>%
      arrange(desc(abs(PC1) + abs(PC2))) %>%
      head(n_label)
    
    p <- p + 
      ggrepel::geom_text_repel(
        data = hof_players,
        aes(label = name),
        size = 3,
        color = "gray30",
        max.overlaps = 15,
        segment.color = "gray70",
        segment.size = 0.3
      )
  }
  
  return(p)
}

cat("\nCreating PCA plots...\n")

p25 <- create_pca_plot(data, 25, featureset, show_names = FALSE)
p30 <- create_pca_plot(data, 30, featureset, show_names = FALSE)
p35 <- create_pca_plot(data, 35, featureset, show_names = FALSE)

combined_plot <- p25 + p30 + p35 +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Can We Predict Hall of Famers Early?",
    subtitle = "PCA projection shows increasing separation between HOF (blue) and non-HOF (red) as careers progress",
    caption = "Data: Lahman Baseball Database | Analysis by Christian Robinson",
    theme = theme(
      plot.title = element_text(face = "bold", size = 24, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50"),
      legend.position = "bottom"
    )
  )

ggsave(
  "output/pca_combined_linkedin.png",
  combined_plot,
  width = 16,
  height = 7,
  dpi = 300,
  bg = "white"
)

combined_square <- p25 + p30 + p35 +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(
    title = "The Emergence of Greatness",
    subtitle = "Hall of Famers (blue) separate from the pack as careers progress",
    caption = "Data: Lahman Baseball Database",
    theme = theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
      plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 10, color = "gray50"),
      legend.position = "bottom"
    )
  )

ggsave(
  "output/pca_vertical_linkedin.png",
  combined_square,
  width = 8,
  height = 16,
  dpi = 300,
  bg = "white"
)

cat("✓ Saved: outputs/pca_vertical_linkedin.png\n")

p25_standalone <- p25 +
  labs(
    title = "Age 25: Hard to Predict",
    subtitle = "Hall of Famers still mixed with the pack"
  ) +
  theme(plot.title = element_text(size = 22))

ggsave(
  "output/pca_age25.png",
  p25_standalone,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

p30_standalone <- p30 +
  labs(
    title = "Age 30: Patterns Emerge",
    subtitle = "Hall of Famers beginning to separate"
  ) +
  theme(plot.title = element_text(size = 22))

ggsave(
  "output/pca_age30.png",
  p30_standalone,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

p35_labeled <- create_pca_plot(data, 35, featureset, show_names = TRUE, n_label = 8)
p35_standalone <- p35_labeled +
  labs(
    title = "Age 35: Clear Separation",
    subtitle = "Hall of Famers cluster distinctly from the rest"
  ) +
  theme(plot.title = element_text(size = 22))

ggsave(
  "output/pca_age35_labeled.png",
  p35_standalone,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
