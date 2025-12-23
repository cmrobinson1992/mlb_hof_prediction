library(Lahman)
library(tidyverse)
library(broom)
library(scales)

cat("=== Team Bias Analysis for Hall of Fame ===\n\n")

#==============================================================================
# 1. LOAD AND PREPARE BASE DATA
#==============================================================================

hof_raw <- HallOfFame %>%
  filter(category == "Player", inducted == "Y") %>%
  filter(!(needed_note == 'Preliminary poll') | is.na(needed_note)) %>%
  distinct(playerID) %>%
  mutate(inducted = 1)

batting_post <- BattingPost

names_df <- People %>%
  dplyr::select(playerID, nameFirst, nameLast, birthYear)

pitcher_ids <- Fielding %>%
  group_by(playerID, POS) %>%
  summarise(games_at_pos = sum(G, na.rm = TRUE), .groups = "drop") %>%
  group_by(playerID) %>%
  slice_max(games_at_pos, n = 1, with_ties = FALSE) %>%
  filter(POS == "P") %>%
  pull(playerID)

cat("Pitchers identified:", length(pitcher_ids), "\n")
cat("These will be excluded from analysis.\n\n")

hof <- hof_raw %>%
  filter(!playerID %in% pitcher_ids)

cat("Position player HOFers:", nrow(hof), "\n")
cat("(Pitcher HOFers excluded:", nrow(hof_raw) - nrow(hof), ")\n\n")

player_teams <- Batting %>%
  filter(!playerID %in% pitcher_ids) %>%  # EXCLUDE PITCHERS
  group_by(playerID, teamID) %>%
  summarise(
    games = sum(G, na.rm = TRUE),
    years = n_distinct(yearID),
    first_year = min(yearID),
    last_year = max(yearID),
    .groups = "drop"
  ) %>%
  group_by(playerID) %>%
  mutate(
    total_games = sum(games),
    pct_career = games / total_games
  ) %>%
  # Primary team = most games played
  slice_max(games, n = 1, with_ties = FALSE) %>%
  ungroup()

cat("Position players with team affiliations:", n_distinct(player_teams$playerID), "\n")

# Historical market size classifications
# Based on metropolitan statistical area populations (approximate, historical average)
# Large: Top 10 markets
# Medium: 11-20
# Small: 21+

market_size <- tribble(
  ~teamID, ~franchise, ~market_size, ~market_rank, ~metro_pop_millions,
  "NYA", "New York Yankees", "Large", 1, 20.0,
  "NYN", "New York Mets", "Large", 1, 20.0,
  "NY1", "New York Giants", "Large", 1, 20.0,
  "BRO", "Brooklyn Dodgers", "Large", 1, 20.0,
  "LAN", "Los Angeles Dodgers", "Large", 2, 13.0,
  "LAA", "Los Angeles Angels", "Large", 2, 13.0,
  "CAL", "California Angels", "Large", 2, 13.0,
  "ANA", "Anaheim Angels", "Large", 2, 13.0,
  "CHN", "Chicago Cubs", "Large", 3, 9.5,
  "CHA", "Chicago White Sox", "Large", 3, 9.5,
  "CHI", "Chicago (historical)", "Large", 3, 9.5,
  "PHI", "Philadelphia Phillies", "Large", 4, 6.0,
  "PHA", "Philadelphia Athletics", "Large", 4, 6.0,
  "WAS", "Washington Senators", "Large", 5, 6.0,
  "WSA", "Washington Senators", "Large", 5, 6.0,
  "WSN", "Washington Nationals", "Large", 5, 6.0,
  "MON", "Montreal Expos", "Medium", 15, 4.0,  
  "BOS", "Boston Red Sox", "Large", 6, 4.8,
  "BSN", "Boston Braves", "Large", 6, 4.8,
  "SFN", "San Francisco Giants", "Large", 7, 4.7,
  "ATL", "Atlanta Braves", "Large", 8, 5.8,
  "HOU", "Houston Astros", "Large", 9, 6.5,
  "TEX", "Texas Rangers", "Large", 10, 7.0,
  
  "DET", "Detroit Tigers", "Medium", 11, 4.3,
  "MIN", "Minnesota Twins", "Medium", 12, 3.5,
  "SEA", "Seattle Mariners", "Medium", 13, 3.8,
  "SLN", "St. Louis Cardinals", "Medium", 14, 2.8,
  "SLA", "St. Louis Browns", "Medium", 14, 2.8,
  "BAL", "Baltimore Orioles", "Medium", 15, 2.8,
  "SDN", "San Diego Padres", "Medium", 16, 3.3,
  "COL", "Colorado Rockies", "Medium", 17, 2.9,
  "ARI", "Arizona Diamondbacks", "Medium", 18, 4.5,
  "TBA", "Tampa Bay Rays", "Medium", 19, 3.0,
  "CLE", "Cleveland Indians/Guardians", "Medium", 20, 2.0,
  "CL4", "Cleveland (historical)", "Medium", 20, 2.0,
  
  "OAK", "Oakland Athletics", "Small", 21, 2.5,  
  "CIN", "Cincinnati Reds", "Small", 22, 2.1,
  "PIT", "Pittsburgh Pirates", "Small", 23, 2.3,
  "MIL", "Milwaukee Brewers", "Small", 24, 1.6,
  "MLA", "Milwaukee Braves", "Small", 24, 1.6,
  "ML4", "Milwaukee (historical)", "Small", 24, 1.6,
  "KCA", "Kansas City Royals", "Small", 25, 2.1,
  "KC1", "Kansas City Athletics", "Small", 25, 2.1,
  "MIA", "Miami Marlins", "Medium", 19, 6.0,  
  "FLO", "Florida Marlins", "Medium", 19, 6.0
)

all_teams <- unique(c(Batting$teamID, Teams$teamID))
missing_teams <- setdiff(all_teams, market_size$teamID)

if (length(missing_teams) > 0) {
  cat("Teams without market classification:", length(missing_teams), "\n")
  market_size <- bind_rows(
    market_size,
    tibble(
      teamID = missing_teams,
      franchise = "Unknown",
      market_size = "Medium",
      market_rank = 15,
      metro_pop_millions = 3.0
    )
  )
}

team_success <- Teams %>%
  group_by(teamID) %>%
  summarise(
    years_active = n(),
    total_wins = sum(W, na.rm = TRUE),
    total_losses = sum(L, na.rm = TRUE),
    win_pct = total_wins / (total_wins + total_losses),
    pennants = sum(LgWin == "Y", na.rm = TRUE),
    world_series = sum(WSWin == "Y", na.rm = TRUE),
    division_titles = sum(DivWin == "Y", na.rm = TRUE),
    playoff_appearances = sum(WCWin == "Y" | DivWin == "Y" | LgWin == "Y", na.rm = TRUE),
    .groups = "drop"
  )

team_success <- team_success %>%
  mutate(
    pennants_per_100 = (pennants / years_active) * 100,
    ws_per_100 = (world_series / years_active) * 100,
    playoffs_per_100 = (playoff_appearances / years_active) * 100
  )

cat("\nTeam success data computed\n")

player_stats <- Batting %>%
  group_by(playerID) %>%
  summarise(
    career_years = n_distinct(yearID),
    career_G = sum(G, na.rm = TRUE),
    career_AB = sum(AB, na.rm = TRUE),
    career_H = sum(H, na.rm = TRUE),
    career_HR = sum(HR, na.rm = TRUE),
    career_RBI = sum(RBI, na.rm = TRUE),
    career_BB = sum(BB, na.rm = TRUE),
    career_SB = sum(SB, na.rm = TRUE),
    debut_year = min(yearID),
    final_year = max(yearID),
    .groups = "drop"
  ) %>%
  mutate(
    career_AVG = ifelse(career_AB > 0, career_H / career_AB, 0),
    era = case_when(
      final_year <= 1919 ~ "Dead Ball",
      final_year <= 1941 ~ "Live Ball",
      final_year <= 1960 ~ "Integration",
      final_year <= 1976 ~ "Expansion",
      final_year <= 1993 ~ "Free Agency",
      final_year <= 2005 ~ "Steroid",
      TRUE ~ "Modern"
    )
  )

player_stats <- player_stats %>%
  filter(career_AB >= 1500) 

cat("Position players with 1500+ ABs:", nrow(player_stats), "\n")

analysis_data <- player_teams %>%
  inner_join(player_stats, by = "playerID") %>%
  left_join(market_size %>% dplyr::select(teamID, market_size, market_rank, metro_pop_millions), 
            by = "teamID") %>%
  left_join(team_success, by = "teamID") %>%
  left_join(hof, by = "playerID") %>%
  left_join(names_df, by = "playerID") %>%
  mutate(
    inducted = replace_na(inducted, 0),
    market_size = factor(market_size, levels = c("Small", "Medium", "Large")),
    full_name = paste(nameFirst, nameLast)
  ) %>%
  filter(!is.na(market_size))

cat("\nFinal analysis dataset:", nrow(analysis_data), "players\n")
cat("HOF inductees:", sum(analysis_data$inducted), "\n")

cat("\n" , strrep("=", 60), "\n")
cat("DESCRIPTIVE STATISTICS\n")
cat(strrep("=", 60), "\n\n")

market_summary <- analysis_data %>%
  group_by(market_size) %>%
  summarise(
    n_players = n(),
    n_hof = sum(inducted),
    hof_rate = mean(inducted) * 100,
    avg_career_games = mean(career_G),
    avg_career_HR = mean(career_HR),
    avg_team_wins = mean(win_pct, na.rm = TRUE),
    avg_team_ws = mean(world_series, na.rm = TRUE),
    .groups = "drop"
  )

cat("HOF Rate by Market Size (Unadjusted):\n")
print(market_summary)
cat("\n")

chi_test <- chisq.test(table(analysis_data$market_size, analysis_data$inducted))
cat("Chi-squared test for market size vs HOF:\n")
cat("  Chi-squared:", round(chi_test$statistic, 3), "\n")
cat("  p-value:", format(chi_test$p.value, digits = 4), "\n\n")

cat(strrep("=", 60), "\n")
cat("CONTROLLING FOR TEAM SUCCESS\n")
cat(strrep("=", 60), "\n\n")

median_pennants <- median(analysis_data$pennants, na.rm = TRUE)

success_stratified <- analysis_data %>%
  mutate(team_success_level = ifelse(pennants > median_pennants, "High Success", "Low Success")) %>%
  group_by(market_size, team_success_level) %>%
  summarise(
    n_players = n(),
    n_hof = sum(inducted),
    hof_rate = mean(inducted) * 100,
    .groups = "drop"
  )

cat("HOF Rate by Market Size, Stratified by Team Success:\n")
print(success_stratified %>% arrange(team_success_level, market_size))
cat("\n")

cat(strrep("=", 60), "\n")
cat("LOGISTIC REGRESSION ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

model1 <- glm(inducted ~ market_size, 
              data = analysis_data, 
              family = binomial)

cat("Model 1: Market Size Only\n")
print(tidy(model1, exponentiate = TRUE, conf.int = TRUE) %>%
        mutate(across(where(is.numeric), ~round(., 3))))
cat("\n")

model2 <- glm(inducted ~ market_size + career_H + career_HR + career_RBI + 
                career_AVG + career_years,
              data = analysis_data,
              family = binomial)

cat("Model 2: Market Size + Player Stats\n")
print(tidy(model2, exponentiate = TRUE, conf.int = TRUE) %>%
        mutate(across(where(is.numeric), ~round(., 3))))
cat("\n")

model3 <- glm(inducted ~ market_size + career_H + career_HR + career_RBI + 
                career_AVG + career_years + pennants + world_series,
              data = analysis_data,
              family = binomial)

cat("Model 3: Market Size + Player Stats + Team Success\n")
print(tidy(model3, exponentiate = TRUE, conf.int = TRUE) %>%
        mutate(across(where(is.numeric), ~round(., 3))))
cat("\n")

model4 <- glm(inducted ~ market_size + career_H + career_HR + career_RBI + 
                career_AVG + career_years + pennants + world_series + era,
              data = analysis_data,
              family = binomial)

cat("Model 4: Full Model (+ Era)\n")
summary_m4 <- tidy(model4, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  filter(grepl("market", term))
print(summary_m4)
cat("\n")

cat(strrep("=", 60), "\n")
cat("SUMMARY: MARKET SIZE EFFECT ACROSS MODELS\n")
cat(strrep("=", 60), "\n\n")

extract_market_effects <- function(model, model_name) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(grepl("market_size", term)) %>%
    mutate(model = model_name) %>%
    dplyr::select(model, term, odds_ratio = estimate, conf.low, conf.high, p.value)
}

market_effects <- bind_rows(
  extract_market_effects(model1, "1. Market Only"),
  extract_market_effects(model2, "2. + Player Stats"),
  extract_market_effects(model3, "3. + Team Success"),
  extract_market_effects(model4, "4. + Era")
) %>%
  mutate(
    significant = ifelse(p.value < 0.05, "*", ""),
    term = gsub("market_size", "", term)
  )

cat("Odds Ratios for Market Size (Reference = Small Market):\n\n")
print(market_effects %>% 
        mutate(across(where(is.numeric), ~round(., 3))) %>%
        arrange(term, model))

cat("\n", strrep("=", 60), "\n")
cat("TEAM-SPECIFIC ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

team_hof_summary <- analysis_data %>%
  group_by(teamID) %>%
  summarise(
    n_players = n(),
    n_hof = sum(inducted),
    hof_rate = mean(inducted) * 100,
    market_size = first(market_size),
    pennants = first(pennants),
    world_series = first(world_series),
    .groups = "drop"
  ) %>%
  filter(n_players >= 20) %>%  
  arrange(desc(hof_rate))

cat("Top 15 Teams by HOF Rate (min 20 players):\n")
print(head(team_hof_summary, 15))
cat("\n")

yankees_data <- analysis_data %>%
  filter(teamID == "NYA")

cat("\nNew York Yankees Analysis:\n")
cat("  Total qualifying players:", nrow(yankees_data), "\n")
cat("  HOF inductees:", sum(yankees_data$inducted), "\n")
cat("  HOF rate:", round(mean(yankees_data$inducted) * 100, 1), "%\n")
cat("  Team pennants:", unique(yankees_data$pennants), "\n")
cat("  Team World Series:", unique(yankees_data$world_series), "\n")

cat("\nComparison: NYY vs Cardinals (similar success, different market):\n")

comparison <- analysis_data %>%
  filter(teamID %in% c("NYA", "SLN")) %>%
  group_by(teamID, market_size) %>%
  summarise(
    n_players = n(),
    n_hof = sum(inducted),
    hof_rate = mean(inducted) * 100,
    avg_career_HR = mean(career_HR),
    pennants = first(pennants),
    world_series = first(world_series),
    .groups = "drop"
  )

print(comparison)

cat("\n", strrep("=", 60), "\n")
cat("LEAGUE EXPANSION ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

teams_by_era <- analysis_data %>%
  mutate(
    era_numeric = case_when(
      debut_year <= 1900 ~ "Pre-1900 (8 teams)",
      debut_year <= 1960 ~ "1901-1960 (16 teams)",
      debut_year <= 1968 ~ "1961-1968 (20-24 teams)",
      debut_year <= 1992 ~ "1969-1992 (24-26 teams)",
      TRUE ~ "1993+ (28-30 teams)"
    )
  ) %>%
  group_by(era_numeric, market_size) %>%
  summarise(
    n_players = n(),
    n_hof = sum(inducted),
    hof_rate = mean(inducted) * 100,
    .groups = "drop"
  )

cat("HOF Rate by Era and Market Size:\n")
print(teams_by_era %>% arrange(era_numeric, market_size))

analysis_data <- analysis_data %>%
  mutate(
    predicted_prob = predict(model4, type = "response"),
    residual = inducted - predicted_prob
  )

unexpected_in <- analysis_data %>%
  filter(inducted == 1, predicted_prob < 0.3) %>%
  arrange(predicted_prob) %>%
  dplyr::select(full_name, teamID, market_size, career_HR, career_H, 
         pennants, world_series, predicted_prob)

cat("HOFers with Low Predicted Probability (possible market bias?):\n")

unexpected_out <- analysis_data %>%
  filter(inducted == 0, predicted_prob > 0.5) %>%
  arrange(desc(predicted_prob)) %>%
  dplyr::select(full_name, teamID, market_size, career_HR, career_H,
         pennants, world_series, predicted_prob)

cat("\nNon-HOFers with High Predicted Probability:\n")
print(head(unexpected_out, 10))

dir.create("outputs", showWarnings = FALSE)

p1 <- ggplot(market_summary, aes(x = market_size, y = hof_rate, fill = market_size)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(round(hof_rate, 1), "%\n(", n_hof, "/", n_players, ")")),
            vjust = -0.2, size = 4) +
  scale_fill_manual(values = c("Small" = "#2ecc71", "Medium" = "#f39c12", "Large" = "#e74c3c")) +
  labs(
    title = "Hall of Fame Induction Rate by Market Size",
    subtitle = "Unadjusted rates (does not control for player ability or team success)",
    x = "Market Size",
    y = "HOF Induction Rate (%)",
    fill = "Market"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  ylim(0, max(market_summary$hof_rate) * 1.3)

ggsave("outputs/hof_rate_by_market.png", p1, width = 10, height = 7, dpi = 300)

p2 <- market_effects %>%
  ggplot(aes(x = model, y = odds_ratio, color = term)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Medium" = "#f39c12", "Large" = "#e74c3c")) +
  labs(
    title = "Market Size Effect on HOF Induction",
    subtitle = "Odds Ratios with 95% CI (Reference = Small Market)",
    x = "Model Specification",
    y = "Odds Ratio",
    color = "Market Size"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip()

ggsave("outputs/market_odds_ratios.png", p2, width = 10, height = 7, dpi = 300)

p3 <- analysis_data %>%
  group_by(teamID) %>%
  filter(n() >= 15) %>%
  summarise(
    hof_rate = mean(inducted) * 100,
    pennants = first(pennants),
    market_size = first(market_size),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = pennants, y = hof_rate, color = market_size, label = teamID)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_text(vjust = -0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(group = 1), color = "gray40") +
  scale_color_manual(values = c("Small" = "#2ecc71", "Medium" = "#f39c12", "Large" = "#e74c3c")) +
  labs(
    title = "Team Success vs HOF Rate",
    subtitle = "Is the market size effect explained by team success?",
    x = "Number of Pennants Won",
    y = "HOF Induction Rate (%)",
    color = "Market Size"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("outputs/team_success_vs_hof.png", p3, width = 12, height = 8, dpi = 300)

p4 <- success_stratified %>%
  ggplot(aes(x = market_size, y = hof_rate, fill = team_success_level)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(hof_rate, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("High Success" = "#3498db", "Low Success" = "#95a5a6")) +
  labs(
    title = "HOF Rate by Market Size, Stratified by Team Success",
    subtitle = "Controls reveal whether market or success drives HOF inductions",
    x = "Market Size",
    y = "HOF Induction Rate (%)",
    fill = "Team Success"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  ylim(0, max(success_stratified$hof_rate) * 1.3)

ggsave("outputs/stratified_market_success.png", p4, width = 11, height = 7, dpi = 300)

cat("Visualizations saved to outputs/ directory\n")

large_vs_small_naive <- market_effects %>% 
  filter(model == "1. Market Only", term == "Large") %>% 
  pull(odds_ratio)

large_vs_small_adjusted <- market_effects %>% 
  filter(model == "4. + Era", term == "Large") %>% 
  pull(odds_ratio)

large_pvalue <- market_effects %>% 
  filter(model == "4. + Era", term == "Large") %>% 
  pull(p.value)

cat("KEY FINDINGS:\n\n")

cat("1. UNADJUSTED MARKET SIZE EFFECT:\n")
cat("   - Large market teams have", round(large_vs_small_naive, 2), 
    "x the odds of HOF induction vs small markets\n")
cat("   - This is the 'naive' estimate that doesn't control for anything\n\n")

cat("2. AFTER CONTROLLING FOR PLAYER ABILITY AND TEAM SUCCESS:\n")
cat("   - Large market odds ratio drops to", round(large_vs_small_adjusted, 2), "\n")
cat("   - p-value:", format(large_pvalue, digits = 3), "\n")

if (large_pvalue < 0.05) {
  cat("   - CONCLUSION: Significant market bias REMAINS after controls\n")
  cat("   - Large market players still have an advantage independent of stats/winning\n")
} else {
  cat("   - CONCLUSION: Market bias is NOT significant after controls\n")
  cat("   - The apparent bias is explained by player ability and team success\n")
}

cat("\n3. EXPLANATION OF THE YANKEES EFFECT:\n")
yankees_hof_rate <- mean(analysis_data$inducted[analysis_data$teamID == "NYA"]) * 100
overall_hof_rate <- mean(analysis_data$inducted) * 100

cat("   - Yankees HOF rate:", round(yankees_hof_rate, 1), "%\n")
cat("   - Overall HOF rate:", round(overall_hof_rate, 1), "%\n")
cat("   - Yankees have won", unique(analysis_data$world_series[analysis_data$teamID == "NYA"]), 
    "World Series\n")
cat("   - Much of Yankee success comes from championships, not just market size\n\n")

cat("4. IMPLICATIONS:\n")
cat("   - Voters should be aware of potential market visibility bias\n")
cat("   - Small market stars may need 'extra' stats to overcome lower visibility\n")
cat("   - Team success (championships) matters independently of market size\n\n")

cat("\n", strrep("=", 60), "\n")
cat("PROPENSITY SCORE ANALYSIS\n")
cat(strrep("=", 60), "\n\n")


tryCatch({
  analysis_data <- analysis_data %>%
    mutate(large_market = ifelse(market_size == "Large", 1, 0))
  
  propensity_model <- glm(large_market ~ career_H + career_HR + career_RBI + 
                           career_AVG + career_years,
                         data = analysis_data,
                         family = binomial)
  
  analysis_data$propensity_score <- predict(propensity_model, type = "response")
  
  analysis_data <- analysis_data %>%
    mutate(propensity_quintile = ntile(propensity_score, 5))
  
  propensity_stratified <- analysis_data %>%
    group_by(propensity_quintile, market_size) %>%
    summarise(
      n = n(),
      hof_rate = mean(inducted) * 100,
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = market_size, values_from = c(n, hof_rate))
  
  cat("HOF Rate by Propensity Score Quintile (controls for ability sorting):\n")
  print(propensity_stratified)
  
}, error = function(e) {
  cat("Propensity score analysis skipped:", e$message, "\n")
})

cat("\n", strrep("=", 60), "\n")
cat("WORLD SERIES EXPOSURE ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

ws_appearances <- batting_post %>%
  filter(round == "WS") %>%
  group_by(playerID) %>%
  summarise(
    ws_games = sum(G, na.rm = TRUE),
    ws_series = n_distinct(yearID),
    ws_hits = sum(H, na.rm = TRUE),
    ws_hrs = sum(HR, na.rm = TRUE),
    .groups = "drop"
  )

analysis_data <- analysis_data %>%
  left_join(ws_appearances, by = "playerID") %>%
  mutate(
    across(starts_with("ws_"), ~replace_na(.x, 0)),
    ever_in_ws = ifelse(ws_series > 0, 1, 0)
  )

model_ws <- glm(inducted ~ market_size + career_H + career_HR + career_RBI + 
                  career_AVG + career_years + ever_in_ws + ws_series,
                data = analysis_data,
                family = binomial)

cat("Model with Individual World Series Participation:\n")
ws_effects <- tidy(model_ws, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("market|ws", term)) %>%
  mutate(across(where(is.numeric), ~round(., 3)))
print(ws_effects)

cat("\nInterpretation:\n")
cat("- If market effects shrink when adding WS participation,\n")
cat("  then 'visibility' through October exposure matters\n")
cat("- Large market teams get more WS exposure -> more HOF inductees\n")

cat("\n", strrep("=", 60), "\n")
cat("CONTINUOUS MARKET SIZE ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

model_continuous <- glm(inducted ~ metro_pop_millions + career_H + career_HR + 
                          career_RBI + career_AVG + career_years + 
                          pennants + world_series,
                        data = analysis_data,
                        family = binomial)

cat("Effect of Metro Population (in millions) on HOF odds:\n")
cont_effect <- tidy(model_continuous, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term == "metro_pop_millions") %>%
  mutate(across(where(is.numeric), ~round(., 3)))
print(cont_effect)

cat("\nInterpretation: For each additional million people in metro area,\n")
cat("HOF odds change by factor of", round(cont_effect$estimate, 3), "\n")

cat("\n", strrep("=", 60), "\n")
cat("INTERACTION ANALYSIS\n")
cat(strrep("=", 60), "\n\n")

median_hr <- median(analysis_data$career_HR)
median_h <- median(analysis_data$career_H)

analysis_data <- analysis_data %>%
  mutate(
    stat_tier = case_when(
      career_HR > quantile(career_HR, 0.75) | career_H > quantile(career_H, 0.75) ~ "Elite",
      career_HR > median_hr | career_H > median_h ~ "Borderline",
      TRUE ~ "Below Average"
    )
  )

interaction_model <- glm(inducted ~ market_size * stat_tier + 
                          career_years + pennants + world_series,
                        data = analysis_data,
                        family = binomial)

cat("Market Size × Player Tier Interaction:\n")
interaction_effects <- tidy(interaction_model, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(grepl("market_size|stat_tier|:", term)) %>%
  mutate(across(where(is.numeric), ~round(., 3)))
print(interaction_effects)

interaction_plot_data <- analysis_data %>%
  group_by(market_size, stat_tier) %>%
  summarise(
    n = n(),
    hof_rate = mean(inducted) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 10)

p_interaction <- ggplot(interaction_plot_data, 
                        aes(x = stat_tier, y = hof_rate, fill = market_size)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(round(hof_rate, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5) +
  scale_fill_manual(values = c("Small" = "#2ecc71", "Medium" = "#f39c12", "Large" = "#e74c3c")) +
  labs(
    title = "Does Market Size Matter More for Borderline Players?",
    subtitle = "Elite players may get in regardless; borderline players need visibility",
    x = "Player Statistical Tier",
    y = "HOF Induction Rate (%)",
    fill = "Market Size"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  ylim(0, max(interaction_plot_data$hof_rate, na.rm = TRUE) * 1.3)

ggsave("outputs/market_tier_interaction.png", p_interaction, width = 12, height = 8, dpi = 300)

cat("\nInteraction plot saved to outputs/market_tier_interaction.png\n")

saveRDS(
  list(
    analysis_data = analysis_data,
    models = list(
      naive = model1,
      with_stats = model2,
      with_success = model3,
      full = model4
    ),
    market_effects = market_effects,
    team_summary = team_hof_summary,
    market_summary = market_summary
  ),
  "outputs/team_bias_analysis.rds"
)

write.csv(market_effects, "outputs/market_size_effects.csv", row.names = FALSE)
write.csv(team_hof_summary, "outputs/team_hof_summary.csv", row.names = FALSE)

cat("Results saved to outputs/ directory\n")
cat("\n=== Analysis Complete ===\n")