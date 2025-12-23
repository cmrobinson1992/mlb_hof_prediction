###############################
###### NOTES ##################
# 1. Filter out players with < 1000 career ABs

library(Lahman)
library(MASS)
library(tidyverse)
source("src/utils/created functions.R")

names_df <- People %>%
  dplyr::select(playerID, nameFirst, nameLast)

current_players <- Batting %>%
  filter(yearID >= 2020) %>%
  distinct(playerID)

current_player_teams <- Batting %>%
  filter(playerID %in% Batting$playerID & yearID == 2024) %>%
  dplyr::select(playerID, teamID)

write.csv('data/processed/player_teams.csv', row.names = FALSE)

Batting <- Batting %>%
  group_by(playerID) %>%
  mutate(
    career_AB = sum(AB, na.rm = TRUE),
    seasons_played = n_distinct(yearID),
    .groups = "drop"
  ) %>%
  filter(career_AB >= 1000) %>%
  dplyr::select(-c(career_AB, seasons_played))

HOFVoting <- names_df %>%
   inner_join(Batting, by = 'playerID') %>%
   left_join(HallOfFame, by = 'playerID') %>%
  filter(!(grepl('poll', needed_note))) %>%
  filter((votedBy %in% c('BBWAA', 'Special Election') & category == 'Player') | is.na(inducted)) %>%
   mutate(inducted = ifelse(category == 'Player' & inducted == 'Y' , 'Y', 'N')) %>%
   group_by(playerID) %>%
   mutate(LastYear = max(yearID.y)) %>%
   filter(yearID.y == LastYear | is.na(yearID.y)) %>%
   dplyr::select(playerID, nameFirst, nameLast, inducted, LastYear = yearID.y) %>%
   mutate(inducted = fct_explicit_na(inducted, na_level = "N")) %>%
   distinct(.keep_all = TRUE) 

HOF_summary <- HOFVoting %>% group_by(inducted) %>% summarise(Count = n()) %>% mutate(PctofTotal = round(100*Count/sum(Count), 2))
print(paste("As of 2025,", HOF_summary[HOF_summary$inducted == 'Y', 3], '% of eligible batters have been inducted via BBWAA Vote'))

batting_age <- Batting %>%
  left_join(People %>% dplyr::select(playerID, birthYear, birthMonth), by = "playerID") %>%
 # top_n(500) %>%
  mutate(
    age = yearID - birthYear - ifelse(birthMonth > 6, 1, 0),
    across(c(G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP), ~replace_na(.x, 0))
  ) %>%
  filter(!is.na(age)) %>%
  group_by(playerID, yearID, age) %>%
  summarise(
    across(c(G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, SH, SF, GIDP), sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    X1B = H - X2B - X3B - HR,
    OBP = ifelse((AB + BB + HBP + SF) > 0,
                 (H + BB + HBP) / (AB + BB + HBP + SF), 0),
    SLG = ifelse(AB > 0,
                 (X1B + 2 * X2B + 3 * X3B + 4 * HR) / AB, 0),
    OPS = round(OBP + SLG, 6)
  ) %>%
  group_by(playerID) %>%
  mutate(debutYear = min(yearID), finalYear = max(yearID)) %>%
  mutate(Era = (debutYear + finalYear)/2) %>%
  ungroup() %>%
  distinct(.keep_all = TRUE)

cat("Players with batting data:", n_distinct(batting_age$playerID), "\n")

league_stats <- batting_age %>%
  filter(AB > 0) %>%
  group_by(yearID) %>%
  summarise(
    lg_OBP = mean(OBP, na.rm = TRUE),
    lg_SLG = mean(SLG, na.rm = TRUE),
    lg_OPS = mean(OPS, na.rm = TRUE),
    sd_OBP = sd(OBP, na.rm = TRUE),
    sd_SLG = sd(SLG, na.rm = TRUE),
    sd_OPS = sd(OPS, na.rm = TRUE),
    total_R = sum(R, na.rm = TRUE), 
    total_G = sum(G, na.rm = TRUE)
  )

batting_age_adj <- batting_age %>%
  left_join(league_stats, by = "yearID") %>%
  mutate(
    OBP_plus = round((OBP / lg_OBP) * 100, 1),
    SLG_plus = round((SLG / lg_SLG) * 100, 1),
    OPS_plus = round((OPS / lg_OPS) * 100, 1),
    
    OBP_z = (OBP - lg_OBP) / sd_OBP,
    SLG_z = (SLG - lg_SLG) / sd_SLG,
    OPS_z = (OPS - lg_OPS) / sd_OPS,
    lg_RPG = total_R / total_G,
    neutralized_HR = HR * (4.5 / lg_RPG), 
    neutralized_H = H * (4.5 / lg_RPG)
  )

batting_by_age_adj <- batting_age_adj %>%
  arrange(playerID, age) %>%
  group_by(playerID) %>%
  mutate(
    cum_PA = cumsum(AB + BB + IBB + HBP + SF)
  ) %>%
  group_by(playerID, age) %>%
  summarise(
    OBP_plus = weighted.mean(OBP_plus, w = AB, na.rm = TRUE),
    SLG_plus = weighted.mean(SLG_plus, w = AB, na.rm = TRUE),
    OPS_plus = weighted.mean(OPS_plus, w = AB, na.rm = TRUE),
    OBP_z = weighted.mean(OBP_z, w = AB, na.rm = TRUE),
    SLG_z = weighted.mean(SLG_z, w = AB, na.rm = TRUE),
    OPS_z = weighted.mean(OPS_z, w = AB, na.rm = TRUE),
    neutralized_HR = sum(neutralized_HR, na.rm = TRUE),
    neutralized_H = sum(neutralized_H, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

batting_by_age <- batting_age %>%
  arrange(playerID, age) %>%
  group_by(playerID) %>%
  mutate(
    Years = cumsum(!duplicated(yearID)),
    G_cum = cumsum(G), AB_cum = cumsum(AB), R_cum = cumsum(R), H_cum = cumsum(H),
    X1B_cum = cumsum(X1B), X2B_cum = cumsum(X2B), X3B_cum = cumsum(X3B), HR_cum = cumsum(HR),
    RBI_cum = cumsum(RBI), SB_cum = cumsum(SB), CS_cum = cumsum(CS),
    BB_cum = cumsum(BB), SO_cum = cumsum(SO), IBB_cum = cumsum(IBB), HBP_cum = cumsum(HBP),
    SH_cum = cumsum(SH), SF_cum = cumsum(SF), GIDP_cum = cumsum(GIDP),
    finalYear = max(yearID)
  ) %>%
  mutate(
    AVG = H_cum / AB_cum,
    SLG = (X1B_cum + 2 * X2B_cum + 3 * X3B_cum + 4 * HR_cum) / AB_cum,
    OBP = (H_cum + BB_cum + IBB_cum + HBP_cum) / (AB_cum + BB_cum + IBB_cum + HBP_cum + SF_cum),
    OPS = SLG + OBP
  ) %>%
  ungroup() %>%
  rename(age_through = age) %>%
  dplyr::select(playerID, age_through, Years, finalYear,
         G = G_cum, AB = AB_cum, R = R_cum, H = H_cum,
         X1B = X1B_cum, X2B = X2B_cum, X3B = X3B_cum, HR = HR_cum,
         RBI = RBI_cum, SB = SB_cum, CS = CS_cum,
         BB = BB_cum, SO = SO_cum, IBB = IBB_cum, HBP = HBP_cum,
         SH = SH_cum, SF = SF_cum, GIDP = GIDP_cum,
         AVG, SLG, OBP, OPS, Era) 

# Function to compute peak OPS window of up to 7 seasons for each player-age group
get_dynamic_peak <- function(df) {
  df <- df %>% arrange(age)
  n <- nrow(df)
  
  if (n == 0) return(tibble())
  
  peak_data <- vector("list", n)
  
  for (i in seq_len(n)) {
    career_so_far <- df[1:i, ]
    n_years <- nrow(career_so_far)
    if (n_years <= 7) {
      best_window <- career_so_far
    } else {
      best_ops <- -Inf
      best_window <- NULL
      for (start_idx in 1:(n_years - 6)) {
        end_idx <- start_idx + 6 
        window_df <- career_so_far[start_idx:end_idx, ]
        
        ab_total <- sum(window_df$AB, na.rm = TRUE)
        pa_total <- sum(window_df$AB + window_df$BB + window_df$IBB + 
                       window_df$HBP + window_df$SF, na.rm = TRUE)
        
        if (ab_total == 0 || pa_total == 0) next
        
        obp <- sum(window_df$H + window_df$BB + window_df$IBB + window_df$HBP, 
                   na.rm = TRUE) / pa_total
        slg <- sum(window_df$X1B + 2 * window_df$X2B + 3 * window_df$X3B + 
                   4 * window_df$HR, na.rm = TRUE) / ab_total
        ops <- obp + slg
        
        if (ops > best_ops) {
          best_ops <- ops
          best_window <- window_df
        }
      }
    }
    
    if (is.null(best_window) || nrow(best_window) == 0) {
      next
    }
    
    ab_total <- sum(best_window$AB, na.rm = TRUE)
    pa_total <- sum(best_window$AB + best_window$BB + best_window$IBB + 
                   best_window$HBP + best_window$SF, na.rm = TRUE)
    
    if (ab_total > 0 && pa_total > 0) {
      obp <- sum(best_window$H + best_window$BB + best_window$IBB + 
                best_window$HBP, na.rm = TRUE) / pa_total
      slg <- sum(best_window$X1B + 2 * best_window$X2B + 3 * best_window$X3B + 
                4 * best_window$HR, na.rm = TRUE) / ab_total
      peak_ops <- obp + slg
    } else {
      peak_ops <- 0
    }
    
    peak_row <- best_window %>%
      summarise(
        peak_H = sum(H, na.rm = TRUE), 
        peak_AB = sum(AB, na.rm = TRUE), 
        peak_X1B = sum(X1B, na.rm = TRUE),
        peak_X2B = sum(X2B, na.rm = TRUE), 
        peak_X3B = sum(X3B, na.rm = TRUE), 
        peak_HR = sum(HR, na.rm = TRUE),
        peak_RBI = sum(RBI, na.rm = TRUE), 
        peak_BB = sum(BB, na.rm = TRUE), 
        peak_SO = sum(SO, na.rm = TRUE),
        peak_HBP = sum(HBP, na.rm = TRUE), 
        peak_IBB = sum(IBB, na.rm = TRUE), 
        peak_SF = sum(SF, na.rm = TRUE),
        peak_OPS = round(peak_ops, 6),
        minyear_peak = min(age),
        maxyear_peak = max(age),
        peak_years = n() 
      )
    
    peak_data[[i]] <- cbind(
      tibble(playerID = df$playerID[i], age = df$age[i]), 
      peak_row
    )
  }
  
  bind_rows(peak_data)
}

cat("Calculating peak windows...\n")
batting_peak_by_age <- batting_age %>%
  group_by(playerID) %>%
  group_modify(~get_dynamic_peak(.x)) %>%
  ungroup()

cat("Peak calculations complete. Records:", nrow(batting_peak_by_age), "\n")
stopifnot("Missing peak data" = sum(is.na(batting_peak_by_age$peak_OPS)) == 0)

batting_combined <- batting_by_age %>%
  left_join(batting_peak_by_age, by = c("playerID", "age_through" = "age")) %>%
  left_join(batting_by_age_adj, by = c('playerID', 'age_through' = 'age'))

cat("Combined batting data. Records:", nrow(batting_combined), "\n")

example <- batting_combined %>%
  filter(playerID == "troutmi01") %>%
  arrange(age_through)

example

fielding_full <- Fielding %>%
  filter(playerID %in% batting_age$playerID) %>%
  left_join(People %>% dplyr::select(playerID, birthYear, birthMonth), by = "playerID") %>%
  mutate(
    age = yearID - birthYear - ifelse(birthMonth > 6, 1, 0)) %>%
  group_by(playerID, age) %>%
  summarise(across(c(InnOuts, PO, A, E, DP), ~sum(.x, na.rm = TRUE)), .groups = "drop")

player_mainpos <- Fielding %>%
  group_by(playerID, POS) %>%
  summarise(pos_games = sum(G, na.rm = TRUE), .groups = "drop") %>%
  group_by(playerID) %>%
  filter(pos_games == max(pos_games)) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  dplyr::select(playerID, POS)

award_start_years <- data.frame(
  awardID = c('GG', 'SS', 'MVP','RoY', 'AllStar'),
  first_year = c(1957, 1980, 1911, 1947, 1933)
)

award_years <- expand.grid(
  yearID = min(Batting$yearID):max(Batting$yearID),
  awardID = award_start_years$awardID,
  stringsAsFactors = FALSE
) %>%
  left_join(award_start_years, by = 'awardID') %>%
  filter(yearID >= first_year) %>%
  dplyr::select(-first_year)

player_awards <- AwardsPlayers %>%
  mutate(playerID = ifelse(playerID == 'braunry01', 'braunry02', playerID)) %>%
  filter(tolower(awardID) %in% c('gold glove', 'silver slugger', 'most valuable player', 'rookie of the year')) %>%
  mutate(award_short = case_when(
    tolower(awardID) == 'gold glove' ~ 'GG',
    tolower(awardID) == 'silver slugger' ~ 'SS',
    tolower(awardID) == 'most valuable player' ~ 'MVP',
    tolower(awardID) == 'rookie of the year' ~ 'RoY',
    TRUE ~ awardID
  )) %>%
  count(playerID, yearID, award_short)

allstar_counts <- AllstarFull %>%
  distinct(playerID, yearID) %>%
  count(playerID, yearID, name = 'AllStar')

award_counts <- player_awards %>%
  pivot_wider(names_from = award_short, values_from = n, values_fill = 0) %>%
  full_join(allstar_counts, by = c('playerID', 'yearID')) %>%
  replace_na(list(GG = 0, SS = 0, MVP = 0, RoY = 0, AllStar = 0)) %>%
  mutate(awards_total = GG + SS + MVP + RoY + AllStar)

award_opportunities <- Batting %>%
  distinct(playerID, yearID) %>%
  crossing(awardID = award_start_years$awardID) %>%
  left_join(award_start_years, by = "awardID") %>%
  filter(yearID >= first_year) %>%
  group_by(playerID, yearID) %>%
  summarise(awards_available = n(), .groups = "drop")

award_summary <- Batting %>%
  distinct(playerID, yearID) %>%
  left_join(award_counts, by = c("playerID", "yearID")) %>%
  left_join(award_opportunities, by = c("playerID", "yearID")) %>%
  replace_na(list(awards_total = 0, awards_available = 0)) %>%
  arrange(playerID, yearID) %>%
  group_by(playerID) %>%
  mutate(
    cum_awards_won = cumsum(awards_total),
    cum_awards_available = cumsum(awards_available),
    award_share = ifelse(cum_awards_available > 0, cum_awards_won / cum_awards_available, 0)
  ) %>%
  dplyr::select(playerID, yearID, awards_won = awards_total, award_share) %>%
  left_join(People %>% dplyr::select(playerID, birthYear, birthMonth), by = "playerID") %>%
  mutate(
    age = yearID - birthYear - ifelse(birthMonth > 6, 1, 0)
  ) %>%
  dplyr::select(-yearID, -birthYear, -birthMonth)

data_full <- names_df %>%
  inner_join(batting_combined, by = "playerID") %>%
  left_join(fielding_full, by = c('playerID', 'age_through' = 'age')) %>%
  left_join(award_summary, by = c('playerID', 'age_through' = 'age')) %>%
  left_join(player_mainpos, by = "playerID") %>%
  left_join(HOFVoting, by = "playerID") %>%
  filter(!is.na(peak_AB), (POS != "P" | playerID == 'ohtansh01')) %>% 
  mutate(inducted = replace_na(inducted, "N")) %>%
  group_by(playerID) %>%
  mutate(
    career_length = max(Years),
    max_age_reached = max(age_through)
  ) %>%
  ungroup() %>%
  distinct(.keep_all = TRUE) %>%
  mutate(nameFirst = nameFirst.x, nameLast = nameLast.x) %>%
  dplyr::select(-c(nameFirst.y, nameLast.y, nameFirst.x, nameLast.x)) 

suspended <- c('Alex Sanchez', 'Jorge Piedra', 'Jamal Strong', 'Rafael Palmeiro', 'Mike Morse', 'Matt Lawton', 'Neifi Perez',
    'Ryan Jorgensen', 'Mike Cameron', 'Jose Guillen', 'Jay Gibbons', 'Eliezer Alfonzo', 'Manny Ramirez', 'Pablo Ozuna', 'Ronny Paulino', 'Mike Jacobs',
  'Freddy Galvis', 'Marlon Byrd', 'Melky Cabrera', 'Yasmani Grandal', 'Ryan Braun', 'Everth Cabrera', 'Francisco Cervelli', 'Nelson Cruz', 'Fernando Martinez',
'Jesus Montero', 'Jhonny Peralta', 'Alex Rodriguez', 'Jordany Valdespin', 'Miguel Tejada', 'Cody Stanley', 'Abraham Almonte', 'Chris Colabello', 'Dee Gordon', 'Adalberto Mondesi',
'Marlon Byrd', 'Starling Marte', 'Raude Read', 'Jorge Bonifacio', 'Jorge Polanco', 'Robinson Cano', 'Wellington Castillo', 'Mike Marjama', 'Tim Beckham', 'Ramon Laureano', 'Pedro Severino',
'Fernando Tatis', 'Noelvi Marte', 'Orelvis Martinez', 'Jurickson Profar')

mitchellreport <- c('Marvin Benard', 'Barry Bonds', 'Jason Giambi', 'Jeremy Giambi', 'Armando Rios', 'Benito Santiago', 'Gary Sheffield', 'Randy Velarde', 
'Chad Allen', 'Mike Bell', 'Gary Bennett', 'Larry Bigbie', 'Kevin Brown', 'Mark Cerreon', 'Jason Christiansen', 'Howie Clark', 'Roger Clemens', 'Jack Cust', 'Brendan Donnelly',
'Chris Donnels', 'Lenny Dykstra', 'Matt Franco', 'Ryan Franklin', 'Eric Gagne', 'Jerry Hairston', 'Matt Herges', 'Phil Hiatt', 'Glenallen Hill', 'Todd Hundley', 'David Justice',
'Chuck Knoblauch', 'Tim Laker', 'Mike Lansing', 'Paul Lo Duca', 'Nook Logan', 'Josias Manzanillo', 'Cody McKay', 'Kent Mercker', 'Bart Miadich', 'Hal Morris', 'Jim Parque', 'Adam Piatt', 'Todd Pratt', 
'Stephen Randolph', 'Adam Riggs', 'Brian Roberts', 'David Segui', 'Miguel Tejada', 'Mo Vaughn', 'Ron Villone', 'Fernando Vina', 'Rondell White', 'Jeff Williams', 'Todd Williams', 'Kevin Young', 'Gregg Zaun',
'Rick Ankiel', 'David Bell', 'Paul Byrd', 'Jose Canseco', 'Jay Gibbons', 'Troy Glaus', 'Jason Grimsley', 'Jose Guillen', 'Darren Holmes', 'Gary Matthews', 'Scott Schoeneweis', 'Ismael Valdez', 'Matt Williams', 'Steve Woodard',
'Wally Joyner', 'Daniel Naulty', 'Manny Alexander', 'Ricky Bones', 'Alex Cabrera', 'Paxton Crawford', 'Juan Gonzalez', 'Mike Judd', 'Ricky Stone')


data_full <- data_full %>%
  mutate(PEDSusp = ifelse(paste(nameFirst, nameLast) %in% suspended & playerID != 'tatisfe01', 1, 0)) %>%
  mutate(PEDMitchell = ifelse(paste(nameFirst, nameLast) %in% mitchellreport & !(playerID %in% c('hairsje01', 'matthga01')), 1, 0)) %>%
  mutate(Era = case_when(
    Era <= 1919 ~ "Dead Ball (-1919)",
    Era <= 1941 ~ "Live Ball (1920-1941)",
    Era <= 1960 ~ "Integration (1942-1960)",
    Era <= 1976 ~ "Expansion (1961-1976)",
    Era <= 1993 ~ "Free Agency (1977-1993)",
    Era <= 2005 ~ "Steroid (1994-2005)",
    TRUE ~ "Sabermetric (2006-)"))

factor_cols <- c('POS', 'inducted', 'PEDMitchell', 'PEDSusp', 'Era')
data_full <- data_full %>% mutate_at(factor_cols, as.factor)

cat("Full dataset created. Records:", nrow(data_full), "\n")
cat("HOF inductees:",  n_distinct(data_full[data_full$inducted == 'Y', 1]), "\n")
cat("Eligible non-HOFers:",  n_distinct(data_full[data_full$inducted == 'N', 1]), "\n")

non_featureset <- c('playerID', 'nameFirst', 'nameLast', 'career_length', 'max_age_reached', 'inducted')
featureset <- colnames(data_full %>% dplyr::select(-all_of(non_featureset)))

df_eligible <- data_full %>% 
  filter(!(playerID %in% current_players$playerID)) %>%
  filter(!is.na(inducted))

df_current <- data_full %>% 
  filter(playerID %in% current_players$playerID)

cat("\n=== FINAL VALIDATION ===\n")
cat("Eligible players (for training):", n_distinct(df_eligible$playerID), "\n")
cat("Current players (for prediction):", n_distinct(df_current$playerID), "\n")
cat("Age range in eligible set:", min(df_eligible$age_through), "to", max(df_eligible$age_through), "\n")
cat("HOF inductees in eligible set:", df_eligible %>% filter(inducted == 'Y') %>% summarise(n_distinct(playerID)) %>% pull(), "\n")

if (any(is.na(df_eligible$peak_OPS))) {
  warning("Some peak_OPS values are still NA!")
}

# Save files
saveRDS(df_eligible, "data/processed/data_clean.rds")
saveRDS(df_current, "data/processed/data_current.rds")

cat("\n✓ Data processing complete!\n")
cat("Files saved to Data/ directory\n")



