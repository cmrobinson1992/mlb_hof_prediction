library(tidyverse)
library(Lahman)
library(glmnet)

set.seed(1992)

cat("================================================================================\n")
cat("HOF PREDICTIONS - CURRENT & BALLOT PLAYERS\n")
cat("================================================================================\n\n")

# =============================================================================
# 2026 BALLOT CANDIDATES
# =============================================================================

bbwaa_2026_position <- c(
  "abreubo01",   # Bobby Abreu
  "beltrca01",   # Carlos Beltrán
  "braunry02",   # Ryan Braun
  "choosh01",    # Shin-Soo Choo
  "encared01",   # Edwin Encarnación
  "gordoal01",   # Alex Gordon
  "hunteto01",   # Torii Hunter
  "jonesan01",   # Andruw Jones
  "kempma01",    # Matt Kemp
  "kendrho01",   # Howie Kendrick
  "markani01",   # Nick Markakis
  "murphda08",   # Daniel Murphy
  "pedrodu01",   # Dustin Pedroia
  "pencehu01",   # Hunter Pence
  "ramirma02",   # Manny Ramirez
  "rodrial01",   # Alex Rodriguez
  "rolliji01",   # Jimmy Rollins
  "utleych01",   # Chase Utley
  "vizquom01",   # Omar Vizquel
  "wrighda03"    # David Wright
)

era_committee_2025 <- c(
  "bondsba01",   # Barry Bonds
  "delgaca01",   # Carlos Delgado
  "kentje01",    # Jeff Kent
  "mattido01",   # Don Mattingly
  "murphda05",   # Dale Murphy
  "sheffga01"    # Gary Sheffield
)

all_ballot_candidates <- unique(c(bbwaa_2026_position, era_committee_2025))

cat("2026 Ballot Candidates:\n")
cat("  BBWAA Position Players:", length(bbwaa_2026_position), "\n")
cat("  Era Committee:", length(era_committee_2025), "\n\n")

# =============================================================================
# LOAD MODELS AND DATA
# =============================================================================

cat("Loading production models...\n")
production_models <- readRDS("models/production_models.rds")

cat("Loading age snapshots (historical players with correct data)...\n")
data_25 <- readRDS("data/processed/data_25.rds") %>% mutate(snapshot_age = 25, data_source = "snapshot")
data_28 <- readRDS("data/processed/data_28.rds") %>% mutate(snapshot_age = 28, data_source = "snapshot")
data_30 <- readRDS("data/processed/data_30.rds") %>% mutate(snapshot_age = 30, data_source = "snapshot")
data_33 <- readRDS("data/processed/data_33.rds") %>% mutate(snapshot_age = 33, data_source = "snapshot")
data_35 <- readRDS("data/processed/data_35.rds") %>% mutate(snapshot_age = 35, data_source = "snapshot")
data_40 <- readRDS("data/processed/data_40.rds") %>% mutate(snapshot_age = 40, data_source = "snapshot")

all_snapshots <- bind_rows(data_25, data_28, data_30, data_33, data_35, data_40)

cat("  Snapshot records:", nrow(all_snapshots), "\n")
cat("  Unique players in snapshots:", n_distinct(all_snapshots$playerID), "\n")

cat("Loading current players data...\n")
data_current <- readRDS("data/processed/data_current_processed.rds") %>% 
  mutate(snapshot_age = NA_integer_, data_source = "current")

cat("  Current player records:", nrow(data_current), "\n")
cat("  Unique current players:", n_distinct(data_current$playerID), "\n")

# =============================================================================
# GET ALL HOF INDUCTEES (BBWAA + COMMITTEE)
# =============================================================================

cat("\nLoading comprehensive HOF data (BBWAA + Committee)...\n")

hof_data <- HallOfFame %>%
  filter(category == "Player") %>%
  group_by(playerID) %>%
  summarise(
    ever_inducted = any(inducted == "Y"),
    bbwaa_inducted = any(inducted == "Y" & votedBy == "BBWAA"),
    committee_inducted = any(inducted == "Y" & votedBy != "BBWAA"),
    .groups = "drop"
  ) %>%
  filter(ever_inducted) %>%
  mutate(
    induction_method = case_when(
      bbwaa_inducted ~ "BBWAA",
      committee_inducted ~ "Committee",
      TRUE ~ "Unknown"
    )
  )

recent_inductees <- tibble(
  playerID = c(
    "allendi01",
    "beltrad01", "heltoto01", "mauerjo01",
    "suzukic01", "parkeda01",
    "kentje01"
  ),
  ever_inducted = TRUE,
  bbwaa_inducted = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
  committee_inducted = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  induction_method = c("Committee", "BBWAA", "BBWAA", "BBWAA", "BBWAA", "Committee", "Committee")
)

hof_comprehensive <- bind_rows(
  hof_data %>% filter(!(playerID %in% recent_inductees$playerID)),
  recent_inductees
)

cat("  Total HOF inductees:", nrow(hof_comprehensive), "\n")
cat("  BBWAA:", sum(hof_comprehensive$bbwaa_inducted), "\n")
cat("  Committee:", sum(hof_comprehensive$committee_inducted), "\n")

snapshot_players <- unique(all_snapshots$playerID)
current_only <- data_current %>% filter(!(playerID %in% snapshot_players))

all_data <- bind_rows(all_snapshots, current_only)

all_data <- all_data %>%
  left_join(
    hof_comprehensive %>% dplyr::select(playerID, ever_inducted, bbwaa_inducted, committee_inducted, induction_method),
    by = "playerID"
  ) %>%
  mutate(
    ever_inducted = replace_na(ever_inducted, FALSE),
    bbwaa_inducted = replace_na(bbwaa_inducted, FALSE),
    committee_inducted = replace_na(committee_inducted, FALSE),
    induction_method = replace_na(induction_method, "Not Inducted")
  )

cat("\nCombined data:\n")
cat("  Total records:", nrow(all_data), "\n")
cat("  Unique players:", n_distinct(all_data$playerID), "\n")

player_career_end <- all_data %>%
  group_by(playerID) %>%
  filter(age_through == max(age_through)) %>%
  ungroup() %>%
  distinct(playerID, .keep_all = TRUE)

cat("  Players with career-end data:", nrow(player_career_end), "\n")
cat("  - BBWAA inducted:", sum(player_career_end$bbwaa_inducted), "\n")
cat("  - Committee inducted:", sum(player_career_end$committee_inducted), "\n")
cat("  - Not inducted:", sum(!player_career_end$ever_inducted), "\n\n")

# =============================================================================
# PREDICTION FUNCTION
# =============================================================================

predict_hof_prob <- function(player_row, model_obj) {
  
  features <- model_obj$features
  
  X <- player_row[, features, drop = FALSE]
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
  
  prob <- 1 - raw_prob
  return(prob)
}

get_model_age <- function(player_age, career_complete = TRUE, available_ages = c(25, 28, 30, 33, 35, 40)) {
  if (career_complete) {
    return(40)
  }
  if (player_age >= 40) {
    return(40)
  }
  valid <- available_ages[available_ages <= player_age]
  if (length(valid) == 0) return(min(available_ages))
  return(max(valid))
}

# =============================================================================
# GENERATE PREDICTIONS FOR BALLOT CANDIDATES
# =============================================================================

cat("================================================================================\n")
cat("PREDICTIONS FOR 2026 BALLOT CANDIDATES\n")
cat("================================================================================\n\n")

ballot_data <- player_career_end %>%
  filter(playerID %in% all_ballot_candidates)

cat("Ballot candidates found:", n_distinct(ballot_data$playerID), 
    "of", length(all_ballot_candidates), "\n")

if (n_distinct(ballot_data$playerID) == 0) {
  cat("\nERROR: No ballot candidates found! Check playerIDs.\n")
  cat("Sample playerIDs in data:\n")
  print(head(unique(player_career_end$playerID), 20))
} else {
  
  cat("Generating predictions for", nrow(ballot_data), "candidates...\n\n")
  
  ballot_predictions <- ballot_data %>%
    rowwise() %>%
    mutate(
      career_complete = TRUE, 
      model_age = get_model_age(age_through, career_complete = career_complete),
      model_key = paste0("data_", model_age),
      hof_probability = {
        if (model_key %in% names(production_models)) {
          model_obj <- production_models[[model_key]]
          tryCatch(
            predict_hof_prob(cur_data(), model_obj),
            error = function(e) { 
              message("Error for ", playerID, ": ", e$message)
              NA_real_
            }
          )
        } else {
          NA_real_
        }
      },
      threshold = {
        if (model_key %in% names(production_models)) {
          production_models[[model_key]]$threshold
        } else {
          0.5
        }
      }
    ) %>%
    ungroup() %>%
    mutate(
      full_name = paste(nameFirst, nameLast),
      pred_hof = ifelse(hof_probability >= threshold, "Y", "N"),
      ballot_type = case_when(
        playerID %in% era_committee_2025 ~ "Era Committee 2025",
        playerID %in% bbwaa_2026_position ~ "BBWAA 2026",
        TRUE ~ "Other"
      )
    ) %>%
    arrange(desc(hof_probability))

  bbwaa_results <- ballot_predictions %>%
    filter(ballot_type == "BBWAA 2026") %>%
    dplyr::select(full_name, age_through, model_age, hof_probability, pred_hof, HR, H, OPS, award_share) %>%
    mutate(
      prob_pct = paste0(round(hof_probability * 100, 1), "%"),
      HR = round(HR),
      H = round(H),
      OPS = round(OPS, 3)
    )
  
  if (nrow(bbwaa_results) > 0) {
    print(bbwaa_results %>% 
            dplyr::select(full_name, age_through, model_age, prob_pct, pred_hof, HR, H, OPS) %>% 
            as.data.frame(), 
          row.names = FALSE)
    
    cat("\nPREDICTED HOF:\n")
    predicted_hof <- bbwaa_results %>% filter(pred_hof == "Y")
    if (nrow(predicted_hof) > 0) {
      cat("  ", paste(predicted_hof$full_name, collapse = ", "), "\n")
    } else {
      cat("  None predicted at model threshold\n")
    }
  } else {
    cat("No BBWAA 2026 candidates found in data.\n")
  }
 
  era_results <- ballot_predictions %>%
    filter(ballot_type == "Era Committee 2025") %>%
    dplyr::select(full_name, age_through, model_age, hof_probability, pred_hof, HR, H, OPS) %>%
    mutate(
      prob_pct = paste0(round(hof_probability * 100, 1), "%"),
      HR = round(HR),
      H = round(H),
      OPS = round(OPS, 3),
      actual_result = case_when(
        full_name == "Jeff Kent" ~ "ELECTED",
        TRUE ~ "Not Elected"
      )
    )
  
  if (nrow(era_results) > 0) {
    print(era_results %>% 
            dplyr::select(full_name, age_through, model_age, prob_pct, pred_hof, actual_result, HR, OPS) %>% 
            as.data.frame(), 
          row.names = FALSE)
  } else {
    cat("No Era Committee 2025 candidates found in data.\n")
  }
}

# =============================================================================
# ALL PLAYERS - TOP CANDIDATES (FROM SNAPSHOTS)
# =============================================================================

cat("\n")
cat("================================================================================\n")
cat("TOP 30 PLAYERS BY HOF PROBABILITY (Career-End Snapshots)\n")
cat("================================================================================\n\n")

cat("Generating predictions for", nrow(player_career_end), "players...\n\n")

all_predictions <- player_career_end %>%
  group_by(playerID) %>%
  filter(age_through == max(age_through)) %>%
  rowwise() %>%
  mutate(
    career_complete = (data_source == "snapshot"), 
    model_age = get_model_age(age_through, career_complete = career_complete),
    model_key = paste0("data_", model_age),
    hof_probability = {
      if (model_key %in% names(production_models)) {
        model_obj <- production_models[[model_key]]
        tryCatch(
          predict_hof_prob(cur_data(), model_obj),
          error = function(e) NA_real_
        )
      } else {
        NA_real_
      }
    }
  ) %>%
  ungroup() %>%
  filter(!is.na(hof_probability)) %>%
  mutate(full_name = paste(nameFirst, nameLast)) %>%
  arrange(desc(hof_probability))


not_inducted <- all_predictions %>% 
  filter(!ever_inducted & !(playerID %in% current_only$playerID))

committee_only <- all_predictions %>%
  filter(committee_inducted & !bbwaa_inducted)

cat("Player breakdown:\n")
cat("  BBWAA inducted:", sum(all_predictions$bbwaa_inducted), "\n")
cat("  Committee-only inducted:", nrow(committee_only), "\n
")
cat("  Not inducted:", nrow(not_inducted), "\n\n")

top_30 <- not_inducted %>%
  arrange(desc(hof_probability)) %>%
  head(30) %>%
  dplyr::select(full_name, age_through, model_age, hof_probability, HR, H, OPS, award_share) %>%
  mutate(
    prob_pct = paste0(round(hof_probability * 100, 1), "%"),
    HR = round(HR),
    H = round(H),
    OPS = round(OPS, 3)
  )

print(top_30 %>% 
        dplyr::select(full_name, age_through, model_age, prob_pct, HR, H, OPS, award_share) %>%
        as.data.frame(), 
      row.names = FALSE)


if (exists("ballot_predictions") && nrow(ballot_predictions) > 0) {
  ballot_output <- ballot_predictions %>%
    dplyr::select(
      playerID, full_name, ballot_type, age_through, model_age,
      hof_probability, pred_hof, threshold,
      HR, H, RBI, OPS, award_share
    )
  
  saveRDS(ballot_output, "data/predictions/ballot_2026_predictions.rds")
  write.csv(ballot_output, "data/predictions/ballot_2026_predictions.csv", row.names = FALSE)
  cat("Saved: data/predictions/ballot_2026_predictions.rds\n")
  cat("Saved: data/predictions/ballot_2026_predictions.csv\n")
}

all_output <- all_predictions %>%
  dplyr::select(
    playerID, full_name, age_through, model_age,
    hof_probability, 
    ever_inducted, bbwaa_inducted, committee_inducted, induction_method,
    HR, H, RBI, OPS, award_share
  )

saveRDS(all_predictions, "data/predictions/all_predictions.rds")
write.csv(all_predictions, "data/predictions/all_predictions.csv", row.names = FALSE)
cat("Saved: data/predictions/all_predictions.rds\n")
cat("Saved: data/predictions/all_predictions.csv\n")

cat("\n")
