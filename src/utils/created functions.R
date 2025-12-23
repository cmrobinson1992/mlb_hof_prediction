get_dynamic_peak <- function(df) {
  df <- df %>% arrange(age)
  n <- nrow(df)
  
  if (n == 0) return(tibble())
  
  peak_data <- vector("list", n)
  
  # For each age in the player's career
  for (i in seq_len(n)) {
    # Only consider career data UP TO current age (rows 1 to i)
    career_so_far <- df[1:i, ]
    n_years <- nrow(career_so_far)
    
    # If player has played 7+ years, try all 7-year windows
    # If player has played < 7 years, only option is all years so far
    if (n_years <= 7) {
      # Career is 7 years or less - use entire career as peak
      best_window <- career_so_far
    } else {
      # Career > 7 years - find best 7-year window
      best_ops <- -Inf
      best_window <- NULL
      
      # Try all possible 7-year windows in career so far
      for (start_idx in 1:(n_years - 6)) {
        end_idx <- start_idx + 6  # 7 years total (inclusive)
        window_df <- career_so_far[start_idx:end_idx, ]
        
        ab_total <- sum(window_df$AB, na.rm = TRUE)
        pa_total <- sum(window_df$AB + window_df$BB + window_df$IBB + 
                       window_df$HBP + window_df$SF, na.rm = TRUE)
        
        if (ab_total == 0 || pa_total == 0) next
        
        # Calculate OPS for this window
        obp <- sum(window_df$H + window_df$BB + window_df$IBB + window_df$HBP, 
                   na.rm = TRUE) / pa_total
        slg <- sum(window_df$X1B + 2 * window_df$X2B + 3 * window_df$X3B + 
                   4 * window_df$HR, na.rm = TRUE) / ab_total
        ops <- obp + slg
        
        # Keep track of best window
        if (ops > best_ops) {
          best_ops <- ops
          best_window <- window_df
        }
      }
    }
    
    # Skip if no valid window was found
    if (is.null(best_window) || nrow(best_window) == 0) {
      next
    }
    
    # Calculate cumulative stats for the best window
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
        peak_years = n()  # How many years in the peak window
      )
    
    peak_data[[i]] <- cbind(
      tibble(playerID = df$playerID[i], age = df$age[i]), 
      peak_row
    )
  }
  
  bind_rows(peak_data)
}