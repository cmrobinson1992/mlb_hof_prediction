# Baseball Hall of Fame Prediction Model
→ [Try the Interactive Dashboard] (https://christian-robinson.shinyapps.io/mlb_hof_predictor/)
→ [View the Code] (https://github.com/cmrobinson1992/mlb_hof_prediction/tree/main/src)

# What This Project Does
This project predicts a position player's probability of induction into the Baseball Hall of Fame by comparing their production against historical players at the same age.
The core problem: How do you evaluate an active player's HOF candidacy when their career isn't finished?
You can't compare a 28-year-old's stats to a retired player's full career. That's not a fair comparison; the retired player had 10+ more years to accumulate numbers. The only fair comparison is production given equal opportunity. A 28-year-old today gets compared to what HOFers looked like at 28. A 33-year-old gets compared to HOFers at 33.
The dashboard lets you select any current player and see how their trajectory stacks up against historical candidates at the same career stage.

# Why Age-Specific Models?
Career stats alone can't answer the question "Is this player on a Hall of Fame trajectory?"
Consider two players:

Player A: 250 HR, 1800 hits at age 30
Player B: 250 HR, 1800 hits at age 38

Same counting stats. Completely different trajectories. Player A is a near-lock if they stay healthy. Player B probably isn't getting in.
The difference is production given opportunity. Player A accumulated those numbers in fewer years, which means higher peak performance, more awards relative to seasons played, and more projected career value.
By training separate models at ages 25, 28, 30, 33, 35, and 40, we capture what "HOF-caliber production" looks like at each career stage. A player's probability updates as they age—not because the model changes, but because the benchmark changes.

# Project Structure
mlb_hof_prediction/
├── app.R                # Shiny dashboard application
├── src/                 # Source scripts
│   ├── collect_stats.R          # Data collection from Lahman
│   ├── stats_features.R         # Feature engineering
│   ├── feature_engineering.R    # Canonical transformations (shared module)
│   ├── model_tuning_training.R  # Cross-validation & hyperparameter tuning
│   ├── train_production_models.R # Final model training
│   ├── predict_players.R        # Generate predictions
│   ├── position_bias.R          # Position effect analysis
│   ├── team_bias.R              # Market size analysis
│   └── feature_importance.R     # Coefficient visualization
├── data/                # Processed datasets & age snapshots
├── models/              # Trained production models (.rds)
└── output/              # Visualizations & prediction results

# The Pipeline
**1. Data Collection**
Pulls batting, fielding, and awards data from the Lahman database.

Minimum 1000 career ABs- filters out cup-of-coffee players
Position players only- pitchers excluded (different evaluation criteria)
Cumulative stats by age- the key transformation for age-specific comparison
Peak 7-year window- captures prime performance independent of longevity
PED flags- suspensions and Mitchell Report mentions

**2. Feature Engineering**
The features are designed to capture production relative to opportunity:
CategoryFeaturesWhy It MattersRate Statshr_per_ab, rbi_per_ab, bb_per_ab, ebh_rateEfficiency independent of playing timePer-Year Statshr_per_year, hits_per_year, awards_per_yearProduction densityPosition Adjustmentspos_difficulty, pos_adj_opsFair comparison across positionsEra Adjustmentsera_adj_hr, era_adj_ops, neutralized_HFair comparison across erasRecognitionaward_shareAwards won / awards available through that ageComposite Scoresoffensive_value, career_achievement, efficiency_scoreMulti-dimensional summaries
award_share is particularly important—it measures recognition relative to opportunity. A player who won 3 MVP awards in 8 seasons has a higher award share than one who won 3 in 15 seasons.

**3. Model Training**
10-fold CV repeated 3 times
SMOTE/upsampling applied inside CV folds (prevents data leakage)
Optimized for Balanced Accuracy
Threshold optimization using Youden's J

Elastic net (glmnet) won over GBM, XGBoost, SVM, and Random Forest—interpretable, robust, and handles correlated features gracefully.
AgeCV Balanced AccCV ROC-AUC250.8820.935280.9250.970300.9340.977330.9380.980350.9440.982400.9590.984
Earlier ages have lower accuracy because there's more uncertainty—careers can diverge significantly after age 25. By age 35, the signal is much clearer.
4. Prediction
For a current player:

Get their cumulative stats through their current age
Select the appropriate age model
Apply identical feature engineering as training
Compare against historical players at that same age
Generate probability

The probability answers: "Among players who looked like this at age X, what percentage made the Hall of Fame?"

# Key Findings
What Predicts HOF Induction?
Strongest positive effects:

career_achievement — sustained production relative to years played
award_share — recognition density (awards per opportunity)
pos_adj_ops — production adjusted for positional difficulty

Strongest negative effects:

PEDSusp — confirmed PED suspensions
PEDMitchell — Mitchell Report inclusion
so_per_ab — high strikeout rate (historical voter bias)

# Position Bias
Chi-square test shows significant association between position and HOF rate (p < 0.001). Catchers and shortstops have higher induction rates than first basemen after controlling for offensive production—the "premium position" bias is real.
Market Size Bias
Large-market teams show higher raw HOF rates. But after controlling for player statistics, team success, and era, the effect shrinks substantially. Most of the "Yankees effect" is explained by championships, not market visibility.

# The Dashboard
The interactive Shiny app lets you:

Select any current or historical player
View their HOF probability at each age snapshot
See how their trajectory compares to historical HOFers
Explore 2026 ballot candidates and their probabilities


# Data Sources

Lahman Database — MLB statistics (1871-present)
Hall of Fame voting records — BBWAA and Committee votes
PED suspensions — manually compiled from MLB announcements
Mitchell Report — 2007 investigation player list


# Requirements
r# Core
library(tidyverse)
library(Lahman)

# Modeling
library(caret)
library(glmnet)
library(pROC)

# Dashboard
library(shiny)

# Visualization
library(ggplot2)
library(patchwork)

Links

Live App: https://christian-robinson.shinyapps.io/mlb_hof_predictor/
GitHub: https://github.com/cmrobinson1992/mlb_hof_prediction
