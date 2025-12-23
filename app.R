# =============================================================================
# Baseball Hall of Fame Candidacy Evaluator
# =============================================================================

library(shiny)
library(shinyjs)
library(bslib)
library(ggplot2)
library(dplyr)
library(plotly)

# DATA LOADING
player_data <- read.csv("data/all_predictions.csv", stringsAsFactors = FALSE)
if ("X" %in% names(player_data)) player_data <- player_data %>% dplyr::select(-X)
player_data <- player_data %>% mutate(inducted_num = ifelse(inducted == 'Y', 1, 0))

hof_data <- read.csv("data/HOF_Players.csv", stringsAsFactors = FALSE)
if ("X" %in% names(hof_data)) hof_data <- hof_data %>% dplyr::select(-X)

# Load pre-computed team data (instead of Lahman - much faster!)
player_teams <- read.csv("data/player_teams.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(playerID, teamID)

# Join team data to player_data
player_data <- player_data %>%
  left_join(player_teams, by = "playerID")

# Team ID to full name mapping
team_names <- c(
  "ARI" = "Arizona Diamondbacks", "ATL" = "Atlanta Braves", "BAL" = "Baltimore Orioles",
  "BOS" = "Boston Red Sox", "CHN" = "Chicago Cubs", "CHA" = "Chicago White Sox",
  "CIN" = "Cincinnati Reds", "CLE" = "Cleveland Guardians", "COL" = "Colorado Rockies",
  "DET" = "Detroit Tigers", "HOU" = "Houston Astros", "KCA" = "Kansas City Royals",
  "LAA" = "Los Angeles Angels", "LAN" = "Los Angeles Dodgers", "MIA" = "Miami Marlins",
  "MIL" = "Milwaukee Brewers", "MIN" = "Minnesota Twins", "NYN" = "New York Mets",
  "NYA" = "New York Yankees", "OAK" = "Oakland Athletics", "PHI" = "Philadelphia Phillies",
  "PIT" = "Pittsburgh Pirates", "SDN" = "San Diego Padres", "SFN" = "San Francisco Giants",
  "SEA" = "Seattle Mariners", "SLN" = "St. Louis Cardinals", "TBA" = "Tampa Bay Rays",
  "TEX" = "Texas Rangers", "TOR" = "Toronto Blue Jays", "WAS" = "Washington Nationals"
)

# Add full team name to player_data
player_data <- player_data %>%
  mutate(team_full = team_names[teamID])

# Load headshots if available
headshots_path <- "mlb_headshots_with_bbref.csv"
if (file.exists(headshots_path)) {
  headshots_df <- read.csv(headshots_path, stringsAsFactors = FALSE) %>%
    dplyr::select(bbrefID, headshot_url) %>%
    distinct(bbrefID, .keep_all = TRUE) %>%
    mutate(playerID = bbrefID)
  player_data <- player_data %>%
    left_join(headshots_df, by = "playerID")
}

# LOAD PREDICTION MODELS
hof_model <- readRDS("data/models/production_models.rds")

# PRE-COMPUTE FILTERED DATASETS AT STARTUP (not reactively)
# This dramatically speeds up the app
current_players_df <- player_data %>%
  filter(data_source == "current") %>%
  group_by(playerID) %>%
  filter(age_through == max(age_through)) %>%
  ungroup() %>%
  arrange(nameLast, nameFirst)

hof_players_df <- player_data %>%
  filter(inducted_num == 1) %>%
  group_by(playerID) %>%
  filter(age_through == max(age_through)) %>%
  ungroup()

unique_players_df <- player_data %>%
  group_by(playerID) %>%
  filter(age_through == max(age_through)) %>%
  ungroup() %>%
  arrange(desc(hof_probability))

# Pre-compute player choices for dropdown
all_player_choices <- setNames(current_players_df$playerID, current_players_df$full_name)

# HELPER FUNCTIONS
format_prob <- function(p) paste0(round(p * 100, 1), "%")
format_number <- function(x, digits = 0) format(round(x, digits), big.mark = ",", nsmall = digits)

# Normalize probability so threshold = 50%
# Maps: 0 -> 0%, threshold -> 50%, 1 -> 100%
normalize_prob <- function(prob, threshold) {
  if (is.na(prob) || is.na(threshold) || threshold <= 0 || threshold >= 1) {
    return(prob)  # Return raw if invalid
  }
  if (prob >= threshold) {
    0.5 + 0.5 * (prob - threshold) / (1 - threshold)
  } else {
    0.5 * prob / threshold
  }
}

# Vectorized version for multiple probabilities
normalize_prob_vec <- function(probs, threshold) {
  sapply(probs, function(p) normalize_prob(p, threshold))
}

# CUSTOM CSS
custom_css <- '
* { margin: 0; padding: 0; box-sizing: border-box; }
body, .bslib-page-fill { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif !important; background: #0f1419 !important; color: #e7e9ea !important; min-height: 100vh; }
.dashboard-header { display: flex; justify-content: space-between; align-items: center; padding: 20px 30px; background: linear-gradient(135deg, #1a1f2e 0%, #16213e 100%); border-radius: 16px; margin-bottom: 24px; border: 1px solid #2a3441; }
.logo { display: flex; align-items: center; gap: 12px; }
.logo-icon { font-size: 32px; }
.logo-text h1 { font-size: 24px; font-weight: 700; background: linear-gradient(90deg, #418FDE, #7CB9E8); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text; margin: 0; }
.logo-text p { font-size: 12px; color: #8899a6; margin-top: 2px; margin-bottom: 0; }
.search-container { position: relative; width: 350px; }
.search-filters { display: flex; flex-direction: column; gap: 12px; align-items: flex-end; }
.filter-row { display: flex; gap: 12px; align-items: center; }
.filter-select { width: 200px; }
.filter-select .selectize-input { min-height: 40px !important; padding: 8px 12px !important; }
.filter-select.disabled .selectize-input { opacity: 0.35; pointer-events: none; background: #151c24 !important; }
.filter-type-group { display: flex; gap: 8px; }
.filter-type-group .btn-filter { padding: 8px 16px; background: #1e2732; border: 1px solid #2a3441; border-radius: 20px; color: #8899a6; font-size: 13px; font-weight: 500; cursor: pointer; transition: all 0.2s; }
.filter-type-group .btn-filter:hover { border-color: #418FDE; color: #e7e9ea; }
.filter-type-group .btn-filter.active { background: #418FDE; border-color: #418FDE; color: white; }
.filter-type-hidden { display: none; }
.selectize-input { background: #1e2732 !important; border: 2px solid #2a3441 !important; border-radius: 12px !important; color: #e7e9ea !important; padding: 12px 16px !important; min-height: 48px !important; }
.selectize-input input { color: #ffffff !important; }
.selectize-input.focus { border-color: #418FDE !important; box-shadow: none !important; }
.selectize-dropdown { background: #1e2732 !important; border: 1px solid #2a3441 !important; border-radius: 8px !important; color: #e7e9ea !important; }
.selectize-dropdown .option { color: #e7e9ea !important; padding: 10px 16px !important; }
.selectize-dropdown .option:hover, .selectize-dropdown .option.active { background: #418FDE !important; color: white !important; }
.main-grid { display: grid; grid-template-columns: 300px 1fr; gap: 24px; }
.player-sidebar { display: flex; flex-direction: column; gap: 20px; }
.player-card { background: linear-gradient(180deg, #1a1f2e 0%, #16213e 100%); border-radius: 16px; padding: 24px; border: 1px solid #2a3441; text-align: center; }
.player-photo { width: 140px; height: 140px; border-radius: 50%; background: linear-gradient(135deg, #418FDE 0%, #2a5a8c 100%); margin: 0 auto 16px; display: flex; align-items: center; justify-content: center; font-size: 48px; border: 4px solid #418FDE; box-shadow: 0 8px 32px rgba(65, 143, 222, 0.3); overflow: hidden; }
.player-photo img { width: 100%; height: 100%; object-fit: cover; display: block; }
.player-name { font-size: 22px; font-weight: 700; margin-bottom: 4px; color: #e7e9ea; }
.player-meta { color: #8899a6; font-size: 14px; margin-bottom: 16px; }
.player-tags { display: flex; justify-content: center; gap: 8px; flex-wrap: wrap; }
.tag { padding: 6px 12px; background: #1e2732; border-radius: 20px; font-size: 12px; font-weight: 600; }
.tag.position { color: #418FDE; border: 1px solid #418FDE; }
.tag.team { color: #8899a6; border: 1px solid #8899a6; }
.tag.hof { color: #C8102E; border: 1px solid #C8102E; }
.tag.era { color: #7CB9E8; border: 1px solid #7CB9E8; }
.stats-card { background: #16213e; border-radius: 16px; padding: 20px; border: 1px solid #2a3441; }
.stats-card h3 { font-size: 14px; color: #8899a6; margin-bottom: 16px; text-transform: uppercase; letter-spacing: 1px; font-weight: 600; }
.stat-row { display: flex; justify-content: space-between; padding: 10px 0; border-bottom: 1px solid #2a3441; }
.stat-row:last-child { border-bottom: none; }
.stat-label { color: #8899a6; font-size: 14px; }
.stat-value { font-weight: 700; font-size: 16px; color: #e7e9ea; }
.stat-value.highlight { color: #418FDE; }
.main-content { display: flex; flex-direction: column; gap: 24px; }
.probability-hero { background: linear-gradient(135deg, #1a1f2e 0%, #16213e 100%); border-radius: 16px; padding: 32px; border: 1px solid #2a3441; display: grid; grid-template-columns: 1fr 1fr; gap: 40px; }
.probability-main { display: flex; flex-direction: column; justify-content: center; }
.probability-label { font-size: 14px; color: #8899a6; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px; }
.probability-value { font-size: 72px; font-weight: 800; background: linear-gradient(90deg, #418FDE, #7CB9E8); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text; line-height: 1; margin-bottom: 16px; }
.probability-bar { height: 12px; background: #1e2732; border-radius: 6px; overflow: hidden; margin-bottom: 16px; }
.probability-fill { height: 100%; background: linear-gradient(90deg, #418FDE, #7CB9E8); border-radius: 6px; transition: width 1s ease; }
.verdict { display: inline-flex; align-items: center; gap: 8px; padding: 10px 20px; background: rgba(65, 143, 222, 0.15); border: 1px solid #418FDE; border-radius: 8px; color: #418FDE; font-weight: 600; font-size: 14px; width: fit-content; }
.verdict.positive { background: rgba(34, 197, 94, 0.15); border-color: #22c55e; color: #22c55e; }
.verdict.negative { background: rgba(200, 16, 46, 0.15); border-color: #C8102E; color: #C8102E; }
.model-info { display: flex; flex-direction: column; justify-content: center; gap: 16px; }
.model-detail { display: flex; justify-content: space-between; padding: 12px 16px; background: #1e2732; border-radius: 8px; }
.model-detail-label { color: #8899a6; font-size: 13px; }
.model-detail-value { font-weight: 600; font-size: 14px; color: #e7e9ea; }
.percentile-badge { text-align: center; padding: 20px; background: linear-gradient(135deg, rgba(65, 143, 222, 0.1) 0%, rgba(65, 143, 222, 0.05) 100%); border-radius: 12px; border: 1px solid rgba(65, 143, 222, 0.3); }
.percentile-label-top { font-size: 11px; color: #8899a6; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 4px; }
.percentile-value { font-size: 36px; font-weight: 800; color: #418FDE; }
.percentile-label { font-size: 12px; color: #8899a6; margin-top: 4px; }
.two-column { display: grid; grid-template-columns: 1fr 1fr; gap: 24px; }
.card { background: #16213e; border-radius: 16px; padding: 24px; border: 1px solid #2a3441; }
.card-header-custom { display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px; }
.card-title { font-size: 16px; font-weight: 600; color: #e7e9ea; margin: 0; }
.card-subtitle { font-size: 12px; color: #8899a6; margin: 0; }
.feature-list { display: flex; flex-direction: column; gap: 12px; }
.feature-item { display: flex; align-items: center; gap: 12px; }
.feature-icon { width: 28px; height: 28px; border-radius: 6px; display: flex; align-items: center; justify-content: center; font-size: 14px; font-weight: bold; }
.feature-icon.positive { background: rgba(34, 197, 94, 0.2); color: #22c55e; }
.feature-icon.negative { background: rgba(200, 16, 46, 0.2); color: #C8102E; }
.feature-info { flex: 1; }
.feature-name { font-size: 13px; font-weight: 500; color: #e7e9ea; }
.feature-bar-container { height: 6px; background: #1e2732; border-radius: 3px; margin-top: 4px; overflow: hidden; }
.feature-bar { height: 100%; border-radius: 3px; }
.feature-bar.positive { background: #22c55e; }
.feature-bar.negative { background: #C8102E; }
.feature-value-text { font-size: 13px; font-weight: 600; min-width: 50px; text-align: right; }
.feature-value-text.positive { color: #22c55e; }
.feature-value-text.negative { color: #C8102E; }
.similar-players { display: flex; flex-direction: column; gap: 10px; }
.similar-player { display: flex; align-items: center; gap: 12px; padding: 12px; background: #1a1f2e; border-radius: 10px; transition: all 0.2s ease; }
.similar-player:hover { background: #1e2732; }
.similar-rank { width: 24px; height: 24px; background: #418FDE; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 12px; font-weight: 700; color: white; }
.similar-info { flex: 1; }
.similar-name { font-size: 14px; font-weight: 600; color: #e7e9ea; }
.similar-stats { font-size: 11px; color: #8899a6; margin-top: 2px; }
.similar-match { font-size: 14px; font-weight: 700; color: #418FDE; }
.comparison-section { background: #16213e; border-radius: 16px; padding: 24px; border: 1px solid #2a3441; }
.career-section { margin-top: 24px; }
.career-section .card { background: #16213e; border-radius: 16px; padding: 24px; border: 1px solid #2a3441; }
.stat-comparison-card { min-height: 300px; }
.comparison-header { display: grid; grid-template-columns: 100px 1fr 80px 80px; gap: 16px; padding-bottom: 12px; border-bottom: 2px solid #2a3441; margin-bottom: 8px; }
.comparison-header span { font-size: 11px; color: #8899a6; text-transform: uppercase; letter-spacing: 0.5px; }
.comparison-row { display: grid; grid-template-columns: 100px 1fr 80px 80px; align-items: center; gap: 16px; padding: 14px 0; border-bottom: 1px solid #2a3441; }
.comparison-row:last-child { border-bottom: none; }
.comparison-stat-name { font-size: 13px; color: #8899a6; }
.comparison-bars { display: flex; flex-direction: column; gap: 4px; }
.comparison-bar-row { display: flex; align-items: center; gap: 8px; }
.comparison-bar-label { font-size: 10px; color: #8899a6; width: 60px; }
.comparison-bar-track { flex: 1; height: 8px; background: #1e2732; border-radius: 4px; overflow: hidden; }
.comparison-bar-fill { height: 100%; border-radius: 4px; }
.comparison-bar-fill.player { background: #418FDE; }
.comparison-bar-fill.avg-hof { background: #8899a6; }
.comparison-value { font-size: 14px; font-weight: 700; text-align: right; }
.comparison-value.player { color: #418FDE; }
.comparison-value.avg { color: #8899a6; }
.chart-container { height: 220px; background: #1a1f2e; border-radius: 12px; padding: 16px; }
.scatter-plot-container { height: 220px; background: #1a1f2e; border-radius: 12px; position: relative; overflow: hidden; }
.well { background: transparent !important; border: none !important; }
.form-group { margin-bottom: 0 !important; }
label { display: none !important; }
@media (max-width: 1200px) { .main-grid { grid-template-columns: 1fr; } .two-column { grid-template-columns: 1fr; } .probability-hero { grid-template-columns: 1fr; } }
'

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML(custom_css))),
  # JavaScript to handle filter button styling
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      // Initial state - By Player is active
      $('.btn-filter[data-value=\"player\"]').addClass('active');
      
      // Handle filter button clicks
      $(document).on('click', '.btn-filter', function() {
        var value = $(this).data('value');
        $('.btn-filter').removeClass('active');
        $(this).addClass('active');
        Shiny.setInputValue('filter_type', value);
      });
    });
  ")),
  div(class = "dashboard", style = "max-width: 1400px; margin: 0 auto; padding: 20px;",
    div(class = "dashboard-header",
      div(class = "logo", span(class = "logo-icon", HTML("&#127942;")),
          div(class = "logo-text", tags$h1(HTML("Hall of Fame Evaluator<br><span style='font-size: 14px; font-weight: 400;'>for Current MLB Players (as of 2024)</span>")))
      ),
      div(class = "search-filters",
          div(class = "filter-row",
              div(class = "filter-type-group",
                  tags$button(class = "btn-filter", `data-value` = "player", "By Player"),
                  tags$button(class = "btn-filter", `data-value` = "team", "By Team"),
                  tags$button(class = "btn-filter", `data-value` = "position", "By Position")
              )
          ),
          div(class = "filter-row",
              div(id = "team_filter_container", class = "filter-select disabled",
                  selectizeInput("team_filter", label = NULL, choices = NULL,
                                options = list(placeholder = "Select team..."))
              ),
              div(id = "position_filter_container", class = "filter-select disabled",
                  selectizeInput("position_filter", label = NULL, choices = NULL,
                                options = list(placeholder = "Select position..."))
              )
          ),
          div(class = "search-container",
              selectizeInput("player_search", label = NULL, choices = NULL, 
                            options = list(placeholder = "Search player by name..."))
          )
      )
    ),
    div(class = "main-grid",
      tags$aside(class = "player-sidebar",
        div(
          class = "player-card",
          uiOutput("player_photo"),  # <-- NEW (replaces emoji)
          div(class = "player-name", textOutput("selected_player_name", inline = TRUE)),
          div(class = "player-meta", textOutput("player_years", inline = TRUE)),
          div(class = "player-tags", uiOutput("player_tags"))
        ),
        div(class = "stats-card", tags$h3(textOutput("stats_header", inline = TRUE)), uiOutput("career_stats"))
#        div(class = "stats-card", tags$h3("Era Context"), uiOutput("era_context"))
      ),
      tags$main(class = "main-content",
        tags$section(class = "probability-hero",
          div(class = "probability-main",
              span(class = "probability-label", "Hall of Fame Probability"),
              div(class = "probability-value", textOutput("prob_value", inline = TRUE)),
              div(class = "probability-bar", uiOutput("prob_bar")),
              uiOutput("verdict_badge")
          ),
          div(class = "model-info",
              div(class = "model-detail", span(class = "model-detail-label", "Model Used"), span(class = "model-detail-value", textOutput("model_age", inline = TRUE))),
              div(class = "model-detail", span(class = "model-detail-label", "Raw Threshold"), span(class = "model-detail-value", textOutput("threshold_display", inline = TRUE))),
              div(class = "model-detail", span(class = "model-detail-label", "Confidence Level"), uiOutput("confidence_level")),
              div(class = "percentile-badge",
                  div(class = "percentile-label-top", "Percentile Rank"),
                  div(class = "percentile-value", textOutput("percentile_value", inline = TRUE)),
                  div(class = "percentile-label", "among qualified players at same age")
              )
          )
        ),
        tags$section(class = "two-column",
          div(class = "card",
              div(class = "card-header-custom",
                  div(tags$h3(class = "card-title", "OPS vs Career Hits"),
                      tags$p(class = "card-subtitle", "Player position relative to HOFers"))
              ),
              div(class = "chart-container", plotOutput("distribution_plot", height = "200px"))
          ),
          div(class = "card",
              div(class = "card-header-custom",
                  div(tags$h3(class = "card-title", "Historical Comparison"),
                      tags$p(class = "card-subtitle", "HR vs AVG across all players"))
              ),
              div(class = "scatter-plot-container", plotOutput("scatter_plot", height = "220px"))
          )
        ),
        tags$section(class = "two-column",
          div(class = "card stat-comparison-card",
              div(class = "card-header-custom",
                  div(tags$h3(class = "card-title", "Statistical Comparison"),
                      tags$p(class = "card-subtitle", "Player stats vs HOFers at same age"))
              ),
              div(class = "comparison-header", span("Statistic"), span("Comparison"), span("Player"), span("Avg HOF")),
              uiOutput("stat_comparison")
          ),
          div(class = "card",
              div(class = "card-header-custom",
                  div(tags$h3(class = "card-title", "Most Similar Players"),
                      tags$p(class = "card-subtitle", "Players with comparable stats at same age"))
              ),
              uiOutput("similar_players")
          )
        ),
        tags$section(class = "career-section",
          div(class = "card",
              div(class = "card-header-custom",
                  div(tags$h3(class = "card-title", "Career Probability Trajectory"),
                      tags$p(class = "card-subtitle", "HOF probability throughout player's career"))
              ),
              div(class = "chart-container", plotlyOutput("career_trajectory", height = "250px"))
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  # Use pre-computed data (no reactive filtering needed)
  unique_players <- reactive({ unique_players_df })
  current_players <- reactive({ current_players_df })
  hof_players_data <- reactive({ hof_players_df })

  # Initialize filter type (default: "player")
  filter_type <- reactiveVal("player")
  
  # ALL teams available (alphabetical) - static list
  all_teams <- sort(c(
    "Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox",
    "Chicago Cubs", "Chicago White Sox", "Cincinnati Reds", "Cleveland Guardians",
    "Colorado Rockies", "Detroit Tigers", "Houston Astros", "Kansas City Royals",
    "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", "Milwaukee Brewers",
    "Minnesota Twins", "New York Mets", "New York Yankees", "Oakland Athletics",
    "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres", 
    "San Francisco Giants", "Seattle Mariners", "St. Louis Cardinals",
    "Tampa Bay Rays", "Texas Rangers", "Toronto Blue Jays", "Washington Nationals"
  ))
  
  # ALL positions available - static list
  all_positions <- c("C", "1B", "2B", "3B", "SS", "OF")
  
  # Initialize dropdowns ONCE on startup
  updateSelectizeInput(session, "team_filter", 
                      choices = c("Select a team..." = "", all_teams),
                      selected = "")
  updateSelectizeInput(session, "position_filter",
                      choices = c("Select a position..." = "", all_positions),
                      selected = "")
  updateSelectizeInput(session, "player_search", 
                      choices = all_player_choices, 
                      selected = all_player_choices[1], 
                      server = TRUE)
  
  # Update filter type when button clicked AND clear previous selections
  observeEvent(input$filter_type, {
    ft <- input$filter_type
    filter_type(ft)
    
    # Clear team and position selections when switching modes
    updateSelectizeInput(session, "team_filter", selected = "")
    updateSelectizeInput(session, "position_filter", selected = "")
    
    # Reset player list to full list
    updateSelectizeInput(session, "player_search", 
                        choices = all_player_choices, 
                        selected = all_player_choices[1], 
                        server = TRUE)
    
    # Update disabled states
    if (ft == "player") {
      runjs("$('#team_filter_container').addClass('disabled');")
      runjs("$('#position_filter_container').addClass('disabled');")
    } else if (ft == "team") {
      runjs("$('#team_filter_container').removeClass('disabled');")
      runjs("$('#position_filter_container').addClass('disabled');")
    } else if (ft == "position") {
      runjs("$('#team_filter_container').addClass('disabled');")
      runjs("$('#position_filter_container').removeClass('disabled');")
    }
  }, ignoreInit = TRUE)
  
  # Filter players when team is selected
  observeEvent(input$team_filter, {
    req(input$team_filter != "")
    if (filter_type() != "team") return()
    
    filtered <- current_players_df %>% 
      filter(team_full == input$team_filter) %>%
      arrange(nameLast, nameFirst)
    
    choices <- setNames(filtered$playerID, filtered$full_name)
    if (length(choices) > 0) {
      updateSelectizeInput(session, "player_search", choices = choices, 
                          selected = choices[1], server = TRUE)
    }
  }, ignoreInit = TRUE)
  
  # Filter players when position is selected
  observeEvent(input$position_filter, {
    req(input$position_filter != "")
    if (filter_type() != "position") return()
    
    filtered <- current_players_df %>% 
      filter(POS == input$position_filter) %>%
      arrange(nameLast, nameFirst)
    
    choices <- setNames(filtered$playerID, filtered$full_name)
    if (length(choices) > 0) {
      updateSelectizeInput(session, "player_search", choices = choices, 
                          selected = choices[1], server = TRUE)
    }
  }, ignoreInit = TRUE)

  selected_player <- reactive({
    req(input$player_search)
    result <- player_data %>% filter(playerID == input$player_search) %>% filter(age_through == max(age_through))
    if (nrow(result) > 0) result[1, ] else NULL
  })

  # ---- NEW: Render headshot above player name --------------------------------
  output$player_photo <- renderUI({
    req(selected_player())
    p <- selected_player()

    url <- NULL
    if ("headshot_url" %in% names(p)) {
      url <- p$headshot_url
    }

    # If URL exists, show image inside the same styled circle
    if (!is.null(url) && !is.na(url) && nchar(url) > 0) {
      div(
        class = "player-photo",
        tags$img(src = url, alt = p$full_name, loading = "lazy")
      )
    } else {
      # Fallback (keeps your original look)
      div(class = "player-photo", HTML("&#127919;"))
    }
  })

  output$selected_player_name <- renderText({ req(selected_player()); selected_player()$full_name })
  output$player_years <- renderText({ req(selected_player()); p <- selected_player(); paste0(p$Years, " seasons through age ", p$age_through) })

  output$player_tags <- renderUI({
    req(selected_player()); p <- selected_player()
    tags_list <- list(span(class = "tag position", p$POS))
    # Add team if available
    if (!is.null(p$teamID) && !is.na(p$teamID) && p$teamID != "") {
      tags_list <- c(tags_list, list(span(class = "tag team", p$teamID)))
    }
    if (p$inducted_num == 1) tags_list <- c(tags_list, list(span(class = "tag hof", "HOF")))
    do.call(tagList, tags_list)
  })

  output$stats_header <- renderText({ req(selected_player()); paste0("Career Stats (Age ", selected_player()$age_through, ")") })

  output$career_stats <- renderUI({
    req(selected_player()); p <- selected_player(); hof_avg <- hof_players_data()
    tagList(
      div(class = "stat-row", span(class = "stat-label", "Home Runs"), span(class = paste("stat-value", if(p$HR > mean(hof_avg$HR, na.rm=TRUE)) "highlight" else ""), format_number(p$HR))),
      div(class = "stat-row", span(class = "stat-label", "Hits"), span(class = paste("stat-value", if(p$H > mean(hof_avg$H, na.rm=TRUE)) "highlight" else ""), format_number(p$H))),
      div(class = "stat-row", span(class = "stat-label", "RBI"), span(class = paste("stat-value", if(p$RBI > mean(hof_avg$RBI, na.rm=TRUE)) "highlight" else ""), format_number(p$RBI))),
      div(class = "stat-row", span(class = "stat-label", "OPS"), span(class = paste("stat-value", if(p$OPS > mean(hof_avg$OPS, na.rm=TRUE)) "highlight" else ""), format_number(p$OPS, 3))),
      div(class = "stat-row", span(class = "stat-label", "Batting Avg"), span(class = paste("stat-value", if(p$AVG > mean(hof_avg$AVG, na.rm=TRUE)) "highlight" else ""), format_number(p$AVG, 3))),
      div(class = "stat-row", span(class = "stat-label", "Award Share"), span(class = paste("stat-value", if(p$award_share > mean(hof_avg$award_share, na.rm=TRUE)) "highlight" else ""), format_number(p$award_share, 2))),
      div(class = "stat-row", span(class = "stat-label", "Games"), span(class = "stat-value", format_number(p$G)))
    )
  })

#  output$era_context <- renderUI({
#    req(selected_player()); p <- selected_player()
#    tagList(
 #     div(class = "stat-row", span(class = "stat-label", "Era"), span(class = "stat-value", gsub(" \\(.*\\)", "", p$Era))),
  #    div(class = "stat-row", span(class = "stat-label", "Position Difficulty"), span(class = "stat-value", paste0(p$pos_difficulty, "/5"))),
   #   div(class = "stat-row", span(class = "stat-label", "Pos-Adj OPS"), span(class = "stat-value highlight", format_number(p$pos_adj_ops, 3))),
    #  div(class = "stat-row", span(class = "stat-label", "Era-Adj HR"), span(class = "stat-value", format_number(p$era_adj_hr, 0)))
  #  )
 # })

  output$prob_value <- renderText({
    req(selected_player()); p <- selected_player()
    model_key <- paste0("data_", p$model_age)
    threshold <- if (model_key %in% names(hof_model)) hof_model[[model_key]]$threshold else 0.5
    norm_prob <- normalize_prob(p$hof_probability, threshold)
    format_prob(norm_prob)
  })

  output$prob_bar <- renderUI({
    req(selected_player()); p <- selected_player()
    model_key <- paste0("data_", p$model_age)
    threshold <- if (model_key %in% names(hof_model)) hof_model[[model_key]]$threshold else 0.5
    norm_prob <- normalize_prob(p$hof_probability, threshold)
    div(class = "probability-fill", style = paste0("width: ", norm_prob * 100, "%;"))
  })

  output$model_age <- renderText({ req(selected_player()); paste0("Age ", selected_player()$model_age, " Snapshot") })

  output$threshold_display <- renderText({
    req(selected_player()); p <- selected_player()
    model_key <- paste0("data_", p$model_age)
    threshold <- if (model_key %in% names(hof_model)) hof_model[[model_key]]$threshold else 0.5
    paste0(round(threshold * 100, 1), "%")
  })

  output$verdict_badge <- renderUI({
    req(selected_player()); p <- selected_player()
    model_key <- paste0("data_", p$model_age)
    threshold <- if (model_key %in% names(hof_model)) hof_model[[model_key]]$threshold else 0.5
    norm_prob <- normalize_prob(p$hof_probability, threshold)
    if (norm_prob >= 0.5) { div(class = "verdict positive", span(HTML("&#10003;")), span("PROJECTED HALL OF FAMER")) }
    else if (norm_prob >= 0.35) { div(class = "verdict", span(HTML("&#8776;")), span("BORDERLINE CANDIDATE")) }
    else { div(class = "verdict negative", span(HTML("&#10007;")), span("BELOW HOF THRESHOLD")) }
  })

  output$confidence_level <- renderUI({
    req(selected_player()); p <- selected_player()
    model_key <- paste0("data_", p$model_age)
    threshold <- if (model_key %in% names(hof_model)) hof_model[[model_key]]$threshold else 0.5
    norm_prob <- normalize_prob(p$hof_probability, threshold)
    distance_from_threshold <- abs(norm_prob - 0.5)
    conf <- if(distance_from_threshold >= 0.35) "HIGH" else if(distance_from_threshold >= 0.2) "MEDIUM" else "LOW"
    direction <- if(norm_prob >= 0.5) "above" else "below"
    span(class = "model-detail-value", paste0(conf, " (", round(norm_prob * 100, 1), "% ", direction, " 50%)"))
  })

  output$percentile_value <- renderText({
    req(selected_player()); p <- selected_player()
    model_key <- paste0("data_", p$model_age)
    threshold <- if (model_key %in% names(hof_model)) hof_model[[model_key]]$threshold else 0.5
    qualified_same_age <- player_data %>%
      filter(age_through == p$age_through, G >= 500)
    if (nrow(qualified_same_age) < 10) {
      qualified_same_age <- player_data %>% filter(age_through == p$age_through)
    }
    qualified_norm_probs <- normalize_prob_vec(qualified_same_age$hof_probability, threshold)
    player_norm_prob <- normalize_prob(p$hof_probability, threshold)
    pct <- round(sum(qualified_norm_probs < player_norm_prob) / length(qualified_norm_probs) * 100)
    suffix <- if (pct %% 100 %in% c(11, 12, 13)) {
      "th"
    } else if (pct %% 10 == 1) {
      "st"
    } else if (pct %% 10 == 2) {
      "nd"
    } else if (pct %% 10 == 3) {
      "rd"
    } else {
      "th"
    }
    paste0(pct, suffix)
  })

  output$distribution_plot <- renderPlot({
    req(selected_player()); p <- selected_player()
    plot_data <- player_data %>% filter(age_through == p$age_through)
    ggplot(plot_data, aes(x = OPS, y = H, color = factor(inducted_num))) +
      geom_point(alpha = 0.5, size = 2) +
      geom_point(data = p, aes(x = OPS, y = H), color = "#22c55e", size = 6, shape = 18) +
      scale_color_manual(values = c("0" = "#C8102E", "1" = "#418FDE"), labels = c("Non-HOF", "HOF")) +
      labs(x = "OPS", y = "Hits", color = NULL, title = paste0("Players at Age ", p$age_through)) +
      theme_minimal() + theme(plot.background = element_rect(fill = "#1a1f2e", color = NA),
                             panel.background = element_rect(fill = "#1a1f2e", color = NA),
                             panel.grid.major = element_line(color = "#2a3441", linewidth = 0.3),
                             panel.grid.minor = element_blank(),
                             axis.text = element_text(color = "#8899a6", size = 10),
                             axis.title = element_text(color = "#8899a6", size = 11),
                             legend.position = "bottom",
                             legend.background = element_rect(fill = "#1a1f2e", color = NA),
                             legend.text = element_text(color = "#8899a6", size = 10),
                             legend.key = element_rect(fill = "#1a1f2e"),
                             plot.title = element_text(color = "#8899a6", size = 11, hjust = 0.5))
  }, bg = "#1a1f2e")

  output$scatter_plot <- renderPlot({
    req(selected_player()); p <- selected_player()
    plot_data <- player_data %>% filter(age_through == p$age_through)
    ggplot(plot_data, aes(x = HR, y = AVG, color = factor(inducted_num))) +
      geom_point(alpha = 0.8, size = 2) +
      geom_point(data = p, aes(x = HR, y = AVG), color = "#22c55e", size = 7, shape = 18) +
      scale_color_manual(values = c("0" = "#C8102E", "1" = "#418FDE"), labels = c("Non-HOF", "HOF")) +
      labs(x = "Home Runs", y = "Batting Average", color = NULL, title = paste0("Players at Age ", p$age_through)) +
      theme_minimal() + theme(plot.background = element_rect(fill = "#1a1f2e", color = NA),
                             panel.background = element_rect(fill = "#1a1f2e", color = NA),
                             panel.grid.major = element_line(color = "#2a3441", linewidth = 0.3),
                             panel.grid.minor = element_blank(),
                             axis.text = element_text(color = "#8899a6", size = 10),
                             axis.title = element_text(color = "#8899a6", size = 11),
                             legend.position = "bottom",
                             legend.background = element_rect(fill = "#1a1f2e", color = NA),
                             legend.text = element_text(color = "#8899a6", size = 10),
                             legend.key = element_rect(fill = "#1a1f2e"),
                             plot.title = element_text(color = "#8899a6", size = 11, hjust = 0.5))
  }, bg = "#1a1f2e")

  output$similar_players <- renderUI({
    req(selected_player()); p <- selected_player()
    same_age_players <- player_data %>%
      filter(age_through == p$age_through, playerID != p$playerID)
    if (nrow(same_age_players) < 5) return(div(class = "similar-players", tags$p("Not enough players found at this age.")))

    all_for_pca <- bind_rows(p, same_age_players)

    pca_features <- c("HR", "H", "RBI", "OPS", "AVG", "SB", "BB", "G", "award_share", "hr_per_ab", "rbi_per_ab", "OPS_z")
    pca_data <- all_for_pca %>% dplyr::select(all_of(pca_features))
    pca_data <- pca_data %>% mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

    pca_result <- prcomp(pca_data, scale. = TRUE, center = TRUE)
    pca_scores <- as.data.frame(pca_result$x)

    player_pca <- pca_scores[1, ]

    distances <- apply(pca_scores[-1, 1:5], 1, function(row) {
      sqrt(sum((row - as.numeric(player_pca[1:5]))^2))
    })

    same_age_players$pca_distance <- distances
    closest_players <- same_age_players %>%
      arrange(pca_distance) %>%
      head(5) %>%
      mutate(similarity = 1 / (1 + pca_distance) * 100)

    similar_items <- lapply(1:nrow(closest_players), function(i) {
      sp <- closest_players[i, ]
      hof_indicator <- if(sp$inducted_num == 1) " ⭐" else ""
      div(class = "similar-player",
          div(class = "similar-rank", i),
          div(class = "similar-info",
              div(class = "similar-name", paste0(sp$full_name, hof_indicator)),
              div(class = "similar-stats", paste0(sp$POS, " • ", format_number(sp$HR), " HR • ", format_number(sp$OPS, 3), " OPS • ", format_number(sp$H), " H"))
          ),
          div(class = "similar-match", paste0(round(sp$similarity), "%"))
      )
    })
    div(class = "similar-players",
        do.call(tagList, similar_items),
        div(class = "similar-legend", style = "margin-top: 12px; font-size: 12px; color: #8899a6;", "⭐ = Hall of Famer"))
  })

  output$stat_comparison <- renderUI({
    req(selected_player()); p <- selected_player()
    player_age <- as.integer(p$age_through)

    hof_same_age <- player_data[player_data$inducted_num == 1 & player_data$age_through == player_age, ]
    if (nrow(hof_same_age) < 3) {
      hof_same_age <- player_data[player_data$inducted_num == 1 & abs(player_data$age_through - player_age) <= 2, ]
    }

    all_same_age <- player_data[player_data$age_through == player_age, ]

    stats <- list(
      list(name = "Home Runs", player_val = p$HR, avg_val = mean(hof_same_age$HR, na.rm=TRUE), max_val = max(all_same_age$HR, na.rm=TRUE), digits = 0),
      list(name = "Hits", player_val = p$H, avg_val = mean(hof_same_age$H, na.rm=TRUE), max_val = max(all_same_age$H, na.rm=TRUE), digits = 0),
      list(name = "OPS", player_val = p$OPS, avg_val = mean(hof_same_age$OPS, na.rm=TRUE), max_val = max(all_same_age$OPS, na.rm=TRUE), digits = 3),
      list(name = "Award Share", player_val = p$award_share, avg_val = mean(hof_same_age$award_share, na.rm=TRUE), max_val = max(all_same_age$award_share, na.rm=TRUE), digits = 2),
      list(name = "Games", player_val = p$G, avg_val = mean(hof_same_age$G, na.rm=TRUE), max_val = max(all_same_age$G, na.rm=TRUE), digits = 0)
    )
    short_name <- tail(strsplit(p$full_name, " ")[[1]], 1)
    comp_rows <- lapply(stats, function(s) {
      div(class = "comparison-row",
          div(class = "comparison-stat-name", s$name),
          div(class = "comparison-bars",
              div(class = "comparison-bar-row", span(class = "comparison-bar-label", short_name),
                  div(class = "comparison-bar-track",
                      div(class = "comparison-bar-fill player", style = paste0("width: ", min(s$player_val / s$max_val * 100, 100), "%;"))
                  )
              ),
              div(class = "comparison-bar-row", span(class = "comparison-bar-label", "Avg HOF"),
                  div(class = "comparison-bar-track",
                      div(class = "comparison-bar-fill avg-hof", style = paste0("width: ", min(s$avg_val / s$max_val * 100, 100), "%;"))
                  )
              )
          ),
          div(class = "comparison-value player", format_number(s$player_val, s$digits)),
          div(class = "comparison-value avg", format_number(s$avg_val, s$digits))
      )
    })
    do.call(tagList, comp_rows)
  })

  output$career_trajectory <- renderPlotly({
    req(selected_player())
    p <- selected_player()

    model_key <- paste0("data_", p$model_age)
    threshold <- if (model_key %in% names(hof_model)) hof_model[[model_key]]$threshold else 0.5

    career_data <- player_data %>%
      filter(playerID == p$playerID) %>%
      arrange(age_through) %>%
      dplyr::select(age_through, hof_probability)

    if (nrow(career_data) < 2) {
      return(plot_ly() %>%
               layout(
                 annotations = list(text = "Insufficient career data", showarrow = FALSE,
                                    font = list(color = "#8899a6", size = 14)),
                 paper_bgcolor = "#192734", plot_bgcolor = "#192734"
               ))
    }

    career_data$norm_prob <- normalize_prob_vec(career_data$hof_probability, threshold)
    career_data$norm_prob_pct <- round(career_data$norm_prob * 100, 1)
    career_data$is_current <- career_data$age_through == p$age_through

    career_data$hover_text <- paste0(
      "Age: ", career_data$age_through, "<br>",
      "HOF Probability: ", career_data$norm_prob_pct, "%"
    )

    plot_ly(career_data, x = ~age_through, y = ~norm_prob_pct) %>%
      add_trace(
        y = rep(50, nrow(career_data)),
        type = "scatter", mode = "lines",
        line = list(color = "#f39c12", dash = "dash", width = 1.5),
        hoverinfo = "none", showlegend = FALSE
      ) %>%
      add_trace(
        type = "scatter", mode = "lines",
        line = list(color = "#418FDE", width = 3),
        hoverinfo = "none", showlegend = FALSE
      ) %>%
      add_trace(
        data = career_data %>% filter(!is_current),
        type = "scatter", mode = "markers",
        marker = list(color = "#418FDE", size = 10, line = list(color = "#418FDE", width = 2)),
        text = ~hover_text, hoverinfo = "text", showlegend = FALSE
      ) %>%
      add_trace(
        data = career_data %>% filter(is_current),
        type = "scatter", mode = "markers",
        marker = list(color = "#C8102E", size = 16, line = list(color = "#C8102E", width = 2)),
        text = ~hover_text, hoverinfo = "text", showlegend = FALSE
      ) %>%
      add_annotations(
        x = min(career_data$age_through) + 0.5, y = 54,
        text = "Threshold (50%)", showarrow = FALSE,
        font = list(color = "#f39c12", size = 11)
      ) %>%
      layout(
        xaxis = list(
          title = list(text = "Age", font = list(color = "#8899a6")),
          tickfont = list(color = "#8899a6"),
          gridcolor = "#2d3e50",
          linecolor = "#2d3e50"
        ),
        yaxis = list(
          title = list(text = "HOF Probability (%)", font = list(color = "#8899a6")),
          tickfont = list(color = "#8899a6"),
          gridcolor = "#2d3e50",
          linecolor = "#2d3e50",
          range = c(0, 100),
          tickvals = c(0, 25, 50, 75, 100)
        ),
        paper_bgcolor = "#192734",
        plot_bgcolor = "#192734",
        margin = list(l = 50, r = 20, t = 20, b = 50),
        hovermode = "closest"
      ) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)