# NFL Receiver Route Q-Learning — Shiny app. See README.md for full documentation.

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
library(data.table)
library(patchwork)
library(stringr)

source("nfl_receiver_rl.R")

# ============================================================
# CONFIGURATION — add more weeks here as needed
# ============================================================

WEEK_FILES <- list(
  "Week 1" = "week1.csv",
  "Week 2" = "week2.csv",
  "Week 3" = "week3.csv",
  "Week 4" = "week4.csv",
  "Week 5" = "week5.csv",
  "Week 6" = "week6.csv",
  "Week 7" = "week7.csv",
  "Week 8" = "week8.csv"
)

# ============================================================
# LOGO HELPERS
# ============================================================

# ESPN team logo URL — works reliably with team abbreviation
team_logo_url <- function(team_abbr) {
  sprintf("https://a.espncdn.com/i/teamlogos/nfl/500/%s.png",
          tolower(team_abbr))
}

# NFL shield logo — hosted directly on nfl.com, always available
nfl_logo_url <- "https://static.www.nfl.com/image/upload/v1554321393/league/nvfr7ogywskqrfaiu38m.svg"

# ============================================================
# LOAD STATIC DATA (players + plays — same across all weeks)
# ============================================================

players_df <- fread("players.csv") %>%
  select(nflId, displayName, officialPosition)

# Load 2021 season rosters from nflreadr — gsis_id matches Big Data Bowl nflId
# install.packages("nflreadr") if needed
headshot_lookup <- tryCatch({
  roster <- nflreadr::load_rosters(seasons = 2021) %>%
    select(full_name, headshot_url) %>%
    filter(!is.na(full_name), !is.na(headshot_url), nchar(headshot_url) > 0) %>%
    distinct(full_name, .keep_all = TRUE)

  # Join BDB players to nflreadr roster by display name
  players_df %>%
    select(nflId, displayName) %>%
    left_join(roster, by = c("displayName" = "full_name")) %>%
    filter(!is.na(headshot_url)) %>%
    select(nflId_match = nflId, headshot_url) %>%
    distinct(nflId_match, .keep_all = TRUE)
}, error = function(e) {
  message("Could not load nflreadr rosters: ", e$message)
  data.frame(nflId_match = integer(), headshot_url = character())
})

get_headshot <- function(nfl_id) {
  id  <- as.integer(nfl_id)
  url <- headshot_lookup %>%
    filter(nflId_match == id) %>%
    pull(headshot_url)
  if (length(url) > 0 && nchar(url[1]) > 0) url[1] else nfl_logo_url
}

plays_df <- fread("plays.csv") %>%
  select(gameId, playId, passResult, possessionTeam,
         absoluteYardlineNumber, yardlineNumber, yardlineSide,
         pff_passCoverage, pff_passCoverageType)

# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "NFL Receiver Route RL"),
  
  dashboardSidebar(
    width = 280,
    
    # ---- Logo image ----
    div(
      style = "text-align:center; padding: 12px 8px 4px 8px;",
      uiOutput("logo_image")
    ),
    
    hr(style = "border-color:#444; margin: 4px 12px;"),
    
    # ---- Data filters ----
    div(style = "padding: 0 12px;",
        
        pickerInput("weeks", "Week(s)",
                    choices  = names(WEEK_FILES),
                    selected = "Week 1",
                    multiple = TRUE,
                    options  = list(`actions-box` = TRUE,
                                    `live-search` = FALSE)),
        
        pickerInput("team_filter", "Team",
                    choices  = c("All Teams" = "ALL"),
                    selected = "ALL",
                    multiple = FALSE,
                    options  = list(`live-search` = TRUE)),
        
        pickerInput("position_filter", "Position",
                    choices  = c("All Positions" = "ALL",
                                 "WR" = "WR",
                                 "TE" = "TE",
                                 "RB" = "RB",
                                 "FB" = "FB"),
                    selected = "ALL",
                    multiple = TRUE,
                    options  = list(`actions-box` = TRUE)),
        
        pickerInput("player_filter", "Player",
                    choices  = c("All Players" = "ALL"),
                    selected = "ALL",
                    multiple = FALSE,
                    options  = list(`live-search` = TRUE)),
        
        radioGroupButtons("redzone_filter",
                          label   = "Field Zone",
                          choices = c("All Plays"    = "all",
                                      "Red Zone"     = "redzone",
                                      "Non-Red Zone" = "nonredzone"),
                          selected   = "all",
                          direction  = "vertical",
                          individual = TRUE,
                          width      = "100%"),
        
        radioGroupButtons("coverage_type_filter",
                          label   = "Coverage Type",
                          choices = c("All"  = "ALL",
                                      "Man"  = "Man",
                                      "Zone" = "Zone",
                                      "Other" = "Other"),
                          selected   = "ALL",
                          direction  = "vertical",
                          individual = TRUE,
                          width      = "100%"),
        
        pickerInput("coverage_scheme_filter", "Coverage Scheme",
                    choices  = c("All Schemes" = "ALL",
                                 "Cover-0" = "Cover-0",
                                 "Cover-1" = "Cover-1",
                                 "Cover-2" = "Cover-2",
                                 "Cover-3" = "Cover-3",
                                 "Cover-6" = "Cover-6",
                                 "2-Man"   = "2-Man",
                                 "Bracket" = "Bracket",
                                 "Quarters" = "Quarters",
                                 "Prevent"  = "Prevent",
                                 "Goal Line" = "Goal Line",
                                 "Red Zone"  = "Red Zone",
                                 "Miscellaneous" = "Miscellaneous"),
                    selected = "ALL",
                    multiple = TRUE,
                    options  = list(`actions-box` = TRUE)),
        
        sliderInput("alpha", "Alpha (learning rate)",
                    min = 0.1, max = 1.0, value = 0.5, step = 0.1),
        
        actionButton("run_btn", "Run Q-Learning",
                     icon  = icon("play"),
                     width = "100%",
                     style = "background-color:#1a73e8; color:white;
                              border:none; margin-top:8px;"),
        
        uiOutput("run_status")
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      body, .content-wrapper, .main-sidebar, .wrapper {
        background-color: #1a1a2e !important;
      }
      .box { background-color: #16213e !important;
             border-top-color: #1a73e8 !important; }
      .box-header { color: white !important; }
      .nav-tabs-custom { background: #16213e !important; }
      .nav-tabs-custom > .nav-tabs > li.active > a {
        color: white !important; background: #1a73e8 !important; }
      .nav-tabs-custom > .nav-tabs > li > a { color: #aaa !important; }
      .tab-content { background: #16213e !important; }
      .form-group label { color: #ccc !important; }
      .btn-default { background:#2a2a4a; color:white; border-color:#444; }
      hr { border-color: #444; }
      .shiny-output-error { color: #e74c3c; }
      #play_slider .irs-bar,
      #play_slider .irs-bar-edge { background: #1a73e8 !important;
                                    border-color: #1a73e8 !important; }
    "))),
    
    tabBox(
      width = 12,
      
      # ---- Tab 1: Q-Value Grid ----
      tabPanel(
        title = tagList(icon("th"), " Q-Value Grid"),
        fluidRow(
          column(12,
            plotOutput("qvalue_plot", height = "750px")
          )
        )
      ),
      
      # ---- Tab 2: Play-by-Play ----
      tabPanel(
        title = tagList(icon("film"), " Play-by-Play"),
        fluidRow(
          column(12,
            div(style = "padding: 8px 0;",
              fluidRow(
                column(4,
                  uiOutput("play_info_box")
                ),
                column(4,
                  div(style = "text-align:center; padding-top:6px;",
                    actionButton("prev_play", label = NULL,
                                 icon  = icon("chevron-left"),
                                 style = "background:#2a2a4a; color:white;
                                          border-color:#444; margin-right:8px;"),
                    actionButton("next_play", label = NULL,
                                 icon  = icon("chevron-right"),
                                 style = "background:#2a2a4a; color:white;
                                          border-color:#444;")
                  )
                ),
                column(4,
                  uiOutput("play_outcome_box")
                )
              ),
              uiOutput("play_slider_ui"),
              plotOutput("pbp_plot", height = "680px")
            )
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ---- Reactive: load and process data ----
  raw_data <- reactive({
    req(input$weeks)
    
    files <- unlist(WEEK_FILES[input$weeks])
    df_list <- lapply(files, function(f) {
      if (file.exists(f)) fread(f) else NULL
    })
    df_list <- Filter(Negate(is.null), df_list)
    if (length(df_list) == 0) return(NULL)
    rbindlist(df_list)
  })
  
  # ---- Reactive: update team dropdown when weeks change ----
  observe({
    df <- raw_data()
    req(df)
    
    # Get teams from plays_df filtered to loaded games
    game_ids <- unique(df$gameId)
    teams <- plays_df %>%
      filter(gameId %in% game_ids) %>%
      pull(possessionTeam) %>%
      unique() %>%
      sort()
    
    updatePickerInput(session, "team_filter",
                      choices  = c("All Teams" = "ALL", setNames(teams, teams)),
                      selected = "ALL")
  })
  
  # ---- Reactive: update player dropdown based on team + position ----
  observe({
    df <- raw_data()
    req(df)
    
    game_ids <- unique(df$gameId)
    
    # Find which team each player played for
    player_teams <- plays_df %>%
      filter(gameId %in% game_ids) %>%
      select(gameId, possessionTeam) %>%
      distinct()
    
    # Join tracking to get nflId -> team mapping via gameId
    player_ids <- df %>%
      select(gameId, nflId) %>%
      distinct() %>%
      left_join(player_teams, by = "gameId") %>%
      distinct(nflId, possessionTeam)
    
    player_list <- players_df %>%
      filter(nflId %in% player_ids$nflId) %>%
      filter(officialPosition %in% SKILL_POSITIONS)  # skill positions only
    
    # Apply position filter
    pos <- input$position_filter
    if (!is.null(pos) && !("ALL" %in% pos)) {
      player_list <- player_list %>% filter(officialPosition %in% pos)
    }
    
    # Apply team filter
    if (!is.null(input$team_filter) && input$team_filter != "ALL") {
      valid_ids <- player_ids %>%
        filter(possessionTeam == input$team_filter) %>%
        pull(nflId)
      player_list <- player_list %>% filter(nflId %in% valid_ids)
    }
    
    choices <- c("All Players" = "ALL",
                 setNames(as.character(player_list$nflId),
                          paste0(player_list$displayName,
                                 " (", player_list$officialPosition, ")")))
    
    updatePickerInput(session, "player_filter",
                      choices  = choices,
                      selected = "ALL")
  })
  
  # ---- Reactive: logo URL ----
  output$logo_image <- renderUI({
    if (!is.null(input$player_filter) && input$player_filter != "ALL") {
      url <- get_headshot(input$player_filter)
    } else if (!is.null(input$team_filter) && input$team_filter != "ALL") {
      url <- team_logo_url(input$team_filter)
    } else {
      url <- nfl_logo_url
    }
    tags$img(src = url, style = "max-width:180px; max-height:130px;
                                  object-fit:contain;",
             onerror = sprintf("this.src='%s'", nfl_logo_url))
  })
  
  # ---- Reactive: run Q-learning on button press ----
  rl_result <- eventReactive(input$run_btn, {
    
    df_raw <- raw_data()
    req(df_raw)
    
    withProgress(message = "Running Q-learning...", value = 0, {
      
      incProgress(0.1, detail = "Normalizing data")
      
      # Join positions
      df <- df_raw %>%
        left_join(players_df %>% select(nflId, officialPosition), by = "nflId") %>%
        filter(officialPosition %in% SKILL_POSITIONS)
      
      # Apply position filter
      pos <- input$position_filter
      if (!is.null(pos) && !("ALL" %in% pos)) {
        df <- df %>% filter(officialPosition %in% pos)
      }
      
      # Apply player filter
      if (!is.null(input$player_filter) && input$player_filter != "ALL") {
        df <- df %>% filter(nflId == as.integer(input$player_filter))
      }
      
      # Snap normalization
      df <- df %>%
        mutate(x = pmin(pmax(x, 0), 120), y = pmin(pmax(y, 0), 53.3))
      
      snap_frames <- df %>%
        filter(event %in% c("ball_snap", "autoevent_ballsnap")) %>%
        group_by(gameId, playId) %>%
        summarise(snap_frame = min(frameId), .groups = "drop")
      
      df <- df %>%
        left_join(snap_frames, by = c("gameId", "playId")) %>%
        filter(!is.na(snap_frame), frameId >= snap_frame) %>%
        group_by(gameId, playId, nflId) %>%
        mutate(
          x_origin = x[which.min(frameId)],
          x_norm   = ifelse(playDirection == "right",
                            x - x_origin, x_origin - x),
          y_norm   = y
        ) %>%
        ungroup()
      
      incProgress(0.2, detail = "Tracking transitions")
      tracked       <- track_transitions(df)
      transitions   <- tracked$transitions
      dot_crossings <- tracked$dot_crossings
      
      incProgress(0.2, detail = "Loading play outcomes")
      
      # Filter plays by red zone
      filtered_plays <- plays_df
      if (input$redzone_filter == "redzone") {
        filtered_plays <- filtered_plays %>%
          filter(absoluteYardlineNumber <= 20)
      } else if (input$redzone_filter == "nonredzone") {
        filtered_plays <- filtered_plays %>%
          filter(absoluteYardlineNumber > 20)
      }
      
      # Apply team filter to plays
      if (!is.null(input$team_filter) && input$team_filter != "ALL") {
        filtered_plays <- filtered_plays %>%
          filter(possessionTeam == input$team_filter)
      }
      
      # Apply coverage type filter (Man / Zone / Other)
      if (!is.null(input$coverage_type_filter) &&
          input$coverage_type_filter != "ALL") {
        filtered_plays <- filtered_plays %>%
          filter(pff_passCoverageType == input$coverage_type_filter)
      }
      
      # Apply coverage scheme filter (Cover-1, Cover-2 etc.)
      if (!is.null(input$coverage_scheme_filter) &&
          !("ALL" %in% input$coverage_scheme_filter)) {
        filtered_plays <- filtered_plays %>%
          filter(pff_passCoverage %in% input$coverage_scheme_filter)
      }
      
      outcomes <- filtered_plays %>%
        filter(passResult %in% c("C", "I", "S", "IN")) %>%
        mutate(success = passResult == "C") %>%
        select(gameId, playId, success)
      
      incProgress(0.3, detail = "Running Q-learning")
      rl_result <- run_q_learning(transitions, outcomes,
                                   alpha = input$alpha, gamma = 0, n_epochs = 1)
      
      incProgress(0.1, detail = "Building plot data")
      
      list(
        df            = df,
        transitions   = transitions,
        dot_crossings = dot_crossings,
        outcomes      = outcomes,
        Q             = rl_result$Q,
        counts        = rl_result$counts,
        alpha         = input$alpha
      )
    })
  })
  
  # ---- Status message ----
  output$run_status <- renderUI({
    res <- rl_result()
    req(res)
    n_plays  <- nrow(res$outcomes)
    n_success <- sum(res$outcomes$success)
    div(style = "color:#aaa; font-size:11px; margin-top:6px; text-align:center;",
        sprintf("%d pass plays  |  %d complete (%.0f%%)",
                n_plays, n_success, 100 * n_success / max(n_plays, 1)))
  })
  
  # ---- Tab 1: Q-value grid plot ----
  output$qvalue_plot <- renderPlot({
    res <- rl_result()
    req(res)
    
    player_name <- if (!is.null(input$player_filter) &&
                       input$player_filter != "ALL") {
      players_df %>%
        filter(nflId == as.integer(input$player_filter)) %>%
        pull(displayName) %>% first()
    } else if (!is.null(input$team_filter) && input$team_filter != "ALL") {
      input$team_filter
    } else {
      "All Skill Players"
    }
    
    zone_label <- switch(input$redzone_filter,
                         "all"        = "All Plays",
                         "redzone"    = "Red Zone Only",
                         "nonredzone" = "Non-Red Zone Only")
    
    cov_type_label <- if (!is.null(input$coverage_type_filter) &&
                          input$coverage_type_filter != "ALL") {
      input$coverage_type_filter
    } else ""
    
    cov_scheme_label <- if (!is.null(input$coverage_scheme_filter) &&
                            !("ALL" %in% input$coverage_scheme_filter)) {
      paste(input$coverage_scheme_filter, collapse = "/")
    } else ""
    
    cov_label <- paste(c(cov_type_label, cov_scheme_label),
                       collapse = " ") %>% trimws()
    
    title <- sprintf("Route Q-Values — %s | %s%s",
                     player_name,
                     zone_label,
                     ifelse(nchar(cov_label) > 0,
                            paste0(" | ", cov_label), ""))
    plot_rl_field(final_poly_df(), title = title)
  }, bg = "#1a1a2e")
  
  # ---- Play-by-play state ----
  play_index <- reactiveVal(1)
  
  # Reset play index when result changes
  observeEvent(rl_result(), { play_index(1) })
  
  # Build ordered play list for play-by-play
  pbp_plays <- reactive({
    res <- rl_result()
    req(res)
    res$transitions %>%
      inner_join(res$outcomes, by = c("gameId", "playId")) %>%
      mutate(reward = ifelse(success, 1, -1)) %>%
      arrange(gameId, playId) %>%
      distinct(gameId, playId, success, reward)
  })
  
  # Pre-compute the final Q state (all plays, 1 epoch) for the grid tab
  # This runs once when result changes — same logic as play-by-play final frame
  final_poly_df <- reactive({
    res   <- rl_result()
    plays <- pbp_plays()
    req(res, plays)
    
    all_trans <- res$transitions %>%
      inner_join(res$outcomes, by = c("gameId", "playId")) %>%
      mutate(reward = ifelse(success, 1, -1)) %>%
      arrange(gameId, playId, nflId, frameId)
    
    Q_final      <- init_q_table()
    counts_final <- init_count_table()
    
    for (i in seq_len(nrow(plays))) {
      p_row   <- plays[i, ]
      p_trans <- all_trans %>%
        filter(gameId == p_row$gameId, playId == p_row$playId)
      for (j in seq_len(nrow(p_trans))) {
        s_col  <- p_trans$col[j]; s_row <- p_trans$row[j]
        action <- p_trans$move_dir[j]; reward <- p_trans$reward[j]
        old_q  <- Q_final[s_col, s_row, action]
        Q_final[s_col, s_row, action] <- old_q + res$alpha * (reward - old_q)
        counts_final[s_col, s_row, action] <- counts_final[s_col, s_row, action] + 1
      }
    }
    
    prob_df <- q_to_prob(Q_final, counts_final, min_visits = 1)
    build_cell_polygons(prob_df)
  })
  
  # Nav buttons
  observeEvent(input$prev_play, {
    play_index(max(1, play_index() - 1))
  })
  observeEvent(input$next_play, {
    n <- nrow(pbp_plays())
    play_index(min(n, play_index() + 1))
  })
  observeEvent(input$play_slider, {
    play_index(input$play_slider)
  })
  
  # Slider UI
  output$play_slider_ui <- renderUI({
    plays <- pbp_plays()
    req(plays)
    n <- nrow(plays)
    sliderInput("play_slider", label = NULL,
                min = 1, max = n, value = play_index(),
                step = 1, width = "100%",
                ticks = FALSE)
  })
  
  # Keep slider in sync with buttons
  observe({
    updateSliderInput(session, "play_slider", value = play_index())
  })
  
  # Play info box
  output$play_info_box <- renderUI({
    plays <- pbp_plays()
    req(plays)
    idx   <- play_index()
    row   <- plays[idx, ]
    div(style = "color:#aaa; font-size:12px; padding-top:8px;",
        sprintf("Play %d of %d  |  gameId: %s  |  playId: %s",
                idx, nrow(plays), row$gameId, row$playId))
  })
  
  # Play outcome box
  output$play_outcome_box <- renderUI({
    plays <- pbp_plays()
    req(plays)
    row <- plays[play_index(), ]
    colour <- if (row$success) "#2ecc71" else "#e74c3c"
    label  <- if (row$success) "COMPLETE (+1)" else "INCOMPLETE / SACK (-1)"
    div(style = sprintf("color:%s; font-size:13px; font-weight:bold;
                         text-align:right; padding-top:8px;", colour),
        label)
  })
  
  # ---- Play-by-play plot ----
  output$pbp_plot <- renderPlot({
    res   <- rl_result()
    plays <- pbp_plays()
    req(res, plays)
    
    idx <- play_index()
    row <- plays[idx, ]
    
    game_id <- row$gameId
    pid     <- row$playId
    
    # Re-run Q up to this play (1 pass, same as final_poly_df)
    all_trans <- res$transitions %>%
      inner_join(res$outcomes, by = c("gameId", "playId")) %>%
      mutate(reward = ifelse(success, 1, -1)) %>%
      arrange(gameId, playId, nflId, frameId)
    
    plays_so_far <- plays[1:idx, ]
    Q_pbp      <- init_q_table()
    counts_pbp <- init_count_table()
    
    for (i in seq_len(nrow(plays_so_far))) {
      p_row   <- plays_so_far[i, ]
      p_trans <- all_trans %>%
        filter(gameId == p_row$gameId, playId == p_row$playId)
      for (j in seq_len(nrow(p_trans))) {
        s_col  <- p_trans$col[j]; s_row <- p_trans$row[j]
        action <- p_trans$move_dir[j]; reward <- p_trans$reward[j]
        old_q  <- Q_pbp[s_col, s_row, action]
        Q_pbp[s_col, s_row, action] <- old_q + res$alpha * (reward - old_q)
        counts_pbp[s_col, s_row, action] <- counts_pbp[s_col, s_row, action] + 1
      }
    }
    
    poly_df <- q_to_prob(Q_pbp, counts_pbp, min_visits = 1) %>%
      build_cell_polygons()
    
    # ---- Left: Q-value grid ----
    q_abs_max <- max(abs(poly_df$q_value))
    q_limit   <- if (q_abs_max == 0) 1 else q_abs_max
    
    stripe_df <- data.frame(
      ymin = seq(0, FIELD_LENGTH - CELL_SIZE_Y, by = CELL_SIZE_Y),
      ymax = seq(CELL_SIZE_Y, FIELD_LENGTH,     by = CELL_SIZE_Y),
      fill = rep(c("forestgreen", "#228B22"), length.out = N_COLS)
    )
    HASH_INNER  <- (FIELD_WIDTH / 2) - 3.08
    HASH_OUTER  <- (FIELD_WIDTH / 2) + 3.08
    hash_df     <- data.frame(y_yd = seq(1, FIELD_LENGTH - 1, by = 1))
    yard_num_df <- data.frame(
      y_pos = seq(10, FIELD_LENGTH, by = 10),
      label = paste0("+", seq(10, FIELD_LENGTH, by = 10))
    )
    cell_grid <- expand.grid(c_idx = 1:N_COLS, r_idx = 1:N_ROWS)
    x_lines <- do.call(rbind, lapply(seq_len(nrow(cell_grid)), function(i) {
      c_idx <- cell_grid$c_idx[i]; r_idx <- cell_grid$r_idx[i]
      x0 <- (c_idx - 1) * CELL_SIZE_Y; x1 <- c_idx * CELL_SIZE_Y
      y0 <- (r_idx - 1) * CELL_SIZE_X; y1 <- r_idx * CELL_SIZE_X
      rbind(data.frame(x = y0, xend = y1, y = x0, yend = x1),
            data.frame(x = y0, xend = y1, y = x1, yend = x0))
    }))
    label_df <- poly_df %>%
      group_by(group_id, col, row, direction, q_value) %>%
      summarise(lx = mean(py), ly = mean(px), .groups = "drop")
    
    pass_result <- plays_df %>%
      filter(gameId == game_id, playId == pid) %>%
      pull(passResult)
    pass_result <- if (length(pass_result) > 0) pass_result[1] else "?"
    
    p_grid <- ggplot() +
      geom_rect(data = stripe_df,
                aes(xmin = 0, xmax = FIELD_WIDTH, ymin = ymin, ymax = ymax),
                fill = stripe_df$fill, inherit.aes = FALSE) +
      annotate("rect", xmin = 0, xmax = FIELD_WIDTH,
               ymin = -5, ymax = 0,
               fill = "blue", alpha = 0.2, color = "white", linewidth = 0.6) +
      geom_hline(data = data.frame(y = seq(5, FIELD_LENGTH, by = 5)),
                 aes(yintercept = y), color = "white", linewidth = 0.3, alpha = 0.8) +
      annotate("segment", x = 0, xend = 0, y = 0, yend = FIELD_LENGTH,
               color = "white", linewidth = 1.2) +
      annotate("segment", x = FIELD_WIDTH, xend = FIELD_WIDTH,
               y = 0, yend = FIELD_LENGTH, color = "white", linewidth = 1.2) +
      geom_segment(data = hash_df,
                   aes(x = HASH_INNER - 0.4, xend = HASH_INNER + 0.4,
                       y = y_yd, yend = y_yd), color = "white", linewidth = 0.3) +
      geom_segment(data = hash_df,
                   aes(x = HASH_OUTER - 0.4, xend = HASH_OUTER + 0.4,
                       y = y_yd, yend = y_yd), color = "white", linewidth = 0.3) +
      geom_polygon(data = poly_df,
                   aes(x = py, y = px, group = group_id, fill = q_value),
                   alpha = 0.75, color = "white", linewidth = 0.15) +
      scale_fill_gradient2(low = "#d32f2f", mid = "#ffd54f", high = "#388e3c",
                           midpoint = 0, limits = c(-q_limit, q_limit),
                           name = "Q-Value",
                           guide = guide_colorbar(barwidth = 0.5, barheight = 7,
                                                  ticks.colour = "white",
                                                  frame.colour = "white")) +
      geom_segment(data = x_lines,
                   aes(x = x, xend = xend, y = y, yend = yend),
                   color = "white", linewidth = 0.15, alpha = 0.4) +
      geom_hline(data = data.frame(y = seq(0, FIELD_LENGTH, by = CELL_SIZE_Y)),
                 aes(yintercept = y), color = "white", linewidth = 0.45, alpha = 0.6) +
      geom_vline(data = data.frame(x = seq(0, FIELD_WIDTH, by = CELL_SIZE_X)),
                 aes(xintercept = x), color = "white", linewidth = 0.45, alpha = 0.6) +
      annotate("segment", x = 0, xend = FIELD_WIDTH, y = 0, yend = 0,
               color = "#4fc3f7", linewidth = 1.8) +
      annotate("text", x = FIELD_WIDTH + 1, y = 0,
               label = "LOS", color = "#4fc3f7", size = 2.5, fontface = "bold", hjust = 0) +
      geom_text(data = label_df,
                aes(x = lx, y = ly, label = round(q_value, 2)),
                color = "white", size = 1.5, fontface = "bold") +
      geom_text(data = yard_num_df,
                aes(x = -1.8, y = y_pos + 1.2, label = label),
                color = "white", size = 2.2, fontface = "bold", hjust = 1) +
      geom_text(data = yard_num_df,
                aes(x = FIELD_WIDTH + 1.8, y = y_pos + 1.2, label = label),
                color = "white", size = 2.2, fontface = "bold", hjust = 0) +
      coord_fixed(ratio = 1,
                  xlim = c(-5, FIELD_WIDTH + 5),
                  ylim = c(-5, FIELD_LENGTH + 2)) +
      labs(title = sprintf("Q-Values after play %d / %d", idx, nrow(plays)),
           subtitle = sprintf("playId: %s  |  Result: %s  |  Reward: %s",
                              pid, pass_result,
                              ifelse(pass_result == "C", "+1", "-1")),
           x = NULL, y = NULL) +
      theme_void(base_size = 10) +
      theme(
        plot.background   = element_rect(fill = "#1a1a2e", color = NA),
        panel.background  = element_rect(fill = "#1a1a2e", color = NA),
        plot.title        = element_text(color = "white", face = "bold",
                                         size = 10, hjust = 0.5),
        plot.subtitle     = element_text(
          color = ifelse(pass_result == "C", "#2ecc71", "#e74c3c"),
          size = 7.5, hjust = 0.5),
        plot.margin       = margin(6, 12, 6, 12),
        legend.position   = "right",
        legend.background = element_rect(fill = "#1a1a2e", color = NA),
        legend.text       = element_text(color = "white", size = 7),
        legend.title      = element_text(color = "white", size = 8)
      )
    
    # ---- Right: route trace ----
    nfl_id_trace <- if (!is.null(input$player_filter) &&
                        input$player_filter != "ALL") {
      as.integer(input$player_filter)
    } else {
      res$transitions %>%
        filter(gameId == game_id, playId == pid) %>%
        pull(nflId) %>% first()
    }
    
    player_df <- res$df %>%
      ungroup() %>%
      filter(gameId == game_id, playId == pid, nflId == nfl_id_trace) %>%
      arrange(frameId) %>%
      filter(!is.na(x_norm), !is.na(y_norm))
    
    play_transitions <- res$dot_crossings %>%
      filter(gameId == game_id, playId == pid, nflId == nfl_id_trace) %>%
      arrange(frameId) %>%
      mutate(label = paste0(substr(move_dir, 1, 1), "\n(", prev_col, ",", prev_row, ")"))
    
    stripe_trace <- data.frame(
      ymin = seq(-5, FIELD_LENGTH - CELL_SIZE_Y, by = CELL_SIZE_Y),
      ymax = seq(CELL_SIZE_Y - 5, FIELD_LENGTH,  by = CELL_SIZE_Y),
      fill = rep(c("forestgreen", "#228B22"), length.out = N_COLS + 1)
    )
    hash_trace <- data.frame(y_yd = seq(-5, FIELD_LENGTH, by = 1))
    
    if (nrow(player_df) > 0) {
      visited_cells <- player_df %>%
        mutate(
          cell_col = pmin(pmax(floor(x_norm / CELL_SIZE_Y) + 1, 1), N_COLS),
          cell_row = pmin(pmax(floor(y_norm / CELL_SIZE_X) + 1, 1), N_ROWS)
        ) %>%
        distinct(cell_col, cell_row) %>%
        mutate(xmin = (cell_col - 1) * CELL_SIZE_Y, xmax = cell_col * CELL_SIZE_Y,
               ymin = (cell_row - 1) * CELL_SIZE_X, ymax = cell_row * CELL_SIZE_X)
      
      p_trace <- ggplot() +
        geom_rect(data = stripe_trace,
                  aes(xmin = 0, xmax = FIELD_WIDTH, ymin = ymin, ymax = ymax),
                  fill = stripe_trace$fill, inherit.aes = FALSE) +
        geom_hline(data = data.frame(y = seq(-5, FIELD_LENGTH, 5)),
                   aes(yintercept = y), color = "white", linewidth = 0.3, alpha = 0.5) +
        annotate("segment", x = 0, xend = 0, y = -5, yend = FIELD_LENGTH,
                 color = "white", linewidth = 1) +
        annotate("segment", x = FIELD_WIDTH, xend = FIELD_WIDTH,
                 y = -5, yend = FIELD_LENGTH, color = "white", linewidth = 1) +
        geom_segment(data = hash_trace,
                     aes(x = HASH_INNER - 0.4, xend = HASH_INNER + 0.4,
                         y = y_yd, yend = y_yd), color = "white", linewidth = 0.3) +
        geom_segment(data = hash_trace,
                     aes(x = HASH_OUTER - 0.4, xend = HASH_OUTER + 0.4,
                         y = y_yd, yend = y_yd), color = "white", linewidth = 0.3) +
        geom_hline(data = data.frame(y = seq(0, FIELD_LENGTH, CELL_SIZE_Y)),
                   aes(yintercept = y), color = "gray60", linewidth = 0.3,
                   linetype = "dashed") +
        geom_vline(data = data.frame(x = seq(0, FIELD_WIDTH, CELL_SIZE_X)),
                   aes(xintercept = x), color = "gray60", linewidth = 0.3,
                   linetype = "dashed") +
        geom_rect(data = visited_cells,
                  aes(xmin = ymin, xmax = ymax, ymin = xmin, ymax = xmax),
                  fill = "yellow", alpha = 0.2, color = "yellow", linewidth = 0.5) +
        annotate("segment", x = 0, xend = FIELD_WIDTH, y = 0, yend = 0,
                 color = "#4fc3f7", linewidth = 1.5) +
        annotate("text", x = FIELD_WIDTH + 0.8, y = 0,
                 label = "LOS", color = "#4fc3f7", size = 2.5,
                 fontface = "bold", hjust = 0) +
        geom_path(data = player_df, aes(x = y_norm, y = x_norm),
                  color = "yellow", linewidth = 1.2, lineend = "round") +
        geom_point(data = play_transitions, aes(x = y_norm, y = x_norm),
                   color = "white", fill = "orange", size = 3,
                   shape = 21, stroke = 0.8) +
        geom_label(data = play_transitions,
                   aes(x = y_norm, y = x_norm, label = label),
                   fill = "black", color = "orange", size = 1.8,
                   nudge_x = 1.8, label.padding = unit(0.1, "lines")) +
        annotate("point", x = player_df$y_norm[1], y = player_df$x_norm[1],
                 fill = "#2ecc71", color = "white", size = 3.5,
                 shape = 21, stroke = 1.2) +
        annotate("point", x = player_df$y_norm[nrow(player_df)],
                 y = player_df$x_norm[nrow(player_df)],
                 fill = "#e74c3c", color = "white", size = 3.5,
                 shape = 21, stroke = 1.2) +
        coord_fixed(xlim = c(-1, FIELD_WIDTH + 8), ylim = c(-5, FIELD_LENGTH)) +
        labs(
          title    = sprintf("Route — %s",
                             players_df %>%
                               filter(nflId == nfl_id_trace) %>%
                               pull(displayName) %>%
                               first() %>%
                               replace(is.na(.), as.character(nfl_id_trace))),
          subtitle = "Green = start  |  Red = end  |  Orange = cell transition",
          x = NULL, y = NULL
        ) +
        theme_void() +
        theme(
          plot.background  = element_rect(fill = "#1a1a2e", color = NA),
          panel.background = element_rect(fill = "#1a1a2e", color = NA),
          plot.title       = element_text(color = "white", face = "bold",
                                          size = 10, hjust = 0.5),
          plot.subtitle    = element_text(color = "#aaaaaa", size = 7,
                                          hjust = 0.5),
          plot.margin      = margin(6, 12, 6, 12)
        )
    } else {
      p_trace <- ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No tracking data\nfor this player/play",
                 color = "#aaaaaa", size = 4, hjust = 0.5) +
        theme_void() +
        theme(plot.background = element_rect(fill = "#1a1a2e", color = NA))
    }
    
    combined <- p_grid + p_trace +
      plot_layout(ncol = 2, widths = c(1, 1)) &
      theme(plot.background = element_rect(fill = "#1a1a2e", color = NA))
    
    print(combined)
    
  }, bg = "#1a1a2e")
}

# ============================================================
# RUN
# ============================================================

shinyApp(ui = ui, server = server)
