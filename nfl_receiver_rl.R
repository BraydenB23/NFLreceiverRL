# NFL Receiver Route Q-Learning — Core library. See README.md for full documentation.

library(tidyverse)
library(ggplot2)
library(data.table)

# ============================================================
# 1. SKILL POSITION FILTER
# ============================================================

SKILL_POSITIONS <- c("WR", "TE", "RB", "FB")

# ============================================================
# 2. LOAD & NORMALIZE DATA
# ============================================================

load_and_normalize <- function(filepath,
                                players_filepath = "players.csv") {
  df      <- fread(filepath)
  players <- fread(players_filepath) %>%
    select(nflId, officialPosition)
  
  # Keep only skill position players
  df <- df %>%
    left_join(players, by = "nflId") %>%
    filter(officialPosition %in% SKILL_POSITIONS)
  
  cat(sprintf("Skill position players retained: %d unique nflIds\n",
              n_distinct(df$nflId)))
  
  df <- df %>%
    mutate(
      x = pmin(pmax(x, 0), 120),
      y = pmin(pmax(y, 0), 53.3)
    )
  
  # Anchor to snap frame — handle both snap event names
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
                        x - x_origin,
                        x_origin - x),
      y_norm   = y
    ) %>%
    ungroup()
  
  return(df)
}

# ============================================================
# 3. GRID CONSTANTS
# ============================================================

FIELD_LENGTH <- 60
FIELD_WIDTH  <- 53.3
CELL_SIZE_Y  <- 5
CELL_SIZE_X  <- 53.3 / 10

N_COLS <- ceiling(FIELD_LENGTH / CELL_SIZE_Y)
N_ROWS <- 10

DIRECTIONS <- c("Straight", "Left", "Right", "Down")

# ============================================================
# 4. DEFINE PLAY SUCCESS FROM plays.csv
# ============================================================
# C  = complete   -> success
# I  = incomplete -> failure
# S  = sack       -> failure
# IN = interception -> failure
# R  = run play   -> excluded
# Anything else   -> excluded

define_success <- function(plays_filepath = "plays.csv") {
  plays <- fread(plays_filepath) %>%
    select(gameId, playId, passResult)
  
  pass_plays <- plays %>%
    filter(passResult %in% c("C", "I", "S", "IN")) %>%
    mutate(success = passResult == "C") %>%
    select(gameId, playId, success)
  
  cat(sprintf("Pass plays: %d  |  Completions (C): %d (%.1f%%)  |  Failures (I/S/IN): %d (%.1f%%)\n",
              nrow(pass_plays),
              sum(pass_plays$success),
              100 * mean(pass_plays$success),
              sum(!pass_plays$success),
              100 * mean(!pass_plays$success)))
  
  return(pass_plays)
}

# ============================================================
# 5. TRACK CELL TRANSITIONS
# ============================================================

track_transitions <- function(df) {
  df <- df %>%
    filter(!is.na(x_norm), !is.na(y_norm)) %>%
    arrange(gameId, playId, nflId, frameId) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(
      col         = pmin(pmax(floor(x_norm / CELL_SIZE_Y) + 1, 1), N_COLS),
      row         = pmin(pmax(floor(y_norm / CELL_SIZE_X) + 1, 1), N_ROWS),
      cell_change = (col != lag(col, default = first(col))) |
                    (row != lag(row, default = first(row)))
    ) %>%
    ungroup()
  
  # Starting cell per player
  starting_cells <- df %>%
    group_by(gameId, playId, nflId) %>%
    slice(1) %>%
    ungroup() %>%
    select(gameId, playId, nflId, start_col = col, start_row = row)
  
  # Each cell-change frame is a boundary crossing.
  # prev_col/prev_row = the cell being LEFT.
  # Direction = derived from change in cell index (dcol/drow).
  # For Q-updates: col/row is set to prev (cell being left).
  # No rows dropped — the final cell has no crossing OUT of it,
  # so it naturally never appears here. Every row = one valid Q-update.
  
  crossings <- df %>%
    filter(cell_change) %>%
    arrange(gameId, playId, nflId, frameId) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(prev_col = lag(col), prev_row = lag(row)) %>%
    ungroup() %>%
    left_join(starting_cells, by = c("gameId", "playId", "nflId")) %>%
    mutate(
      prev_col = ifelse(is.na(prev_col), start_col, prev_col),
      prev_row = ifelse(is.na(prev_row), start_row, prev_row),
      dcol     = col - prev_col,
      drow     = row - prev_row,
      move_dir = case_when(
        abs(dcol) >= abs(drow) & dcol > 0  ~ "Straight",
        abs(dcol) >= abs(drow) & dcol < 0  ~ "Down",
        abs(drow) >  abs(dcol) & drow > 0  ~ "Right",
        abs(drow) >  abs(dcol) & drow < 0  ~ "Left",
        TRUE ~ "Straight"
      )
    ) %>%
    select(-start_col, -start_row)
  
  # transitions: col/row = cell being LEFT (prev), for Q-updates
  transitions <- crossings %>%
    mutate(col = prev_col, row = prev_row) %>%
    select(gameId, playId, nflId, frameId, col, row,
           move_dir, x_norm, y_norm, event)
  
  # dot_crossings: col/row = cell being ENTERED, for orange dot display
  dot_crossings <- crossings %>%
    select(gameId, playId, nflId, frameId, col, row,
           prev_col, prev_row, move_dir, x_norm, y_norm, event)
  
  return(list(full = df, transitions = transitions, dot_crossings = dot_crossings))
}


# ============================================================
# 6. Q-LEARNING
# ============================================================
# gamma = 0 keeps Q-values strictly between -1 and +1

init_q_table <- function() {
  array(0, dim = c(N_COLS, N_ROWS, 4),
        dimnames = list(paste0("col", 1:N_COLS),
                        paste0("row", 1:N_ROWS),
                        DIRECTIONS))
}

init_count_table <- function() {
  array(0, dim = c(N_COLS, N_ROWS, 4),
        dimnames = list(paste0("col", 1:N_COLS),
                        paste0("row", 1:N_ROWS),
                        DIRECTIONS))
}

run_q_learning <- function(transitions, play_outcomes,
                            alpha = 0.1, gamma = 0, n_epochs = 20) {
  Q      <- init_q_table()
  counts <- init_count_table()
  
  trans_with_outcome <- transitions %>%
    inner_join(play_outcomes, by = c("gameId", "playId")) %>%
    mutate(reward = ifelse(success, 1, -1))
  
  cat(sprintf("Transitions for Q-learning: %d  |  Success: %d  |  Failure: %d\n",
              nrow(trans_with_outcome),
              sum(trans_with_outcome$reward ==  1),
              sum(trans_with_outcome$reward == -1)))
  
  plays <- trans_with_outcome %>%
    group_by(gameId, playId, nflId) %>%
    group_split()
  
  for (epoch in seq_len(n_epochs)) {
    cat(sprintf("Epoch %d / %d\n", epoch, n_epochs))
    
    for (play in plays) {
      if (nrow(play) < 1) next
      for (i in seq_len(nrow(play))) {
        s_col  <- play$col[i]
        s_row  <- play$row[i]
        action <- play$move_dir[i]
        reward <- play$reward[i]
        
        max_next_q <- if (i < nrow(play)) {
          max(Q[play$col[i + 1], play$row[i + 1], ])
        } else 0
        
        old_q <- Q[s_col, s_row, action]
        Q[s_col, s_row, action] <- old_q + alpha * (reward + gamma * max_next_q - old_q)
        counts[s_col, s_row, action] <- counts[s_col, s_row, action] + 1
      }
    }
  }
  
  return(list(Q = Q, counts = counts))
}

# ============================================================
# 7. Q-TABLE TO DATA FRAME
# ============================================================

q_to_prob <- function(Q, counts, min_visits = 3) {
  result <- expand.grid(col = 1:N_COLS, row = 1:N_ROWS,
                        direction = DIRECTIONS, stringsAsFactors = FALSE)
  
  result$q_value <- mapply(function(c, r, d) Q[c, r, d],
                           result$col, result$row, result$direction)
  
  result$visits  <- mapply(function(c, r, d) counts[c, r, d],
                           result$col, result$row, result$direction)
  
  # Zero out cells with too few visits so they appear neutral
  result <- result %>%
    mutate(q_value = ifelse(visits < min_visits, 0, q_value))
  
  result %>%
    group_by(col, row) %>%
    mutate(
      exp_q       = exp(q_value - max(q_value)),
      prob        = exp_q / sum(exp_q),
      best_action = direction[which.max(q_value)]
    ) %>%
    ungroup()
}

# ============================================================
# 8. BUILD TRIANGLE POLYGONS
# ============================================================

build_cell_polygons <- function(prob_df) {
  polys <- vector("list", nrow(prob_df))
  
  for (i in seq_len(nrow(prob_df))) {
    c_idx <- prob_df$col[i]
    r_idx <- prob_df$row[i]
    dir   <- prob_df$direction[i]
    qval  <- prob_df$q_value[i]
    
    x0 <- (c_idx - 1) * CELL_SIZE_Y
    x1 <-  c_idx      * CELL_SIZE_Y
    y0 <- (r_idx - 1) * CELL_SIZE_X
    y1 <-  r_idx      * CELL_SIZE_X
    cx <- (x0 + x1) / 2
    cy <- (y0 + y1) / 2
    
    coords <- switch(dir,
      "Straight" = data.frame(px = c(cx, x1, x1), py = c(cy, y0, y1)),
      "Down"     = data.frame(px = c(cx, x0, x0), py = c(cy, y0, y1)),
      "Right"    = data.frame(px = c(cx, x0, x1), py = c(cy, y1, y1)),
      "Left"     = data.frame(px = c(cx, x0, x1), py = c(cy, y0, y0))
    )
    
    coords$col       <- c_idx
    coords$row       <- r_idx
    coords$direction <- dir
    coords$q_value   <- qval
    coords$group_id  <- paste(c_idx, r_idx, dir, sep = "_")
    
    polys[[i]] <- coords
  }
  
  bind_rows(polys)
}

# ============================================================
# 9. PLOT RL FIELD (Q-VALUES)
# ============================================================

plot_rl_field <- function(poly_df, title = "Receiver Route Q-Values") {
  
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
    rbind(
      data.frame(x = y0, xend = y1, y = x0, yend = x1),
      data.frame(x = y0, xend = y1, y = x1, yend = x0)
    )
  }))
  
  label_df <- poly_df %>%
    group_by(group_id, col, row, direction, q_value) %>%
    summarise(lx = mean(py), ly = mean(px), .groups = "drop")
  
  q_abs_max <- max(abs(poly_df$q_value))
  q_limit   <- if (q_abs_max == 0) 1 else q_abs_max
  
  ggplot() +
    geom_rect(data = stripe_df,
              aes(xmin = 0, xmax = FIELD_WIDTH, ymin = ymin, ymax = ymax),
              fill = stripe_df$fill, inherit.aes = FALSE) +
    annotate("rect", xmin = 0, xmax = FIELD_WIDTH,
             ymin = -5, ymax = 0,
             fill = "blue", alpha = 0.2, color = "white", linewidth = 0.6) +
    geom_hline(data = data.frame(y = seq(5, FIELD_LENGTH, by = 5)),
               aes(yintercept = y), color = "white", linewidth = 0.4, alpha = 0.8) +
    annotate("segment", x = 0, xend = 0,
             y = 0, yend = FIELD_LENGTH, color = "white", linewidth = 1.4) +
    annotate("segment", x = FIELD_WIDTH, xend = FIELD_WIDTH,
             y = 0, yend = FIELD_LENGTH, color = "white", linewidth = 1.4) +
    geom_segment(data = hash_df,
                 aes(x = HASH_INNER - 0.4, xend = HASH_INNER + 0.4,
                     y = y_yd, yend = y_yd),
                 color = "white", linewidth = 0.5) +
    geom_segment(data = hash_df,
                 aes(x = HASH_OUTER - 0.4, xend = HASH_OUTER + 0.4,
                     y = y_yd, yend = y_yd),
                 color = "white", linewidth = 0.5) +
    geom_polygon(data = poly_df,
                 aes(x = py, y = px, group = group_id, fill = q_value),
                 alpha = 0.75, color = "white", linewidth = 0.2) +
    scale_fill_gradient2(
      low      = "#d32f2f",
      mid      = "#ffd54f",
      high     = "#388e3c",
      midpoint = 0,
      limits   = c(-q_limit, q_limit),
      name     = "Q-Value",
      guide    = guide_colorbar(barwidth = 0.8, barheight = 10,
                                ticks.colour = "white",
                                frame.colour = "white")
    ) +
    geom_segment(data = x_lines,
                 aes(x = x, xend = xend, y = y, yend = yend),
                 color = "white", linewidth = 0.2, alpha = 0.4) +
    geom_hline(data = data.frame(y = seq(0, FIELD_LENGTH, by = CELL_SIZE_Y)),
               aes(yintercept = y), color = "white", linewidth = 0.55, alpha = 0.6) +
    geom_vline(data = data.frame(x = seq(0, FIELD_WIDTH, by = CELL_SIZE_X)),
               aes(xintercept = x), color = "white", linewidth = 0.55, alpha = 0.6) +
    annotate("segment", x = 0, xend = FIELD_WIDTH, y = 0, yend = 0,
             color = "#4fc3f7", linewidth = 2) +
    annotate("text", x = FIELD_WIDTH + 1, y = 0,
             label = "LOS", color = "#4fc3f7", size = 3, fontface = "bold", hjust = 0) +
    geom_text(data = label_df,
              aes(x = lx, y = ly, label = round(q_value, 2)),
              color = "white", size = 1.8, fontface = "bold") +
    geom_text(data = yard_num_df,
              aes(x = -1.8, y = y_pos + 1.2, label = label),
              color = "white", size = 2.8, fontface = "bold", hjust = 1) +
    geom_text(data = yard_num_df,
              aes(x = FIELD_WIDTH + 1.8, y = y_pos + 1.2, label = label),
              color = "white", size = 2.8, fontface = "bold", hjust = 0) +
    coord_fixed(ratio = 1,
                xlim = c(-5, FIELD_WIDTH + 5),
                ylim = c(-5, FIELD_LENGTH + 2)) +
    labs(
      title    = title,
      subtitle = "Green = high Q-value (success)  |  Red = low Q-value (failure)  |  Yellow = neutral / insufficient data",
      x = NULL, y = NULL
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.background   = element_rect(fill = "#1a1a2e", color = NA),
      panel.background  = element_rect(fill = "#1a1a2e", color = NA),
      plot.title        = element_text(color = "white", face = "bold",
                                       size = 15, hjust = 0.5, margin = margin(b = 4)),
      plot.subtitle     = element_text(color = "#aaaaaa", size = 8,
                                       hjust = 0.5, margin = margin(b = 8)),
      plot.margin       = margin(12, 24, 12, 24),
      legend.position   = "right",
      legend.background = element_rect(fill = "#1a1a2e", color = NA),
      legend.text       = element_text(color = "white", size = 8),
      legend.title      = element_text(color = "white", size = 9, face = "bold")
    )
}

# ============================================================
# 10. MAIN PIPELINE
# ============================================================

run_pipeline <- function(filepath,
                          players_filepath = "players.csv",
                          plays_filepath   = "plays.csv",
                          output_plot      = "rl_field_plot.png",
                          filter_nflId     = NULL,
                          alpha            = 0.1,
                          min_visits       = 3) {
  cat("Loading data...\n")
  df <- load_and_normalize(filepath, players_filepath)
  
  if (!is.null(filter_nflId)) {
    cat(sprintf("Filtering to nflId: %d\n", filter_nflId))
    df <- df %>% filter(nflId == filter_nflId)
    cat(sprintf("Rows after filter: %d\n", nrow(df)))
  }
  
  cat("Tracking transitions...\n")
  tracked       <- track_transitions(df)
  transitions   <- tracked$transitions
  dot_crossings <- tracked$dot_crossings
  
  cat("Defining play outcomes from plays.csv...\n")
  outcomes <- define_success(plays_filepath)
  
  cat("Running Q-learning (gamma = 0)...\n")
  rl_result <- run_q_learning(transitions, outcomes,
                               alpha = alpha, gamma = 0, n_epochs = 20)
  
  cat("Converting to data frame...\n")
  prob_df <- q_to_prob(rl_result$Q, rl_result$counts, min_visits = min_visits)
  
  cat("Building polygons...\n")
  poly_df <- build_cell_polygons(prob_df)
  
  cat("Plotting...\n")
  plot_title <- ifelse(!is.null(filter_nflId),
                       paste0("Route Q-Values — Player #", filter_nflId),
                       "Receiver Route Q-Values (WR / TE / RB / FB)")
  p <- plot_rl_field(poly_df, title = plot_title)
  
  ggsave(output_plot, p, width = 10, height = 16, dpi = 150, bg = "#1a1a2e")
  cat(sprintf("Saved to %s\n", output_plot))
  
  return(list(
    df            = df,
    transitions   = transitions,
    dot_crossings = dot_crossings,
    Q             = rl_result$Q,
    counts        = rl_result$counts,
    prob_df       = prob_df,
    poly_df       = poly_df,
    plot          = p,
    filepath      = filepath,
    plays_filepath = plays_filepath
  ))
}
