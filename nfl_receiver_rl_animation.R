# NFL Receiver Route Q-Learning — Animation script. See README.md for full documentation.

library(patchwork)
library(gifski)

game_id  <- result$transitions$gameId[1]
outcomes  <- define_success("plays.csv")
plays_raw <- fread("plays.csv") %>% select(gameId, playId, passResult)

game_transitions <- result$transitions %>%
  filter(gameId == game_id) %>%
  inner_join(outcomes, by = c("gameId", "playId")) %>%
  mutate(reward = ifelse(success, 1, -1)) %>%
  arrange(playId, nflId, frameId)

play_ids <- sort(unique(game_transitions$playId))

Q      <- init_q_table()
counts <- init_count_table()
alpha  <- 0.5
gamma  <- 0

frame_dir <- "qvalue_frames"
dir.create(frame_dir, showWarnings = FALSE)

for (p_idx in seq_along(play_ids)) {
  pid  <- play_ids[p_idx]
  # transitions already have exit-direction shift and terminal state
  # dropped from track_transitions — use directly
  play <- game_transitions %>% filter(playId == pid)
  
  # Update Q
  for (i in seq_len(nrow(play))) {
    s_col  <- play$col[i]; s_row <- play$row[i]
    action <- play$move_dir[i]; reward <- play$reward[i]
    old_q  <- Q[s_col, s_row, action]
    Q[s_col, s_row, action] <- old_q + alpha * (reward - old_q)
    counts[s_col, s_row, action] <- counts[s_col, s_row, action] + 1
  }
  
  pass_result <- plays_raw %>%
    filter(gameId == game_id, playId == pid) %>%
    pull(passResult)
  pass_result <- if (length(pass_result) > 0) pass_result[1] else "?"
  
  # ---- Left panel: Q-value grid ----
  prob_df <- q_to_prob(Q, counts, min_visits = 1)
  poly_df <- build_cell_polygons(prob_df)
  
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
    rbind(
      data.frame(x = y0, xend = y1, y = x0, yend = x1),
      data.frame(x = y0, xend = y1, y = x1, yend = x0)
    )
  }))
  label_df <- poly_df %>%
    group_by(group_id, col, row, direction, q_value) %>%
    summarise(lx = mean(py), ly = mean(px), .groups = "drop")
  
  p_grid <- ggplot() +
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
                 color = "white", linewidth = 0.4) +
    geom_segment(data = hash_df,
                 aes(x = HASH_OUTER - 0.4, xend = HASH_OUTER + 0.4,
                     y = y_yd, yend = y_yd),
                 color = "white", linewidth = 0.4) +
    geom_polygon(data = poly_df,
                 aes(x = py, y = px, group = group_id, fill = q_value),
                 alpha = 0.75, color = "white", linewidth = 0.2) +
    scale_fill_gradient2(
      low = "#d32f2f", mid = "#ffd54f", high = "#388e3c",
      midpoint = 0, limits = c(-q_limit, q_limit), name = "Q-Value",
      guide = guide_colorbar(barwidth = 0.6, barheight = 8,
                             ticks.colour = "white", frame.colour = "white")
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
              color = "white", size = 2.5, fontface = "bold", hjust = 1) +
    geom_text(data = yard_num_df,
              aes(x = FIELD_WIDTH + 1.8, y = y_pos + 1.2, label = label),
              color = "white", size = 2.5, fontface = "bold", hjust = 0) +
    coord_fixed(ratio = 1,
                xlim = c(-5, FIELD_WIDTH + 5),
                ylim = c(-5, FIELD_LENGTH + 2)) +
    labs(title = sprintf("Q-Values after play %d / %d", p_idx, length(play_ids)),
         subtitle = sprintf("playId: %s  |  Result: %s  |  Reward: %s",
                            pid, pass_result,
                            ifelse(pass_result == "C", "+1", "-1")),
         x = NULL, y = NULL) +
    theme_void(base_size = 11) +
    theme(
      plot.background   = element_rect(fill = "#1a1a2e", color = NA),
      panel.background  = element_rect(fill = "#1a1a2e", color = NA),
      plot.title        = element_text(color = "white", face = "bold",
                                       size = 11, hjust = 0.5),
      plot.subtitle     = element_text(
        color = ifelse(pass_result == "C", "#2ecc71", "#e74c3c"),
        size = 8, hjust = 0.5),
      plot.margin       = margin(8, 16, 8, 16),
      legend.position   = "right",
      legend.background = element_rect(fill = "#1a1a2e", color = NA),
      legend.text       = element_text(color = "white", size = 7),
      legend.title      = element_text(color = "white", size = 8)
    )
  
  # ---- Right panel: route trace ----
  nfl_ids_on_play <- unique(play$nflId)
  
  player_df <- result$df %>%
    ungroup() %>%
    filter(gameId == game_id, playId == pid,
           nflId == nfl_ids_on_play[1]) %>%
    arrange(frameId) %>%
    filter(!is.na(x_norm), !is.na(y_norm))
  
  play_transitions <- result$dot_crossings %>%
    filter(gameId == game_id, playId == pid,
           nflId == nfl_ids_on_play[1]) %>%
    arrange(frameId) %>%
    mutate(label = paste0(substr(move_dir, 1, 1), "\n(", prev_col, ",", prev_row, ")"))
  
  visited_cells <- player_df %>%
    mutate(
      cell_col = pmin(pmax(floor(x_norm / CELL_SIZE_Y) + 1, 1), N_COLS),
      cell_row = pmin(pmax(floor(y_norm / CELL_SIZE_X) + 1, 1), N_ROWS)
    ) %>%
    distinct(cell_col, cell_row) %>%
    mutate(
      xmin = (cell_col - 1) * CELL_SIZE_Y, xmax = cell_col * CELL_SIZE_Y,
      ymin = (cell_row - 1) * CELL_SIZE_X, ymax = cell_row * CELL_SIZE_X
    )
  
  stripe_trace <- data.frame(
    ymin = seq(-5, FIELD_LENGTH - CELL_SIZE_Y, by = CELL_SIZE_Y),
    ymax = seq(CELL_SIZE_Y - 5, FIELD_LENGTH,  by = CELL_SIZE_Y),
    fill = rep(c("forestgreen", "#228B22"), length.out = N_COLS + 1)
  )
  hash_trace <- data.frame(y_yd = seq(-5, FIELD_LENGTH, by = 1))
  
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
                     y = y_yd, yend = y_yd),
                 color = "white", linewidth = 0.3) +
    geom_segment(data = hash_trace,
                 aes(x = HASH_OUTER - 0.4, xend = HASH_OUTER + 0.4,
                     y = y_yd, yend = y_yd),
                 color = "white", linewidth = 0.3) +
    geom_hline(data = data.frame(y = seq(0, FIELD_LENGTH, CELL_SIZE_Y)),
               aes(yintercept = y), color = "gray60", linewidth = 0.3, linetype = "dashed") +
    geom_vline(data = data.frame(x = seq(0, FIELD_WIDTH, CELL_SIZE_X)),
               aes(xintercept = x), color = "gray60", linewidth = 0.3, linetype = "dashed") +
    geom_rect(data = visited_cells,
              aes(xmin = ymin, xmax = ymax, ymin = xmin, ymax = xmax),
              fill = "yellow", alpha = 0.2, color = "yellow", linewidth = 0.5) +
    annotate("segment", x = 0, xend = FIELD_WIDTH, y = 0, yend = 0,
             color = "#4fc3f7", linewidth = 1.5) +
    annotate("text", x = FIELD_WIDTH + 0.8, y = 0,
             label = "LOS", color = "#4fc3f7", size = 2.5, fontface = "bold", hjust = 0) +
    geom_path(data = player_df, aes(x = y_norm, y = x_norm),
              color = "yellow", linewidth = 1.2, lineend = "round") +
    geom_point(data = play_transitions, aes(x = y_norm, y = x_norm),
               color = "white", fill = "orange", size = 3, shape = 21, stroke = 0.8) +
    geom_label(data = play_transitions, aes(x = y_norm, y = x_norm, label = label),
               fill = "black", color = "orange", size = 1.8,
               nudge_x = 1.8, label.padding = unit(0.1, "lines")) +
    annotate("point", x = player_df$y_norm[1], y = player_df$x_norm[1],
             fill = "#2ecc71", color = "white", size = 3.5, shape = 21, stroke = 1.2) +
    annotate("point", x = player_df$y_norm[nrow(player_df)],
             y = player_df$x_norm[nrow(player_df)],
             fill = "#e74c3c", color = "white", size = 3.5, shape = 21, stroke = 1.2) +
    coord_fixed(xlim = c(-1, FIELD_WIDTH + 8), ylim = c(-5, FIELD_LENGTH)) +
    labs(title = sprintf("Route — nflId: %s", nfl_ids_on_play[1]),
         subtitle = "Green = start  |  Red = end  |  Orange = cell transition",
         x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "#1a1a2e", color = NA),
      panel.background = element_rect(fill = "#1a1a2e", color = NA),
      plot.title       = element_text(color = "white", face = "bold",
                                      size = 11, hjust = 0.5),
      plot.subtitle    = element_text(color = "#aaaaaa", size = 7, hjust = 0.5),
      plot.margin      = margin(8, 16, 8, 16)
    )
  
  # ---- Combine side by side and save frame ----
  combined <- p_grid + p_trace +
    plot_layout(ncol = 2, widths = c(1, 1)) &
    theme(plot.background = element_rect(fill = "#1a1a2e", color = NA))
  
  frame_path <- file.path(frame_dir, sprintf("frame_%03d.png", p_idx))
  ggsave(frame_path, combined, width = 14, height = 10,
         dpi = 100, bg = "#1a1a2e")
  cat(sprintf("Frame %d / %d saved\n", p_idx, length(play_ids)))
}

# Stitch all frames into a GIF
frame_files <- sort(list.files(frame_dir, pattern = "*.png", full.names = TRUE))
gifski(frame_files, gif_file = "qvalue_combined.gif",
       width = 1400, height = 1000, delay = 1)

cat("Saved qvalue_combined.gif\n")
