# NFL Receiver Route Q-Learning

An application of tabular Q-learning to NFL receiver route tracking data from the [NFL Big Data Bowl 2023](https://www.kaggle.com/competitions/nfl-big-data-bowl-2023). The project models receiver movement as a reinforcement learning problem, learning which directions tend to lead to completions from any position on the field. Results are explored through an interactive Shiny app and a frame-by-frame GIF animation.

**Author:** Brayden Butt

---

## How It Works

### The Problem as Reinforcement Learning

Every pass play in the dataset involves one or more skill position players (WR, TE, RB, FB) whose movement is tracked at 10 frames per second from the snap. The goal is to learn, for any location on the field, which movement direction is most associated with a completed pass.

This is framed as a Q-learning problem:

**State** — The field is divided into a grid of cells. The downfield axis (from the line of scrimmage) is split into 12 columns of 5 yards each, covering 60 yards past the LOS. The width is split into 10 rows. A player's state at any moment is the grid cell they currently occupy, defined as `(col, row)`.

**Action** — When a player crosses from one cell into another, a direction is recorded: `Straight` (downfield), `Down` (back toward LOS), `Left`, or `Right`. This crossing event is the action taken from the previous cell.

**Reward** — The reward is determined at the play level. If the pass is completed (`passResult == "C"`), every transition on that play receives a reward of `+1`. If the pass is incomplete, a sack, or an interception, every transition receives `-1`. Run plays are excluded entirely.

**Q-Update** — A simplified Bellman update with `gamma = 0` (no discounting) is used:

```
Q(s, a) ← Q(s, a) + α × (r − Q(s, a))
```

Setting `gamma = 0` means Q-values are purely driven by the immediate reward signal rather than future state values, which is appropriate here since the reward is assigned uniformly across all transitions in a play rather than being step-wise.

After enough plays, each `(cell, direction)` pair accumulates a Q-value between `-1` and `+1` reflecting how often that movement direction from that field zone led to a completion. Cells with fewer than a minimum number of visits are displayed as neutral (yellow).

### Visualization

Each cell is divided into four triangles — one per direction — colored on a gradient from red (low Q-value) to green (high Q-value). This lets you see, at a glance, which directions are rewarded from any zone on the field.

---

## Files

`nfl_receiver_rl.R` — Core library. Defines all constants, data loading, grid logic, Q-learning functions, and the field plotting function. Sourced by both the app and the animation script.

`nfl_receiver_rl_app.R` — Interactive Shiny app. Filter by week, team, player, position, field zone, and coverage type. Includes a Q-value grid tab and a play-by-play tab showing the Q-state after each play alongside the route trace.

`nfl_receiver_rl_animation.R` — Console script that renders a GIF animating the Q-value grid updating play by play through a single game, with the corresponding route trace shown alongside.

---

## Data

This project uses tracking data from the **NFL Big Data Bowl 2023**, available on Kaggle. The following files are required in the working directory:

| File | Description |
|------|-------------|
| `week1.csv` – `week8.csv` | Player tracking data (10 fps), one file per week |
| `players.csv` | Player metadata including `nflId`, `displayName`, `officialPosition` |
| `plays.csv` | Play-level data including `gameId`, `playId`, `passResult`, and coverage info |

The tracking files contain `x`, `y`, `frameId`, `playDirection`, `event`, `nflId`, and `gameId` among other columns. Movement is normalized relative to each player's position at the snap so that all routes are oriented in the same direction regardless of which end of the field the play occurred on.

> **Note:** Data files are not included in this repository and must be downloaded separately from the [NFL Big Data Bowl 2023 Kaggle page](https://www.kaggle.com/competitions/nfl-big-data-bowl-2023). The data is subject to Kaggle's terms of use. Place all CSV files in the same directory as the R scripts before running.

---

## Dependencies

Install all required packages before running:

```r
install.packages(c(
  "tidyverse",
  "ggplot2",
  "data.table",
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "patchwork",
  "stringr",
  "nflreadr",
  "gifski"
))
```

---

## Running the App

```r
# From the directory containing all data files:
shiny::runApp("nfl_receiver_rl_app.R")
```

1. Select one or more weeks of data in the sidebar
2. Optionally filter by team, position, player, field zone, or coverage type
3. Click **Run Q-Learning**
4. The **Q-Value Grid** tab shows the final learned Q-values across all selected plays
5. The **Play-by-Play** tab lets you step through each play and see the Q-state update in real time alongside the player's route

---

## Running the Animation

The animation script runs after the core pipeline and produces a GIF showing the Q-value grid evolving play by play through a single game.

```r
source("nfl_receiver_rl.R")

result <- run_pipeline(
  filepath   = "week1.csv",
  alpha      = 0.5
)

source("nfl_receiver_rl_animation.R")
```

Output is saved as `qvalue_combined.gif` in the working directory. Each frame shows the Q-value grid on the left and the route trace for one player on the right.

---

## Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `alpha` | `0.5` | Learning rate — how much each new play shifts the Q-value |
| `min_visits` | `1` | Minimum cell visits before a Q-value is displayed (app uses 1, pipeline default is 3) |
| `n_epochs` | `20` | Number of passes through the data during training (used in `run_pipeline` only) |
| `gamma` | `0` | Discount factor — fixed at 0 throughout |

---

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) for details.

The code in this repository is original work by Brayden Butt. The NFL Big Data Bowl tracking data is not included and remains subject to its own terms of use via Kaggle.

---

## Acknowledgements

Data sourced from the [NFL Big Data Bowl 2023](https://www.kaggle.com/competitions/nfl-big-data-bowl-2023), hosted by the NFL and Kaggle. Player headshot data sourced via the [`nflreadr`](https://nflreadr.nflverse.com/) package.
