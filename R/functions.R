# Shared functions for the NFL rating model and season simulation.
# Used by NFL_Predictions.Rmd (full report) and update.R (weekly run).
# Requires dplyr, tidyr and nflreadr to be loaded before sourcing.

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1TZWoCWfkL4TMYdyU3FMrlaGYjZBmDaOmvcRrRW1dUxQ/edit?gid=0#gid=0"   # the "NFL Playoffs" Google Sheet


# Teams ------------------------------------------------------------------------

# Relocated teams: map old abbreviations to current ones so ratings carry over
team_map <- c(OAK = "LV", SD = "LAC", STL = "LA")
fix_team <- function(x) coalesce(unname(team_map[x]), x)

teams <- load_teams(current = TRUE) |>
  transmute(team = team_abbr, conf = team_conf, div = team_division)
div_of  <- setNames(teams$div,  teams$team)
conf_of <- setNames(teams$conf, teams$team)

# Wild cards per conference: 3 since 2020, 2 before
n_wc_for <- function(s) if (s >= 2020) 3 else 2


# Parameters -------------------------------------------------------------------

read_params <- function(path = "params.csv") {
  d <- read.csv(path)
  as.list(setNames(d$value, d$parameter))
}

write_params <- function(p, path = "params.csv") {
  write.csv(data.frame(parameter = names(p), value = unlist(p)), path, row.names = FALSE)
}


# Data -------------------------------------------------------------------------

# Adds a team-level table to games as home_* or away_* columns
add_side <- function(games, df, side) {
  df <- rename_with(df, \(x) paste0(side, "_", x), -game_id)
  left_join(games, df, by = c("game_id", paste0(side, "_team")))
}

# One row per regular-season game, with offensive EPA and a pregame backup-QB flag
build_games <- function(seasons) {
  sched <- load_schedules(seasons) |>
    filter(game_type == "REG") |>
    mutate(across(c(home_team, away_team), fix_team))

  team_epa <- load_team_stats(seasons, summary_level = "week") |>
    filter(season_type == "REG") |>
    transmute(game_id, team = fix_team(team), epa = passing_epa + rushing_epa)

  # Backup QB = started fewer than half of the team's last 4 games this season.
  # Week 1 is never flagged; a new permanent starter clears after 2-3 starts.
  qb_flags <- bind_rows(
    transmute(sched, game_id, season, week, team = home_team, qb = home_qb_id),
    transmute(sched, game_id, season, week, team = away_team, qb = away_qb_id)
  ) |>
    arrange(season, week) |>
    mutate(lag1 = lag(qb, 1), lag2 = lag(qb, 2), lag3 = lag(qb, 3), lag4 = lag(qb, 4),
           .by = c(season, team)) |>
    mutate(prior_n    = rowSums(!is.na(pick(lag1:lag4))),
           prior_same = rowSums(pick(lag1:lag4) == qb, na.rm = TRUE)) |>
    transmute(game_id, team,
              backup = !is.na(qb) & prior_n > 0 & prior_same < prior_n / 2)

  team_game <- full_join(team_epa, qb_flags, by = c("game_id", "team"))

  sched |>
    select(game_id, season, week, gameday, location, home_team, away_team,
           home_score, away_score, spread_line, total_line, wind) |>
    add_side(team_game, "home") |>
    add_side(team_game, "away") |>
    mutate(wind        = coalesce(wind, 0),          # NA in domes
           home_backup = coalesce(home_backup, FALSE),
           away_backup = coalesce(away_backup, FALSE)) |>
    arrange(season, week, gameday)
}

# League average score and the EPA-to-points line, fit on training seasons
fit_epa_scale <- function(games, seasons) {
  g <- filter(games, season %in% seasons)
  b <- coef(lm(c(g$home_score, g$away_score) ~ c(g$home_epa, g$away_epa)))
  list(mu = mean(c(g$home_score, g$away_score), na.rm = TRUE),
       epa_b0 = b[[1]], epa_b1 = b[[2]])
}

add_epa_pts <- function(games, p) {
  mutate(games,
         home_epa_pts = p$epa_b0 + p$epa_b1 * home_epa,
         away_epa_pts = p$epa_b0 + p$epa_b1 * away_epa)
}


# Ratings ----------------------------------------------------------------------

# Offense rating = points scored above average; defense rating = points prevented.
# Home field, backup QBs and wind adjust each team's expected score.
run_ratings <- function(games, p) {
  tm    <- unique(c(games$home_team, games$away_team))
  off_r <- def_r <- setNames(rep(0, length(tm)), tm)

  # Known pregame adjustments to each team's expected score
  hfa    <- ifelse(games$location == "Home", p$hfa / 2, 0)
  base_h <- p$mu + hfa - p$qb * games$home_backup - p$wind * games$wind
  base_a <- p$mu - hfa - p$qb * games$away_backup - p$wind * games$wind

  # What each update learns from: blend of actual points and EPA points
  h_obs <- p$w * games$home_score + (1 - p$w) * games$home_epa_pts
  a_obs <- p$w * games$away_score + (1 - p$w) * games$away_epa_pts

  h_team <- games$home_team
  a_team <- games$away_team
  season <- games$season
  pred_h <- pred_a <- numeric(nrow(games))

  for (i in seq_along(h_team)) {
    if (i > 1 && season[i] != season[i - 1]) {   # offseason regression
      off_r <- off_r * (1 - p$regress)
      def_r <- def_r * (1 - p$regress)
    }
    h <- h_team[i]
    a <- a_team[i]
    pred_h[i] <- base_h[i] + off_r[h] - def_r[a]
    pred_a[i] <- base_a[i] + off_r[a] - def_r[h]
    if (is.na(h_obs[i])) next                     # future game: predict only

    err_h <- h_obs[i] - pred_h[i]
    err_a <- a_obs[i] - pred_a[i]
    off_r[h] <- off_r[h] + p$k * err_h
    def_r[a] <- def_r[a] - p$k * err_h
    off_r[a] <- off_r[a] + p$k * err_a
    def_r[h] <- def_r[h] - p$k * err_a
  }

  games <- games |>
    mutate(pred_margin = pred_h - pred_a,
           pred_total  = pred_h + pred_a,
           home_wp     = pnorm(pred_margin / p$sd_margin),
           margin      = home_score - away_score,
           total       = home_score + away_score,
           home_win    = case_when(margin > 0 ~ 1, margin < 0 ~ 0, TRUE ~ 0.5))

  list(games   = games,
       ratings = tibble(team = tm, off = off_r, def = def_r, net = off + def) |>
         arrange(desc(net)))
}


# Standings helpers ------------------------------------------------------------

# Wins per team (ties = half) from a set of completed games
season_wins <- function(g) {
  w   <- tapply(c(g$home_win, 1 - g$home_win), c(g$home_team, g$away_team), sum)
  out <- setNames(rep(0, nrow(teams)), teams$team)
  out[names(w)] <- w
  out
}

# Playoff teams whose seed can't change with one game left
locked_teams <- function(wins, n_wc = 3) {
  tm       <- names(wins)
  conf     <- conf_of[tm]
  clear    <- vapply(tm, \(x) all(abs(wins[x] - wins[conf == conf[x] & tm != x]) > 1), logical(1))
  div_lead <- wins == ave(wins, div_of[tm], FUN = max)
  wc_rank  <- ave(ifelse(div_lead, -Inf, wins), conf, FUN = \(v) rank(-v, ties.method = "min"))
  tm[clear & (div_lead | wc_rank <= n_wc)]
}

# Locked teams in each past season's final week, and whether they started a backup QB
rest_history <- function(rated_games, seasons) {
  bind_rows(lapply(seasons, \(s) {
    g      <- filter(rated_games, season == s, !is.na(margin))
    final  <- max(g$week)
    locked <- locked_teams(season_wins(filter(g, week < final)), n_wc_for(s))
    fw     <- filter(g, week == final)
    tibble(season = s, team = locked,
           rested = c(fw$home_backup, fw$away_backup)[match(locked, c(fw$home_team, fw$away_team))])
  }))
}

# Win/loss/tie plus division and conference results, per team (and per sim if present)
tally <- function(g) {
  bind_rows(
    transmute(g, across(any_of("sim")), team = home_team, opp = away_team, res = home_res),
    transmute(g, across(any_of("sim")), team = away_team, opp = home_team, res = 1 - home_res)
  ) |>
    mutate(same_div = div_of[team] == div_of[opp], same_conf = conf_of[team] == conf_of[opp]) |>
    summarise(w = sum(res == 1), l = sum(res == 0), t = sum(res == 0.5),
              div_pts  = sum(res * same_div),  div_n  = sum(same_div),
              conf_pts = sum(res * same_conf), conf_n = sum(same_conf),
              .by = any_of(c("sim", "team")))
}


# Simulation -------------------------------------------------------------------

# One simulated finish to the season; returns TRUE/FALSE home win per game.
# Ratings update after every game; locked teams may rest their QB in the final week.
sim_season <- function(fut, off_r, def_r, wins, p, n_wc) {
  h_team <- fut$home_team
  a_team <- fut$away_team
  week   <- fut$week
  noise  <- matrix(rnorm(2 * nrow(fut), sd = p$sd_margin / sqrt(2)), ncol = 2)
  home_win <- logical(nrow(fut))
  rested   <- character(0)
  checked  <- FALSE

  for (i in seq_along(h_team)) {
    if (!checked && week[i] == max(week)) {      # entering the final week
      locked  <- locked_teams(wins, n_wc)
      rested  <- locked[runif(length(locked)) < p$p_rest]
      checked <- TRUE
    }
    h <- h_team[i]
    a <- a_team[i]
    rest_h <- h %in% rested && !fut$home_backup[i]   # don't double-penalize
    rest_a <- a %in% rested && !fut$away_backup[i]
    exp_h  <- fut$base_h[i] + off_r[h] - def_r[a] - p$qb * rest_h
    exp_a  <- fut$base_a[i] + off_r[a] - def_r[h] - p$qb * rest_a
    home_win[i] <- exp_h + noise[i, 1] > exp_a + noise[i, 2]
    wins[h] <- wins[h] + home_win[i]
    wins[a] <- wins[a] + !home_win[i]

    # Noise = simulated score - expected score, i.e. the surprise
    off_r[h] <- off_r[h] + p$k * noise[i, 1]
    def_r[a] <- def_r[a] - p$k * noise[i, 1]
    off_r[a] <- off_r[a] + p$k * noise[i, 2]
    def_r[h] <- def_r[h] - p$k * noise[i, 2]
  }
  home_win
}

# Simulates the rest of season s from a run_ratings() result.
# Returns list(stand = one row per team per sim, games = every simulated game).
# Tiebreakers are simplified: win %, division or conference record, then random.
simulate_rest <- function(r, s, n_sims, p) {
  n_wc   <- n_wc_for(s)
  off0   <- setNames(r$ratings$off, r$ratings$team)
  def0   <- setNames(r$ratings$def, r$ratings$team)
  played <- filter(r$games, season == s, !is.na(margin))

  # Expected score without the rating terms (league avg, HFA, QB, wind)
  fut <- r$games |>
    filter(season == s, is.na(margin)) |>
    mutate(base_h = (pred_total + pred_margin) / 2 - unname(off0[home_team]) + unname(def0[away_team]),
           base_a = (pred_total - pred_margin) / 2 - unname(off0[away_team]) + unname(def0[home_team]))

  home_wins <- replicate(n_sims, sim_season(fut, off0, def0, season_wins(played), p, n_wc))

  sim_games <- fut |>
    select(week, home_team, away_team) |>
    slice(rep(seq_len(n()), n_sims)) |>
    mutate(sim = rep(seq_len(n_sims), each = nrow(fut)), home_res = as.numeric(home_wins))

  stand <- bind_rows(tally(sim_games),
                     cross_join(tibble(sim = seq_len(n_sims)),
                                tally(mutate(played, home_res = home_win)))) |>
    summarise(across(w:conf_n, sum), .by = c(sim, team)) |>
    left_join(teams, by = "team") |>
    mutate(pct = (w + t / 2) / (w + l + t), div_pct = div_pts / div_n,
           conf_pct = conf_pts / conf_n, rand = runif(n())) |>
    arrange(sim, desc(pct), desc(div_pct), desc(conf_pct), rand) |>
    mutate(div_winner = row_number() == 1, .by = c(sim, div)) |>
    arrange(sim, desc(div_winner), desc(pct), desc(conf_pct), rand) |>
    mutate(seed = row_number(), .by = c(sim, conf)) |>
    mutate(playoffs = seed <= 4 + n_wc)

  list(stand = stand, games = sim_games)
}


# Dashboard tables -------------------------------------------------------------

# The four tables written to Google Sheets. Probabilities are proportions (0-1).
dashboard_tables <- function(sim, r, s, n_sims) {
  stand <- sim$stand

  playoff_odds <- stand |>
    summarise(avg_wins = mean(w + t / 2),
              playoffs = mean(playoffs),
              division = mean(div_winner),
              top_seed = mean(seed == 1),
              .by = c(conf, div, team)) |>
    arrange(conf, desc(playoffs))

  # Ties shown as wins for display; tied teams get an asterisk
  tied <- unique(stand$team[stand$t > 0])
  record_odds <- stand |>
    mutate(wins = w + t, record = paste(wins, l, sep = "-")) |>
    count(team, wins, record) |>
    arrange(desc(wins)) |>                       # columns run from best to worst record
    mutate(prob = n / n_sims) |>
    select(team, record, prob) |>
    pivot_wider(names_from = record, values_from = prob, values_fill = 0) |>
    left_join(select(playoff_odds, team, avg_wins), by = "team") |>
    relocate(avg_wins, .after = team) |>
    arrange(desc(avg_wins)) |>
    mutate(team = if_else(team %in% tied, paste0(team, "*"), team))

  # Playoff odds if each team wins vs. loses its next game
  next_wk <- min(sim$games$week)
  nw      <- filter(sim$games, week == next_wk)
  next_week <- bind_rows(
    transmute(nw, sim, team = home_team, opp = away_team, home = TRUE,  win = home_res == 1),
    transmute(nw, sim, team = away_team, opp = home_team, home = FALSE, win = home_res == 0)
  ) |>
    left_join(select(stand, sim, team, playoffs), by = c("sim", "team")) |>
    summarise(win_prob = mean(win),
              now      = mean(playoffs),
              if_win   = mean(playoffs[win]),
              if_loss  = mean(playoffs[!win]),
              .by = c(team, opp, home)) |>
    mutate(game  = if_else(home, paste("vs", opp), paste("@", opp)),
           swing = if_win - if_loss) |>
    select(team, game, win_prob, now, if_win, if_loss, swing) |>
    arrange(desc(swing))

  played   <- filter(r$games, season == s, !is.na(margin))
  run_info <- tibble(updated     = format(Sys.time(), "%Y-%m-%d %H:%M %Z", tz = "America/New_York"),
                     season      = s,
                     through_wk  = max(played$week),
                     next_week   = next_wk,
                     simulations = n_sims)

  list(playoff_odds = playoff_odds, record_odds = record_odds,
       next_week = next_week, run_info = run_info)
}
