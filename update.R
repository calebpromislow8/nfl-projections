# Weekly update: rebuild ratings with the saved parameters, simulate the rest of the
# season and write the dashboard tables to Google Sheets.
# Runs on GitHub Actions every Tuesday; can also be run locally with `Rscript update.R`.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(nflreadr)
  library(googlesheets4)
})

source("R/functions.R")

season <- most_recent_season()
n_sims <- 10000
p      <- read_params("params.csv")

games <- build_games(2015:season) |> add_epa_pts(p)
res   <- run_ratings(games, p)

if (!any(res$games$season == season & is.na(res$games$margin))) {
  message("No regular-season games left in ", season, "; nothing to update.")
  quit(save = "no")
}

set.seed(as.integer(Sys.Date()))
sim  <- simulate_rest(res, season, n_sims, p)
tabs <- dashboard_tables(sim, res, season, n_sims)

# Service account key on GitHub Actions; browser sign-in when run locally
key <- Sys.getenv("GCP_SA_KEY_PATH")
if (nzchar(key)) gs4_auth(path = key) else gs4_auth()

for (tab in names(tabs)) sheet_write(tabs[[tab]], ss = SHEET_URL, sheet = tab)

message("Updated ", length(tabs), " tabs: ", season, " through week ", tabs$run_info$through_wk)
