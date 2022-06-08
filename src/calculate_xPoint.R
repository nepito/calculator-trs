#!/usr/bin/env Rscript
#
#

library(tidyverse)
library(comprehenr)
source("R/xTable.R")

opciones <- cli_calculate_xpoints();
league_season <- opciones[["league-season"]]
statistics_path <- glue::glue("tests/data/statistics_{league_season}.csv")
xGol <- xgoal_from_league_season(league_season)
league <- read_csv(statistics_path)
league[is.na(league)] <- 0
league <- league %>%
  add_home_shots_outsidebox() %>%
  add_away_shots_outsidebox() %>%
  mutate("home_xGol" = calculate_xgoal(xGol, home_shots_outsidebox, home_shots_insidebox, home_total_penalties)) %>%
  mutate("away_xGol" = calculate_xgoal(xGol, away_shots_outsidebox, away_shots_insidebox, away_total_penalties))

league <- add_xpoints_and_points(league)

league %>% write_csv(glue::glue("results/league_{league_season}.csv"))