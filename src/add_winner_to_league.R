library("tidyverse")
source("R/xTable.R")

opciones <- cli_calculate_xpoints();
league_season <- opciones[["league-season"]]
path_names <- glue::glue("tests/data/names_{league_season}.csv")
names <- read_csv(path_names)
path_league <- glue::glue("results/league_{league_season}.csv")
league <- read_csv(path_league)

how_won <- function(home, away) {
  if (home > away) {
    return("home")
  }
  if (home < away) {
    return("away")
  }
  return("draw")
}



attack <- comprehenr::to_vec(for (id in names[["ids"]]) get_strength_atack(league, id))
deffense <- comprehenr::to_vec(for (id in names[["ids"]]) get_strength_deffense(league, id))
strength <- tibble(ids = names[["ids"]], attack = attack, deffense = deffense)
add_attack <- function(id) {
  return(strength %>% filter(ids == id) %>% .$attack)
}
add_deffense <- function(id) {
  return(strength %>% filter(ids == id) %>% .$deffense)
}

data <- league %>%
  mutate(won = mapply(function(x, y) how_won(x, y), home, away)) %>%
  mutate(home_attack = mapply(function(x) add_attack(x), home_id)) %>%
  mutate(away_attack = mapply(function(x) add_attack(x), away_id)) %>%
  mutate(home_deffense = mapply(function(x) add_deffense(x), home_id)) %>%
  mutate(away_deffense = mapply(function(x) add_deffense(x), away_id)) %>%
  select(home, away, won, home_attack, home_deffense, away_attack, away_deffense)

path_output <- glue::glue("results/strength_league_{league_season}.csv")
write_csv(data, path_output)

