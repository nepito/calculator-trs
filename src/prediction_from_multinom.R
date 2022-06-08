library("tidyverse")
library("jsonlite")
library("nnet")
source("R/xTable.R")

opciones <- cli_prediction_from_multinom();
league_season <- opciones[["league-season"]]
previous_league_season <- previous_season(league_season)
path_strength_league <- glue::glue("results/strength_league_{previous_league_season}.csv")
strength_league <- read_csv(path_strength_league)

model <- multinom(
  won ~ home_attack + home_deffense + away_attack + away_deffense,
  data=strength_league
)


get_names <- function(id) {
  name <- names %>%
    filter(ids == id) %>%
    .$names
  return(name)
}


path_names <- glue::glue("tests/data/names_{league_season}.csv")
names <- read_csv(path_names)
path_strength_league <- glue::glue("results/strength_league_{league_season}.csv")
strength_league <- read_csv(path_strength_league)
path_league <- glue::glue("results/league_{league_season}.csv")
league <- read_csv(path_league)
path_season <- glue::glue("tests/data/season_{league_season}.csv")
season <- read_csv(path_season)
round_str <- opciones[["round"]]
n_round <- glue::glue("Regular Season - {round_str}")
round <- season %>%
  filter(round == n_round)

home_id <- round$home_id
away_id <- round$away_id
home_attack <- comprehenr::to_vec(for (id in home_id) get_strength_atack_streak(league, id))
home_deffense <- comprehenr::to_vec(for (id in home_id) get_strength_deffense_streak(league, id))
away_attack <- comprehenr::to_vec(for (id in away_id) get_strength_atack_streak(league, id))
away_deffense <- comprehenr::to_vec(for (id in away_id) get_strength_deffense_streak(league, id))
to_predict <- tibble(away_attack, away_deffense, home_deffense, home_attack)
pred <- predict(model, to_predict, type="prob")
(predictions <- tibble("home" = pred[,3], "draw" = pred[,2], "away" = pred[,1]) %>%
  cbind(home_id, away_id) %>%
  mutate(home_team = mapply(function(x) get_names(x), home_id)) %>%
  mutate(away_team = mapply(function(x) get_names(x), away_id)) %>%
  select(c(6,1,2,3,7)))
predictions_round <- cbind(predictions, id_match = round$id_match, home_id = round$home_id, away_id = round$away_id)
output_file <- glue::glue("api_predictions/data/predictions_{league_season}_{round_str}.json")
write(toJSON(predictions_round), output_file)
