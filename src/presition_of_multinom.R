library("tidyverse")
library("nnet")
source("R/xTable.R")

opciones <- cli_calculate_xpoints();
league_season <- opciones[["league-season"]]
previous_league_season <- previous_season(league_season)
path_strength_league <- glue::glue("results/strength_league_{previous_league_season}.csv")
strength_league <- read_csv(path_strength_league)

model <- multinom(
  won ~ home_attack + home_deffense + away_attack + away_deffense,
  data=strength_league
)

path_strength_league <- glue::glue("results/strength_league_{league_season}.csv")
tests_strength_league <- read_csv(path_strength_league)
upth <- 0.99
threshold <- 0.50

predictions <- cbind(predict(model, tests_strength_league, type="prob"), tests_strength_league) %>%
  select(c(3, 2, 1, won)) %>%
  mutate(pred_won = ifelse(home > threshold & home < upth, "home", ifelse(away > threshold & away < upth, "away", ifelse(draw > threshold & away < upth, "draw", 0)))) %>%
  mutate(pred = won == pred_won)
mean(predictions %>% filter(pred_won != 0) %>% .$pred)


(predictions %>%
  filter(pred_won != 0) %>%
  group_by(won) %>%
  summarize(
    correct = mean(pred),
    N = n()
  ))
