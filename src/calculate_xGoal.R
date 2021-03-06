library(tidyverse)
source("R/xTable.R")

league <- read_csv("tests/data/statistics_39_2020.csv")
league <- league %>%
  add_home_shots_outsidebox() %>%
  add_away_shots_outsidebox()
league_home <- league %>%
  select("home", "home_shots_outsidebox", "home_shots_insidebox") %>%
  mutate(is_home = TRUE)
league_away <- league %>%
  select("away", "away_shots_outsidebox", "away_shots_insidebox") %>%
  mutate(is_home = FALSE)
colnames(league_away) <- c("gol", "outsidebox", "insidebox", "is_home")
colnames(league_home) <- c("gol", "outsidebox", "insidebox", "is_home")
cleaned_league <- bind_rows(league_home, league_away)
fit_xGoal <- function(league){
  modelo <- glm(
    gol ~ 0 + outsidebox + insidebox,
    data = league,
    poisson(link = "sqrt")
    )
}
modelo <- fit_xGoal(cleaned_league)
resumen_modelo <- summary(modelo)
xGol_inside <- resumen_modelo$coefficients[2,1]
xGol_outside <- resumen_modelo$coefficients[1,1]