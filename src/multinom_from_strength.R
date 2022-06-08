library("tidyverse")
library("nnet")

league_season <- "140_2020"
path_strength_league <- glue::glue("results/strength_league_{league_season}.csv")
strength_league <- read_csv(path_strength_league)

n_data <- nrow(strength_league)
smp_size <- floor(0.80 * n_data)
train_ind <- sample(seq_len(n_data), size = smp_size)
train_strength_league <- strength_league[train_ind,]
tests_strength_league <- strength_league[-train_ind,]
train_labels <- strength_league[train_ind, "won"]
tests_labels <- strength_league[-train_ind, "won"]
model <- multinom(
  won ~ home_attack + home_deffense + away_attack + away_deffense,
  data=train_strength_league
)

upth <- 0.99
threshold <- 0.50

predictions <- cbind(predict(model, tests_strength_league, type="prob"), tests_strength_league) %>%
  select(c(3, 2, 1, won)) %>%
  mutate(pred_won = ifelse(home > threshold & home < upth, "home", ifelse(away > threshold & away < upth, "away", ifelse(draw > threshold & away < upth, "draw", 0)))) %>%
  mutate(pred = won == pred_won)
mean(predictions %>% filter(pred_won != 0) %>% .$pred)


predictions %>%
  filter(pred_won != 0) %>%
  group_by(won) %>%
  summarize(
    correct = mean(pred),
    N = n()
  )
