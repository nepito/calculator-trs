library("tidyverse")
library("class")

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
model_knn <- knn(
  train=train_strength_league[,4:7],
  test=tests_strength_league[, 4:7],
  cl=as.factor(strength_league[train_ind,]$won),
  k=17,
  prob=T
)

upth <- 0.99
threshold <- 0.50
predictions <- cbind("prob"=attr(model_knn, "prob"), model_knn, tests_strength_league) %>%
  select(c("prob", "model_knn", won)) %>%
  mutate(pred_won = ifelse(prob > threshold & prob < upth, model_knn, 0)) %>%
  mutate(pred = won == model_knn)
mean(predictions %>% filter(pred_won != 0) %>% .$pred)

predictions %>%
  filter(pred_won != 0) %>%
  group_by(won) %>%
  summarize(
    correct = mean(pred),
    N = n()
  )
