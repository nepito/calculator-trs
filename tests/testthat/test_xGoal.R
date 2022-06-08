library("tidyverse")
setwd("/workdir")
source("R/xGoal.R")
source("R/xTable.R")

describe("The class Teams", {
  teams <- Teams$new()
  path_league <- "tests/data/league_262_2021.csv"
  raw_league <- read_csv(path_league)
  expected_league <- xgoal_team_place(raw_league)
  it("Read a league file", {
    teams$read(path_league)
  })
  it("The method get_id_teams", {
    obtained_ids <- teams$get_id_teams()
    expect_true("2288" %in% obtained_ids)
    expect_true("2290" %in% obtained_ids)
    expect_true("2298" %in% obtained_ids)
  })
  it("The method set_team_from_id", {
    expected_team <- expected_league %>% filter(team_id == "2288")
    teams$set_team_from_id("2288")
    obtained_team <- teams$team
    expect_equal(expected_team, obtained_team)
    expected_team <- expected_league %>% filter(team_id == "2290")
    teams$set_team_from_id("2290")
    obtained_team <- teams$team
    expect_equal(expected_team, obtained_team)
  })
  it("The method bootstrapping_xgoal", {
    obtained_bootstrapped_xgoal <- teams$bootstrapping_xgoal()
    is_lower_max_xgol <- mean(obtained_bootstrapped_xgoal) < max(teams$team$xGol)
    expect_true(is_lower_max_xgol)
    is_greater_min_xgol <- mean(obtained_bootstrapped_xgoal) > min(teams$team$xGol)
    expect_true(is_greater_min_xgol)
    expect_true(length(obtained_bootstrapped_xgoal) == 2000)
  })
  it("The method set_names works correctly", {
    path_names <- "tests/data/names_ids_262_2021.csv"
    teams$set_names(path_names)
    expected_names <- read_csv(path_names)
    expect_equal(expected_names, teams$names)
  })
  it("The method get_name_from_id works correctly", {
    expected_name <- "Necaxa"
    obtained_name <- teams$get_name_from_id("2288")
    expect_equal(expected_name, obtained_name)
    expected_name <- "Leon"
    obtained_name <- teams$get_name_from_id("2289")
    expect_equal(expected_name, obtained_name)
    expected_name <- "Club America"
    obtained_name <- teams$get_name_from_id("2287")
    expect_equal(expected_name, obtained_name)
  })
})

describe("The class Calculator_Density", {
  density <- Calculator_Density$new()
  it("The method probability_goal", {
    expected_density <- c(rep(1 / 5, 5), 0)
    bootstrapped_xgoal <- rep(seq(0, 4), 400)
    obtained_density <- density$probability_goal(bootstrapped_xgoal)
    expect_equal(expected_density, obtained_density)
    expected_density <- c(rep(c(1 / 4, 0), 2), rep(1 / 4, 2))
    bootstrapped_xgoal <- rep(seq(0, 6, 2), 500)
    obtained_density <- density$probability_goal(bootstrapped_xgoal)
    expect_equal(expected_density, obtained_density)
  })
})

describe("The class Heat_Map", {
  heat_map <- Heat_Map$new()
  it("The method matrix_heat_map works right", {
    probability_goal <- rep(1 / 6, 6)
    expected_matrix_heat_map <- matrix(rep(probability_goal^2, 6), nrow = 6)
    obtained_matrix_heat_map <- heat_map$matrix_heat_map(probability_goal, probability_goal)
    expect_equal(expected_matrix_heat_map, obtained_matrix_heat_map)
    probability_goal <- c(rep(0.2, 5), 0)
    expected_matrix_heat_map <- matrix(c(rep(probability_goal^2, 5), rep(0, 6)), nrow = 6)
    obtained_matrix_heat_map <- heat_map$matrix_heat_map(probability_goal, probability_goal)
    expect_equal(expected_matrix_heat_map, obtained_matrix_heat_map)
    probability_goal <- c(rep(0.5, 2), rep(0, 4))
    expected_matrix_heat_map <- matrix(c(rep(probability_goal^2, 2), rep(0, 24)), nrow = 6)
    obtained_matrix_heat_map <- heat_map$matrix_heat_map(probability_goal, probability_goal)
    expect_equal(expected_matrix_heat_map, obtained_matrix_heat_map)
  })
  it("The builder make a Teams and a Calculator_Density", {
    is_there_object_Teams <- ("Teams" %in% class(heat_map$teams))
    expect_true(is_there_object_Teams)
    is_there_object_Calculator_Density <- ("Calculator_Density" %in% class(heat_map$density))
    expect_true(is_there_object_Calculator_Density)
  })
  it("The method read works right", {
    path_league <- "tests/data/league_262_2021.csv"
    heat_map$read(path_league)
    path_names <- "tests/data/names_ids_262_2021.csv"
    heat_map$set_names(path_names)
  })
  it("The method get the correct names", {
    goal_match <- heat_map$get_probable_score("2287", "2289")
    expected_home_team <- "Club America"
    obtained_home_team <- heat_map$home_team
    expect_equal(expected_home_team, obtained_home_team)
    expected_away_team <- "Leon"
    obtained_away_team <- heat_map$away_team
    expect_equal(expected_away_team, obtained_away_team)
  })
  it("The score 1-1 is the more probable", {
    goal_match <- heat_map$get_probable_score("2288", "2288")
    expect_equal(1, sum(goal_match))
    expect_equal(36, length(goal_match))
    expect_true(goal_match[1, 1] < goal_match[2, 2])
    expect_true(goal_match[3, 3] < goal_match[2, 2])
    expect_true(all(goal_match[-2, -2] < goal_match[2, 2]))
  })
})

describe("The function probability_win_draw_win", {
  it("Matrix 3x3", {
    probable_score <- matrix(rep(1 / 9, 9), nrow = 3)
    expected_probability <- rep(1 / 3, 3)
    obtained_probability <- probability_win_draw_win(probable_score)
    expect_equal(expected_probability, obtained_probability)
  })
  it("Matrix 3x3 with no draw", {
    probable_score <- matrix(rep(1 / 6, 9), nrow = 3)
    diag(probable_score) <- 0
    expected_probability <- c(1 / 2, 0, 1 / 2)
    obtained_probability <- probability_win_draw_win(probable_score)
    expect_equal(expected_probability, obtained_probability)
  })
  it("Matrix 5x5", {
    probable_score <- matrix(rep(1 / 15, 25), nrow = 5)
    probable_score[lower.tri(probable_score)] <- 0
    expected_probability <- c(2 / 3, 1 / 3, 0)
    obtained_probability <- probability_win_draw_win(probable_score)
    expect_equal(expected_probability, obtained_probability)
  })
})

describe("The class Matches", {
  it("the builder exist", {
    # matches <- Matches$new()
  })
})
