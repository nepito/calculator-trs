setwd("/workspaces/calculator-trs")
source("R/xTable.R")
source("R/xGoal.R")
source("R/calculate_trs.R")
in_file <- "tests/data/statistics_262_2019.csv"
input_file <- read.csv(in_file)
testthat::test_that("dummy test", {
  # testthat::expect_equal(input_file, input_file)
})


testthat::test_that("obtained expected data", {
  home_shots <- 3
  away_shots <- 3
  expected_trs <- 0.5
  obtained_trs <- calculate_trs(home_shots, away_shots)
  #  testthat::expect_equal(expected_trs, obtained_trs)
  home_shots <- 1
  away_shots <- 9
  expected_trs <- 0.1
  obtained_trs <- calculate_trs(home_shots, away_shots)
  # testthat::expect_equal(expected_trs, obtained_trs)
})

out_file <- "tests/data/expected_statistics_262_2019.csv"
output_file <- read.csv(out_file)
testthat::test_that("obtained expected data frame", {
  #  testthat::expect_equal(input_file, output_file)
})
