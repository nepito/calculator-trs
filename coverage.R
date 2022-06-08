library(covr)
library(testthat)
cobertura <- file_coverage(
    c(
        "R/xGoal.R",
        "R/xTable.R"
    ),
    c(
        "tests/testthat/test_xGoal.R",
        "tests/testthat/test_xTable.R"
    )
)
covr::codecov(covertura=cobertura, token='1618c982-0462-40d3-b33c-7b7f1c654e96')