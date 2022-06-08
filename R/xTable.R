library(tidyverse)
library(optparse)

calculate_points <- function(home_xGol, away_xGol) {
  diff_goals <- home_xGol - away_xGol
  points <- sum(diff_goals > 0) * 3 + sum(diff_goals == 0)
  return(points)
}

add_home_shots_outsidebox <- function(datos) {
  salida <- datos %>% mutate("home_shots_outsidebox" = home_total_shots - home_shots_insidebox)
  return(salida)
}

add_away_shots_outsidebox <- function(datos) {
  salida <- datos %>% mutate("away_shots_outsidebox" = away_total_shots - away_shots_insidebox)
  return(salida)
}

calculate_diff_goals <- function(home_xGol, away_xGol) {
  n_sample <- 2000
  diff_goals <- rpois(n_sample, home_xGol) - rpois(n_sample, away_xGol)
  return(diff_goals)
}

xGoal_all_league <- list(
  "262_2021" = list(inside = 0.096171, outside = 0.045958, penalty = 0.785234),
  "140_2020" = list(inside = 0.117440, outside = 0.043654, penalty = 0.744681),
  "140_2021" = list(inside = 0.117440, outside = 0.043654, penalty = 0.744681),
  "78_2020"  = list(inside = 0.110081, outside = 0.037332, penalty = 0.774774),
  "78_2021"  = list(inside = 0.110081, outside = 0.037332, penalty = 0.774774),
  "39_2021"  = list(inside = 0.107191, outside = 0.052831, penalty = 0.809524),
  "61_2020"  = list(inside = 0.108780, outside = 0.065102),
  "88_2021"  = list(inside = 0.097606, outside = 0.059503, penalty = 0.815126),
  "88_2020"  = list(inside = 0.097606, outside = 0.059503, penalty = 0.815126),
  "94_2021"  = list(inside = 0.102894, outside = 0.056361, penalty = 0.718182),
  "135_2021" = list(inside = 0.104484, outside = 0.054466, penalty = 0.846666)
)

xgoal_from_league_season <- function(league_season) {
  xGoal <- xGoal_all_league[[league_season]]
  return(xGoal)
}

calculate_xgoal <- function(xGol, shots_outsidebox, shots_insidebox, total_penalties) {
  xgoal <- shots_outsidebox * xGol$outside + shots_insidebox * xGol$inside + total_penalties * xGol$penalty
  return(xgoal)
}

calculate_xpoints <- function(home_xGol, away_xGol) {
  diff_goals <- calculate_diff_goals(home_xGol, away_xGol)
  xpoints <- sum(diff_goals > 0) * 3 / 2000 + sum(diff_goals == 0) / 2000
  return(xpoints)
}


cli_calculate_xpoints <- function() {
  listaOpciones <- list(
    make_option(
      c("-l", "--league-season"),
      default = "262_2021",
      help = "League and season like 78_2020: \n
        Bundesliga id is 78 \n
        Premier id is 39 \n",
      metavar = "character",
      type = "character"
    )
  )
  opt_parser <- OptionParser(option_list = listaOpciones)
  opciones <- parse_args(opt_parser)
  return(opciones)
}

cli_prediction_from_multinom <- function() {
  listaOpciones <- list(
    make_option(
      c("-l", "--league-season"),
      default = "262_2021",
      help = "League and season like 78_2020: \n
        Bundesliga id is 78 \n
        Premier id is 39 \n",
      metavar = "character",
      type = "character"
    ),
    make_option(
      c("-r", "--round"),
      default = "1",
      help = "Round",
      metavar = "character",
      type = "character"
    )
  )
  opt_parser <- OptionParser(option_list = listaOpciones)
  opciones <- parse_args(opt_parser)
  return(opciones)
}

#' @export
xgoal_team_place <- function(league) {
  league %>%
    dplyr::select(home_xGol, away_xGol, home_id, away_id, match_id) %>%
    unite(col = "home", c(home_xGol, home_id), sep = "--") %>%
    unite(col = "away", c(away_xGol, away_id), sep = "--") %>%
    gather(key = "local", value = "xGol-d", -match_id) %>%
    separate(col = "xGol-d", into = c("xGol", "team_id"), sep = "--") %>%
    mutate(xGol = as.numeric(xGol))
}

xpoint_team_place <- function(league) {
  league %>%
    select(home_xPoints, away_xPoints, home_id, away_id) %>%
    unite(col = "home", c(home_xPoints, home_id), sep = "--") %>%
    unite(col = "away", c(away_xPoints, away_id), sep = "--") %>%
    gather(key = "local", value = "xPoint-d") %>%
    separate(col = "xPoint-d", into = c("xPoints", "team_id"), sep = "--") %>%
    mutate(xPoints = as.numeric(xPoints))
}

point_team_place <- function(league) {
  league %>%
    select(home_Points, away_Points, home_id, away_id) %>%
    unite(col = "home", c(home_Points, home_id), sep = "--") %>%
    unite(col = "away", c(away_Points, away_id), sep = "--") %>%
    gather(key = "local", value = "Point-d") %>%
    separate(col = "Point-d", into = c("Points", "team_id"), sep = "--") %>%
    mutate(Points = as.numeric(Points))
}

summarize_points_played_match <- function(league) {
  league %>%
    group_by(team_id) %>%
    summarize(
      puntos = sum(Points),
      jj = n()
    )
}

summarize_xpoints_played_match <- function(league) {
  league %>%
    group_by(team_id) %>%
    summarize(
      xpuntos = sum(xPoints),
      jj = n()
    )
}

home_xPoints_all_matches <- function(league) {
  number_of_matches <- nrow(league)
  home_xPoints <- to_vec(
    for (match in 1:number_of_matches) {
      calculate_xpoints(league[match, ]$home_xGol, league[match, ]$away_xGol)
    }
  )
}

away_xPoints_all_matches <- function(league) {
  number_of_matches <- nrow(league)
  away_xPoints <- to_vec(
    for (match in 1:number_of_matches) {
      calculate_xpoints(league[match, ]$away_xGol, league[match, ]$home_xGol)
    }
  )
}

home_Points_all_matches <- function(league) {
  number_of_matches <- nrow(league)
  home_Points <- to_vec(
    for (match in 1:number_of_matches) {
      calculate_points(league[match, ]$home, league[match, ]$away)
    }
  )
}

away_Points_all_matches <- function(league) {
  number_of_matches <- nrow(league)
  away_Points <- to_vec(
    for (match in 1:number_of_matches) {
      calculate_points(league[match, ]$away, league[match, ]$home)
    }
  )
}

add_xpoints_and_points <- function(league) {
  number_of_matches <- nrow(league)
  home_xPoints <- home_xPoints_all_matches(league)
  away_xPoints <- away_xPoints_all_matches(league)
  home_Points <- home_Points_all_matches(league)
  away_Points <- away_Points_all_matches(league)
  league <- cbind(league, tibble(home_xPoints, away_xPoints, home_Points, away_Points))
}

previous_season <- function(id_season) {
  id <- str_split(id_season, "_")[[1]][1]
  previous_season <- as.character(as.numeric(str_split(id_season, "_")[[1]][2]) - 1)
  id_previous_season <- paste(id, previous_season, sep = "_")
  return(id_previous_season)
}

get_strength_atack <- function(league, id) {
  attack <- c(league %>% filter(home_id == id) %>% .$home_xGol, league %>% filter(away_id == id) %>% .$away_xGol)
  return(mean(attack))
}

get_strength_deffense <- function(league, id) {
  attack <- c(league %>% filter(home_id == id) %>% .$away_xGol, league %>% filter(away_id == id) %>% .$home_xGol)
  return(mean(attack))
}
