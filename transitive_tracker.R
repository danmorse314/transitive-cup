# initial transitive cup tracker

# get current year
month <- as.numeric(format(Sys.Date(), "%m"))
if (month >= 10) {
  current_season <- as.numeric(format(Sys.Date(), "%Y")) + 1
} else {
  current_season <- as.numeric(format(Sys.Date(), "%Y"))
}

current_season = 2022

# find last year's cup winner
last_year <- hockeyR::get_game_ids(season = current_season - 1) |>
  dplyr::filter(game_type == "POST") |>
  dplyr::arrange(desc(date)) |>
  dplyr::slice(1)

games <- hockeyR::get_game_ids(season = current_season)

games <- games |>
  dplyr::filter(date <= Sys.Date() & game_type == "REG" & !(home_final_score == 0 & away_final_score == 0)) |>
  dplyr::arrange(date)

if (nrow(games) > 0) {
  double_games <- games |>
    dplyr::mutate(
      home_team = home_name
    ) |>
    dplyr::rename(
      team = home_name,
      team_score = home_final_score,
      opponent = away_name,
      opponent_score = away_final_score
    ) |>
    dplyr::bind_rows(
      games |>
        dplyr::mutate(
          home_team = home_name
        ) |>
        dplyr::rename(
          team = away_name,
          team_score = away_final_score,
          opponent = home_name,
          opponent_score = home_final_score,
        )
    ) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::arrange(date, home_team) |>
    dplyr::mutate(win = ifelse(team_score > opponent_score, TRUE, FALSE))

  tracker <- dplyr::tibble(
    date = as.Date(last_year$date),
    team = ifelse(
      last_year$home_final_score > last_year$away_final_score,
      last_year$home_name,
      last_year$away_name
    ),
    opponent = ifelse(
      last_year$home_final_score > last_year$away_final_score,
      last_year$away_name,
      last_year$home_name
    ),
    team_score = ifelse(
      last_year$home_final_score > last_year$away_final_score,
      last_year$home_final_score,
      last_year$away_final_score
    ),
    opponent_score = ifelse(
      last_year$home_final_score > last_year$away_final_score,
      last_year$away_final_score,
      last_year$home_final_score
    ),
    home_team = last_year$home_name,
    mov = team_score - opponent_score
  )

  for (i in 1:nrow(games)) {
    last_winner <- dplyr::last(tracker$team)

    last_game <- double_games |>
      dplyr::filter(team == last_winner & date > dplyr::last(tracker$date)) |>
      dplyr::slice(1)

    tracker <- dplyr::bind_rows(
      tracker,
      dplyr::tibble(
        date = as.Date(last_game$date),
        team = dplyr::if_else(last_game$win, last_game$team, last_game$opponent),
        opponent = dplyr::if_else(last_game$win, last_game$opponent, last_game$team),
        team_score = dplyr::if_else(last_game$win, last_game$team_score, last_game$opponent_score),
        opponent_score = dplyr::if_else(last_game$win, last_game$opponent_score, last_game$team_score),
        home_team = last_game$home_team,
        mov = team_score - opponent_score,
        cup_change = !last_game$win
      )
    )

    new_sched <- double_games |>
      dplyr::filter(team == dplyr::last(tracker$team) & date > dplyr::last(tracker$date))

    if (0 %in% new_sched$win & nrow(new_sched > 0)) {
      next
    } else {
      break
    }
  }

  tracker |> saveRDS(paste0("data/transitive_tracker_an_", current_season - 1, "_", substr(current_season, 3, 4), ".rds"))
}
