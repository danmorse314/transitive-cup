# initial transitive cup tracker

# get current year
month <- as.numeric(format(Sys.Date(), "%m"))
if(month >= 10){
  current_season <- as.numeric(format(Sys.Date(), "%Y"))+1
} else{
  current_season <- as.numeric(format(Sys.Date(), "%Y"))
}

# find last year's cup winner
last_year <- hockeyR::get_game_ids(season = current_season-1) |>
  dplyr::filter(game_type == "POST") |>
  dplyr::arrange(desc(date)) |>
  dplyr::slice(1)

games <- hockeyR::get_game_ids(season = 2023)

games <- games |>
  dplyr::filter(date <= Sys.Date()) |>
  dplyr::arrange(date)

if(nrow(games) > 0){
  
  double_games <- games |>
    dplyr::rename(
      team = home_name,
      team_score = home_final_score,
      opponent = away_name,
      opponent_score = away_final_score
    ) |>
    dplyr::bind_rows(
      games |>
        dplyr::rename(
          team = away_name,
          team_score = away_final_score,
          opponent = home_name,
          opponent_score = home_final_score
        )
    ) |>
    dplyr::arrange(date) |>
    dplyr::mutate(win = ifelse(team_score > opponent_score, 1, 0))
  
  tracker <- dplyr::tibble(
    date = last_year$date,
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
    mov = team_score - opponent_score
  )
  
  for(i in 1:nrow(games)){
    
    last_winner <- dplyr::last(tracker$team)
    
    last_game <- double_games |>
      dplyr::filter(team == last_winner & win == 0) |>
      dplyr::slice(1)
    
    tracker <- dplyr::bind_rows(
      tracker,
      dplyr::tibble(
        date = last_game$date,
        team = last_game$opponent,
        opponent = last_game$team,
        team_score = last_game$opponent_score,
        opponent_score = last_game$team_score,
        mov = team_score - opponent_score
      )
    )
    
    new_sched <- double_games |>
      dplyr::filter(team == dplyr::last(tracker$team) & date > dplyr::last(tracker$date))
    
    if(0 %in% new_sched$win & nrow(new_sched > 0)){
      next
    } else {break}
  }
  
  tracker |> saveRDS("data/trasitive_tracker.rds")
}