library(tidyverse)
library(ggpath)

tracker <- readRDS("data/transitive_tracker_an_2021_22.rds")

tracker <- tracker |>
  padr::pad(start_val = as.Date("2021-10-01")) |>
  tidyr::fill(team) |>
  mutate(
    month = factor(months(date),levels = c("October","November","December","January","February","March","April")),
    weekday = factor(weekdays(date),levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
    )

chart <- tracker |>
  mutate(
    week_num = ifelse(weekday == "Monday", 1, 0)
  ) |>
  group_by(month) |>
  mutate(
    week_num = cumsum(week_num),
    first_week = first(week_num),
    week_num = ifelse(first_week == 1, week_num-1, week_num)
  ) |>
  ungroup() |>
  separate(
    date, into = c("y","m","d"), sep = "-", remove = FALSE
  ) |>
  mutate(d = as.integer(d)) |>
  left_join(
    hockeyR::team_logos_colors |>
      mutate(full_team_name = ifelse(team_abbr == "MTL","Montréal Canadiens", full_team_name)),
    by = c("team" = "full_team_name")) |>
  left_join(
    hockeyR::team_logos_colors |>
      mutate(full_team_name = ifelse(team_abbr == "MTL","Montréal Canadiens", full_team_name)),
    by = c("opponent" = "full_team_name"), suffix=c("","_opponent")) |>
  mutate(
    angle = runif(n(),min=-15,max=15),
    angle_opponent = runif(n(),min=-15,max=15)
  )

transparent <- function(img) {
  magick::image_fx(img, expression = "0.8*a", channel = "alpha")
}

image_mod <- function(img) {
  # edge <- magick::image_canny(img, '5x5+15%+100%')
  # return(magick::image_mosaic(c(edge, img)) |> magick::image_fill("none"))
  
  return(magick::image_rotate(img,runif(1,min=-15,max=15))|> magick::image_fill("none"))
}


chart |>
  ggplot(aes(weekday, week_num)) +
  geom_text(
    aes(label = d),
    size = 2,
    nudge_x = .5, nudge_y = .5,
    hjust = 1, vjust = 1
    ) +
  ggpath::geom_from_path(
    data = chart |> filter(cup_change),
    aes(path = team_logo_espn_opponent, angle=angle_opponent),
    width = .075
  ) +
  ggpath::geom_from_path(
    aes(path = team_logo_espn, angle=angle),
    width = .075
  ) +
  geom_text(
    data = chart |> drop_na(team_score),
    aes(label = paste(team_score,opponent_score,sep="-")),
    face = "bold",
    size = 1.75, vjust = 0,  hjust = 1,  nudge_x = .5, nudge_y = -.45,
  ) +
  facet_wrap(~month, nrow = 3) +
  scale_y_reverse() +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold", size = 24,
      vjust = -65, hjust = .9
      ),
    plot.subtitle = element_text(
      size = 18,
      vjust = -85, hjust = .9
    ),
    panel.background = element_rect(fill = "darkgray"),
    plot.background = element_rect(fill = "darkgray"),
    strip.background = element_rect(size = .5, fill = "gray"),
    strip.text.x = element_text(margin = margin(.1, 0, .1, 0, "cm"))
  ) +
  labs(
    title = "The Transitive Stanley Cup",
    subtitle = "2021-22 NHL Season"
  )
ggsave("figures/transitive_cup_calendar_an.png", width = 9, height = 6)
