library(tidyverse)

tracker <- readRDS("data/transitive_tracker_2021_22.rds")

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
    by = c("team" = "full_team_name"))

transparent <- function(img) {
  magick::image_fx(img, expression = "0.8*a", channel = "alpha")
}

chart |>
  ggplot(aes(weekday, week_num)) +
  geom_text(
    aes(label = d),
    size = 2,
    nudge_x = .5, nudge_y = .5,
    hjust = 1, vjust = 1
    ) +
  ggimage::geom_image(
    aes(image = team_logo_espn),
    size = .075, asp = 1.5,
    image_fun = transparent
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
ggsave("figures/transitive_cup_calendar.png", width = 9, height = 6)
