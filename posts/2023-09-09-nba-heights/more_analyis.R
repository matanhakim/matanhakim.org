library(tidyverse)
df_seasons_raw <- read_rds("posts/2023-09-09-nba-heights/data/seasons.rds")
df_players_raw <- read_rds("posts/2023-09-09-nba-heights/data/players.rds")

df_players <- df_players_raw |> 
  separate(ht, into = c("ht_feet", "ht_inches"), sep = "-", remove = FALSE) |> # <1>
  mutate(
    ht_tot_inches = parse_double(ht_feet) * 12 + parse_double(ht_inches),      # <2>
    ht_tot_cm = ht_tot_inches * 2.54,                                          # <2>
    birth_date = parse_date_time(birth_date, "bdy"),                           # <3>
    birth_year = year(birth_date),                                             # <3>
    birth_year_effective = if_else(                                            # <4>
      month(birth_date) > 1,                                                   # <4>
      birth_year + 1,                                                          # <4>
      birth_year                                                               # <4>
    )
  )

df_seasons <- df_seasons_raw |> 
  mutate(
    player = str_remove_all(player, "\\*"), # <1>
    across(c(age, mp), parse_double),       # <2>
    birth_year_effective = year - age       # <3>
  )

df_all <- df_seasons |> 
  left_join(df_players, join_by(player, birth_year_effective)) |> 
  mutate(
    decade = (year - year %% 10) |> # <1>
      factor() |>                   # <1>
      str_c("'s")                   # <1>
  )

mean_by <- function(data, mean_var, by_vars) {
  data |> 
    group_by({{ by_vars }}) |>
    summarise(
      mean = weighted.mean({{ mean_var }}),
      .groups = "drop"
    )
}

df_mean_years <- df_all |> 
  distinct(player, birth_year_effective, .keep_all = TRUE) |> 
  mean_by(ht_tot_inches, year)

df_mean_decades <- df_all |> 
  mean_by(ht_tot_inches, decade)

summarize_heigths <- function(data, var, by_vars, weights = NULL) {
  data |> 
    count({{ by_vars }}, {{ var }}, wt = {{ weights }}) |> 
    mutate(
      .by = {{ by_vars }},
      pct = n / sum(n),
    )
}

df_heights_years <- df_all |> 
  summarize_heigths(ht_tot_inches, by_vars = year)

df_heights_decades <- df_all |>
  summarize_heigths(ht_tot_inches, by_vars = decade)

inches_to_feet <- function(x) {
  str_c(
    x %/% 12,
    x %% 12,
    sep = "-"
  )
}

breaks_height <- (23:29) * 3
labels_height <- inches_to_feet(breaks_height)

df_mean_years |> 
  ggplot(aes(year, mean)) +
  geom_line() +
  geom_point() + 
  scale_y_continuous(labels = inches_to_feet) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  labs(
    title = "NBA players' average height has dropped over the last 20 years",
    subtitle = "2023 featured a spike in average height",
    x = "Year",
    y = "Mean Height (feet-inches)",
    caption = "Data: basketball-reference.com. Analysis: Matan Hakim."
  )

ggsave("posts/2023-09-09-nba-heights/unweighted_plot_mean.png", width = 6, height = 5)
  