#tidy_data

library(tidyverse)
library(mdsr)
library(googlesheets4)
gs4_deauth()
hiv_key <- "1kWH_xdJDM4SMfT_Kzpkk-1yuxWChfurZuWYjfmv51EA"
hiv <- read_sheet(hiv_key) |>
  rename(Country = 1) |>
  filter(
    Country %in% c("United States", "France", "South Africa")
  ) |>
  select(Country, `1979`, `1989`, `1999`, `2009`) |>
  unnest(cols = c(`2009`)) |>
  mutate(across(matches("[0-9]"), as.double))
hiv


hiv|>
  pivot_longer(-Country, names_to = "Year", values_to = "hiv_rate")

library(babynames)

popular_names <- babynames |>
  group_by(sex, name) |>
  summarize(total_births = sum(n), .groups = "drop") |>
  arrange(desc(total_births))
popular_names

babynames |>
  filter(name == "Sue") |>
  group_by(name, sex) |>
  summarize(total = sum(n), .groups = "drop")


babynames |>
  filter(name == "Robin") |>
  group_by(name, sex) |>
  summarize(total = sum(n), .groups = "drop")
