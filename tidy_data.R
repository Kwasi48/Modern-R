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
  summarize(total=sum(n), .groups = "drop")

babynames |> 
  filter( name %in% c("Sue", "Robin", "Leslie")) |>
  group_by(name, sex) |>
  summarize(total = sum(n)) |>
  pivot_wider(
    names_from = sex,
    values_from = total
  )

baby_wide <- babynames |>
  group_by(sex, name) |>
  summarize(total = sum(n), .groups = "drop") |>
  pivot_wider(
    names_from = sex, 
    values_from = total, 
    values_fill = 0
  )
head(baby_wide, 3)

baby_wide |> 
  filter(M > 50000, F > 50000) |> 
  mutate(ratio = pmin(M / F, F / M)) |>
  arrange(desc(ratio)) |>
  head(3)


mdsr_url <- "https://raw.githubusercontent.com/mdsr-book/mdsr/master/data-raw/"
houses <- mdsr_url |>
  paste0("houses-for-sale.csv") |>
  read_csv()
head(houses, 3)


library(rvest)
url <- "http://en.wikipedia.org/wiki/Mile_run_world_record_progression"
tables <- url |>
  read_html() |>
  html_nodes("table")

length(tables)
head(tables)

amateur <- tables |>
  purrr::pluck(3) |>
  html_table()
amateur
