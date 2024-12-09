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

records <- tables |>
  purrr::pluck(5) |>
  html_table() |>
  select(-Auto)  |> # remove unwanted column 
  arrange(desc(Athlete))
records


translations <- mdsr_url |>
  paste0("house_codes.csv") |>
  read_csv()
translations |> 
  head(5)

codes <- translations |>
  pivot_wider(
    names_from = system_type, 
    values_from = meaning, 
    values_fill = "invalid"
  )
codes

houses <- houses |>
  left_join(
    codes |>
      select(code, fuel_type), 
    by = c(fuel = "code")
  ) |>
  left_join(
    codes |> 
      select(code, heat_type), 
    by = c(heat = "code")
  ) |>
  left_join(
    codes |> 
      select(code, sewer_type), 
    by = c(sewer = "code")
  )
houses

ordway_birds |>
  select(Timestamp, Year, Month, Day) |>
  glimpse()

library(readr)
ordway_birds <- ordway_birds |>
  mutate(
    Month = parse_number(Month),
    Year = parse_number(Year),
    Day = parse_number(Day),
  )

ordway_birds |> 
  select(Timestamp, Year, Month, Day) |>
  glimpse()

library(lubridate)
birds <- ordway_birds |>
  mutate(When = mdy_hms(Timestamp)) |>
  select(Timestamp, Year, Month, Day, When, DataEntryPerson)
birds |>
  glimpse()

birds |>
  ggplot(aes(x = When, y = DataEntryPerson)) + 
  geom_point(alpha = 0.1, position = "jitter") 

bird_summary <- birds |>
  group_by(DataEntryPerson) |>
  summarize(
    start = first(When), 
    finish = last(When)
  ) |>
  mutate(duration = interval(start, finish) / ddays(1))
bird_summary


now()
class(now())

class(as.POSIXlt(now()))
as.Date(now())


library(lubridate)
example <- c("2021-04-29 06:00:00", "2021-12-31 12:00:00")
str(example)

converted <- ymd_hms(example)
str(converted)

converted

converted[2] - converted[1]

#Japanese Reactors
tables <- "https://en.wikipedia.org/wiki/List_of_commercial_nuclear_reactors" |>
  read_html() |>
  html_nodes(css = "table")

idx <- tables |>
  html_text() |>
  str_detect("Fukushima Daiichi") |>
  which()

reactors <- tables |>
  purrr::pluck(idx) |>
  html_table(fill = TRUE) |>
  janitor::clean_names() |>
  rename(
    reactor_type = type,
    reactor_model = model,
    capacity_net = capacity_mw
  ) |>
  tail(-1)

glimpse(reactors)


reactors <- reactors |>
  mutate(
    plant_status = ifelse(
      str_detect(status, "Shut down"), 
      "Shut down", "Not formally shut down"
    ), 
    construct_date = dmy(beginbuilding), 
    operation_date = dmy(commercialoperation), 
    closure_date = dmy(closed)
  )

glimpse(reactors)


ggplot(
  data = reactors, 
  aes(x = construct_date, y = capacity_net, color = plant_status
  )
) +
  geom_point() + 
  geom_smooth() + 
  xlab("Date of Plant Construction") + 
  ylab("Net Plant Capacity (MW)")
