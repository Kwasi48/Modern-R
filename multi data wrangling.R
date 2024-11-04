#data wrangling on multiple 

library(tidyverse)
library(mdsr)
library(nycflights13)
glimpse(flights)

head(airlines, 3)

#inner join used here
flights_joined <- flights |>
  inner_join(airlines, by = c("carrier" = "carrier"))

glimpse(flights_joined)

flights_joined |>
  select(carrier, name, flight, origin, dest) |> 
  head(3)

nrow(flights)
nrow(flights_joined)

airports_pt <- airports |>
  filter(tz == -8)
nrow(airports_pt)

nyc_dests_pt <- flights |>
  inner_join(airports_pt, by = c("dest" = "faa"))
nrow(nyc_dests_pt)

nyc_dests <- flights |>
  left_join(airports_pt, by = c("dest" = "faa"))

nyc_dests |>
  summarize(
    num_flights = n(),
    num_flights_pt = sum(!is.na(name)),
    num_flights_not_pt = sum(is.na(name))
  )

#Extended example
library(Lahman)
manny <- Batting |>
  filter(playerID == "ramirma02")
nrow(manny)

manny |>
  group_by(lgID) |>
  summarise(
    span = paste(min(yearID), max(yearID), sep = "-"),
    num_years = n_distinct(yearID),
    num_teams = n_distinct(teamID),
    BA = sum(H) / sum(AB),
    tH = sum(H),
    tHR = sum(HR),
    tRBI = sum(RBI)
  ) |>
  arrange(span)

#number of seasons in which Ramirez hit at least 30 home runs (wrong)
manny |>
  filter(HR >= 30) |>
  nrow()

#correct 
manny |>
  group_by(yearID) |>
  summarise(tHR = sum(HR)) |>
  filter(tHR >= 30) |>
  nrow()

Batting |>
  filter(playerID == "ramirma02") |>
  inner_join(People, by = c("playerID" = "playerID")) |>
  group_by(yearID) |>
  summarize(
    Age = max(yearID - birthYear), 
    num_teams = n_distinct(teamID), 
    BA = sum(H)/sum(AB), 
    tH = sum(H), 
    tHR = sum(HR), 
    tRBI = sum(RBI)
  ) |>
  arrange(yearID)
