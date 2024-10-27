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
