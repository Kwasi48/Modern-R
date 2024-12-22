#statistics foundation 

library(tidyverse)
library(mdsr)
library(nycflights13)

SF <- flights |>
  filter(dest == "SFO", !is.na(arr_delay))

set.seed(101)
sf_25 <- SF |>
  slice_sample(n = 25)

sf_25 |>
  skim(arr_delay)

SF |> 
  skim(arr_delay)


sf_25 |> 
  summarise(q28 = quantile(arr_delay, p = 0.98))

SF |> 
  group_by(arr_delay < 90) |> 
  count() |> 
  mutate(pct = n / nrow(SF))


SF |> 
  summarise(q28 = quantile(arr_delay, p = 0.98))
 

n  <- 25
SF |> 
  slice_sample(n = n) |>
  summarise(mean_arr_delay = mean(arr_delay))

SF |> 
  slice_sample(n = n) |>
  summarise(mean_arr_delay = mean(arr_delay))

num_trials <- 500
sf_25_means <- 1:num_trials |>
  map_dfr(
    ~SF |>
      slice_sample(n = n) |>
      summarise(mean_arr_delay = mean(arr_delay))
  ) |>
  mutate(n = n)
head(sf_25_means)

sf_25_means |>
  skim(mean_arr_delay)

sf_25_means |>
  summarise(
    x_bar = mean(mean_arr_delay),
    se = sd(mean_arr_delay)
  ) |>
  mutate(
    ci_lower = x_bar - 2 * se,
    ci_upper = x_bar + 2 * se
  )


#T test
sf_25_means |>
  pull(mean_arr_delay) |>
  t.test()

n <- 100
sf_100_means <- 1:500 |>
  map_dfr(
    ~SF |>
      slice_sample(n = n) |>
      summarise(mean_arr_delay = mean(arr_delay))
  ) |> 
  mutate(n = n)

sf_25_means |> 
  bind_rows(sf_100_means) |>
  ggplot(aes(x = mean_arr_delay)) + 
  geom_histogram(bins = 30) + 
  facet_grid( ~ n) + 
  xlab("Sample mean")

#bootstrap
three_flights <- SF |> 
  slice_sample(n = 3, replace = TRUE) |>
  select(year, month, day, dep_time)

three_flights
three_flights |> slice_sample(n = 3, replace = TRUE)
