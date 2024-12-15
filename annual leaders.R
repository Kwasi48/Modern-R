#Annual leaders

library(tidyverse)
library(mdsr)
library(Lahman)
library(bench)
names(Teams)

hr_leader <- function(x){
  # x is a subset of Teams for a single year and league 
  x |>
    select(teamID, HR) |>
    arrange(desc(HR)) |>
    head(1)
}

Teams |> 
  filter(yearID == 1961 & lgID == "AL") |>
  hr_leader()

hr_leaders <- Teams |> 
  group_by(yearID, lgID) |>
  group_modify(~hr_leader(.x), .keep = TRUE)

tail(hr_leaders,4)

hr_leaders |>
  group_by(lgID) |>
  summarise(mean_hr = mean(HR))

hr_leaders |>
  filter(yearID >= 1916) |>
  group_by(lgID) |>
  summarise(mean_hr = mean(HR))

hr_leaders |>
  filter(yearID >= 1916) |>
  ggplot(aes(x = yearID, y=HR, colour = lgID)) + 
  geom_line() + 
  geom_point() + 
  geom_smooth( se = FALSE) +
  geom_vline(xintercept = 1973) + 
  annotate(
    "Text", x = 1974, y = 25, 
    label = "AL adopts DH", hjust = "left"
  ) + 
  labs(x='Year', y = "Home runs", color = "League")


k_actual <- TeamRuns |> 
  group_by(yearID) |> 
  group_modify(~fit_k(.x))
k_actual |>
  ungroup() |>
  skim(k)

ggplot(data=k_actual,aes(x=k)) + 
  geom_density() +
  xlab("Best fit exponent for a single season")