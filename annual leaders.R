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
