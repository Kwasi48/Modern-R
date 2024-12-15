#Iteration
library(tidyverse)
library(mdsr)
library(Lahman)
library(bench)
names(Teams)

str(Teams)
glimpse(Teams)

averages <- NULL
for (i in 15:40) {
  averages[i -14] <- mean(Teams[,i], na.rm =TRUE)
}
names(averages) <- names(Teams)[15:40]
averages

a <- "a String"
class(a)
is.vector(a)
length(a)

exp(1:3)

if (c(TRUE, FALSE)) {
  cat("This is a great book!")
}

x <- 1:1e5
bench::mark(
  exp(x),
  map_dbl(x, exp)
)

Teams |> 
  summarise(
    across(
      where(is.numeric), function(x) mean(x, na.rm = TRUE)
    )
  )
Teams |>
  select(15:40) |>
  map_dbl(mean, na.rm=TRUE)


angels <- Teams |> 
  filter(franchID == "ANA") |> 
  group_by(teamID, name) |>
  summarize(began = first(yearID), ended = last(yearID), .groups = "drop") |> 
  arrange(began)
angels

angels_names <- angels |>
  pull(name)
nchar(angels_names)


top5 <- function(data, team_name) {
  data |>
    filter(name == team_name) |>
    select(teamID, yearID, W, L, name) |>
    arrange(desc(W)) |>
    head(n = 5)
}

angels_names |>
  map(top5, data=Teams)

angels_names |>
  map_dfr(top5, data=Teams) |>
  group_by(teamID, name) |>
  summarise(N = n(), means_wins= mean(W)) |>
  arrange(desc(means_wins))
