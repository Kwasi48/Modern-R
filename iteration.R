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

exp_wpct <- function(x) { 
  return(1/(1 + (1/x)^2))
}

TeamRuns <- Teams |> 
  filter(yearID >= 1954) |>
  rename(RS = R) |> 
  mutate(WPct = W / (W + L), run_ratio = RS/RA) |>
  select(yearID, teamID, lgID, WPct, run_ratio)

ggplot(data = TeamRuns, aes(x = run_ratio, y = WPct)) +
  geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
  geom_hline(yintercept = 0.5, color = "darkgray", linetype = 2) +
  geom_point(alpha = 0.2) + 
  stat_function(fun = exp_wpct, linewidth = 2, color = "blue") + 
  xlab("Ratio of Runs Scored to Runs Allowed") + 
  ylab("Winning Percentage")

TeamRuns |>
  nls(
    formula = WPct ~ 1/(1 + (1/run_ratio)^k), 
    start = list(k = 2)
  ) |>
  coef()

fit_k <- function(x) {
  mod <- nls(
    formula = WPct ~ 1/(1 + (1/run_ratio)^k), 
    data = x,
    start = list(k = 2)
  )
  return(tibble(k = coef(mod), n = nrow(x)))
}

fit_k(TeamRuns)

TeamRuns |> 
  mutate(decade = yearID %/% 10 * 10) |>
  group_by(decade) |> 
  group_modify(~fit_k(.x))
