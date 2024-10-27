#extended example data wrangling

library(Lahman)
dim(Teams)

mets <- Teams |> 
  filter(teamID == "NYN")
my_mets <- mets |>
  filter(yearID %in% 2004:2012)
my_mets |> 
  select(yearID, teamID, W, L)

nrow(mets)

select(filter(Teams, teamID == "NYN" & yearID %in% 2004:2012), yearID, teamID, W, L)

#Improved Example
Teams |> 
  filter(teamID == "NYN" & yearID %in% 2004:2012) |>
  select(yearID, teamID, W, L)


mets_ben <- Teams |> 
  select(yearID, teamID, W, L, R, RA) |>
  filter(teamID == "NYN" & yearID %in% 2004:2012)

mets_ben

mets_ben <- mets_ben |>
  rename(RS = R)
mets_ben

mets_ben <- mets_ben |>
  mutate(WPct = W/ (W + L))

mets_ben

mets_ben <- mets_ben |>
  mutate(WPct_hat = 1 / (1 + (RA/RS)^2))
mets_ben

mets_ben <- mets_ben |>
  mutate(W_hat = WPct_hat * (W + L))
mets_ben

filter(mets_ben, W >= W_hat)

filter(mets_ben, W < W_hat)

arrange(mets_ben, desc(WPct))

mets_ben |>
  mutate(Diff = W - W_hat) |>
  arrange(desc(Diff))

#summarise a single variable with the skim() command
mets_ben |>
  skim(W)

mets_ben |>
  summarise(
    num_years = n(),
    total_W = sum(W),
    total_L = sum(L),
    total_WPct = sum(W) / sum(W + L),
    sum_resid = sum(W - W_hat)
  )

mets_ben <- mets_ben |>
  mutate(
    gm = ifelse(
      yearID == 2004, 
      "Duquette", 
      ifelse(
        yearID >= 2011, 
        "Alderson", 
        "Minaya")
    )
  )
mets_ben

mets_ben <- mets_ben |>
  mutate(
    gm = case_when(
      yearID == 2004 ~ "Duquette",
      yearID >= 2011 ~ "Alderson",
      TRUE ~ "Minaya"
    )
  )

mets_ben  |>
  group_by(gm) |>
  summarise(
    num_years = n(),
    total_W = sum(W),
    total_L = sum(L),
    total_WPct = sum(W) / sum(W + L),
    sum_resid = sum(W - W_hat)
  ) |>
  arrange(desc(sum_resid))

Teams |>
  select(yearID, teamID, W, L, R, RA) |>
  filter(teamID == "NYN" & yearID %in% 2004:2012) |>
  rename(RS = R) |>
  mutate(
    WPct = W / (W + L), 
    WPct_hat = 1 / (1 + (RA/RS)^2), 
    W_hat = WPct_hat * (W + L), 
    gm = case_when(
      yearID == 2004 ~ "Duquette", 
      yearID >= 2011 ~ "Alderson", 
      TRUE ~ "Minaya"
    )
  ) |>
  group_by(gm) |>
  summarize(
    num_years = n(), 
    total_W = sum(W), 
    total_L = sum(L),
    total_WPct = sum(W) / sum(W + L), 
    sum_resid = sum(W - W_hat)
  ) |>
  arrange(desc(sum_resid))

Teams |>
  select(yearID, teamID, franchID, W, L, R, RA) |>
  filter(yearID %in% 2004:2012) |>
  rename(RS = R) |>
  mutate(
    WPct = W / (W + L), 
    WPct_hat = 1 / (1 + (RA/RS)^2), 
    W_hat = WPct_hat * (W + L)
  ) |>
  group_by(franchID) |>
  summarize(
    num_years = n(), 
    total_W = sum(W), 
    total_L = sum(L),
    total_WPct = sum(W) / sum(W + L), 
    sum_resid = sum(W - W_hat)
  ) |>
  arrange(sum_resid) |>
  head(6)


library(babynames)
head(babynames)

Random_subset <-  babynames |>
  filter( year %in% 1941:2010 )
Random_subset

babynames |> select(n > 100)
babynames |> select(- year)
babynames |> mutate(name_length = nchar(name))

babynames |> sex == M |> select(-prop)

babynames |> select(year, year, sex)

babynames |> group_by(n) |> summarize(ave = mean(n))

babynames |> group_by(n > 100) |> summarize(total = sum(n))

library(tidyverse)
mtcars |>
  group_by(cyl) |>
  summarize(avg_mpg = mean(mpg)) |>
  filter(am == 1)

Teams

my_teams <- Teams |>
  mutate( BA = H / AB,
          total_bases =  )
my_teams



