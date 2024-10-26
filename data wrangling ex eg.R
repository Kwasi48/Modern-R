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