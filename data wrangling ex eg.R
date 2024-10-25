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
