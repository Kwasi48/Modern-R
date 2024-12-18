# ethics 
library(tidyverse)
library(wru)
predict_race(voter.file = voters, surname.only = TRUE, year = 2010) |>
  select(surname, pred.whi, pred.bla, pred.his, pred.asi, pred.oth)
