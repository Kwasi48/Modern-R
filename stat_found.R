#statistics foundation 

library(tidyvese)
library(msdr)
library(nycflights13)

SF <- flights |>
  filter(dest == "SFO", !is.na(arr_delay))

set.seed(101)
sf_25 <- SF |>
  slice_sample(n = 25)

sf_25 |>
  skim(arr_delay)