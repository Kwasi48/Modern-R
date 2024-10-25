#Data wrangling

library(tidyverse)
library(mdsr)
presidential

select(presidential, name, party)

filter(presidential, party == "Republican")

select(
  filter(presidential, year(start) > 1973 & party == "Democratic"), name
)

my_presidents <- presidential |> 
  mutate(term.length = interval(start, end) / dyears(1))
my_presidents
