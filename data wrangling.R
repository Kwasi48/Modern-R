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

my_presidents <- my_presidents |> 
  mutate(elected = year(start) - 1)

my_presidents <- my_presidents |>
  mutate(elected = ifelse(elected %in% c(1962, 1973), NA, elected))
my_presidents


my_presidents <- my_presidents |> 
  rename(term_length = term.length)
my_presidents 
  

my_presidents |>
  arrange(desc(term_length))

my_presidents %>% 
  arrange(desc(term_length), party, elected)


my_presidents |>
  summarize(
    N= n(),
    first_year = min(year(start)),
    last_year = max(year(end)),
    num_dems = sum(party == "Democrats"),
    years = sum(term_length),
    avg_term_length = mean(term_length)
  )

