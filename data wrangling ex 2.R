#data wrangling multiple tables 

statenames <- tibble(names = state.name, twoletter = state.abb)
glimpse(statenames)


statedata <- tibble(
  names = state.name, 
  income = state.x77[, 2], 
  illiteracy = state.x77[, 3]
)
glimpse(statedata)

glimpse(statedata)
