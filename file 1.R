library(mdsr)
library(tidyverse)

view(CIACountries)

g <- ggplot(data = CIACountries, aes(y = gdp, x = educ)) + geom_point((color=net_users),size = 3)
g
