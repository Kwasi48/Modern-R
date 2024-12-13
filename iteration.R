#Iteration
library(tidyverse)
library(mdsr)
library(Lahman)
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
