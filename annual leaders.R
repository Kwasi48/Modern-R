#Annual leaders

library(tidyverse)
library(mdsr)
library(Lahman)
library(bench)
names(Teams)

hr_leader <- function(x){
  # x is a subset of Teams for a single year and league 
  x |>
    select(teamID, HR) |>
    arrange(desc(HR)) |>
    head(1)
}

Teams |> 
  filter(yearID == 1961 & lgID == "AL") |>
  hr_leader()

hr_leaders <- Teams |> 
  group_by(yearID, lgID) |>
  group_modify(~hr_leader(.x), .keep = TRUE)

tail(hr_leaders,4)

hr_leaders |>
  group_by(lgID) |>
  summarise(mean_hr = mean(HR))

hr_leaders |>
  filter(yearID >= 1916) |>
  group_by(lgID) |>
  summarise(mean_hr = mean(HR))

hr_leaders |>
  filter(yearID >= 1916) |>
  ggplot(aes(x = yearID, y=HR, colour = lgID)) + 
  geom_line() + 
  geom_point() + 
  geom_smooth( se = FALSE) +
  geom_vline(xintercept = 1973) + 
  annotate(
    "Text", x = 1974, y = 25, 
    label = "AL adopts DH", hjust = "left"
  ) + 
  labs(x='Year', y = "Home runs", color = "League")


k_actual <- TeamRuns |> 
  group_by(yearID) |> 
  group_modify(~fit_k(.x))
k_actual |>
  ungroup() |>
  skim(k)

ggplot(data=k_actual,aes(x=k)) + 
  geom_density() +
  xlab("Best fit exponent for a single season")

n <- 10000

bstrap <- 1:n |>
  map_dbl(
    ~k_actual |>
      pull(k) |>
      sample(replace = TRUE) |>
      mean()
  )

civals <- bstrap |>
  quantile(probs = c(0.025, .975))

civals


ggplot(data = enframe(bstrap, value = "k"), aes(x = k)) + 
  geom_density() + 
  xlab("Distribution of resampled means") + 
  geom_vline(
    data = enframe(civals), aes(xintercept = value),
    color = "red", linetype = 3
  )


#BMI

library(NHANES)
 ggplot(NHANES, aes(x=Age, y = BMI)) + 
  geom_point() + 
   geom_smooth()
 
 
 bmi_plot <- function(.data, x_var) {
   ggplot(.data, aes(y = BMI)) + 
     aes_string(x = x_var) + 
     geom_jitter(alpha = 0.3) + 
     geom_smooth() + 
     labs(
       title = paste("BMI by", x_var),
       subtitle = "NHANES",
       caption = "US National Center for Health Statistics (NCHS)"
     )
 }
bmi_plot(NHANES, "Age")


c("Age", "HHIncomeMid", "PhysActiveDays", 
  "TVHrsDay", "AlcoholDay", "Pulse") |>
  map(bmi_plot, .data = NHANES) |>
  patchwork::wrap_plots(ncol = 2)
