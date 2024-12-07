library(mdsr)
library(tidyverse)

view(CIACountries)

g <- ggplot(data = CIACountries, aes(y = gdp, x = educ)) +
  geom_point(alpha = 0.9,aes( size= roadways)) + 
   coord_trans(y="log10") + facet_wrap(~net_users, nrow=1)+
  theme(legend.position = "top")
g


ChargesNJ <- MedicareCharges %>%
  filter(stateProvider == "NJ")
ChargesNJ


p <- ggplot(
  data = ChargesNJ, 
  aes(x = reorder(drg, mean_charge), y = mean_charge)
) +
  geom_col(fill = "gray") +
  ylab("Statewide Average Charges ($)") + 
  xlab("Medical Procedure (DRG)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(0.5))) +
  geom_point(data = MedicareCharges, size = 1, alpha = 0.3)
p


ggplot(data = mosaicData::HELPrct, aes(x = homeless)) + 
  geom_bar(aes(fill = substance), position = "fill") +
  scale_fill_brewer(palette = "Spectral") + 
  coord_flip()

g <- ggplot(data = SAT_2010, aes( x = expenditure, y = math)) + geom_point()+
  geom_smooth(method = 'lm', se=FALSE) + 
  xlab("Average expenditure per student ($1000)")+
  ylab("Average score on math SAT")

SAT_2010 <- SAT_2010 %>%
  mutate(
    SAT_rate = cut(
      sat_pct, 
      breaks = c(0, 30, 60, 100), 
      labels = c("low", "medium", "high")
    )
  )

g <- g %+% SAT_2010

g <- g %+% SAT_2010
g + aes(color = SAT_rate)
g + facet_wrap(~SAT_rate)

library(NHANES)
ggplot(
  data = slice_sample(NHANES, n = 1000), 
  aes(x = Age, y = Height, color = fct_relevel(Gender, "male"))
) + 
  geom_point() + 
  geom_smooth() + 
  xlab("Age (years)") + 
  ylab("Height (cm)") +
  labs(color = "Gender")


library(macleish)
ggplot(data = whately_2015, aes(x = when, y = temperature)) + 
  geom_line(color = "darkgray") + 
  geom_smooth() + 
  xlab(NULL) + 
  ylab("Temperature (degrees Celsius)")


whately_2015 |> 
  mutate(month = as.factor(lubridate::month(when, label = TRUE))) |>
  group_by(month) |>
  skim(temperature) |>
  select(-na)

ggplot(data = whately_2015, aes(
  x=lubridate::month(when, label=TRUE),
  y=temperature
)) + 
  geom_boxplot()+ 
  xlab("Month") +
  ylab("Temperature(degree Celsius)")


CIACountries |> 
  select(country, oil_prod) |>
  mutate(oil_prod_disc = cut(oil_prod, 
                             breaks = c(0, 1e3, 1e5, 1e6, 1e7, 1e8), 
                             labels = c(">1000", ">10,000", ">100,000", ">1 million", ">10 million" ))) |> 
  mosaic::mWorldMap(key="country") + 
  geom_polygon(aes(fill= oil_prod_disc)) + 
                 scale_fill_brewer("Oil Prod. (bbl/day)", na.value="white")+
                 theme(legend.position = "top")

library(babynames)
BabynamesDist <- make_babynames_dist()
BabynamesDist

BabynamesDist |> 
  filter(name == "Benjamin")

joseph <- BabynamesDist |>
  filter( name == "Joseph" & sex == "M") 
name_plot <- ggplot(data = joseph, aes(x=year))

name_plot <- name_plot + geom_col(
  aes(y = count_thousands * alive_prob),
  fill = "#b2d7e9",
  color = "white",
  linewidth = 0.1
)

name_plot <- name_plot + geom_line(aes(y = count_thousands), linewidth = 2)
name_plot <- name_plot + ylab("Number of People(thousands)") +
  xlab(NULL)

summary(name_plot)
wtd_quartile <- Hmisc::wtd.quantile
median_yob <- joseph |> 
  summarise(
    year = wtd_quartile(year, est_alive_today, probs = 0.5)
  ) |> 
  pull(year)
median_yob

name_plot <- name_plot +
  geom_col(
    color = "white", fill = "#008fd5",
    aes(y =ifelse(year == median_yob, est_alive_today / 1000,0))
  )

context <- tribble(
  ~year, ~num_people, ~label,
  1935, 40, "Number of Josephs\nborn each year",
  1915, 13, "Number of Josephs\nborn each year
  \nestimated to be alive\non 1/1/2014", 
  2003, 40, "The median\nliving Joseph\nis 37 years old", 
)

name_plot +
  ggtitle("Age Distribution of American Boys Named Joseph") + 
  geom_text(
    data = context, 
    aes(y = num_people, label = label, color = label)
  ) + 
  geom_curve(
    x = 1990, xend = 1974, y = 40, yend = 24, 
    arrow = arrow(length = unit(0.3, "cm")), curvature = 0.5
  ) + 
  scale_color_manual(
    guide = "none", 
    values = c("black", "#b2d7e9", "darkgray")
  ) + 
  ylim(0, 42)

name_plot %+% filter(
  BabynamesDist, 
  name == "Josephine" & sex == "F"
)


names_plot <- name_plot + 
  facet_wrap(~sex)
names_plot %+% filter(BabynamesDist, name == "Jessie")

many_names_plot <- name_plot +
  facet_grid (sex ~ name)
mnp <- many_names_plot %+% filter(
  BabynamesDist,  name %in% c ("Jessie", "Marion", "Jackie")
)
mnp

com_fem <- BabynamesDist |>
 filter( n > 100, sex == "F") |>
  mutate( wgt =  est_alive_today / sum(est_alive_today)) |>
   summarize(
     N = n(),
     est_num_alive = sum(est_alive_today),
     quantiles = list(
       wtd_quartile(
         age_today, est_alive_today, probs = 1:3/4, na.rm = TRUE
       )
     )
   ) |>
  mutate(measures = list(c("q1_age", "median_age", "q3_age"))) |>
  unnest(cols = c(quantiles, measures)) |>
  pivot_wider(names_from = measures, values_from = quantiles) |>
  arrange(desc(est_num_alive)) |>
  head(25)

w_plot <- ggplot(
  data = com_fem, 
  aes(x = reorder(name, -median_age), y = median_age)
) + 
  xlab(NULL) + 
  ylab("Age (in years)") + 
  ggtitle("Median ages for females with the 25 most common names")

w_plot <- w_plot + 
  geom_linerange(
    aes(ymin = q1_age, ymax = q3_age), 
    color = "#f3d478", 
    linewidth = 4.5, 
    alpha = 0.8
  )

w_plot <- w_plot +
  geom_point(
    fill = "#ed3324", 
    color = "white", 
    size = 2, 
    shape = 21
  )

context <- tribble(
  ~median_age, ~x, ~label, 
  65, 24, "median",
  29, 16, "25th", 
  48, 16, "75th percentile",
)

age_breaks <- 1:7 * 10 + 5

w_plot + 
  geom_point(
    aes(y = 60, x = 24), 
    fill = "#ed3324", 
    color = "white", 
    size = 2, 
    shape = 21
  ) + 
  geom_text(data = context, aes(x = x, label = label)) + 
  geom_point(aes(y = 24, x = 16), shape = 17) + 
  geom_point(aes(y = 56, x = 16), shape = 17) +
  geom_hline(
    data = tibble(x = age_breaks), 
    aes(yintercept = x), 
    linetype = 3
  ) +
  scale_y_continuous(breaks = age_breaks) + 
  coord_flip()
w_plot
