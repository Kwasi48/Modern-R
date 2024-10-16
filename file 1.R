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
