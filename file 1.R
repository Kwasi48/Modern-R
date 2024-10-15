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
