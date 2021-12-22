### Sitters 2014 - Interactions between C : N : P stoichiometry and soil
### macrofauna control dung decomposition of savanna
### herbivores ###
### Data wrangling ###

library(tidyr)
library(dplyr)
library(rvest)



setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/0030_002_sitters_2014_traits/data_availability")
sitters2014 = read.csv("Dung_nutrient_overview_r.csv", sep=";")

colnames(sitters2014) = c('Species',	'C',	'N' ,	'P',	'date',	'day',
                          'month'	,'year',	'data', 'location',
                          'period', 'feces_ID')

sitters2014_longer =  pivot_longer(sitters2014,
                    cols = c("C", "N", "P"),
                    names_to = "component_name",
                    values_to = "component_mean",
                    values_drop_na = TRUE)

write.csv(sitters2014_longer, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/0030_002_sitters_2014_traits/data_availability/Dung_nutrient_overview_longer.csv", row.names = TRUE)
