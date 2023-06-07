### Hulot 2019 - A first assessment of megaherbivore subsidies in artificial
### waterholes in Hwange National Park, Zimbabwe
### Data wrangling ###

library(tidyr)
library(dplyr)
library(rvest)



setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/0030_004_hulot_2019/raw_data")
hulot2019 = read.csv("data_dung_r.csv", sep=";")

colnames(hulot2019) = c('REFCLIENT',	'day',	'month',	'year'	,
                        'Species',	'Season',	'Type',	'MS'	,
                        'C',	'MO',	'N',	'P',	'feces_ID')

hulot2019_longer =  pivot_longer(hulot2019,
                                   cols = c("C", "N", "P"),
                                   names_to = "component_name",
                                   values_to = "component_mean",
                                   values_drop_na = TRUE)

write.csv(hulot2019_longer, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/0030_004_hulot_2019/raw_data/dung_data_longer.csv", row.names = TRUE)
