### Herbivore dung quality affects plant community diversity ###
### Data wrangling

library(tidyr)
library(dplyr)
library(rvest)



setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_stoichiometry/0_data_literature/2_done_to_check/0302_001_valdes.correcher_2019_herbivores")
vd2019 = read.csv("data_availability_Herbivoredung_CNP_for_r.csv", sep=";")

colnames(vd2019) = c('Herbivore_species',	'Site',	'Season',	'Body_weight',	
                     'C',	'N',	'P',	'CN',	'CP',	'NP',	'feces_ID',	'unit')
vd2 =  pivot_longer(vd2019,
    cols = c("C", "N", "P"),
    names_to = "component_name",
    values_to = "component_mean",
    values_drop_na = TRUE)

write.csv(vd2, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_stoichiometry/0_data_literature/2_done_to_check/0302_001_valdes.correcher_2019_herbivores/data_availability_Herbivoredung_CNP_wide.csv", row.names = TRUE)
