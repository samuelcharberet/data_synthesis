### Sitters 2017 - A stoichiometric perspective of the effect of herbivore dung on ecosystem functioning

### Data wrangling ###

library(tidyr)
library(dplyr)
library(rvest)
library(psych)

#### Excreta #### 

setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/13_006_sitters_2017/raw_data")
sitters2017 = read.csv("dung_nutrient_data_forR.csv", sep=";")

colnames(sitters2017) = c('species',	'latin_species_names',	'feeding_strategy',
                       'N',	'C',	'P',	'd13C',	'feces_ID')

str(sitters2017)
sitters2017$species=as.factor(sitters2017$species)
sitters2017$latin_species_names=as.factor(sitters2017$latin_species_names)
sitters2017$feeding_strategy=as.factor(sitters2017$feeding_strategy)


sitters2017_longer =  pivot_longer(sitters2017,
                                 cols = c('C',	'N', 'P', 'd13C'),
                                 names_to = "component_name",
                                 values_to = "component_mean",
                                 values_drop_na = TRUE)

write.csv(sitters2017_longer, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/13_006_sitters_2017/raw_data/dung_nutrient_data_longer.csv", row.names = TRUE)
