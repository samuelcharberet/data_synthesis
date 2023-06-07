### Wang 2018 - Feces nitrogen release induced by different 
### large herbivores in a dry grassland

### Data wrangling ###

library(tidyr)
library(dplyr)
library(rvest)
library(psych)

#### Excreta #### 

setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/13_005_wang_2018_large_herbivores/raw_data")
wang2018 = read.csv("Feces_Chem_Fiber_forR.csv", sep=";")

str(wang2018)
wang2018$Herbivores=as.factor(wang2018$Herbivores)
wang2018$feces_ID=as.factor(wang2018$feces_ID)


colnames(wang2018) = c('Herbivores',	'C_mg/g',	'N_mg/g',	'Hemicellulose_mg/g',
                         'Cellulose_mg/g',	'ADL_mg/g',	'ADL/N',	'feces_ID')

wang2018_longer =  pivot_longer(wang2018,
                                 cols = c('C_mg/g',	'N_mg/g'),
                                 names_to = "component_name",
                                 values_to = "component_mean",
                                 values_drop_na = TRUE)

write.csv(wang2018_longer, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/13_005_wang_2018_large_herbivores/raw_data/Feces_Chem_Fiber_longer.csv", row.names = TRUE)
