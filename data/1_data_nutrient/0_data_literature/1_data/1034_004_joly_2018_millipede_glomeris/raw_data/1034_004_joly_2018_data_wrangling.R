### Joly 2018 - Litter conversion into detritivore faeces reshuffles the quality
### control over C and N dynamics during decomposition
### Data wrangling ###

library(tidyr)
library(dplyr)
library(plyr)
library(rvest)
library(psych)

#### Excreta #### 

setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/1034_004_joly_2018_millipede_glomeris/raw_data")
joly2018 = read.csv("Phys&Chem_characteristics_forR.csv", sep=";")

str(joly2018)
colnames(joly2018) = c('tree_species',	'form',	'replicate',	'C',	'N',
                       'CN_ratio')

joly2018$tree_species=as.factor(joly2018$tree_species)
joly2018$form=as.factor(joly2018$form)
joly2018$replicate=as.factor(joly2018$replicate)


joly2018_lengthened =  pivot_longer(joly2018,
                                    cols = c('C',	'N', 'CN_ratio'),
                                    names_to = "component_name",
                                    values_to = "component_value",
                                    values_drop_na = TRUE)


joly2018_lengthened_aggregated <- ddply(joly2018_lengthened, c("tree_species", "form", "component_name"), summarise,
                component_mean=mean(component_value),
                error=sd(component_value),
                min=min(component_value),
                max=max(component_value))



write.csv(joly2018_lengthened_aggregated, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/1034_004_joly_2018_millipede_glomeris/raw_data/Phys&Chem_characteristics_lengthened_aggregated.csv", row.names = TRUE)
