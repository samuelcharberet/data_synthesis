### De Swardt 2018 - Insect outbreaks alter nutrient dynamics in a southern 
### African savanna: patchy defoliation of Colophospermum mopane savanna by Imbrasia belina larvae
### Data wrangling ###

library(tidyr)
library(dplyr)
library(plyr)
library(rvest)
library(psych)

#### Insect frass, body and food #### 

setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/1234_001_deswardt_2018_insects/data_dryad")
deswardt2018 = read.csv("1234_001_deswardt_2018_raw_data.csv", sep=";")

str(deswardt2018)
colnames(deswardt2018) = c('Treatment',	'P_%',	'K_%',	'Ca_%',	'Mg_%',	
                             'Organic_C_%',	'N_%',	'Na_mg/kg',	
                           'Fe_mg/kg',	'Cu_mg/kg',	'Zn_mg/kg',	'Mn_mg/kg'	,
                           'B_mg/kg',	'Al_mg/kg',	'obs_ID')


deswardt2018_lengthened =  pivot_longer(deswardt2018,
                                       cols = c('P_%',	'K_%', 'Ca_%','Mg_%', 
                                                'Organic_C_%', 'N_%',	'Na_mg/kg',	
                                                'Fe_mg/kg',	'Cu_mg/kg',	
                                                'Zn_mg/kg',	'Mn_mg/kg','B_mg/kg',
                                                'Al_mg/kg'),
                             
                                       names_to = "component_name",
                                       values_to = "component_value",
                                       values_drop_na = TRUE)

deswardt2018_lengthened_aggregated <- ddply(deswardt2018_lengthened, c("Treatment", "component_name"), summarise,
                      component_mean=mean(component_value),
                      error=sd(component_value),
                      min=min(component_value),
                      max=max(component_value))


deswardt2018_lengthened_aggregated$unit=NA
deswardt2018_lengthened_aggregated$method=NA


# Loop to put in a unit column the right unit

for (i in 1:length(deswardt2018_lengthened_aggregated$Treatment)) {
  if (grepl("%+",deswardt2018_lengthened_aggregated$component_name[i])==TRUE) {
    deswardt2018_lengthened_aggregated$unit[i]='pourcentage'
  } else {
    deswardt2018_lengthened_aggregated$unit[i]='mg/kg'
  }
}

# Loop to put in a method column the right method based on the paper

for (i in 1:length(deswardt2018_lengthened_aggregated$Treatment)) {
  if (deswardt2018_lengthened_aggregated$component_name[i]=='N_%') {
    deswardt2018_lengthened_aggregated$method[i]='elemental_analyzer'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='P_%' || deswardt2018_lengthened_aggregated$component_name[i]=='K_%'){
    deswardt2018_lengthened_aggregated$method[i]='dry_ashing'
  }
}

# Loop to put only the element name instead of the element name and the unit,
# as the unit is now in a separate column


for (i in 1:length(deswardt2018_lengthened_aggregated$Treatment)) {
  if (deswardt2018_lengthened_aggregated$component_name[i]=='P_%') {
    deswardt2018_lengthened_aggregated$component_name[i]='P'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='K_%'){
    deswardt2018_lengthened_aggregated$component_name[i]='K'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='Ca_%'){
    deswardt2018_lengthened_aggregated$component_name[i]='Ca'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='Mg_%'){
    deswardt2018_lengthened_aggregated$component_name[i]='Mg'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='Organic_C_%'){
    deswardt2018_lengthened_aggregated$component_name[i]='C'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='N_%'){
    deswardt2018_lengthened_aggregated$component_name[i]='N'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='Na_mg/kg'){
    deswardt2018_lengthened_aggregated$component_name[i]='Na'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='Fe_mg/kg'){
    deswardt2018_lengthened_aggregated$component_name[i]='Fe'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='Cu_mg/kg'){
    deswardt2018_lengthened_aggregated$component_name[i]='Cu'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='Zn_mg/kg'){
    deswardt2018_lengthened_aggregated$component_name[i]='Zn'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='Mn_mg/kg'){
    deswardt2018_lengthened_aggregated$component_name[i]='Mn'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='B_mg/kg'){
    deswardt2018_lengthened_aggregated$component_name[i]='B'
  } else if (deswardt2018_lengthened_aggregated$component_name[i]=='Al_mg/kg'){
    deswardt2018_lengthened_aggregated$component_name[i]='Al'
  }
}


write.csv(deswardt2018_lengthened_aggregated, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/1234_001_deswardt_2018_insects/data_dryad/deswardt2018_lengthened_aggregated.csv", row.names = TRUE)
