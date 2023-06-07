### Studier 1994 - Mineral and nitrogen concentrations in feces of some neotropical bats
### Data wrangling ###

library(tidyr)
library(dplyr)
library(plyr)
library(rvest)
library(psych)

#### Excreta #### 

setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/1030_002_studier_1994_bats_traits/data")
studier1994 = read.csv("studier_1994_table_2.csv", sep=";")

str(studier1994)
colnames(studier1994) = c('species',	'food_habit',	'M',	'F',	'male_prop',
                          'Ca','Ca_error',	'Mg',	'Mg_error',	'K',
                          'K_error','Na',	'Na_error',	'Fe',
                          'Fe_error',	'N','N_error')


studier1994_lengthened =  pivot_longer(studier1994,
                                    cols = c('Ca',	'Mg', 'K','Na', 'Fe', 'N'),
                                    names_to = "component_name",
                                    values_to = "component_value",
                                    values_drop_na = TRUE)

studier1994_lengthened$error=NA
studier1994_lengthened$n_indiv=NA


# Loop to put in a error column the right error 
for (i in 1:length(studier1994_lengthened$species)) {
  if (studier1994_lengthened$component_name[i]=='Ca') {
    studier1994_lengthened$error[i]=studier1994_lengthened$Ca_error[i]
  } else if (studier1994_lengthened$component_name[i]=='Mg') {
    studier1994_lengthened$error[i]=studier1994_lengthened$Mg_error[i]
  } else if (studier1994_lengthened$component_name[i]=='K') {
    studier1994_lengthened$error[i]=studier1994_lengthened$K_error[i]
  } else if (studier1994_lengthened$component_name[i]=='Na') {
    studier1994_lengthened$error[i]=studier1994_lengthened$Na_error[i]
  } else if (studier1994_lengthened$component_name[i]=='Fe') {
    studier1994_lengthened$error[i]=studier1994_lengthened$Fe_error[i]
  } else if (studier1994_lengthened$component_name[i]=='N') {
    studier1994_lengthened$error[i]=studier1994_lengthened$N_error[i]
  }
}

# Remove error columns
studier1994_lengthened$n_indiv=studier1994_lengthened$M+studier1994_lengthened$F
studier1994_lengthened = studier1994_lengthened[ ,-c(2, 3, 4, 6:11)]

write.csv(studier1994_lengthened, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/1030_002_studier_1994_bats_traits/data/studier_1994_table_2_lengthened.csv", row.names = TRUE)
