### Barnes 2019 - Predators buffer the effects of variation in prey nutrient content
### for nutrient deposition
### Data wrangling ###

library(tidyr)
library(dplyr)
library(rvest)
library(psych)

#### Excreta #### 

setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/13_004_barnes_2019_spiders/raw_data")
barnes2019 = read.csv("elemegest.csv", sep=";")

str(barnes2019)
barnes2019$Spider=as.factor(barnes2019$Spider)
barnes2019$Method=as.factor(barnes2019$Method)
barnes2019$obs_ID=as.factor(barnes2019$obs_ID)

barnes2019$animal_group = NA

colnames(barnes2019) = c('Spider',	'Treatment',	'Weight_mg',	'Method',	'N_Area',	
                         'C_Area',	'N',	'C',	'CN_ratio', 	'N_Factor',	
                         'C_Factor',	'obs_ID', 'animal_group')

# Give a number to spiders instead of character string
for(i in 1:length(barnes2019[,1])){
  for(j in 1:length(levels(barnes2019$Spider))){
    if (barnes2019[i,1]==levels(barnes2019$Spider)[j]) {
      barnes2019$animal_group[i]=j
    }
  }
}


barnes2019_longer =  pivot_longer(barnes2019,
                                 cols = c("C", "N", "CN_ratio"),
                                 names_to = "component_name",
                                 values_to = "component_mean",
                                 values_drop_na = TRUE)

barnes2019$animal_group=as.factor(barnes2019$animal_group)
levels(barnes2019$animal_group) # check that there are indeed 52 levels


write.csv(barnes2019_longer, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/13_004_barnes_2019_spiders/raw_data/elemegest_longer.csv", row.names = TRUE)

#### Food #### 

barnes2019food = read.csv("elemprey.csv", sep=";")
colnames(barnes2019food) = c('Cricket',	'Treatment',	'Weight_mg',	'Method',
                             'N_Area',	'C_Area',	'N',	'C',	'CN_ratio', 	
                             'N_Factor',	'C_Factor',	'CrickWetMass',	
                             'CrckEstDry',	'MassN',	'MassC')

describe = describe.by(barnes2019food,barnes2019food$Treatment)
a=describe$`10:45:45`[c(7,8,9), c(1,2,3, 4)]; a$treatment='10:45:45';
b=describe$`100:00:00`[c(7,8,9), c(1,2,3, 4)]; b$treatment='100:00:00';
c=describe$`60:20:20`[c(7,8,9), c(1,2,3, 4)]; c$treatment='60:20:20';
d=describe$`80:10:10`[c(7,8,9), c(1,2,3, 4)]; d$treatment='80:10:10';

barnes2019food_sum = rbind(a,b,c,d)

barnes2019food_sum[which(barnes2019food_sum$vars==7), ]$vars='N'
barnes2019food_sum[which(barnes2019food_sum$vars==8), ]$vars='C'
barnes2019food_sum[which(barnes2019food_sum$vars==9), ]$vars='C/N'

write.csv(barnes2019food_sum, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/13_004_barnes_2019_spiders/raw_data/elemprey_summary.csv", row.names = TRUE)

