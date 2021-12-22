### Zhang 2014 - Grasshoppers regulate N:P stoichiometric homeostasis by 
### changing phosphorus contents in their frass
### Data wrangling ###

library(tidyr)
library(dplyr)
library(plyr)
library(rvest)
library(psych)

#### Insect frass N and P, obtained through digitizing data of figure 1 b ###


setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/0034_014_zhang_2014_grasshopper/graph_grabber/fig_1_b")
zhang2014 = read.csv("fig_1_b.csv", sep=";")

str(zhang2014)
colnames(zhang2014) = c('P',	'N',	'feces_ID')

# We lengthen the table and add a colmun to keep track of the previous columns
zhang2014_lengthened =  pivot_longer(zhang2014,
                                        cols = c('P',	'N'),
                                        
                                        names_to = "component_name",
                                        values_to = "component_value",
                                        values_drop_na = TRUE)

# Create a csv file from the zhang2014_lengthened table and put it in the right 
### directory

write.csv(zhang2014_lengthened, "C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/0_data_literature/2_done_to_check/0034_014_zhang_2014_grasshopper/graph_grabber/fig_1_b/fig_1_b_lengthened.csv", row.names = TRUE)
