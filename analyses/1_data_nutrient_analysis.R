################################ NETSTO PROJECT ################################

# This script contains basic visual exploration on the structure of the nutrient database
# Authors : Samuel Charberet
# date : 19/07/2021

#### 0. Libraries ####

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(RColorBrewer)
library(rotl)
library(ape)
library(taxize)
library(tidyverse)
library(forcats)


#### 1. Working directory and file ####

  
  setwd("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/2_data_analysis/")
  data_nutrient = read.csv(here::here("1_data", "1_data_nutrient"))
  str(data_nutrient)


#### 2. Clean data ####

  # Define the columns containing numeric data as numeric class
  
  cols.num = c("bodymass_mean", "bodymass_min", "bodymass_max", "bodymass_value_error", "bodymass_n_samples", "age_mean",
               "age_max", "age_value_error", "age_n_samples", "male_prop", "sampling_year", "sampling_month", "sampling_day",
               "sampling_latitude", "sampling_longitude", "nb_items_per_sample", "nb_individuals_per_sample", "component_mean",
               "component_min", "component_max", "component_value_error", "component_n_samples", "drying_time", "drying_temp",
               "grinding_fineness")
  
  data_nutrient[cols.num] <- sapply(data_nutrient[cols.num],as.numeric)
  
  
  # Define the columns containing factor data as factor class
  
  cols.factor = c( "reference_ID","data_source", "sample_type", "egestion_age_unit" , "bodymass_error_type" ,
                      "bodymass_unit", "bodymass_weight_type" , "age_error_type" , "age_unit", "species_latin_name_gbif",
                      "gbif_id","gbif_id2", "environment" ,"food_ID","body_ID", "feces_ID","urine_ID", "individual_known",
                      "observation_resolution","observation_ID","animal_group_ID","times_series_ID", "time_unit","repetition_ID","component_name",
                      "component_error_type" , "component_unit", "component_weight_type","component_data_type","component_detail",
                      "freezing","autoclaving","drying","oven", "drying_time_unit", "drying_temp_unit","grinding",
                      "grinding_fineness_unit","canonical_name","rank","status", "match_type","species","phylum","class","family", "order","genus")

  data_nutrient[cols.factor] <- lapply(data_nutrient[cols.factor], as.factor)
  
  str(data_nutrient)

  # Check where there are no GBIF_ID in the database and go see why in the articles
  data_nogbif = data_nutrient[which(is.na(data_nutrient$gbif_id)), ]
  
  
  
#### 3. DATA ANALYSIS  ####

#### 3.1. Sources  ####
  
  
  # Number of sources
  length(levels(data_nutrient$reference_ID)) # 79 references
  
  # Number of references each year
  
  
  data_source = unique(data_nutrient[, c("reference_ID", "reference_year")])
  data_source = data.frame(table(data_source$reference_year)); names(data_source)=c("reference_year", "reference_count")
  data_source$reference_year <- as.Date(data_source$reference_year, format="%Y")
  
  p <- data_source %>%
    ggplot( aes(x=reference_year, y=reference_count)) +
    geom_area(fill="#69b3a2", alpha=0.5) +
    geom_line(color="#69b3a2") +
    ylab("Number of studies") +
    xlab("Year")+
    theme_ipsum()
  p


#### 3.2. Sample type ####
  
  levels(data_nutrient$sample_type) # list of all sample types
  
  data_sampletype = data.frame(table(data_nutrient$sample_type)); names(data_sampletype)=c("sample_type", "count")
  
  coul <- brewer.pal(5, "Set2") 
  barplot(height=data_sampletype$count, names=data_sampletype$sample_type, 
          col=coul, xlab="Type of sample", ylab="Number of data")


#### 3.3. Data type ####

  levels(data_nutrient$component_data_type) # list of all sample types
  
  data_datatype = data.frame(table(data_nutrient$component_data_type)); names(data_datatype)=c("data_type", "count")
  
  coul <- brewer.pal(5, "Set2") 
  barplot(height=data_datatype$count, names=data_datatype$data_type, 
          col=coul, xlab="Type of data", ylab="Number of data")

#### 3.4. Number and identity of species ####

  length(unique(data_nutrient$gbif_id)) # 184 species
  length(unique(data_nutrient$gbif_id2)) # 187 species or subspecies
  
  ### Phylogenetic tree
  
  taxize_species = classification(levels(data_nutrient$gbif_id), db = "gbif")
  taxize_tree = class2tree(taxize_species, check = TRUE)
  plot(taxize_tree)


#### 3.4. Sampling environment ####

  data_nutrient$environment[data_nutrient$environment=="lab"] = "laboratory"
  data_nutrient$environment = droplevels(data_nutrient$environment)
  levels(data_nutrient$environment)
  
  data_samplingenv = data.frame(table(data_nutrient$environment)); names(data_samplingenv)=c("sampling_environment", "count")
  
  coul <- brewer.pal(5, "Set2") 
  barplot(height=data_samplingenv$count, names=data_samplingenv$sampling_environment, 
          col=coul, xlab="Sampling environment", ylab="Number of data")

#### 3.5. Coupling between sample types ####

  
# Total number of dejection data (feces, urine, or both: guano, frass)
tot_dej = length(which(!is.na(data_nutrient$feces_ID) | !is.na(data_nutrient$urine_ID))) #4985
  
# Proportion of dejection data that also have a coupling with food
  
dej_food = length(which((!is.na(data_nutrient$feces_ID) | !is.na(data_nutrient$urine_ID)) & !is.na(data_nutrient$food_ID))) #1537
dej_food/tot_dej
# Proportion of dejection data that also have a coupling with body

dej_body = length(which((!is.na(data_nutrient$feces_ID) | !is.na(data_nutrient$urine_ID)) & !is.na(data_nutrient$body_ID))) #1537
dej_body/tot_dej

# Proportion of dejection data that also have a coupling with food and body

dej_food_body = length(which((!is.na(data_nutrient$feces_ID) | !is.na(data_nutrient$urine_ID)) & !is.na(data_nutrient$body_ID) & !is.na(data_nutrient$food_ID) )) #1537
dej_food_body/tot_dej

#### 3.6. Body mass ####


levels(data_nutrient$bodymass_unit)

# A for loop to put all bodymass data

for (i in 1:nrow(data_nutrient)){
  if (data_nutrient$bodymass_unit[i]=="G"){
    data_nutrient$bodymass_mean[i]=data_nutrient$bodymass_mean[i]/1000
    data_nutrient$bodymass_min[i]=data_nutrient$bodymass_min[i]/1000
    data_nutrient$bodymass_max[i]=data_nutrient$bodymass_max[i]/1000
    data_nutrient$bodymass_unit[i]=="kg"
    
    
  } else if (data_nutrient$bodymass_unit[i]=="mg") {
    data_nutrient$bodymass_mean[i]=data_nutrient$bodymass_mean[i]/1000000
    data_nutrient$bodymass_min[i]=data_nutrient$bodymass_min[i]/1000000
    data_nutrient$bodymass_max[i]=data_nutrient$bodymass_max[i]/1000000
    data_nutrient$bodymass_unit[i]=="kg"
    
  } else if (data_nutrient$bodymass_unit[i]=="ton") {
    data_nutrient$bodymass_mean[i]=data_nutrient$bodymass_mean[i]*1000
    data_nutrient$bodymass_min[i]=data_nutrient$bodymass_min[i]*1000
    data_nutrient$bodymass_max[i]=data_nutrient$bodymass_max[i]*1000
    data_nutrient$bodymass_unit[i]=="kg"
    
  }
}

data_spbodymass = aggregate(data_nutrient[,c("bodymass_mean", "species_latin_name_gbif")], by=list(species = data_nutrient$species_latin_name_gbif), mean, simplify = TRUE, drop = TRUE)



# plot
  p <- data_spbodymass %>%
  ggplot( aes(x=log(bodymass_mean))) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  theme_ipsum() +
  ylab("Number of species")+
  xlab("Log bodymass (kg)")+
  theme(
    plot.title = element_text(size=15)
  )
p


#### 3.7. Observation resolution ####

levels(data_nutrient$observation_resolution)
data_nutrient$observation_resolution[data_nutrient$observation_resolution=="inter_indiv"] = "inter_individual"
data_nutrient$observation_resolution[data_nutrient$observation_resolution=="intra_indiv"] = "intra_individual"
data_nutrient$observation_resolution = droplevels(data_nutrient$observation_resolution)

data_obsres = data.frame(table(data_nutrient$observation_resolution)); names(data_obsres)=c("observation_resolution", "count")
coul <- brewer.pal(5, "Set2") 
barplot(height=data_obsres$count, names=c("food", "individual" ,"inter individual", "inter population", 
                                          "intra food","intra individua", "intra population", "population"),
        col=coul, xlab="Observation resolution", ylab="Number of data")

#### 3.8. Nature of the chemical analysis ####


levels(data_nutrient$component_name)
data <- data.frame(
  name=c("north","south","south-east","north-west","south-west","north-east","west","east"),
  val=sample(seq(1,10), 8 )
)

data_chem = data.frame(table(data_nutrient$component_name)); names(data_chem)=c("chemical_component", "count")


data_chem %>%
  mutate(chemical_component = fct_reorder(chemical_component, count)) %>%
  ggplot( aes(x=chemical_component, y=count)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

coul <- brewer.pal(5, "Set2") 
barplot(height=data_chem$count, names=data_chem$observation_resolution,
        col=coul, xlab="Observation resolution", ylab="Number of data")
