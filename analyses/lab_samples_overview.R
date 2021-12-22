################################ NETSTO PROJECT ################################

# This script contains basic statistics and numbers on the samples hosted at iEES Paris
# Authors : Samuel Charberet
# date : 22/09/2021

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


faeces_samples = readxl::read_excel("C:/Users/Samuel/Dropbox/NetSto/0_database/1_data_nutrient/1_data_samples/faeces_samples.xlsx", na="NA")


#### 2. Clean data ####

faeces_samples$gbif_id = as.factor(faeces_samples$gbif_id)

#### 3. DATA ANALYSIS  ####

# faeces_samples = faeces_samples[which(is.na(faeces_samples$date_thawing)),] # if you want to extract information only on samples not yet analyzed or not.

#### 3.1. Sources  ####
length(unique(faeces_samples$origin)) # 13 zoos total

#### 3.4. Number and identity of species ####

length(unique(faeces_samples$gbif_id)) # 172 species total
length(unique(faeces_samples$gbif_id2)) # 181 species or subspecies total

### Phylogenetic tree

taxize_species = classification(levels(faeces_samples$gbif_id), db = "gbif")
taxize_tree = class2tree(taxize_species, check = TRUE)
plot(taxize_tree)



