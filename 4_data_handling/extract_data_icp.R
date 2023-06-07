################################ NETSTO PROJECT ################################

# This script extracts chemical analysis (ICP) data from the file
# 'Tableau récapitulatif SORBONNE A2206601'
# It was executed one time to add more easily ICP data to the database without doing in by hand
# And outputing a database table with the added data
# It is not intended to excectuing it anymore
# Authors : Samuel Charberet
# date : 07/10/2021

library(stringr)
library(tidyr)
# Loading the ICP data
data_icp = read.csv(
  here::here(
    "1_data",
    "1_data_nutrient",
    "2_data_samples",
    "1030_999_charberet_2020",
    "raw_chemical_analysis",
    "2022_09_13_ICP_data_raw.csv"
  )
)

# Removing some unused columns
data_icp = data_icp[-1, -c(which(names(data_icp) == "internal_ref_1"),
                           which(names(data_icp) ==
                                   "internal_ref_2"))]

# Create a column containing only the sample ID
data_icp$sample_id = str_extract(data_icp$sample_name, 'F.+-[1-9]{1,2}')

# Put the table in longer format
data_icp = pivot_longer(
  data_icp,
  cols = c(K, Mg, P),
  names_to = "component_name",
  values_to = "component_mean"
)

# Create a unit column
data_icp$component_unit = NA

# Selecting the correct unit for each line
for (i in 1:nrow(data_icp)) {
  component = data_icp$component_name[i]
  column = which(str_detect(as.character(names(data_icp)[2:4]), as.character(component))) +
    1
  data_icp$component_unit[i] = data_icp[i, column]
}

# Removing the obsolete columns containing units
data_icp = data_icp[, -c(which(names(data_icp) == "K_unit"),
                         which(names(data_icp) == "Mg_unit"),
                         which(names(data_icp) == "P_unit"))]

data_samples = readxl::read_excel(
  here::here(
    "1_data",
    "1_data_nutrient",
    "2_data_samples",
    "1030_999_charberet_2020",
    "1030_999_charberet_2020.xlsx"
  )
)

data_icp$component_unit = as.character(data_icp$component_unit)
data_samples$component_mean = as.numeric(data_samples$component_mean)

data_samples_reference = data_samples

for (i in 1:nrow(data_icp)) {
  # For each line in the ICP datatable, we create a new line of data in our database
  sample_id = data_icp$sample_id[i]
  component = data_icp$component_name[i]
  unit = data_icp$component_unit[i]
  value = data_icp$component_mean[i]
  
  # This is the row in the original database that already has some data about this sample
  row_in_data_samples = which(data_samples_reference$comments == sample_id)[1]
  
  # We create a row in the database containing all the other information about the sample
  data_samples[nrow(data_samples) + 1, ] = data_samples[row_in_data_samples,]
  
  # We only replace the chemical data
  data_samples[nrow(data_samples), ]$component_name = component
  data_samples[nrow(data_samples), ]$component_unit = unit
  data_samples[nrow(data_samples), ]$component_mean = value
  data_samples[nrow(data_samples), ]$data_digitizer = "charberet"
  data_samples[nrow(data_samples), ]$component_measure_method = "icp"
  
  
}

# We replace the observation ID, which is just a row counter

data_samples$observation_ID = 1:nrow(data_samples)

write.xlsx()
