################################ NETSTO PROJECT ################################

# This script extracts chemical analysis (ICP) data from the file
# 'Tableau récapitulatif SORBONNE A2206601'
# It was executed one time to add more easily ICP data to the database without doing it by hand
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
    "1_data_nutrients",
    "2_data_samples",
    "1030_999_charberet_2020",
    "0_appendix",
    "2_raw_chemical_analysis",
    "2022_09_13_ICP_data_raw.csv"
  )
)

# Removing some unused columns and the first row which is a standard
data_icp = data_icp[-1,-c(which(names(data_icp) == "internal_ref_1"),
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
data_icp = data_icp[,-c(which(names(data_icp) == "K_unit"),
                        which(names(data_icp) == "Mg_unit"),
                        which(names(data_icp) == "P_unit"))]

data_chn = read.csv(
  here::here(
    "1_data",
    "1_data_nutrients",
    "2_data_samples",
    "1030_999_charberet_2020",
    "0_appendix",
    "3_backup",
    "1030_999_charberet_2020_without_icp.csv"
  )
)

data_icp$component_unit = as.character(data_icp$component_unit)
data_icp$component_mean = as.numeric(data_icp$component_mean)

data_chn_raw = data_chn

for (i in 1:nrow(data_icp)) {
  # For each line in the ICP datatable, we create a new line of data in our database
  sample_id = data_icp$sample_id[i]
  component = data_icp$component_name[i]
  unit = data_icp$component_unit[i]
  value = data_icp$component_mean[i]
  
  if (is.na(which(data_chn_raw$comments == sample_id)[1])) {
    data_chn[nrow(data_chn) + 1,] = data_chn[1, ]
    data_chn[nrow(data_chn),]$data_source =  gsub(".*-(\\d{3})-.*", "\\1", sample_id)
    data_chn[nrow(data_chn),]$data_digitizer = "charberet"
    data_chn[nrow(data_chn), c(6, 7:37, 41, 43:45, 47:50, 54:57, 60:63)] = NA
    data_chn[nrow(data_chn), 37:38] = gsub(" ", "_", tolower(
      gsub(".*\\s(\\D+\\s\\D+)\\s-.*", "\\1", data_icp$sample_name[i])
    ))
    data_chn[nrow(data_chn), 39:40] = gsub("F-(\\d+)-.*", "\\1", sample_id)
    data_chn[nrow(data_chn),]$environment = "zoo"
    data_chn[nrow(data_chn),]$feces_ID = gsub(".*-(\\d)$", "\\1", sample_id)
    data_chn[nrow(data_chn),]$observation_resolution = ifelse(str_extract(sample_id, "[a-z]") == "i",
                                                              "individual",
                                                              "population")
    data_chn[nrow(data_chn),]$animal_group_ID =  str_extract(sample_id, "(?<=[ig])(\\d{1,2})")
    data_chn[nrow(data_chn),]$component_n_samples =  1
    data_chn[nrow(data_chn),]$component_name = component
    data_chn[nrow(data_chn),]$component_mean = value
    data_chn[nrow(data_chn),]$component_unit = unit
    data_chn[nrow(data_chn),]$component_weight_type = "dw"
    data_chn[nrow(data_chn),]$component_data_type = "stock"
    data_chn[nrow(data_chn),]$component_measure_method = "icp"
    data_chn[nrow(data_chn),]$freezing = 1
    data_chn[nrow(data_chn),]$autoclaving = 1
    data_chn[nrow(data_chn),]$drying = 1
    data_chn[nrow(data_chn),]$oven = 1
    data_chn[nrow(data_chn),]$drying_time = 48
    data_chn[nrow(data_chn),]$drying_time_unit = "hour"
    data_chn[nrow(data_chn),]$drying_temp = 60
    data_chn[nrow(data_chn),]$drying_temp_unit = "c"
    data_chn[nrow(data_chn),]$grinding = 1
    data_chn[nrow(data_chn),]$grinding_fineness = 80
    data_chn[nrow(data_chn),]$grinding_fineness_unit = "µm"
    data_chn[nrow(data_chn),]$comments = sample_id
    
    
    
  } else {
    # This is the row in the original database that already has some data about this sample
    row_in_data_chn = which(data_chn_raw$comments == sample_id)[1]
    
    # We create a row in the database containing all the other information about the sample
    data_chn[nrow(data_chn) + 1,] = data_chn[row_in_data_chn, ]
    
    # We only replace the chemical data
    data_chn[nrow(data_chn),]$data_source =  gsub(".*-(\\d{3})-.*", "\\1", sample_id)
    data_chn[nrow(data_chn),]$data_digitizer = "charberet"
    data_chn[nrow(data_chn), 37:38] = gsub(" ", "_", tolower(
      gsub(".*\\s(\\D+\\s\\D+)\\s-.*", "\\1", data_icp$sample_name[i])
    ))
    data_chn[nrow(data_chn), 39:40] = gsub("F-(\\d+)-.*", "\\1", sample_id)
    data_chn[nrow(data_chn),]$environment = "zoo"
    data_chn[nrow(data_chn),]$feces_ID = gsub(".*-(\\d)$", "\\1", sample_id)
    data_chn[nrow(data_chn),]$observation_resolution = ifelse(str_extract(sample_id, "[a-z]") == "i",
                                                              "individual",
                                                              "population")
    data_chn[nrow(data_chn),]$animal_group_ID =  str_extract(sample_id, "(?<=[ig])(\\d{1,2})")
    data_chn[nrow(data_chn),]$component_n_samples =  1
    data_chn[nrow(data_chn),]$component_name = component
    data_chn[nrow(data_chn),]$component_mean = value
    data_chn[nrow(data_chn),]$component_unit = unit
    data_chn[nrow(data_chn),]$component_weight_type = "dw"
    data_chn[nrow(data_chn),]$component_data_type = "stock"
    data_chn[nrow(data_chn),]$component_measure_method = "icp"
    data_chn[nrow(data_chn),]$freezing = 1
    data_chn[nrow(data_chn),]$autoclaving = 1
    data_chn[nrow(data_chn),]$drying = 1
    data_chn[nrow(data_chn),]$oven = 1
    data_chn[nrow(data_chn),]$drying_time = 48
    data_chn[nrow(data_chn),]$drying_time_unit = "hour"
    data_chn[nrow(data_chn),]$drying_temp = 60
    data_chn[nrow(data_chn),]$drying_temp_unit = "c"
    data_chn[nrow(data_chn),]$grinding = 1
    data_chn[nrow(data_chn),]$grinding_fineness = 80
    data_chn[nrow(data_chn),]$grinding_fineness_unit = "µm"
    data_chn[nrow(data_chn),]$comments = sample_id
    
  }
  
}

# We replace the observation ID, which is just a row counter

data_chn$observation_ID = 1:nrow(data_chn)

#We add the faeces _ID based on C/N faeces_ID

for (i in 1:nrow(data_chn)) {
  if (is.na(data_chn$feces_ID[i])) {
    row_cn = which(data_chn$comments == data_chn$comment[i] &
                     data_chn$component_name == "C")[1]
    data_chn$feces_ID[i] = data_chn$feces_ID[row_cn]
  }
}

write.csv(
  data_chn,
  here::here(
    "1_data",
    "1_data_nutrients",
    "2_data_samples",
    "1030_999_charberet_2020",
    "1030_999_charberet_2020_copy.csv"
  )
)
