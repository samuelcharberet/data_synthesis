library(dplyr)
library(stringr)
data_p = read.csv(
  here::here(
    "1_data",
    "1_data_nutrients",
    "2_data_samples",
    "1030_999_charberet_2020",
    "1030_999_charberet_2020.csv"
  )
)

colnames(data_p)

for (i in 1:nrow(data_p)) {
  if (data_p$data_digitizer[i] == "pussacq") {
    if (data_p$component_name[i] == "C" |
        data_p$component_name[i] == "N")
      data_p$observation_resolution[i] = ifelse(str_extract(data_p$comments[i], "[a-z]") == "i",
                                                "individual",
                                                "population")
    data_p$component_n_samples[i] = 1
    data_p$component_weight_type[i] = "dw"
    data_p$component_data_type[i] = "stock"
    
    data_p$component_extraction_method[i] = "elemental_analyzer"
    data_p$component_measure_method[i] = "elemental_analyzer"
    data_p[i, 72:75] = 1
    data_p$drying_time[i] = 2
    data_p$drying_time_unit[i] = "d"
    data_p$drying_temp[i] = 60
    data_p$drying_temp_unit[i] = "c"
    data_p$grinding[i] = 1
    data_p$grinding_fineness[i] = 80
    data_p$grinding_fineness_unit[i] = "µm"
    
    
  }
}

write.csv(
  data_p,
  here::here(
    "1_data",
    "1_data_nutrients",
    "2_data_samples",
    "1030_999_charberet_2020",
    "1030_999_charberet_2020_copy.csv"
  )
)
