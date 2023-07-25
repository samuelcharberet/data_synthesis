library(dplyr)
data_tb = read.csv(here::here("1_data",
                              "3_data_traits",
                              "data_traits_blank.csv"))

data_t = read.csv(here::here("1_data",
                             "3_data_traits",
                             "data_traits.csv"))


setdiff(data_tb$species_names, data_t$species_names)
