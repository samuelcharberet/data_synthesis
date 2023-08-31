#################################################



#################################################
# _targets.R file

# Libraries

library(targets)
library(here)


tar_option_set(
  packages = c(
    "ggplot2",
    "grid",
    "readr",
    "plyr" ,
    "dplyr",
    "tidyr",
    "taxize",
    "here",
    "traitdataform",
    "stringr",
    "ggtree",
    "ggtreeExtra",
    "ggnewscale",
    "ggtext",
    "FactoMineR",
    "factoextra",
    "ggpubr"
  )
)

# We source all functions contained in all files in the R directory
lapply(list.files(here::here("R"), recursive = TRUE, full.names = T), source)


list(
  # load nutrient literature files
  tar_target(data_nutrients_literature,
             load_dnl(
               paths = list.files(pattern = "[0-9]{4}_[0-9]{3}_.*?_[0-9]{4}.xls",
                                  recursive = TRUE)
             )),
  
  # load nutrient proprietary files
  tar_target(data_nutrients_proprietary,
             load_dnp(
               path = here::here(
                 "1_data",
                 "1_data_nutrients",
                 "2_data_samples",
                 "1030_999_charberet_2020",
                 "1030_999_charberet_2020.csv"
               )
             )),
  # combine nutrient data
  tar_target(
    data_nutrients_combined,
    combine_nutrient_data(data_nl = data_nutrients_literature,
                          data_np = data_nutrients_proprietary)
  ),
  # load traits files
  tar_target(data_traits_combined,
             load_dt(
               path = here::here("1_data",
                                 "3_data_traits",
                                 "data_traits.csv")
             )),
  # Combine traits and nutrient data
  tar_target(
    data_combined,
    combine_nutrients_traits(data_n = data_nutrients_combined,
                             data_t = data_traits_combined)
  ),
  # load fluxes files
  tar_target(data_fluxes,
             load_df(
               path = here::here("1_data",
                                 "2_data_fluxes",
                                 "data_fluxes.csv")
             )),
  # Model the data
  tar_target(models,
             model_ds(data = data_combined, data_f = data_fluxes)),
  
  # Plot the data
  tar_target(plots,
             plot_ds(data = data_combined, data_f = data_fluxes))
)