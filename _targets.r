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
    "taxize",
    "here",
    "traitdataform"
  )
)

# We source all functions contained in all files in the R directory
lapply(list.files(here::here("R"), recursive = TRUE, full.names = T), source)


list(
  # define nutrient literature files
  tar_target(
    data_nutrient_literature_files,
    list.files(pattern = "[0-9]{4}_[0-9]{3}_.*?_[0-9]{4}.xls",
               recursive = TRUE),
    format = "file"
  ),
  # load nutrient literature files
  tar_target(
    data_nutrient_literature,
    load_dnl(paths = data_nutrient_literature_files)
  ),
  # define nutrient proprietary files
  tar_target(
    data_nutrient_proprietary_files,
    here::here(
      "1_data",
      "1_data_nutrient",
      "1_data_samples",
      "1030_999_charberet_2020"
    ),
    format = "file"
  ),
  # load nutrient proprietary files
  tar_target(
    data_nutrient_proprietary,
    load_dnp(path = data_nutrient_proprietary_files)
  ),
  # combine nutrient data
  tar_target(
    data_nutrients_combined,
    combine_nutrient_data(data_nl = data_nutrient_literature,
                          data_np = data_nutrient_proprietary)
  ),
  # define traits databases files
  tar_target(
    data_traits_files,
    here::here("1_data", "1_data_traits", "1_databases"),
    format = "file"
  ),
  # load traits files
  tar_target(data_traits_combined,
             load_dt(path = data_traits_files)),
  # Combine traits and nutrient data
  tar_target(
    data_combined,
    combine_data(data_n = data_nutrients_combined,
                 data_t = data_traits_combined)
  ),
  # Model the data
  tar_target(models_ds,
             model_irn(data = data_combined)),
  
  # Plot the data
  tar_target(plots_ds,
             plot_irn(data = data_combined))
)