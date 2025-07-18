#################################################



#################################################
# _targets.R file

# Libraries

library(targets)

# Other new packages

# if ("traitdataform" %in% new.packages) {
#   devtools::install_github('EcologicalTraitData/traitdataform')
# }
# if ("ggtree" %in% new.packages) {
#   BiocManager::install("ggtree")
# }
# if ("ggtreeExtra" %in% new.packages) {
#   BiocManager::install("ggtreeExtra")
# }

tar_option_set(
  packages = c(
    "tidyverse",
    "mgcv",
    "grid",
    "readr",
    "plyr",
    "taxize",
    "here",
    #"traitdataform",
    "stringr",
    #"ggtree",
    #"ggtreeExtra",
    "ggnewscale",
    "ggtext",
    "maps",
    "FactoMineR",
    "factoextra",
    "ggpubr",
    "rstatix",
    "ggrepel",
    "devtools",
    "ggsci",
    "patchwork",
    "ape"
  )
)

# Load all functions contained in all R files in the R directory ####
tar_source(
  files = here::here("R"),
  envir = targets::tar_option_get("envir"),
  change_directory = FALSE
)


list(
  # Define nutrient literature files ####
  tar_target(
    data_nutrients_literature_files,
    list.files(pattern = "[0-9]{4}_[0-9]{3}_.*?_[0-9]{4}.xls", recursive = TRUE),
    format = "file"
  ),
  # Load nutrient literature files ####
  tar_target(
    data_nutrients_literature,
    load_dnl(data_nutrients_literature_files)
  ),
  # Define nutrient proprietary files ####
  tar_target(
    data_nutrients_proprietary_file,
    here::here(
      "1_data",
      "1_data_nutrients",
      "2_data_samples",
      "1030_999_charberet_2020",
      "1030_999_charberet_2020.csv"
    ),
    format = "file"
  ),
  # Load nutrient proprietary files ####
  tar_target(
    data_nutrients_proprietary,
    load_dnp(data_nutrients_proprietary_file)
  ),
  # Combine nutrient data ####
  tar_target(
    data_nutrients_combined,
    combine_nutrient_data(data_nl = data_nutrients_literature, data_np = data_nutrients_proprietary)
  ),
  # Define traits file ####
  tar_target(
    data_traits_combined_file,
    here::here("1_data", "3_data_traits", "data_traits.csv")
  ),
  # Load traits file ####
  tar_target(data_traits_combined, load_dt(data_traits_combined_file)),
  # Combine traits and nutrient data ####
  tar_target(
    data_combined,
    combine_nutrients_traits(data_n = data_nutrients_combined, data_t = data_traits_combined)
  ),
  # Generate usable datatables for analysis ####
  tar_target(
    datasets_for_analyses,
    create_datasets_for_analyses(data = data_combined)
  ),
  # Define fluxes file ####
  tar_target(
    data_fluxes_file,
    here::here("1_data", "2_data_fluxes", "data_fluxes.csv")
  ),
  # Load fluxes file ####
  tar_target(data_fluxes, load_df(data_fluxes_file)),
  # Model the data ####
  tar_target(models, model_ds(data = data_combined, data_f = data_fluxes)),
  # Plot the data ####
  tar_target(plots, plot_ds(data = data_combined, data_f = data_fluxes))
)