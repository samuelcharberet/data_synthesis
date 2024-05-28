#################################################



#################################################
# _targets.R file

# Libraries

library(targets)

#CRAN new packages
list.of.packages = c(
  "targets",
  "ggplot2",
  "grid",
  "readr",
  "plyr" ,
  "dplyr",
  "tidyr",
  "taxize",
  "here",
  "stringr",
  "ggnewscale",
  "ggtext",
  "maps",
  "FactoMineR",
  "factoextra",
  "ggpubr",
  "rstatix",
  "ggrepel",
  "devtools",
  "BiocManager"
)
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  install.packages(new.packages)
}

# Other new packages

if ("traitdataform" %in% new.packages) {
  devtools::install_github('EcologicalTraitData/traitdataform')
}
if ("ggtree" %in% new.packages) {
  BiocManager::install("ggtree")
}
if ("ggtreeExtra" %in% new.packages) {
  BiocManager::install("ggtreeExtra")
}

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
    "maps",
    "FactoMineR",
    "factoextra",
    "ggpubr",
    "rstatix",
    "ggrepel",
    "devtools"
  )
)

# We source all functions contained in all files in the R directory
lapply(list.files(here::here("R"), recursive = TRUE, full.names = T), source)

list(
  # define nutrient literature files
  tar_target(
    data_nutrients_literature_files,
    list.files(pattern = "[0-9]{4}_[0-9]{3}_.*?_[0-9]{4}.xls", recursive = TRUE),
    format = "file"
  ),
  # load nutrient literature files
  tar_target(
    data_nutrients_literature,
    load_dnl(data_nutrients_literature_files)
  ),
  # define nutrient proprietary files
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
  # load nutrient proprietary files
  tar_target(
    data_nutrients_proprietary,
    load_dnp(data_nutrients_proprietary_file)
  ),
  # combine nutrient data
  tar_target(
    data_nutrients_combined,
    combine_nutrient_data(data_nl = data_nutrients_literature, data_np = data_nutrients_proprietary)
  ),
  # define traits file
  tar_target(
    data_traits_combined_file,
    here::here("1_data", "3_data_traits", "data_traits.csv")
  ),
  # load traits file
  tar_target(data_traits_combined, load_dt(data_traits_combined_file)),
  # Combine traits and nutrient data
  tar_target(
    data_combined,
    combine_nutrients_traits(data_n = data_nutrients_combined, data_t = data_traits_combined)
  ),
  # define fluxes file
  tar_target(
    data_fluxes_file,
    here::here("1_data", "2_data_fluxes", "data_fluxes.csv")
  ),
  # load fluxes file
  tar_target(data_fluxes, load_df(data_fluxes_file)),
  # Model the data
  tar_target(models, model_ds(data = data_combined, data_f = data_fluxes)),
  # Plot the data
  tar_target(plots, plot_ds(data = data_combined, data_f = data_fluxes))
)