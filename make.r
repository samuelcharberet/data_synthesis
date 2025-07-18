# Libraries
library(targets)
library(tidyverse)
library(grid)
library(readr)
library(plyr)
library(taxize)
library(ggsci)
library(mgcv)
library(ggtext)
library(formula.tools)
library(maps)
library(ggnewscale)
library(patchwork)
library(stringr)
library(ape)
library(ggpubr)
library(rstatix)

tar_source(
  files = here::here("R"),
  envir = targets::tar_option_get("envir"),
  change_directory = FALSE
)

data_nutrients_literature_files = list.files(pattern = "[0-9]{4}_[0-9]{3}_.*?_[0-9]{4}.xls", recursive = TRUE)
data_nutrients_literature = load_dnl(data_nutrients_literature_files)

data_nutrients_proprietary_file = here::here(
  "1_data",
  "1_data_nutrients",
  "2_data_samples",
  "1030_999_charberet_2020",
  "1030_999_charberet_2020.csv"
)

data_nutrients_proprietary = load_dnp(path = data_nutrients_proprietary_file)

data_nutrients = combine_nutrient_data(data_nl = data_nutrients_literature, data_np = data_nutrients_proprietary)

data_traits_file = here::here("1_data", "3_data_traits", "data_traits.csv")

data_traits = load_dt(data_traits_file)

data = combine_nutrients_traits(data_n = data_nutrients, data_t = data_traits)

create_datasets_for_analyses(data)

data_fluxes_file = here::here("1_data", "2_data_fluxes", "data_fluxes.csv")

data_fluxes = load_df(data_fluxes_file)

plot_ds(data = data, data_f = data_fluxes)

model_ds(data = data, data_f = data_fluxes)
