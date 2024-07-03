







library(stringr)
library(tidyr)
library(xlsx)
library(taxize)

# Loading the ICP data
data_p = read.csv(
  here::here(
    "1_data",
    "1_data_nutrients",
    "2_data_samples",
    "1030_999_charberet_2020",
    "1030_999_charberet_2020.csv"
  )
)

gbif_keys = regmatches(data_p$comments,
                       regexpr("(?<=F-)[0-9]+", data_p$comments, perl = TRUE))


for (i in 1:nrow(data_p)) {
  try(if (is.na(data_p$species_latin_name_gbif[i])) {
    if (is.null(gbif_name_usage(key = gbif_keys[i])$species) == F) {
      data_p$species_latin_name_gbif[i] <-
        gsub(" ", "_", tolower(gbif_name_usage(key = gbif_keys[i])$species))
    }
  })
}


write.csv(
  data_p,
  here::here(
    "1_data",
    "1_data_nutrients",
    "2_data_samples",
    "1030_999_charberet_2020",
    "1030_999_charberet_2020_added_names.csv"
  ),
  row.names = F
)
