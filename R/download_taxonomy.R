# Import species names found in the nutrient data and in the trait data


traitdataform::pulldata("amphibio")

files <- c(
  here::here("1_data", "1_data_nutrient", "3_data_nutrient_combined", "data_nutrient_combined.csv"),
  
)

list_files <- lapply(files, readxl::read_excel, na = "NA")


list_sp <-
  unique(data_nutrient$species_latin_name_gbif)

list_data_gbif = get_gbifid_(gsub("_", " ", list_sp))

data_gbif <- NULL

for (i in 1:length(list_data_gbif)) {
  if (nrow(list_data_gbif[[i]]) == 1) {
    data_gbif <- bind_rows(data_gbif, list_data_gbif[[i]])
  } else if (nrow(list_data_gbif[[i]]) == 0) {
    ligne_na = rep(NA, length.out = length(list_data_gbif[[1]]))
    names(ligne_na) = names(list_data_gbif[[1]])
    
    data_gbif = bind_rows(data_gbif, ligne_na)
  } else {
    accepted_rows = which(list_data_gbif[[i]]$status == "ACCEPTED")
    synonym_rows = which(list_data_gbif[[i]]$status == "SYNONYM")
    exact_rows = which(list_data_gbif[[i]]$matchtype == "EXACT")
    fuzzy_rows = which(list_data_gbif[[i]]$matchtype == "FUZZY")
    if (length(intersect(accepted_rows, exact_rows)) > 0) {
      max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence ==
                         max(list_data_gbif[[i]][intersect(accepted_rows, exact_rows),]$confidence)[1])
      data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
    }
    else if (length(intersect(synonym_rows, exact_rows)) > 0) {
      max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence ==
                         max(list_data_gbif[[i]][intersect(synonym_rows, exact_rows),]$confidence)[1])
      data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
    }
    else if (length(intersect(accepted_rows, fuzzy_rows)) > 0) {
      max_conf = which(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence ==
                         max(list_data_gbif[[i]][intersect(accepted_rows, fuzzy_rows),]$confidence)[1])
      data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
    }
    else if (length(intersect(synonym_rows, fuzzy_rows)) > 0) {
      max_conf = which(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence ==
                         max(list_data_gbif[[i]][intersect(synonym_rows, fuzzy_rows),]$confidence)[1])
      data_gbif = bind_rows(data_gbif, list_data_gbif[[i]][max_conf, ])
    }
  }
}
rm(
  list_data_gbif,
  accepted_rows,
  exact_rows,
  fuzzy_rows,
  synonym_rows,
  ligne_na,
  max_conf
)

factor_columns = c("rank", "status", "matchtype")
data_gbif[factor_columns] = lapply(data_gbif[factor_columns], factor)
rm(factor_columns)

useful_data_gbif =  data_gbif[c(
  "canonicalname",
  "rank",
  "status",
  "matchtype",
  "species",
  "specieskey",
  "phylum",
  "class",
  "order",
  "family",
  "genus"
)]


colnames(useful_data_gbif) <-
  c(
    "canonical_name",
    "rank",
    "status",
    "match_type",
    "species",
    "specieskey",
    "phylum",
    "class",
    "order",
    "family",
    "genus"
  )

