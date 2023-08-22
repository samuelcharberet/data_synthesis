#' model_ds
#'
#' @param data, data_fluxes
#'
#' @return models for the data synthesis
#' @export
#'
#' @examples
#' @authors Samuel Charberet
#'
#'

model_ds = function(data, data_fluxes) {
  # We make a model at the level of species averages as we have traits at this level only (body mass and diet)
  
  # One model for animal with a cloaca, and one for animals without
  
  # No cloaca ####
  
  
  
  # We select only dry weight data, as fresh weight are more rare and not comparable to dry weight data
  
  data <- data |>
    filter(component_weight_type != "fw")
  stock_data <- data |>
    filter(component_data_type == "stock")
  
  ##### a. No cloaca faeces models #####
  
  faeces_stock_data <- stock_data |>
    filter(cloaca == 0)
  faeces_stock_data <- faeces_stock_data |>
    filter(sample_type == "feces" | sample_type == "faeces")
  
  # Selecting CNP in faeces stock data
  cnp_fsd <- faeces_stock_data |>
    filter(component_name == "C" |
             component_name == "N" | component_name == "P")
  
  # Keep only relevant rows
  cnp_fsd <- cnp_fsd |>
    dplyr::select(species_latin_name_gbif,
                  component_name,
                  component_mean,
                  body_mass,
                  diet)
  
  # Average over species
  a_cnp_fsd <- cnp_fsd |>
    group_by(species_latin_name_gbif, component_name) |>
    dplyr::summarise(
      avg_component_mean = mean(component_mean),
      n_obs = length(component_mean),
      body_mass = first(body_mass),
      diet = first(diet)
    )
  
  # Compute the ratios
  species = unique(a_cnp_fsd$species_latin_name_gbif)
  for (i in species) {
    crow = which(a_cnp_fsd$species_latin_name_gbif == i &
                   a_cnp_fsd$component_name == "C")
    nrow = which(a_cnp_fsd$species_latin_name_gbif == i &
                   a_cnp_fsd$component_name == "N")
    prow = which(a_cnp_fsd$species_latin_name_gbif == i &
                   a_cnp_fsd$component_name == "P")
    cn_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "CN",
      avg_component_mean = ifelse(
        any(crow) &&
          any(nrow),
        a_cnp_fsd$avg_component_mean[crow] / a_cnp_fsd$avg_component_mean[nrow],
        NA
      ),
      body_mass = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "C"),]),
                   nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "N"),]))
    )
    cp_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "CP",
      avg_component_mean = ifelse(
        any(crow) &&
          any(prow),
        a_cnp_fsd$avg_component_mean[crow] / a_cnp_fsd$avg_component_mean[prow],
        NA
      ),
      body_mass = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "C"),]),
                   nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "P"),]))
    )
    np_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "NP",
      avg_component_mean = ifelse(
        any(nrow) &&
          any(prow),
        a_cnp_fsd$avg_component_mean[nrow] / a_cnp_fsd$avg_component_mean[prow],
        NA
      ),
      body_mass = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "diet"],
      n_obs = mean(nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "N"),]),
                   nrow(cnp_fsd[which(cnp_fsd$species_latin_name_gbif == i &
                                        cnp_fsd$component_name == "P"),]))
    )
    
    # Add Row using rbind()
    a_cnp_fsd = rbind(a_cnp_fsd, cn_row, cp_row, np_row)
  }
  
  # Creating a comparative data object incorporating phylogeny
  a_cnp_fsd$diet = as.factor(a_cnp_fsd$diet)
  a_cnp_fsd = within(a_cnp_fsd, diet <-
                       relevel(diet, ref = "omnivore"))
  a_cnp_fsd = a_cnp_fsd[,-which(names(a_cnp_fsd) == "n_obs")]
  a_cnp_fsd = tidyr::pivot_wider(a_cnp_fsd, names_from = "component_name", values_from = "avg_component_mean")
  a_cnp_fsd$species_latin_name_gbif = stringr::str_to_sentence(gsub("_", " ", a_cnp_fsd$species_latin_name_gbif))
  species_names = a_cnp_fsd$species_latin_name_gbif
  taxize_species = classification(species_names, db = "gbif")
  species_tree = class2tree(taxize_species, check = T)
  
  cdat = caper::comparative.data(
    data = as.data.frame(a_cnp_fsd),
    phy = species_tree$phylo,
    names.col = "species_latin_name_gbif",
    force.root = T
  )
  
  
  # Phylogenetic GLS with body mass and diet
  model_c = caper::pgls(C ~ log(body_mass) * diet, cdat)
  summary(model_c)
  model_n = caper::pgls(N ~ log(body_mass) * diet, cdat)
  summary(model_n)
  model_p = caper::pgls(P ~ log(body_mass) * diet, cdat)
  summary(model_p)
  model_cn = caper::pgls(CN ~ log(body_mass) * diet, cdat)
  summary(model_cn)
  model_cp = caper::pgls(CP ~ log(body_mass) * diet, cdat)
  summary(model_cp)
  model_np = caper::pgls(NP ~ log(body_mass) * diet, cdat)
  summary(model_np)
  
  #  GLS with body mass and diet
  model_c = lm(C ~ log(body_mass) * diet, a_cnp_fsd)
  tmodel_c = broom::tidy(model_c)
  model_n = lm(N ~ log(body_mass) * diet, a_cnp_fsd)
  tmodel_n = broom::tidy(model_n)
  model_p = lm(P ~ log(body_mass) * diet, a_cnp_fsd)
  tmodel_p = broom::tidy(model_p)
  model_cn = lm(CN ~ log(body_mass) * diet, a_cnp_fsd)
  tmodel_cn = broom::tidy(model_cn)
  model_cp = lm(CP ~ log(body_mass) * diet, a_cnp_fsd)
  tmodel_cp = broom::tidy(model_cp)
  model_np = lm(NP ~ log(body_mass) * diet, a_cnp_fsd)
  tmodel_np = broom::tidy(model_np)
  
  plot(log(a_cnp_fsd$body_mass), a_cnp_fsd$CN)
  
  # Faeces stock data stoichiometric models
  models_fsd = rbind(tmodel_cn, tmodel_cp, tmodel_np)
  models_fsd = models_fsd %>%
    mutate_if(is.numeric, signif, 3)
  
  write.csv(
    models_fsd,
    here::here("2_outputs",
               "1_statistical_results",
               "models_fsd.csv"),
    row.names = FALSE
  )
}
