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
  
  # One model for animals with a cloaca, and one for animals without
  
  # No cloaca ####
  a_cnp_fsd = read.csv(here::here("1_data",
                                  "a_cnp_fsd.csv"))
  
  a_cnp_fsd$diet = as.factor(a_cnp_fsd$diet)
  a_cnp_fsd = within(a_cnp_fsd, diet <-
                       relevel(diet, ref = "herbivore"))
  a_cnp_fsd = a_cnp_fsd[,-which(names(a_cnp_fsd) == "n_obs")]
  a_cnp_fsd = tidyr::pivot_wider(a_cnp_fsd, names_from = "component_name", values_from = "avg_component_mean")
  
  #  lm with diet
  model_c = lm(C ~ diet, a_cnp_fsd)
  tmodel_c = broom::tidy(model_c)
  model_n = lm(N ~ diet, a_cnp_fsd)
  tmodel_n = broom::tidy(model_n)
  model_p = lm(P ~ diet, a_cnp_fsd)
  tmodel_p = broom::tidy(model_p)
  model_cn = lm(CN ~ diet, a_cnp_fsd)
  tmodel_cn = broom::tidy(model_cn)
  model_cp = lm(CP ~ diet, a_cnp_fsd)
  tmodel_cp = broom::tidy(model_cp)
  model_np = lm(NP ~ diet, a_cnp_fsd)
  tmodel_np = broom::tidy(model_np)
  
  
  # Faeces stock data stoichiometric models
  models_fsd_diet = rbind(tmodel_c, tmodel_n, tmodel_p, tmodel_cn, tmodel_cp, tmodel_np)
  models_fsd_diet = models_fsd_diet %>%
    mutate_if(is.numeric, signif, 3)
  
  write.csv(
    models_fsd_diet,
    here::here(
      "2_outputs",
      "1_statistical_results",
      "models_fsd_diet.csv"
    ),
    row.names = FALSE
  )
  
  
  # Creating a comparative data object incorporating phylogeny
  
  # a_cnp_fsd$species_latin_name_gbif = stringr::str_to_sentence(gsub("_", " ", a_cnp_fsd$species_latin_name_gbif))
  # species_names = a_cnp_fsd$species_latin_name_gbif
  # taxize_species = classification(species_names, db = "gbif")
  # species_tree = class2tree(taxize_species, check = T)
  #
  # cdat = caper::comparative.data(
  #   data = as.data.frame(a_cnp_fsd),
  #   phy = species_tree$phylo,
  #   names.col = "species_latin_name_gbif",
  #   force.root = T
  # )
  
  # Centering body mass to ease the interpretation of intercepts
  
  a_cnp_fsd$body_mass =
    log(a_cnp_fsd$body_mass) -
    log(mean(a_cnp_fsd$body_mass))
  
  # Phylogenetic GLS with body mass and diet
  # model_c = caper::pgls(C ~ body_mass * diet, cdat)
  # summary(model_c)
  # model_n = caper::pgls(N ~ body_mass * diet, cdat)
  # summary(model_n)
  # model_p = caper::pgls(P ~ body_mass * diet, cdat)
  # summary(model_p)
  # model_cn = caper::pgls(CN ~ body_mass * diet, cdat)
  # summary(model_cn)
  # model_cp = caper::pgls(CP ~ body_mass * diet, cdat)
  # summary(model_cp)
  # model_np = caper::pgls(NP ~ body_mass * diet, cdat)
  # summary(model_np)
  
  #  lm with body mass and diet
  model_c = lm(C ~ body_mass * diet, a_cnp_fsd)
  tmodel_c = broom::tidy(model_c)
  model_n = lm(N ~ body_mass * diet, a_cnp_fsd)
  tmodel_n = broom::tidy(model_n)
  model_p = lm(P ~ body_mass * diet, a_cnp_fsd)
  tmodel_p = broom::tidy(model_p)
  model_cn = lm(CN ~ body_mass * diet, a_cnp_fsd)
  tmodel_cn = broom::tidy(model_cn)
  model_cp = lm(CP ~ body_mass * diet, a_cnp_fsd)
  tmodel_cp = broom::tidy(model_cp)
  model_np = lm(NP ~ body_mass * diet, a_cnp_fsd)
  tmodel_np = broom::tidy(model_np)
  
  
  # Faeces stock data stoichiometric models
  models_fsd_diet_bodymass = rbind(tmodel_c, tmodel_n, tmodel_p, tmodel_cn, tmodel_cp, tmodel_np)
  models_fsd_diet_bodymass = models_fsd_diet_bodymass %>%
    mutate_if(is.numeric, signif, 3)
  
  write.csv(
    models_fsd_diet_bodymass,
    here::here(
      "2_outputs",
      "1_statistical_results",
      "models_fsd_diet_bodymass.csv"
    ),
    row.names = FALSE
  )
  
  
  # Cloaca ####
  
  a_cnp_gsd = read.csv(here::here("1_data",
                                  "a_cnp_gsd.csv"))
  
  a_cnp_gsd$diet = as.factor(a_cnp_gsd$diet)
  a_cnp_gsd = within(a_cnp_gsd, diet <-
                       relevel(diet, ref = "herbivore"))
  a_cnp_gsd = a_cnp_gsd[,-which(names(a_cnp_gsd) == "n_obs")]
  a_cnp_gsd = tidyr::pivot_wider(a_cnp_gsd, names_from = "component_name", values_from = "avg_component_mean")
  
  #  lm with diet
  model_c = lm(C ~ diet, a_cnp_gsd)
  tmodel_c = broom::tidy(model_c)
  model_n = lm(N ~ diet, a_cnp_gsd)
  tmodel_n = broom::tidy(model_n)
  model_p = lm(P ~ diet, a_cnp_gsd)
  tmodel_p = broom::tidy(model_p)
  model_cn = lm(CN ~ diet, a_cnp_gsd)
  tmodel_cn = broom::tidy(model_cn)
  model_cp = lm(CP ~ diet, a_cnp_gsd)
  tmodel_cp = broom::tidy(model_cp)
  model_np = lm(NP ~ diet, a_cnp_gsd)
  tmodel_np = broom::tidy(model_np)
  
  # Wastes stock data diet models
  models_gsd_diet = rbind(tmodel_c, tmodel_n, tmodel_p, tmodel_cn, tmodel_cp, tmodel_np)
  models_gsd_diet = models_gsd_diet %>%
    mutate_if(is.numeric, signif, 3)
  
  write.csv(
    models_gsd_diet,
    here::here(
      "2_outputs",
      "1_statistical_results",
      "models_gsd_diet.csv"
    ),
    row.names = FALSE
  )
  
  # Centering body mass to ease the interpretation of intercepts
  
  a_cnp_gsd$body_mass = log(a_cnp_gsd$body_mass) - log(mean(a_cnp_gsd$body_mass))
  
  #  lm with body mass and diet
  model_c = lm(C ~ body_mass * diet, a_cnp_gsd)
  tmodel_c = broom::tidy(model_c)
  model_n = lm(N ~ body_mass * diet, a_cnp_gsd)
  tmodel_n = broom::tidy(model_n)
  model_p = lm(P ~ body_mass * diet, a_cnp_gsd)
  tmodel_p = broom::tidy(model_p)
  model_cn = lm(CN ~ body_mass * diet, a_cnp_gsd)
  tmodel_cn = broom::tidy(model_cn)
  model_cp = lm(CP ~ body_mass * diet, a_cnp_gsd)
  tmodel_cp = broom::tidy(model_cp)
  model_np = lm(NP ~ body_mass * diet, a_cnp_gsd)
  tmodel_np = broom::tidy(model_np)
  
  
  # Wastes stock data stoichiometric models
  models_gsd_diet_bodymass = rbind(tmodel_c, tmodel_n, tmodel_p, tmodel_cn, tmodel_cp, tmodel_np)
  models_gsd_diet_bodymass = models_gsd_diet_bodymass %>%
    mutate_if(is.numeric, signif, 3)
  
  write.csv(
    models_gsd_diet_bodymass,
    here::here(
      "2_outputs",
      "1_statistical_results",
      "models_gsd_diet_bodymass.csv"
    ),
    row.names = FALSE
  )
  
  
}
