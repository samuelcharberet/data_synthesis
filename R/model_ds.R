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
  # Fluxes ####
  
  ## Nitrogen ####
  # Fit linear and nls models
  linear_ln = lm(CP_ad ~ CP_Crude_Protein_diet, data = data_fluxes)
  nls_ln =
    nls(
      CP_ad ~ (k * CP_Crude_Protein_diet - E) / CP_Crude_Protein_diet,
      data = data_fluxes,
      start = list(k = 50, E = 1)
    )
  
  linear_ln = broom::glance(linear_ln)
  nls_ln = broom::glance(nls_ln)
  
  comp_ln = bind_rows(linear_ln, nls_ln)
  
  ## Phosphorus ####
  # Fit linear and nls models
  linear_lp = lm(P_ad ~ P_diet , data = data_fluxes)
  nls_lp =
    nls(P_ad ~ (k * P_diet - E) / P_diet,
        data = data_fluxes,
        start = list(k = 50, E = 1))
  linear_lp = broom::glance(linear_lp)
  nls_lp = broom::glance(nls_lp)
  comp_lp = bind_rows(linear_lp, nls_lp)
  
  ## Potassium ####
  # Fit linear and nls models
  linear_lk = lm(K_ad ~ K_diet , data = data_fluxes)
  nls_lk =
    nls(K_ad ~ (k * K_diet - E) / K_diet,
        data = data_fluxes,
        start = list(k = 50, E = 1))
  linear_lk = broom::glance(linear_lk)
  nls_lk = broom::glance(nls_lk)
  comp_lk = bind_rows(linear_lk, nls_lk)
  
  ## Calcium ####
  # Fit linear and nls models
  linear_lca = lm(Ca_ad ~ Ca_diet , data = data_fluxes)
  nls_lca =
    nls(Ca_ad ~ (k * Ca_diet - E) / Ca_diet,
        data = data_fluxes,
        start = list(k = 50, E = 1))
  linear_lca = broom::glance(linear_lca)
  nls_lca = broom::glance(nls_lca)
  comp_lca = bind_rows(linear_lca, nls_lca)
  
  ## Magnesium ####
  # Fit linear and nls models
  linear_lmg = lm(Mg_ad ~ Mg_diet , data = data_fluxes)
  nls_lmg =
    nls(Mg_ad ~ (k * Mg_diet - E) / Mg_diet,
        data = data_fluxes,
        start = list(k = 50, E = 1))
  linear_lmg = broom::glance(linear_lmg)
  nls_lmg = broom::glance(nls_lmg)
  comp_lmg = bind_rows(linear_lmg, nls_lmg)
  
  ## Sodium ####
  # Fit linear and nls models
  linear_lna = lm(Na_ad ~ Na_diet , data = data_fluxes)
  nls_lna =
    nls(Na_ad ~ (k * Na_diet - E) / Na_diet,
        data = data_fluxes,
        start = list(k = 50, E = 1))
  linear_lna = broom::glance(linear_lna)
  nls_lna = broom::glance(nls_lna)
  comp_lna = bind_rows(linear_lna, nls_lna)
  
  comp_l_all = bind_rows(comp_ln, comp_lp, comp_lk, comp_lca, comp_lmg, comp_lna)
  comp_l_all$element = c("N", "N", "P", "P", "K", "K", "Ca", "Ca", "Mg", "Mg", "Na", "Na")
  write.csv(
    comp_l_all,
    here::here("2_outputs",
               "1_statistical_results",
               "comp_lucas_all.csv"),
    row.names = FALSE
  )
  
  # Stocks ####
  # We make a model at the level of species averages as we have traits at this level only (body mass and diet)
  
  # One model for animals with a cloaca, and one for animals without
  
  ## Mammals ####
  data_mammals_species = read.csv(here::here("1_data",
                                  "data_mammals_species.csv"))
  
  data_mammals_species$diet = as.factor(data_mammals_species$diet)
  data_mammals_species = within(data_mammals_species, diet <-
                       relevel(diet, ref = "Herbivore"))
  data_mammals_species = data_mammals_species[,-which(names(data_mammals_species) == "n_obs")]
  data_mammals_species = tidyr::pivot_wider(data_mammals_species, names_from = "component_name", values_from = "avg_component_mean")
  
  
  
  
  #  lm with diet
  model_c = lm(C ~ diet, data_mammals_species)
  tmodel_c = broom::tidy(model_c)
  model_n = lm(N ~ diet, data_mammals_species)
  tmodel_n = broom::tidy(model_n)
  model_p = lm(P ~ diet, data_mammals_species)
  tmodel_p = broom::tidy(model_p)
  model_cn = lm(`C/N` ~ diet, data_mammals_species)
  tmodel_cn = broom::tidy(model_cn)
  model_cp = lm(`C/P` ~ diet, data_mammals_species)
  tmodel_cp = broom::tidy(model_cp)
  model_np = lm(`N/P` ~ diet, data_mammals_species)
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
  
  # data_mammals_species$species_latin_name_gbif = stringr::str_to_sentence(gsub("_", " ", data_mammals_species$species_latin_name_gbif))
  # species_names = data_mammals_species$species_latin_name_gbif
  # taxize_species = classification(species_names, db = "gbif")
  # species_tree = class2tree(taxize_species, check = T)
  #
  # cdat = caper::comparative.data(
  #   data = as.data.frame(data_mammals_species),
  #   phy = species_tree$phylo,
  #   names.col = "species_latin_name_gbif",
  #   force.root = T
  # )
  
  # Centering body mass to ease the interpretation of intercepts
  
  data_mammals_species$body_mass =
    log(data_mammals_species$body_mass) -
    log(mean(data_mammals_species$body_mass))
  
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
  model_c = lm(C ~ body_mass * diet, data_mammals_species)
  tmodel_c = broom::tidy(model_c)
  gmodel_c = broom::glance(model_c)
  
  model_n = lm(N ~ body_mass * diet, data_mammals_species)
  tmodel_n = broom::tidy(model_n)
  gmodel_n = broom::glance(model_n)
  
  model_p = lm(P ~ body_mass * diet, data_mammals_species)
  tmodel_p = broom::tidy(model_p)
  gmodel_p = broom::glance(model_p)
  
  model_cn = lm(`C/N` ~ body_mass * diet, data_mammals_species)
  tmodel_cn = broom::tidy(model_cn)
  gmodel_cn = broom::glance(model_cn)
  
  model_cp = lm(`C/P` ~ body_mass * diet, data_mammals_species)
  tmodel_cp = broom::tidy(model_cp)
  gmodel_cp = broom::glance(model_cp)
  
  model_np = lm(`N/P` ~ body_mass * diet, data_mammals_species)
  tmodel_np = broom::tidy(model_np)
  gmodel_np = broom::glance(model_np)
  
  
  # Faeces stock data stoichiometric models
  bmodels_fsd_diet_bodymass = rbind(tmodel_c, tmodel_n, tmodel_p, tmodel_cn, tmodel_cp, tmodel_np)
  bmodels_fsd_diet_bodymass = bmodels_fsd_diet_bodymass %>%
    mutate_if(is.numeric, signif, 3)
  
  write.csv(
    bmodels_fsd_diet_bodymass,
    here::here(
      "2_outputs",
      "1_statistical_results",
      "bmodels_fsd_diet_bodymass.csv"
    ),
    row.names = FALSE
  )
  
  gmodels_fsd_diet_bodymass = rbind(gmodel_c, gmodel_n, gmodel_p, gmodel_cn, gmodel_cp, gmodel_np)
  gmodels_fsd_diet_bodymass = gmodels_fsd_diet_bodymass %>%
    mutate_if(is.numeric, signif, 3)
  
  write.csv(
    gmodels_fsd_diet_bodymass,
    here::here(
      "2_outputs",
      "1_statistical_results",
      "gmodels_fsd_diet_bodymass.csv"
    ),
    row.names = FALSE
  )
  

  
  
}
