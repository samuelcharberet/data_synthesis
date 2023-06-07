################################ NETSTO PROJECT ################################

#' plot_ds
#'
#' @param path
#'
#' @return plots for the data synthesis
#' @export
#'
#' @examples
#' @authors Samuel Charberet
#'
#'

plots_ds = function(data, data_fluxes) {

  
  
  # Set global options for the ggplot2 plots
  ggplot2::theme_set(theme_classic() + theme(
    panel.grid.major = element_line(color = "gray95", linetype = 1),
    legend.position = "none"
  ))
  
  colours_elements = c(
    "C" = "#808080",
    "N" = "#5A5ACA",
    "P" = "#EC9200",
    "Na" = "#403EFF",
    "Mg" = "#5CC55C",
    "S" = "#D69F09",
    "K" = "#9B4BE1",
    "Ca" = "#DF4F4F"
  ) # The colors used for elements and isotopes, modified after Jmol
  
  colours_diet = c(
    "Herbivore" = "#428741",
    "Omnivore" = "#F5CD07",
    "Carnivore" = "#B30E0E",
    "Insectivore" = "#000000",
    "Frugivore" = "#AE2CB8",
    "Nectarivore" = "#FA6A02"
  ) # The colors used for diet
  
  # Nitrogen ####
  
  lucas_nitrogen = ggplot2::ggplot(data_fluxes ,
                                   aes(x = CP_Crude_Protein_diet,
                                       y = CP_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$CP_Crude_Protein_diet, na.rm = T))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(color = Feed)) +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet N (%DM)",
         y = "N absorption efficiency (%)") +
    theme(legend.position = "right")
  
  # Save the plot
  ggsave(
    filename = "lucas_nitrogen.pdf",
    plot = lucas_nitrogen,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  
  # Sodium ####
  
  lucas_sodium = ggplot2::ggplot(data_fluxes ,
                                 aes(x = Na_diet,
                                     y = Na_ad,
                                     color = Feed)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$Na_diet, na.rm = T))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet Na (%DM)",
         y = "Na absorption efficiency (%)") +
    theme(legend.position = "right")
  
  # Save the plot
  ggsave(
    filename = "lucas_sodium.pdf",
    plot = lucas_sodium,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Magnesium ####
  
  lucas_magnesium = ggplot2::ggplot(data_fluxes ,
                                    aes(x = Mg_diet,
                                        y = Mg_ad,
                                        color = Feed)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$Mg_diet, na.rm = T))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet Mg (%DM)",
         y = "Mg absorption efficiency (%)") +
    theme(legend.position = "right")
  
  # Save the plot
  ggsave(
    filename = "lucas_magnesium.pdf",
    plot = lucas_magnesium,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Phosphorus ####
  
  lucas_phosphorus = ggplot2::ggplot(data_fluxes ,
                                     aes(x = P_diet,
                                         y = P_ad,
                                         color = Feed)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$P_diet, na.rm = T))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet P (%DM)",
         y = "P absorption efficiency (%)") +
    theme(legend.position = "right")
  
  # Save the plot
  ggsave(
    filename = "lucas_phosphorus.pdf",
    plot = lucas_phosphorus,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Potassium ####
  
  lucas_potassium = ggplot2::ggplot(data_fluxes ,
                                    aes(x = K_diet,
                                        y = K_ad,
                                        color = Feed)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$K_diet, na.rm = T))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point() +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet K (%DM)",
         y = "K absorption efficiency (%)") +
    theme(legend.position = "right")
  
  # Save the plot
  ggsave(
    filename = "lucas_potassium.pdf",
    plot = lucas_potassium,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Calcium ####
  
  lucas_calcium = ggplot2::ggplot(data_fluxes ,
                                  aes(x = Ca_diet,
                                      y = Ca_ad,
                                      color = Feed)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    
    geom_point() +
    geom_smooth(
      color = "black",
      method = "scam",
      formula = y ~ s(x, k = 5, bs = "micv"),
      se = T
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet Ca (%DM)",
         y = "Ca absorption efficiency (%)") +
    theme(legend.position = "right")
  
  # Save the plot
  ggsave(
    filename = "lucas_calcium.pdf",
    plot = lucas_calcium,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  
  # Apparent digestibility according to species
  
  # Nitrogen
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , CP_ad, median , na.rm =
                                                        T))
  
  species_nad = ggplot(data_fluxes[!is.na(data_fluxes$CP_ad),], aes(x =
                                                                      Species_lat, y = CP_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "N absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_nad.pdf",
    plot = species_nad,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Sodium
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Na_ad, median , na.rm =
                                                        T))
  
  species_naad = ggplot(data_fluxes[!is.na(data_fluxes$Na_ad),], aes(x =
                                                                       Species_lat, y = Na_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Na absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_naad.pdf",
    plot = species_naad,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Magnesium
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Mg_ad, median , na.rm =
                                                        T))
  
  species_mgad = ggplot(data_fluxes[!is.na(data_fluxes$Mg_ad),], aes(x =
                                                                       Species_lat, y = Mg_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Mg absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_mgad.pdf",
    plot = species_mgad,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Phosphorus
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , P_ad, median , na.rm =
                                                        T))
  
  species_pad = ggplot(data_fluxes[!is.na(data_fluxes$P_ad),], aes(x = Species_lat, y =
                                                                     P_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "P absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_pad.pdf",
    plot = species_pad,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Potassium
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , K_ad, median , na.rm =
                                                        T))
  
  species_kad = ggplot(data_fluxes[!is.na(data_fluxes$K_ad),], aes(x = Species_lat, y =
                                                                     K_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "K absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_kad.pdf",
    plot = species_kad,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Caclium
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Ca_ad, median , na.rm =
                                                        T))
  
  species_caad = ggplot(data_fluxes[!is.na(data_fluxes$Ca_ad),], aes(x =
                                                                       Species_lat, y = Ca_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Ca absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_caad.pdf",
    plot = species_caad,
    device = cairo_pdf,
    path = here::here("3_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  
}