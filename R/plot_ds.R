################################ NETSTO PROJECT ################################

#' plot_ds
#'
#' @param data? data_fluxes
#'
#' @return plots for the data synthesis
#' @export
#'
#' @examples
#' @authors Samuel Charberet
#'
#'

plots_ds = function(data, data_fluxes) {
  ##### I. FLUXES #####
  
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
    coord_cartesian(xlim = c(0, max(
      data_fluxes$CP_Crude_Protein_diet, na.rm = T
    ))) +
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
    path = here::here("2_outputs", "2_figures"),
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
    path = here::here("2_outputs", "2_figures"),
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
    path = here::here("2_outputs", "2_figures"),
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
    path = here::here("2_outputs", "2_figures"),
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
    path = here::here("2_outputs", "2_figures"),
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
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  
  # Apparent digestibility according to species
  
  # Nitrogen
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , CP_ad, median , na.rm =
                                                        T))
  
  species_nad = ggplot(data_fluxes[!is.na(data_fluxes$CP_ad), ], aes(x =
                                                                       Species_lat, y = CP_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "N absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_nad.pdf",
    plot = species_nad,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Sodium
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Na_ad, median , na.rm =
                                                        T))
  
  species_naad = ggplot(data_fluxes[!is.na(data_fluxes$Na_ad), ], aes(x =
                                                                        Species_lat, y = Na_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Na absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_naad.pdf",
    plot = species_naad,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Magnesium
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Mg_ad, median , na.rm =
                                                        T))
  
  species_mgad = ggplot(data_fluxes[!is.na(data_fluxes$Mg_ad), ], aes(x =
                                                                        Species_lat, y = Mg_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Mg absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_mgad.pdf",
    plot = species_mgad,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Phosphorus
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , P_ad, median , na.rm =
                                                        T))
  
  species_pad = ggplot(data_fluxes[!is.na(data_fluxes$P_ad), ], aes(x = Species_lat, y =
                                                                      P_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "P absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_pad.pdf",
    plot = species_pad,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Potassium
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , K_ad, median , na.rm =
                                                        T))
  
  species_kad = ggplot(data_fluxes[!is.na(data_fluxes$K_ad), ], aes(x = Species_lat, y =
                                                                      K_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "K absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_kad.pdf",
    plot = species_kad,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Calcium
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Ca_ad, median , na.rm =
                                                        T))
  
  species_caad = ggplot(data_fluxes[!is.na(data_fluxes$Ca_ad), ], aes(x =
                                                                        Species_lat, y = Ca_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Ca absorption efficiency (%)",
         x = "Species")
  
  ggsave(
    filename = "species_caad.pdf",
    plot = species_caad,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### II. STOCKS #####
  
  ##### Number of reference per year #####
  
  references_and_years  = unique(data[, c("reference_ID", "reference_year")])
  study_counts = table(references_and_years$reference_year)
  study_counts_df = data.frame(year = as.numeric(names(study_counts)), count = as.vector(study_counts))
  
  number_studies_f_time = ggplot(study_counts_df, aes(x = year, y = count)) +
    geom_area(fill = "#3366CC",
              color = "black",
              size = 1.5) +
    labs(x = "Year", y = "Number of Studies")+
    scale_x_continuous(breaks = seq(1965, 2020, 5))
  
  ggsave(
    filename = "number_studies_f_time.pdf",
    plot = number_studies_f_time,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### Number of species per study #####
  
  # Step 1: Get the unique study IDs
  study_ids = unique(data$reference_ID)
  
  # Step 2: Count the number of species per study
  species_counts = data %>%
    group_by(reference_ID) %>%
    summarise(num_species = n_distinct(species_latin_name_gbif))
  
  # Step 3: Determine the number of species per study and the count of studies with that number of species
  species_per_study = table(species_counts$num_species)
  num_studies = as.numeric(names(species_per_study))
  
  # Step 4: Create a histogram
  plot_data = data.frame(num_species = num_studies, num_studies = as.numeric(species_per_study))
  
  hist_nb_sp_per_stu = ggplot(plot_data, aes(x = num_species, y = num_studies)) +
    geom_bar(stat = "identity", fill = "#3366CC", color = "black") +
    labs(x = "Number of species per study", y = "Number of studies") +
    xlim(0, 25)
  
  ggsave(
    filename = "hist_nb_sp_per_stu.pdf",
    plot = hist_nb_sp_per_stu,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### Measure method proportion for C, N and P #####
  
  
  # Step 1: Filter the data for "C", "N", and "P" components
  filtered_data <- data %>%
    filter(component_name %in% c("C", "N", "P"))
  
  # Step 2: Group the filtered data by "component_name" and "component_measure_method" and calculate the count
  grouped_data <- filtered_data %>%
    group_by(component_name, component_measure_method) %>%
    summarise(count = n())
  
  # Step 3: Create pie charts for each "component_name" and save as PDF
  for (name in unique(grouped_data$component_name)) {
    subset_data <- grouped_data %>% filter(component_name == name)
    
    # Step 4: Create a pie chart with measure method labels
    p <- ggplot(subset_data, aes(x = "", y = count, fill = component_measure_method)) +
      labs(title = paste("Proportion of measure methods for", name)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3")+
      theme_void()

    # Save the plot as a PDF with a specific name
    file_name <- paste(name, "measure_method_pie_chart.pdf", sep = "_")
    ggsave(
      filename = file_name,
      plot = p,
      device = cairo_pdf,
      path = here::here("2_outputs", "2_figures"),
      scale = 1,
      width = 7,
      height = 4,
      units = "in"
    )
  }
  
  
  ##### Map the sampling locations #####
  
  data$sampling_latitude = as.numeric(data$sampling_latitude)
  data$sampling_longitude = as.numeric(data$sampling_longitude)
  
  # Create a subset of unique positions
  unique_positions <- unique(data[, c("sampling_longitude", "sampling_latitude")])
  
  # Create a base map
  world_map <- map_data("world")
  
  # Plot the map
  p <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "gray50") +
    geom_point(data = unique_positions, aes(x = sampling_longitude, y = sampling_latitude), color = "#3366CC", size = 2) +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw()
  
  # Save the plot as a PDF
  ggsave(
    filename = "sampling_locations_map",
    plot = p,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )  
  
  
  
  ##### Number of observation for sample type according to matrix #####
  
  df_split <- split(data, data$component_data_type)
  
  plots_matrix_diet_unit_list <- lapply(df_split, function(df) {
    # Sort sample_type levels based on the number of observations
    sorted_levels <- df %>%
      count(sample_type) %>%
      arrange(desc(n)) %>%
      pull(sample_type)
    
    # Reorder the sample_type factor based on the sorted levels
    df$sample_type <- factor(df$sample_type, levels = sorted_levels)
    
    
    
    ggplot(df, aes(x = sample_type, fill = diet)) +
      geom_bar() +
      labs(x = "Sample Type", y = "Number of Observations") +
      scale_fill_manual(values = c("herbivore" = "olivedrab", 
                                   "omnivore" = "lightgoldenrod2", 
                                   "frugivore" = "darkorange", 
                                   "carnivore" = "red4", 
                                   "insectivore" = "grey20", 
                                   "detritivore" = "saddlebrown")) +
      theme_bw()
  })
  
  # Save each plot separately from the plot_list
  ggsave(
    filename = "fluxes.pdf",
    plot = plots_matrix_diet_unit_list[[1]],
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ggsave(
    filename = "rates.pdf",
    plot = plots_matrix_diet_unit_list[[2]],
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ggsave(
    filename = "stocks.pdf",
    plot = plots_matrix_diet_unit_list[[3]],
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  
  
}