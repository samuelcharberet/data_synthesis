#' plot_ds
#'
#' @param data, data_fluxes
#'
#' @return plots for the data synthesis
#' @export
#'
#' @examples
#' @authors Samuel Charberet
#'
#'

plot_ds = function(data, data_f) {
  ##### Structuring data  #####
  
  # Specify the strings to match in factor column names
  factor_columns =
    c(
      "type",
      "unit",
      "season",
      "resolution",
      "stage",
      "environment",
      "ID",
      "freezing",
      "autoclaving",
      "drying",
      "oven",
      "grinding",
      "diet"
    )
  
  # Use mutate() and across() to transform columns matching the strings
  
  data <- data %>%
    mutate(across(matches(factor_columns), as.factor))
  
  # Specify the strings to match in numeric column names
  numeric_columns =
    c("sampling_latitude",
      "sampling_longitude",
      "nb_items_per_sample")
  
  # Use mutate() and across() to transform columns matching the strings
  data <- data %>%
    mutate(across(matches(numeric_columns), as.numeric))
  
  
  ##### Global option for the plots  #####
  
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
    "Nectarivore" = "#FA6A02",
    "Detritivore" = "#9E5748"
  ) # The colors used for diet
  
  
  ##### I. FLUXES #####
  
  data_fluxes = data_f
  
  # Nitrogen ####
  
  lucas_nitrogen = ggplot2::ggplot(data_fluxes , aes(x = CP_Crude_Protein_diet, y = CP_ad)) +
    coord_cartesian(xlim = c(0, max(
      data_fluxes$CP_Crude_Protein_diet, na.rm = T
    ))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#F6E6E6',
      alpha = 0.1
    ) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = 0,
        ymax = +Inf
      ),
      fill = '#E7F6E6',
      alpha = 0.1
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet N (%DM)", y = "N AE (%)") +
    theme(legend.position = "right") + ylim(-100, 100)
  
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
  
  lucas_sodium = ggplot2::ggplot(data_fluxes , aes(x = Na_diet, y = Na_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$Na_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#F6E6E6',
      alpha = 0.1
    ) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = 0,
        ymax = +Inf
      ),
      fill = '#E7F6E6',
      alpha = 0.1
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet Na (%DM)", y = "Na AE (%)") +
    theme(legend.position = "right") + ylim(-100, 100)
  
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
  
  lucas_magnesium = ggplot2::ggplot(data_fluxes , aes(x = Mg_diet, y = Mg_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$Mg_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#F6E6E6',
      alpha = 0.1
    ) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = 0,
        ymax = +Inf
      ),
      fill = '#E7F6E6',
      alpha = 0.1
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet Mg (%DM)", y = "Mg AE (%)") +
    theme(legend.position = "right") + ylim(-100, 100)
  
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
  
  lucas_phosphorus = ggplot2::ggplot(data_fluxes , aes(x = P_diet, y = P_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$P_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#F6E6E6',
      alpha = 0.1
    ) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = 0,
        ymax = +Inf
      ),
      fill = '#E7F6E6',
      alpha = 0.1
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 100, E = 0.5)),
      se = F,
      colour = "black",
      fullrange = TRUE
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet P (%DM)", y = "P AE (%)") +
    theme(legend.position = "right") + ylim(-100, 100)
  
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
  
  lucas_potassium = ggplot2::ggplot(data_fluxes , aes(x = K_diet, y = K_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$K_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#F6E6E6',
      alpha = 0.1
    ) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = 0,
        ymax = +Inf
      ),
      fill = '#E7F6E6',
      alpha = 0.1
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet K (%DM)", y = "K AE (%)") +
    theme(legend.position = "right") + ylim(-100, 100)
  
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
  
  lucas_calcium = ggplot2::ggplot(data_fluxes , aes(x = Ca_diet, y = Ca_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$Ca_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#F6E6E6',
      alpha = 0.1
    ) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = 0,
        ymax = +Inf
      ),
      fill = '#E7F6E6',
      alpha = 0.1
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(aes(color = Diet)) +
    stat_smooth(
      method = 'nls',
      formula = 'y~(k*x-E)/x',
      method.args = list(start = c(k = 50, E = 0)),
      se = FALSE,
      colour = "black"
    ) +
    scale_color_manual(values = colours_diet) +
    labs(x = "Diet Ca (%DM)", y = "Ca AE (%)") +
    theme(legend.position = "right") + ylim(-100, 100)
  
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
  
  # Complete Lucas plots ####
  complete_lucas_plot = ggpubr::ggarrange(
    lucas_phosphorus,
    lucas_potassium,
    lucas_calcium,
    NULL,
    NULL,
    NULL,
    lucas_magnesium,
    lucas_sodium,
    NULL,
    ncol = 3,
    nrow = 3,
    labels = c("a.", "b.", "c.", "", "", "", "d.", "e."),
    label.y = 1.16,
    label.x = 0,
    heights = c(1, 0.05, 1),
    widths = c(1, 1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  complete_lucas_plot = ggpubr::annotate_figure(complete_lucas_plot, bottom = "", top = "")
  
  ggsave(
    filename = "lucas_all.pdf",
    plot = complete_lucas_plot,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # Lucas plots ####
  np_lucas_plot = ggpubr::ggarrange(
    lucas_nitrogen,
    lucas_phosphorus,
    ncol = 2,
    nrow = 1,
    labels = c("a.", "b."),
    label.y = 1.1,
    label.x = 0,
    heights = c(1),
    widths = c(1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  np_lucas_plot = ggpubr::annotate_figure(np_lucas_plot, bottom = "", top = "")
  
  ggsave(
    filename = "lucas_np.pdf",
    plot = np_lucas_plot,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 3,
    units = "in"
  )
  
  # Apparent digestibility according to species ####
  
  # Nitrogen ####
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , CP_ad, median , na.rm =
                                                        T))
  
  species_nad = ggplot(data_fluxes[!is.na(data_fluxes$CP_ad), ], aes(x =
                                                                       Species_lat, y = CP_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "N AE (%)", x = "Species")
  
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
  
  # Sodium ####
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Na_ad, median , na.rm =
                                                        T))
  
  species_naad = ggplot(data_fluxes[!is.na(data_fluxes$Na_ad), ], aes(x =
                                                                        Species_lat, y = Na_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Na AE (%)", x = "Species")
  
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
  
  # Magnesium ####
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Mg_ad, median , na.rm =
                                                        T))
  
  species_mgad = ggplot(data_fluxes[!is.na(data_fluxes$Mg_ad), ], aes(x =
                                                                        Species_lat, y = Mg_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Mg AE (%)", x = "Species")
  
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
  
  # Phosphorus ####
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , P_ad, median , na.rm =
                                                        T))
  
  species_pad = ggplot(data_fluxes[!is.na(data_fluxes$P_ad), ], aes(x = Species_lat, y =
                                                                      P_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "P AE (%)", x = "Species")
  
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
  
  # Potassium ####
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , K_ad, median , na.rm =
                                                        T))
  
  species_kad = ggplot(data_fluxes[!is.na(data_fluxes$K_ad), ], aes(x = Species_lat, y =
                                                                      K_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "K AE (%)", x = "Species")
  
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
  
  # Calcium ####
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Ca_ad, median , na.rm =
                                                        T))
  
  species_caad = ggplot(data_fluxes[!is.na(data_fluxes$Ca_ad), ], aes(x =
                                                                        Species_lat, y = Ca_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Ca AE (%)", x = "Species")
  
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
  
  ##### 1. Preliminary figures #####
  
  #### Number of observation by elements ####
  required_sample_types <- c("feces", "urine", "guano", "frass")
  required_components <- c(
    "C",
    "N",
    "P",
    "Al",
    "As",
    "B",
    "Ba",
    "Ca",
    "Cd",
    "Cl",
    "Co",
    "Cr",
    "Cu",
    "Fe",
    "Hg",
    "K",
    "Mg",
    "Mn",
    "Mo",
    "Na",
    "Ni",
    "O",
    "Pb",
    "S",
    "Se",
    "Si",
    "Sr",
    "Ti",
    "V",
    "Zn"
  )
  
  filtered_data <- data %>%
    filter(sample_type %in% required_sample_types,
           component_name %in% required_components) %>%
    group_by(component_name) %>%
    tally(name = "count")
  
  # Arrange the data by count in descending order
  filtered_data <- filtered_data %>%
    arrange(desc(count))
  
  # Create the bar plot
  number_obs_elements = ggplot(filtered_data, aes(x = reorder(component_name, count), y = count)) +
    geom_col(fill = "#3366CC", color = "black") +
    coord_flip() +
    labs(x = "Element", y = "Number of observation (wastes)") +
    theme_pubclean()
  
  
  ggsave(
    filename = "number_obs_elements.pdf",
    plot = number_obs_elements,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### Number of reference per year #####
  
  
  references_and_years  = unique(data[, c("reference_ID", "reference_year")])
  study_counts = table(references_and_years$reference_year)
  study_counts_df = data.frame(year = as.numeric(names(study_counts)), count = as.vector(study_counts))
  
  number_studies_f_time = ggplot(study_counts_df, aes(x = year, y = count)) +
    ylim(0, NA) +
    geom_line(color = "black", linewidth = 1) +
    labs(x = "Year", y = "Number of Studies") +
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
  species_counts = data |>
    group_by(reference_ID) |>
    dplyr::summarise(num_species = n_distinct(species_latin_name_gbif))
  
  # Step 3: Determine the number of species per study and the count of studies with that number of species
  species_per_study = table(species_counts$num_species)
  num_studies = as.numeric(names(species_per_study))
  
  # Step 4: Create a histogram
  plot_data = data.frame(num_species = num_studies,
                         num_studies = as.numeric(species_per_study))
  
  hist_nb_sp_per_stu = ggplot(plot_data, aes(x = num_species, y = num_studies)) +
    geom_bar(
      stat = "identity",
      fill = "#3366CC",
      color = "black",
      alpha = 0.7
    ) +
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
  
  # Replace values based on specific conditions
  data$component_measure_method[data$component_measure_method == "autoanalyzer"] <-
    "Autoanalyzer"
  data$component_measure_method[data$component_measure_method == "elemental_analyser"] <-
    "Elemental analyzer"
  data$component_measure_method[data$component_measure_method %in% c("micro_kjeldahl", "macro_kjeldahl", "kjeldahl")] <-
    "Kjeldahl"
  data$component_measure_method[data$component_measure_method %in% c("icp", "icp_oes")] <-
    "ICP"
  data$component_measure_method[data$component_measure_method == "ammonium_molybdate_method"] <-
    "Ammonium molybdate"
  
  # Define required methods
  methods <- c(
    "Autoanalyzer",
    "Elemental analyzer",
    "Kjeldahl",
    "ICP",
    "Ammonium molybdate",
    "TOC"
  )
  
  # Update values not in required_methods to "others"
  data$component_measure_method[!(data$component_measure_method %in% methods)] <-
    "Others"
  
  required_methods <- c(
    "Autoanalyzer",
    "Elemental analyzer",
    "Kjeldahl",
    "ICP",
    "Ammonium molybdate",
    "TOC",
    "Others"
  )
  required_sample_types <- c("feces", "urine", "guano", "frass")
  required_components <- c("C", "N", "P")
  
  filtered_data <- data %>%
    filter(
      sample_type %in% required_sample_types,
      component_name %in% required_components,
      component_measure_method %in% required_methods
    ) %>%
    group_by(component_name, component_measure_method) %>%
    tally(name = "count")
  
  library(ggplot2)
  
  # Filtered data containing only components C, N, and P
  filtered_C_N_P <- filtered_data %>%
    filter(component_name %in% c("C", "N", "P"))
  
  # Define the desired order for the legend
  desired_order <-
    c(
      "Elemental analyzer",
      "TOC",
      "ICP",
      "Autoanalyzer",
      "Kjeldahl",
      "Ammonium molybdate",
      "Others"
    )
  
  # Convert 'component_measure_method' to a factor with the desired order
  filtered_C_N_P$component_measure_method <-
    factor(filtered_data$component_measure_method, levels = desired_order)
  
  
  # Create a plot with facets for each component_name
  plot_measure_methods = ggplot(filtered_C_N_P,
                                aes(x = "", y = count, fill = component_measure_method)) +
    geom_bar(
      stat = "identity",
      position = "fill",
      width = 1,
      color = "black",
      alpha = 0.7
    ) +
    coord_polar("y", start = 0) +
    theme_void() +
    ggsci::scale_fill_npg() +
    facet_wrap( ~ component_name, nrow = 1) +
    labs(fill = "Measure method")
  
  
  ggsave(
    filename = "plot_measure_methods.pdf",
    plot = plot_measure_methods,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### Map the sampling locations #####
  
  data$sampling_latitude = as.numeric(data$sampling_latitude)
  data$sampling_longitude = as.numeric(data$sampling_longitude)
  
  # Create a subset of unique positions
  unique_positions <-
    unique(data[, c("sampling_longitude", "sampling_latitude")])
  
  # Create a base map
  world_map <- map_data("world")
  
  # Plot the map
  p <- ggplot() +
    geom_polygon(
      data = world_map,
      aes(x = long, y = lat, group = group),
      fill = "lightgray",
      color = "gray50"
    ) +
    geom_point(
      data = unique_positions,
      aes(x = sampling_longitude, y = sampling_latitude),
      color = "#3366CC",
      size = 2
    ) +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw()
  
  # Save the plot as a PDF
  ggsave(
    filename = "sampling_locations_map.pdf",
    plot = p,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### Body masses of species histogram  #####
  
  selected_data <-
    dplyr::select(data, species_latin_name_gbif, body_mass, diet)
  
  grouped_data <- unique(selected_data)
  
  bodymass_diet_histogram = ggplot(grouped_data, aes(log10(body_mass))) + geom_histogram(
    aes(fill = diet),
    bins = 50,
    color = 'black',
    alpha = 0.7
  ) +
    scale_fill_manual(values = colours_diet) +
    labs(x = "Body mass <br> (log<sub>10</sub> g)", y = "Number of species", fill = "Diet") +
    theme_bw() +
    theme(axis.title.x = element_markdown())
  
  
  
  # Save the plot as a PDF
  ggsave(
    filename = "bodymass_diet_histogram.pdf",
    plot = bodymass_diet_histogram,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### Histograms showing the proportion of supplementary data #####
  
  datref0 = subset(data, first_author != "charberet")
  datref = datref0[order(datref0$first_author, decreasing = FALSE), ]
  datref$obsref_ID = interaction(datref$reference_ID, datref$observation_ID)
  dim(datref0)
  dim(datref)
  
  papers = unique(datref$reference_ID)
  length(papers)
  sample_types = unique(datref$sample_type)
  tabref0 = matrix(0, nrow = length(papers), ncol = c(3 + length(sample_types) *
                                                        3))
  colnames(tabref0) = c("ref", "first_author", "year", paste(rep(sample_types, each =
                                                                   3), rep(
                                                                     c("sp", "obs", "data"), length(sample_types)
                                                                   ), sep = "_"))
  tabref = as.data.frame((tabref0))
  
  for (i in 1:length(papers)) {
    x = subset(datref, reference_ID == papers[i])
    x = droplevels(x)
    tabref[i, "ref"] = x[1, "reference_ID"]
    tabref[i, "first_author"] = x[1, "first_author"]
    tabref[i, "year"] = x[1, "reference_year"]
    st_x = unique(x$sample_type)
    for (j in 1:length(st_x)) {
      st = st_x[j]
      sub = subset(x, sample_type == st)
      tabref[i, paste(st, "sp", sep = "_")] = length(unique(sub$species))
      tabref[i, paste(st, "obs", sep = "_")] = length(unique(sub$obsref_ID))
      tabref[i, paste(st, "data", sep = "_")] = nrow(sub)
    }
  }
  head(tabref)
  
  #--- Add total
  total = tabref[1, ]
  total[1, "ref"] = 0
  total[1, "first_author"] = "total"
  total[1, "year"] = 2023
  for (i in 1:length(sample_types)) {
    st = sample_types[i]
    sub = subset(datref, sample_type == st)
    sub = droplevels(sub)
    total[1, paste(st, "sp", sep = "_")] = length(unique(sub$species))
    total[1, paste(st, "obs", sep = "_")] = length(unique(sub$obsref_ID))
    total[1, paste(st, "data", sep = "_")] = nrow(sub)
  }
  tabref = rbind(tabref, total)
  
  #--- select only feces, guano, frass
  unique(data$sample_type)
  dat2 = subset(data, !is.na(sample_type) == T & !is.na(class) == T)
  datwaste = subset(dat2, sample_type %in% c("feces", "guano", "frass"))
  datwaste = droplevels(datwaste)
  
  #--- add a column for observations across refs
  datwaste$obsref_ID = interaction(datwaste$reference_ID, datwaste$observation_ID)
  
  #--- make 2 datasets for literature versus addition by Samuel
  nosam = subset(datwaste, first_author != "charberet")
  sam = subset(datwaste, first_author == "charberet")
  
  #--- add log10 body mass
  nosam$log10_bodymass = log10(as.numeric(nosam$body_mass))
  sam$log10_bodymass = log10(as.numeric(sam$body_mass))
  
  
  #--- add log body mass classes (20 classes) based on an histogram
  b = hist(nosam$log10_bodymass, freq = T, breaks = 20)
  
  mids = b$mids
  #a = hist(sam$log10_bodymass,freq=T,ylim=c(0,870),breaks=br)
  findbodyclass = function(x)
    return(which(abs(mids - x) == min(abs(mids - x)))[1])
  sam$bodymass_class = unlist(lapply(sam$log10_bodymass, FUN = findbodyclass))
  nosam$bodymass_class = unlist(lapply(nosam$log10_bodymass, FUN = findbodyclass))
  
  
  #--- Table for body mass classes
  tab = tabspecies = matrix(
    NA,
    nrow = 2,
    ncol = length(mids),
    dimnames = list(c("Litterature", "Zoo addition"), mids)
  )
  for (i in 1:length(mids)) {
    x = subset(nosam, bodymass_class == i)
    x2 = subset(sam, bodymass_class == i)
    tab[1, i] = ifelse(nrow(x) == 0, 0, length(unique(x$obsref_ID)))
    tab[2, i] = ifelse(nrow(x2) == 0, 0, length(unique(x2$obsref_ID)))
    tabspecies[2, i] = ifelse(nrow(x2) == 0, 0, length(unique(c(
      x$species, x2$species
    ))) - length(unique(x$species)))
    tabspecies[1, i] = ifelse(nrow(x) == 0, 0, length(unique(x$species)))
  }
  
  #--- Table for diet
  tab2 = tabspecies2 = matrix(NA,
                              nrow = 2,
                              ncol = 4,
                              dimnames = list(
                                c("Litterature", "Zoo addition"),
                                c("Detritivore", "Herbivore", "Omnivore", "Carnivore")
                              ))
  for (i in 1:4) {
    x = subset(nosam, diet == colnames(tab2)[i])
    x2 = subset(sam, diet == colnames(tab2)[i])
    tab2[1, i] = ifelse(nrow(x) == 0, 0, length(unique(x$obsref_ID)))
    tab2[2, i] = ifelse(nrow(x2) == 0, 0, length(unique(x2$obsref_ID)))
    tabspecies2[2, i] = ifelse(nrow(x2) == 0, 0, length(unique(c(
      x$species, x2$species
    ))) - length(unique(x$species)))
    tabspecies2[1, i] = ifelse(nrow(x) == 0, 0, length(unique(x$species)))
  }
  
  
  
  #--- Table for classes
  classes = unique(datwaste$class)
  
  tab3 = tabspecies3 = matrix(
    NA,
    nrow = 2,
    ncol = length(classes),
    dimnames = list(c("Litterature", "Zoo addition"), classes)
  )
  for (i in 1:length(classes)) {
    x = subset(nosam, class == colnames(tab3)[i])
    x2 = subset(sam, class == colnames(tab3)[i])
    tab3[1, i] = ifelse(nrow(x) == 0, 0, length(unique(x$obsref_ID)))
    tab3[2, i] = ifelse(nrow(x2) == 0, 0, length(unique(x2$obsref_ID)))
    tabspecies3[2, i] = ifelse(nrow(x2) == 0, 0, length(unique(c(
      x$species, x2$species
    ))) - length(unique(x$species)))
    tabspecies3[1, i] = ifelse(nrow(x) == 0, 0, length(unique(x$species)))
  }
  
  
  pdf(
    file = here::here("2_outputs", "2_figures" , "addition_class_diet_bm.pdf"),
    width = 7,
    height = 5
  )
  
  par(mar = c(5, 5, 2, 1))
  layout(matrix(1:6, ncol = 3, byrow = T), widths = c(1.5, 1, 1.5))
  barplot(
    tab,
    col = nrow(tab):1,
    border = NA,
    legend.text = TRUE,
    args.legend = list(
      x = "topleft",
      inset = c(0, 0),
      border = 0,
      cex = 1,
      bty = "n"
    ),
    las = 2,
    xlab = "log10 body mass",
    ylab = "number of observations",
    main = "feces, guano, frass"
  )
  barplot(
    tab2,
    col = nrow(tab2):1,
    border = NA,
    las = 2,
    ylab = "number of observations",
    main = "feces, guano, frass"
  )
  barplot(
    tab3,
    col = nrow(tab3):1,
    border = NA,
    las = 2,
    ylab = "number of observations",
    main = "feces, guano, frass"
  )
  
  par(mar = c(7, 5, 2, 1))
  barplot(
    tabspecies,
    col = nrow(tabspecies):1,
    border = NA,
    las = 2,
    xlab = "log10 body mass",
    ylab = "number of species"
  )
  barplot(
    tabspecies2,
    col = nrow(tabspecies2):1,
    border = NA,
    las = 2,
    ylab = "number of species"
  )
  barplot(
    tabspecies3,
    col = nrow(tabspecies3):1,
    border = NA,
    las = 2,
    ylab = "number of species"
  )
  
  dev.off()
  
  ##### A phylogenetic tree with diet, bodymasses distribution and sample type #####
  taxize_classes = readRDS(file = here::here("1_data", "4_data_taxonomy", "taxize_classes.RData"))
  
  species_traits_taxonomy = unique(data[, c("species", "class", "diet", "body_mass")])
  
  
  classes_tree = class2tree(taxize_classes, check = F)
  
  classes_phylo = classes_tree$phylo
  
  # Group the data by class and species, and calculate the count for each combination
  grouped_data_nb_species <- species_traits_taxonomy %>%
    group_by(class, species) %>%
    dplyr::tally(name = "count")
  
  # Compute the total count of species for each class
  class_totals_nb_species <- grouped_data_nb_species %>%
    group_by(class) %>%
    dplyr::summarise(total = sum(count)) %>%
    na.omit()
  
  tree_classes = c(
    "Insecta",
    "Diplopoda",
    "Arachnida",
    "Malacostraca",
    "Collembola",
    "Aves",
    "Mammalia",
    "Squamata",
    "Amphibia",
    "Testudines",
    "Gastropoda"
  )
  class_totals_nb_species$class =
    factor(class_totals_nb_species$class, levels = tree_classes)
  
  matching_indices = match(tree_classes, class_totals_nb_species$class)
  class_totals_nb_species = class_totals_nb_species[matching_indices, ]
  
  
  # Group the data by class and diet, and calculate the count for each combination
  grouped_data <- species_traits_taxonomy %>%
    group_by(class, diet) %>%
    dplyr::tally(name = "count")
  
  # Compute the total count for each class
  class_totals <- grouped_data %>%
    group_by(class) %>%
    dplyr::summarise(total = sum(count))
  
  # Calculate the proportion of each diet within each class
  proportions <- grouped_data %>%
    left_join(class_totals, by = "class") %>%
    mutate(proportion = count / total) %>%
    dplyr::select(-count, -total)
  
  # Convert the data to long format
  long_data_diet <- proportions %>%
    pivot_longer(
      cols = c(proportion),
      names_to = "variable",
      values_to = "value"
    )
  
  # Group the data by class and component_data_type in the wastes data and calculate the count for each combination
  grouped_data_component_data_type <- data %>%
    filter(
      sample_type == "feces" |
        sample_type == "urine" |
        sample_type == "guano" | sample_type == "frass"
    ) %>%
    group_by(class, component_data_type) %>%
    dplyr::tally(name = "count")
  
  # Compute the total count for each class
  class_totals_component_data_type <-
    grouped_data_component_data_type %>%
    group_by(class) %>%
    dplyr::summarise(total = sum(count))
  
  class_totals_component_data_type$class = factor(class_totals_component_data_type$class, levels = tree_classes)
  matching_indices = match(tree_classes, class_totals_component_data_type$class)
  class_totals_component_data_type = class_totals_component_data_type[matching_indices, ]
  
  # Calculate the proportion of each component_data_type within each class
  proportions <- grouped_data_component_data_type %>%
    left_join(class_totals_component_data_type, by = "class") %>%
    mutate(proportion = count / total) %>%
    dplyr::select(-count, -total)
  
  # Convert the data to long format
  long_data_component_data_type <- proportions %>%
    pivot_longer(
      cols = c(proportion),
      names_to = "variable",
      values_to = "value"
    )
  
  
  tree_bm_diet_wdt = ggtree(classes_phylo, branch.length = "none") +
    ggtree::geom_tiplab() +
    geom_fruit(
      data = species_traits_taxonomy,
      geom = geom_boxplot,
      mapping = aes(y = class, x = log10(body_mass)),
      axis.params = list(
        axis = "x",
        title = "Body mass \n (log g)",
        title.height = 0.2,
        text.size = 1.5
      ),
      grid.params = list(),
      offset = 2,
      pwidth = 1
    ) +
    geom_fruit(
      data = long_data_diet,
      geom = geom_bar,
      mapping = aes(y = class, x = value, fill = diet),
      stat = "identity",
      axis.params = list(
        axis = "x",
        title = "Diet",
        title.height = 0.2,
        line.color = "white",
        text.size = 1.5
      ),
      offset = 0.1,
      pwidth = 1
    ) +
    scale_fill_manual(
      name = "Diet",
      breaks = c(
        "Herbivore",
        "Carnivore",
        "Omnivore",
        "Frugivore",
        "Insectivore",
        "Detritivore"
      ),
      labels = c(
        "Herbivore",
        "Carnivore",
        "Omnivore",
        "Frugivore",
        "Insectivore",
        "Detritivore"
      ),
      guide = guide_legend(keywidth = 0.6, keyheight = 1, ),
      values = colours_diet
    ) +
    new_scale_fill() +
    geom_fruit(
      data = long_data_component_data_type,
      geom = geom_bar,
      mapping = aes(y = class, x = value, fill = component_data_type),
      stat = "identity",
      axis.params = list(
        axis = "x",
        title = "Waste \n data type",
        title.height = 0.2,
        line.color = "white",
        text.size = 1.5
      ),
      offset = 0.1,
      pwidth = 1
    ) +
    scale_fill_manual(
      name = "Waste data type",
      breaks = c("stock", "flux", "rate"),
      labels = c("Stock", "Flux", "Rate"),
      na.translate = T,
      guide = guide_legend(keywidth = 0.6, keyheight = 1),
      values = c(
        "stock" = "lightgoldenrod2",
        "flux" = "darkorange",
        "rate" = "red4"
      )
    ) +
    geom_fruit(
      data = class_totals_nb_species,
      geom = geom_text,
      label = na.omit(class_totals_nb_species)$total,
      mapping = aes(y = class, x = ""),
      axis.params = list(
        axis = "x",
        title = "Nb \n species",
        title.height = 0.2,
        line.color = "white",
        text.size = 0.5
      ),
      offset = 0.3,
      pwidth = 0.2
    ) +
    geom_fruit(
      data = class_totals_component_data_type,
      geom = geom_text,
      label = na.omit(class_totals_component_data_type)$total,
      mapping = aes(y = class, x = ""),
      axis.params = list(
        axis = "x",
        title = "Nb \n obs",
        title.height = 0.2,
        line.color = "white",
      ),
      offset = 0.4,
      pwidth = 0.2
    )
  
  ggsave(
    filename = "tree_bm_diet_wdt.pdf",
    plot = tree_bm_diet_wdt,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  
  
  
  
  ##### Number of observation for sample type according to environment of sampling #####
  
  
  selected_data <- drop_na(dplyr::select(data, environment, diet))
  
  environment_diet_histogram = ggplot(selected_data, aes(environment)) + geom_bar(aes(fill = diet), color = 'black', alpha = 0.7) +
    scale_fill_manual(values = colours_diet) +
    labs(x = "Sampling environment", y = "Number of osbervations", fill = "Diet") +
    theme_bw()
  
  # Save the plot as a PDF
  ggsave(
    filename = "environment_diet_histogram.pdf",
    plot = environment_diet_histogram,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### Number of observation for sample type according to the observation resolution of sampling #####
  
  
  selected_data <- data %>%
    filter(sample_type %in% c("feces", "frass", "guano", "urine"))  %>%
    dplyr::select(observation_resolution, diet) %>%
    drop_na() %>%
    filter(
      observation_resolution %in% c(
        "individual",
        "population",
        "intra_individual",
        "inter_individual",
        "intra_population",
        "inter_population"
      )
    )  # Replace with the desired levels
  
  desired_order <- c(
    "individual",
    "population",
    "intra_individual",
    "inter_individual",
    "intra_population",
    "inter_population"
  )
  
  
  wastes_observation_resolution_diet = ggplot(selected_data, aes(x = factor(observation_resolution, levels = desired_order))) +
    geom_bar(aes(fill = diet), color = 'black', alpha = 0.7) +
    scale_fill_manual(values = colours_diet) +
    scale_x_discrete(
      labels = c(
        "Individual",
        "Population",
        "Intra-individual",
        "Inter-individual",
        "Intra-population",
        "Inter-population"
      )
    ) +
    labs(x = "Wastes observation resolution", y = "Number of osbervations", fill = "Diet") +
    theme_bw() +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1
    ))
  
  # Save the plot as a PDF
  ggsave(
    filename = "wastes_observation_resolution_diet.pdf",
    plot = wastes_observation_resolution_diet,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### Number of observation for sample type according to matrix #####
  data_p = data[!is.na(data$sample_type), ]
  df_split <- split(data_p, data_p$component_data_type)
  
  plots_matrix_diet_unit_list <-
    lapply(df_split, function(df) {
      # Sort sample_type levels based on the number of observations
      sorted_levels <- df %>%
        dplyr::count(sample_type) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::pull(sample_type)
      
      # Reorder the sample_type factor based on the sorted levels
      df$sample_type <-
        factor(df$sample_type, levels = sorted_levels)
      
      
      
      ggplot(df, aes(x = sample_type, fill = diet)) +
        geom_bar(color = 'black', alpha = 0.7) +
        labs(x = "Sample Type", y = "Number of observations") +
        scale_fill_manual(values = colours_diet) +
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
  
  ##### 2. C, N, P stock plots #####
  
  # Mammals CNP diet bodymass plots ####
  
  # For each element, and each ratio, we do plots versus body mass and diet
  # As well as a "chemical plan" plot
  
  el_ra = c("C", "N", "P", "C/N", "C/P", "N/P")
  y_name = c(
    "log<sub>10</sub> C",
    "log<sub>10</sub> N",
    "log<sub>10</sub> P",
    "log<sub>10</sub> C/N",
    "log<sub>10</sub> C/P",
    "log<sub>10</sub> N/P"
  )
  filenames = c("C", "N", "P", "CN", "CP", "NP")
  nb_elements = length(el_ra)
  units = c("", "", "", "", "", "")
  variables = c("body_mass", "diet")
  nb_variables = length(variables)
  
  plots_bm_diet = vector("list", nb_elements)
  names(plots_bm_diet) = el_ra
  plots_diet = vector("list", nb_elements)
  names(plots_diet) = el_ra
  plots_bm = vector("list", nb_elements)
  names(plots_bm) = el_ra
  
  data_mammals_species = read.csv(here::here("1_data", "data_mammals_species.csv"))
  
  data_mammals_species$diet = factor(
    data_mammals_species$diet,
    levels = c('Herbivore', 'Omnivore', 'Carnivore', 'Detritivore')
  )
  
  diet_comparisons = list(
    c("Herbivore", "Omnivore"),
    c("Herbivore", "Carnivore"),
    c("Omnivore", "Carnivore")
  )
  
  
  for (i in 1:length(el_ra)) {
    data_element = subset(data_mammals_species, data_mammals_species$component_name == el_ra[i])
    
    # Filter out diets with less than 3 rows
    data_element = data_element %>%
      group_by(diet) %>%
      filter(!is.na(avg_component_mean)) %>%
      filter(n() >= 3) %>%
      ungroup() %>%
      droplevels()
    
    ylim_max = max(data_element$avg_component_mean, na.rm = T) + 0.5 * (
      max(data_element$avg_component_mean, na.rm = T) - min(data_element$avg_component_mean, na.rm = T)
    )
    
    # The body mass plot
    
    plots_bm[[i]] = ggplot2::ggplot(data_element , aes(x = log10(body_mass), y = avg_component_mean, )) +
      geom_point(aes(col = diet), shape = 16, alpha = 0.7) +
      labs(x = "Body mass <br> (log<sub>10</sub> g)" ,
           y = paste("Faeces", y_name[i], units[i], sep = " ")) +
      theme(axis.title.x = element_markdown()) +
      theme(axis.title.y = element_markdown()) +
      scale_color_manual(
        name = 'Diet',
        values = colours_diet,
        breaks = c('Herbivore', 'Omnivore', 'Carnivore', 'Detritivore')
      ) +
      theme(legend.position = "right") +
      geom_smooth(
        method = "lm",
        se = FALSE,
        fullrange = TRUE,
        col = "black"
      ) +
      ylim(NA, ylim_max) +
      stat_cor(
        aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
        size = 2.2,
        label.x.npc = 0,
        label.y.npc = 0.99,
        geom = "text"
      )
    # Save each plot
    ggsave(
      filename = paste(filenames[i], "_&_bm_mammals.pdf", sep = ""),
      plot = plots_bm[[i]],
      device = cairo_pdf,
      path = here::here("2_outputs", "2_figures"),
      scale = 1,
      width = 7,
      height = 4,
      units = "in"
    )
    
    
    # The body mass diet plot
    
    
    plots_bm_diet[[i]] = ggplot2::ggplot(data_element ,
                                         aes(
                                           x = log10(body_mass),
                                           y = avg_component_mean,
                                           col = diet,
                                           group = diet,
                                         )) +
      geom_point(shape = 16, alpha = 0.7) +
      labs(x = "Body mass <br> (log<sub>10</sub> g)" ,
           y = paste("Faeces", y_name[i], units[i], sep = " ")) +
      theme(axis.title.x = element_markdown()) +
      theme(axis.title.y = element_markdown()) +
      scale_color_manual(
        name = 'Diet',
        values = colours_diet,
        breaks = c('Herbivore', 'Omnivore', 'Carnivore', 'Detritivore')
      ) +
      theme(legend.position = "right")
    
    
    # Calculate correlations and p-values for each diet group
    lm_pvals = data_element %>%
      dplyr::summarise(.by = diet, p_value = summary(lm(
        avg_component_mean ~ log10(body_mass)
      ))$coefficients[2, 4])
    
    # Filter significant diet groups
    significant_diets = lm_pvals$diet[lm_pvals$p_value < 0.05]
    
    # Add geom_smooth() only for significant diet groups
    if (length(significant_diets) > 0) {
      for (j in 1:length(significant_diets)) {
        plots_bm_diet[[i]] = plots_bm_diet[[i]] + geom_smooth(
          data = subset(data_element, diet == significant_diets[j]),
          method = "lm",
          se = FALSE,
          fullrange = TRUE
        )
      }
    }
    
    # Add the statistical correlation for significant diet groups
    if (length(significant_diets) > 0) {
      plots_bm_diet[[i]] = plots_bm_diet[[i]] +
        ylim(NA, ylim_max) +
        stat_cor(
          data = filter(data_element, diet %in% significant_diets),
          aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
          size = 2.2,
          label.x.npc = 0,
          label.y.npc = 0.99,
          geom = "text"
        )
    }
    # Save each plot
    ggsave(
      filename = paste(filenames[i], "_&_bm_&_diet_mammals_dg.pdf", sep = ""),
      plot = plots_bm_diet[[i]],
      device = cairo_pdf,
      path = here::here("2_outputs", "2_figures"),
      scale = 1,
      width = 7,
      height = 4,
      units = "in"
    )
    
    if (length(levels(data_element$diet)) > 1) {
      wilcox_test = data_element %>%
        wilcox_test(avg_component_mean ~ diet, comparisons = diet_comparisons) %>%
        adjust_pvalue(method = "holm") %>%
        add_significance("p.adj") %>%
        add_xy_position(x = "diet", step.increase = 0.2)
    }
    
    
    # The diet plot
    
    plots_diet[[i]] = ggplot2::ggplot(data_element,
                                      aes(
                                        x = diet,
                                        y = avg_component_mean,
                                        col = as.factor(diet)
                                      )) +
      geom_jitter(
        color = "black",
        width = 0.2,
        height = 0.2,
        size = 0.4,
        alpha = 0.9,
        shape = 16
      ) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      stat_pvalue_manual(
        wilcox_test,
        label = "p.adj.signif",
        tip.length = 0,
        hide.ns = T
      ) +
      labs(x = "Diet",
           y = paste("Faeces", y_name[i], units[i], sep = " ")) +
      theme(axis.title.y = element_markdown()) +
      scale_color_manual(
        name = 'Diet',
        values = colours_diet,
        breaks = c('Herbivore', 'Omnivore', 'Carnivore', 'Detritivore')
      ) +
      theme(
        legend.position = "right",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    if (length(levels(data_element$diet)) >= 3) {
      plots_diet[[i]] = plots_diet[[i]] +
        stat_compare_means(
          method = "kruskal.test",
          label = "p.signif",
          col = "grey70",
          label.x.npc = 'center',
          label.y.npc = 'top',
          size = 7,
          hide.ns = T
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.3)))
    }
    # Save each plot
    ggsave(
      filename = paste(filenames[i], "_&_diet_mammals.pdf", sep = ""),
      plot = plots_diet[[i]],
      device = cairo_pdf,
      path = here::here("2_outputs", "2_figures"),
      scale = 1,
      width = 7,
      height = 4,
      units = "in"
    )
    
    
  }
  
  cnp_diet_mammals = ggpubr::ggarrange(
    plots_diet[[1]],
    plots_diet[[2]],
    plots_diet[[3]],
    NULL,
    NULL,
    NULL,
    plots_diet[[4]],
    plots_diet[[5]],
    plots_diet[[6]],
    ncol = 3,
    nrow = 3,
    labels = c("a.", "b.", "c.", "", "", "", "d.", "e.", "f."),
    label.y = 1.16,
    label.x = 0,
    heights = c(1, 0.05, 1),
    widths = c(1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  cnp_diet_mammals = ggpubr::annotate_figure(cnp_diet_mammals, bottom = "", top = "")
  
  ggsave(
    filename = "cnp_diet_mammals.pdf",
    plot = cnp_diet_mammals,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  cnp_bm_mammals = ggpubr::ggarrange(
    plots_bm[[1]],
    plots_bm[[2]],
    plots_bm[[3]],
    NULL,
    NULL,
    NULL,
    plots_bm[[4]],
    plots_bm[[5]],
    plots_bm[[6]],
    ncol = 3,
    nrow = 3,
    labels = c("a.", "b.", "c.", "", "", "", "d.", "e.", "f."),
    label.y = 1.16,
    label.x = 0,
    heights = c(1, 0.05, 1),
    widths = c(1, 1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  cnp_bm_mammals = ggpubr::annotate_figure(cnp_bm_mammals, bottom = "", top = "")
  
  ggsave(
    filename = "cnp_bm_mammals.pdf",
    plot = cnp_bm_mammals,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  cnp_bm_diet_mammals = ggpubr::ggarrange(
    plots_bm_diet[[1]],
    plots_bm_diet[[2]],
    plots_bm_diet[[3]],
    NULL,
    NULL,
    NULL,
    plots_bm_diet[[4]],
    plots_bm_diet[[5]],
    plots_bm_diet[[6]],
    ncol = 3,
    nrow = 3,
    labels = c("a.", "b.", "c.", "", "", "", "d.", "e.", "f."),
    label.y = 1.16,
    label.x = 0,
    heights = c(1, 0.05, 1),
    widths = c(1, 1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  cnp_bm_diet_mammals = ggpubr::annotate_figure(cnp_bm_diet_mammals, bottom = "", top = "")
  
  ggsave(
    filename = "cnp_bm_diet_mammals.pdf",
    plot = cnp_bm_diet_mammals,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  # We do %C, N% and %P biplots with averages and sd per diet group
  
  # Mammals CNP biplots ####
  
  
  cnp_plan_data = pivot_wider(data_mammals_species,
                              names_from = "component_name",
                              values_from = "avg_component_mean")
  cnp_plan_data_summary = ddply(
    cnp_plan_data,
    .(diet),
    summarise,
    C_mean = mean(C, na.rm = T),
    N_mean = mean(N, na.rm = T),
    P_mean = mean(P, na.rm = T),
    CSD = stats::sd(C, na.rm = T),
    NSD = stats::sd(N, na.rm = T),
    PSD = stats::sd(P, na.rm = T)
  )
  names(cnp_plan_data_summary)[2:4] = c("C", "N", "P")
  
  cn_plan_mammals = ggplot2::ggplot(cnp_plan_data, aes(x = N, y = C, col = as.factor(diet))) +
    geom_point(data = cnp_plan_data_summary, aes(colour = diet), shape = 16) +
    geom_errorbarh(data = cnp_plan_data_summary,
                   aes(
                     xmin = N - NSD,
                     xmax = N + NSD,
                     y = C,
                     colour = diet,
                     height = 0
                   )) +
    geom_errorbar(data = cnp_plan_data_summary, aes(
      ymin = C - CSD,
      ymax = C + CSD,
      x = N,
      colour = diet,
      width = 0
    )) +
    labs(
      x = paste("Faeces log<sub>10</sub> N"),
      y = paste("Faeces log<sub>10</sub> C")
    ) +
    scale_color_manual(name = 'Diet', values = colours_diet)
  
  cp_plan_mammals = ggplot2::ggplot(cnp_plan_data, aes(x = P, y = C, col = as.factor(diet))) +
    geom_point(data = cnp_plan_data_summary, aes(colour = diet), shape = 16) +
    geom_errorbarh(data = cnp_plan_data_summary,
                   aes(
                     xmin = P - PSD,
                     xmax = P + PSD,
                     y = C,
                     colour = diet,
                     height = 0
                   )) +
    geom_errorbar(data = cnp_plan_data_summary, aes(
      ymin = C - CSD,
      ymax = C + CSD,
      x = P,
      colour = diet,
      width = 0
    )) +
    labs(
      x = paste("Faeces log<sub>10</sub> P"),
      y = paste("Faeces log<sub>10</sub> C")
    ) +
    scale_color_manual(name = 'Diet', values = colours_diet)
  
  np_plan_mammals = ggplot2::ggplot(cnp_plan_data, aes(x = P, y = N, col = as.factor(diet))) +
    geom_point(data = cnp_plan_data_summary, aes(colour = diet), shape = 16) +
    geom_errorbarh(data = cnp_plan_data_summary,
                   aes(
                     xmin = P - PSD,
                     xmax = P + PSD,
                     y = N,
                     colour = diet,
                     height = 0
                   )) +
    geom_errorbar(data = cnp_plan_data_summary, aes(
      ymin = N - NSD,
      ymax = N + NSD,
      x = P,
      colour = diet,
      width = 0
    )) +
    labs(
      x = paste("Faeces log<sub>10</sub> P"),
      y = paste("Faeces log<sub>10</sub> N")
    ) +
    scale_color_manual(name = 'Diet', values = colours_diet)
  
  complete_cnp_plans = ggpubr::ggarrange(
    cn_plan_mammals,
    cp_plan_mammals,
    np_plan_mammals,
    ncol = 3,
    nrow = 1,
    labels = c("a.", "b.", "c."),
    label.y = 1,
    label.x = 0,
    heights = c(1),
    widths = c(1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  complete_cnp_plans = ggpubr::annotate_figure(complete_cnp_plans, bottom = "", top = "")
  
  ggsave(
    filename = "cnp_biplots_mammals.pdf",
    plot = complete_cnp_plans,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 3,
    units = "in"
  )
  
}
