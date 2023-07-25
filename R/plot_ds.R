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
  ##### 0. Structuring data  #####
  
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
  data$diet = str_to_title(data$diet)
  
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
    "Nectarivore" = "#FA6A02",
    "Detritivore" = "#9E5748"
  ) # The colors used for diet
  
  # Nitrogen ####
  
  lucas_nitrogen = ggplot2::ggplot(data_fluxes ,
                                   aes(x = CP_Crude_Protein_diet,
                                       y = CP_ad)) +
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
      fill = '#BDDCEC',
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
    labs(x = "Diet N (%DM)",
         y = "N AE (%)") +
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
  
  lucas_sodium = ggplot2::ggplot(data_fluxes ,
                                 aes(x = Na_diet,
                                     y = Na_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$Na_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#BDDCEC',
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
    labs(x = "Diet Na (%DM)",
         y = "Na AE (%)") +
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
  
  lucas_magnesium = ggplot2::ggplot(data_fluxes ,
                                    aes(x = Mg_diet,
                                        y = Mg_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$Mg_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#BDDCEC',
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
    labs(x = "Diet Mg (%DM)",
         y = "Mg AE (%)") +
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
  
  lucas_phosphorus = ggplot2::ggplot(data_fluxes ,
                                     aes(x = P_diet,
                                         y = P_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$P_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#BDDCEC',
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
    labs(x = "Diet P (%DM)",
         y = "P AE (%)") +
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
  
  lucas_potassium = ggplot2::ggplot(data_fluxes ,
                                   aes(x = K_diet,
                                       y = K_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$K_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#BDDCEC',
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
    labs(x = "Diet K (%DM)",
         y = "K AE (%)") +
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
  
  lucas_calcium = ggplot2::ggplot(data_fluxes ,
                                  aes(x = Ca_diet,
                                      y = Ca_ad)) +
    coord_cartesian(xlim = c(0, max(data_fluxes$Ca_diet, na.rm = T))) +
    geom_rect(
      aes(
        xmin = -Inf,
        xmax = +Inf,
        ymin = -Inf,
        ymax = 0
      ),
      fill = '#BDDCEC',
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
    labs(x = "Diet Ca (%DM)",
         y = "Ca AE (%)") +
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
    lucas_nitrogen,
    lucas_phosphorus,
    lucas_potassium,
    NULL,
    NULL,
    NULL,
    lucas_calcium,
    lucas_magnesium,
    lucas_sodium,
    ncol = 3,
    nrow = 3,
    labels = c("a.",
               "b.",
               "c.",
               "",
               "",
               "",
               "d.",
               "e.",
               "f."),
    label.y = 1.16,
    label.x = 0,
    heights = c(1, 0.05, 1),
    widths = c(1, 1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  complete_lucas_plot = ggpubr::annotate_figure(complete_lucas_plot,
                                                bottom = "",
                                                top = "")
  
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
  
  # Apparent digestibility according to species ####
  
  # Nitrogen ####
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , CP_ad, median , na.rm =
                                                        T))
  
  species_nad = ggplot(data_fluxes[!is.na(data_fluxes$CP_ad), ], aes(x =
                                                                       Species_lat, y = CP_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "N AE (%)",
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
  
  # Sodium ####
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Na_ad, median , na.rm =
                                                        T))
  
  species_naad = ggplot(data_fluxes[!is.na(data_fluxes$Na_ad), ], aes(x =
                                                                        Species_lat, y = Na_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Na AE (%)",
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
  
  # Magnesium ####
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Mg_ad, median , na.rm =
                                                        T))
  
  species_mgad = ggplot(data_fluxes[!is.na(data_fluxes$Mg_ad), ], aes(x =
                                                                        Species_lat, y = Mg_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Mg AE (%)",
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
  
  # Phosphorus ####
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , P_ad, median , na.rm =
                                                        T))
  
  species_pad = ggplot(data_fluxes[!is.na(data_fluxes$P_ad), ], aes(x = Species_lat, y =
                                                                      P_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "P AE (%)",
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
  
  # Potassium ####
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , K_ad, median , na.rm =
                                                        T))
  
  species_kad = ggplot(data_fluxes[!is.na(data_fluxes$K_ad), ], aes(x = Species_lat, y =
                                                                      K_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "K AE (%)",
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
  
  # Calcium ####
  
  
  data_fluxes$Species_lat = with(data_fluxes, reorder(Species_lat , Ca_ad, median , na.rm =
                                                        T))
  
  species_caad = ggplot(data_fluxes[!is.na(data_fluxes$Ca_ad), ], aes(x =
                                                                        Species_lat, y = Ca_ad)) +
    geom_boxplot() +
    coord_flip() +
    labs(y = "Ca AE (%)",
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
  
  ##### 1. Preliminary figures #####
  
  
  ##### Number of reference per year #####
  
  references_and_years  = unique(data[, c("reference_ID", "reference_year")])
  study_counts = table(references_and_years$reference_year)
  study_counts_df = data.frame(year = as.numeric(names(study_counts)), count = as.vector(study_counts))
  
  number_studies_f_time = ggplot(study_counts_df, aes(x = year, y = count)) +
    geom_area(fill = "#3366CC",
              color = "black",
              size = 1.5) +
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
  
  
  # Step 1: Filter the data for "C", "N", and "P" components
  filtered_data <- data %>%
    filter(component_name %in% c("C", "N", "P"))
  
  # Step 2: Group the filtered data by "component_name" and "component_measure_method" and calculate the count
  grouped_data <- filtered_data %>%
    group_by(component_name, component_measure_method) %>%
    dplyr::summarise(count = n())
  
  # Step 3: Create pie charts for each "component_name" and save as PDF
  for (name in unique(grouped_data$component_name)) {
    subset_data <- grouped_data %>% filter(component_name == name)
    
    # Step 4: Create a pie chart with measure method labels
    p <-
      ggplot(subset_data,
             aes(x = "", y = count, fill = component_measure_method)) +
      labs(title = paste("Proportion of measure methods for", name)) +
      geom_bar(
        stat = "identity",
        width = 1,
        color = "black",
        alpha = 0.7
      ) +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Set3") +
      theme_void()
    
    # Save the plot as a PDF with a specific name
    file_name <-
      paste(name, "measure_method_pie_chart.pdf", sep = "_")
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
    select(data, species_latin_name_gbif, body_mass, diet)
  
  grouped_data <- unique(selected_data)
  
  bodymass_diet_histogram = ggplot(grouped_data, aes(log(body_mass))) + geom_histogram(
    aes(fill = diet),
    bins = 50,
    color = 'black',
    alpha = 0.7
  ) +
    scale_fill_manual(values = colours_diet) +
    labs(x = "Body mass <br> (log<sub>10</sub> g)", y = "Number of species", fill = "Diet") +
    theme_bw()
  
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
  
  
  ##### A phylogenetic with diet, bodymasses distribution and sample type #####
  taxize_classes = readRDS(file = here::here("1_data",
                                             "4_data_taxonomy",
                                             "taxize_classes.RData"))
  
  species_traits_taxonomy = unique(data[, c("species", "class", "diet", "body_mass")])
  
  
  classes_tree = class2tree(taxize_classes, check = F)
  
  classes_phylo = classes_tree$phylo
  
  # Group the data by class and diet, and calculate the count for each combination
  grouped_data <- species_traits_taxonomy %>%
    group_by(class, diet) %>%
    dplyr::summarise(count = n())
  
  # Compute the total count for each class
  class_totals <- grouped_data %>%
    group_by(class) %>%
    dplyr::summarise(total = sum(count))
  
  # Calculate the proportion of each diet within each class
  proportions <- grouped_data %>%
    left_join(class_totals, by = "class") %>%
    mutate(proportion = count / total) %>%
    select(-count, -total)
  
  # Convert the data to long format
  long_data_diet <- proportions %>%
    pivot_longer(
      cols = c(proportion),
      names_to = "variable",
      values_to = "value"
    )
  
  # Group the data by class and component_data_type in the wastes data and calculate the count for each combination
  grouped_data <- data %>%
    filter(
      sample_type == "feces" |
        sample_type == "urine" |
        sample_type == "guano" | sample_type == "frass"
    ) %>%
    group_by(class, component_data_type) %>%
    dplyr::summarise(count = n())
  
  # Compute the total count for each class
  class_totals <- grouped_data %>%
    group_by(class) %>%
    dplyr::summarise(total = sum(count))
  
  # Calculate the proportion of each component_data_type within each class
  proportions <- grouped_data %>%
    left_join(class_totals, by = "class") %>%
    mutate(proportion = count / total) %>%
    select(-count, -total)
  
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
      mapping = aes(y = class, x = log(body_mass)),
      axis.params = list(
        axis = "x",
        title = "Body mass <br> (log<sub>10</sub> g)",
        title.height = 0.2,
        text.size = 0.8
      ),
      grid.params = list(),
      offset = 1,
      pwidth = 0.4
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
      ),
      offset = 0.2,
      pwidth = 0.4
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
      guide = guide_legend(keywidth = 0.6,
                           keyheight = 1,),
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
        title = "Waste data type",
        title.height = 0.2,
        line.color = "white",
      ),
      offset = 0.2,
      pwidth = 0.4
    ) +
    scale_fill_manual(
      name = "Waste data type",
      breaks = c("stock", "flux", "rate"),
      labels = c("Stock", "Flux", "Rate"),
      na.translate = T,
      guide = guide_legend(keywidth = 0.6,
                           keyheight = 1),
      values = c(
        "stock" = "lightgoldenrod2",
        "flux" = "darkorange",
        "rate" = "red4"
      )
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
  
  
  selected_data <- drop_na(select(data, environment, diet))
  
  environment_diet_histogram = ggplot(selected_data, aes(environment)) + geom_bar(aes(fill = diet),
                                                                                  color = 'black',
                                                                                  alpha = 0.7) +
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
    select(observation_resolution, diet) %>%
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
    theme_bw()
  
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
  
  df_split <- split(data, data$component_data_type)
  
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
        labs(x = "Sample Type", y = "Number of Observations") +
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
  
  
  # We select only dry weight data, as fresh weight are more rare and not comparable to dry weight data
  
  data <- data |>
    filter(component_weight_type != "fw")
  stock_data <- data |>
    filter(component_data_type == "stock")
  
  ##### a. No cloaca faeces plots #####
  
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
    select(species_latin_name_gbif,
           component_name,
           component_mean,
           body_mass,
           diet)
  
  # Average over species
  
  a_cnp_fsd <- cnp_fsd |>
    group_by(species_latin_name_gbif, component_name) |>
    dplyr::summarise(
      avg_component_mean = mean(component_mean),
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
      component_name = "C/N",
      avg_component_mean = ifelse(
        any(crow) &&
          any(nrow),
        a_cnp_fsd$avg_component_mean[crow] / a_cnp_fsd$avg_component_mean[nrow],
        NA
      ),
      body_mass = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "diet"]
    )
    cp_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/P",
      avg_component_mean = ifelse(
        any(crow) &&
          any(prow),
        a_cnp_fsd$avg_component_mean[crow] / a_cnp_fsd$avg_component_mean[prow],
        NA
      ),
      body_mass = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "diet"]
    )
    np_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "N/P",
      avg_component_mean = ifelse(
        any(nrow) &&
          any(prow),
        a_cnp_fsd$avg_component_mean[nrow] / a_cnp_fsd$avg_component_mean[prow],
        NA
      ),
      body_mass = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_fsd[which(a_cnp_fsd$species_latin_name_gbif == i)[1], "diet"]
    )
    
    # Add Row using rbind()
    a_cnp_fsd = rbind(a_cnp_fsd, cn_row, cp_row, np_row)
  }
  
  # For each element, and each ratio, we do plots versus body mass and diet
  
  el_ra = c("C", "N", "P", "C/N", "C/P", "N/P")
  filenames = c("C", "N", "P", "CN", "CP", "NP")
  nb_elements = length(el_ra)
  units = c("%", "%", "%", "", "", "")
  variables = c("body_mass", "diet")
  nb_variables = length(variables)
  
  plots_bm = vector("list", nb_elements)
  names(plots_bm) = el_ra
  plots_diet = vector("list", nb_elements)
  names(plots_diet) = el_ra
  
  
  for (i in 1:length(el_ra)) {
    data_element = subset(a_cnp_fsd, a_cnp_fsd$component_name == el_ra[i])
    ylim_max = max(data_element$avg_component_mean, na.rm = T) + 0.2 * (
      max(data_element$avg_component_mean, na.rm = T) - min(data_element$avg_component_mean, na.rm = T)
    )
    
    # C, N, P and ratio in faeces versus body mass ####
    
    
    plots_bm[[i]] = ggplot2::ggplot(data_element ,
                                    aes(
                                      x = log10(body_mass),
                                      y = avg_component_mean,
                                      col = as.factor(diet)
                                    )) +
      ylim(NA, ylim_max) +
      geom_smooth(method = "lm", color = "black") +
      geom_point() +
      labs(x = "Body mass <br> (log<sub>10</sub> g)" ,
           y = paste(el_ra[i], units[i], "in faeces", sep = " ")) +
      theme(axis.title.x = element_markdown()) +
      scale_color_manual(name = 'Diet',
                         values = colours_diet) +
      theme(legend.position = "right")
    
    
    # Save each plot
    ggsave(
      filename = paste(filenames[i], "_&_bm_no_cloaca.pdf", sep = ""),
      plot = plots_bm[[i]],
      device = cairo_pdf,
      path = here::here("2_outputs", "2_figures"),
      scale = 1,
      width = 7,
      height = 4,
      units = "in"
    )
    
    # C, N, P and ratio in faeces versus diet ####
    
    
    plots_diet[[i]] = ggplot2::ggplot(data_element ,
                                      aes(
                                        x = diet,
                                        y = avg_component_mean,
                                        col = as.factor(diet)
                                      )) +
      ylim(NA, ylim_max) +
      geom_smooth(method = "lm", color = "black") +
      geom_boxplot() +
      labs(x = "Diet",
           y = paste(el_ra[i], units[i], "in faeces", sep = " ")) +
      scale_color_manual(name = 'Diet',
                         values = colours_diet) +
      theme(
        legend.position = "right",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    
    # Save each plot
    ggsave(
      filename = paste(filenames[i], "_&_diet_no_cloaca.pdf", sep = ""),
      plot = plots_diet[[i]],
      device = cairo_pdf,
      path = here::here("2_outputs", "2_figures"),
      scale = 1,
      width = 7,
      height = 4,
      units = "in"
    )
  }
  
  # Complete CNP plots ####
  
  complete_cnp_plot = ggpubr::ggarrange(
    plots_bm[[1]],
    plots_bm[[2]],
    plots_bm[[3]],
    NULL,
    NULL,
    NULL,
    plots_diet[[1]],
    plots_diet[[2]],
    plots_diet[[3]],
    ncol = 3,
    nrow = 3,
    labels = c("a.",
               "b.",
               "c.",
               "",
               "",
               "",
               "d.",
               "e.",
               "f."),
    label.y = 1.16,
    label.x = 0,
    heights = c(1, 0.05, 1),
    widths = c(1, 1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  complete_cnp_plot = ggpubr::annotate_figure(complete_cnp_plot,
                                              bottom = "",
                                              top = "")
  
  ggsave(
    filename = "cnp_no_cloaca.pdf",
    plot = complete_cnp_plot,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  complete_ratios_plot = ggpubr::ggarrange(
    plots_bm[[4]],
    plots_bm[[5]],
    plots_bm[[6]],
    NULL,
    NULL,
    NULL,
    plots_diet[[4]],
    plots_diet[[5]],
    plots_diet[[6]],
    ncol = 3,
    nrow = 3,
    labels = c("a.",
               "b.",
               "c.",
               "",
               "",
               "",
               "d.",
               "e.",
               "f."),
    label.y = 1.16,
    label.x = 0,
    heights = c(1, 0.05, 1),
    widths = c(1, 1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  complete_ratios_plot = ggpubr::annotate_figure(complete_ratios_plot,
                                                 bottom = "",
                                                 top = "")
  
  ggsave(
    filename = "ratios_no_cloaca.pdf",
    plot = complete_ratios_plot,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  ##### b. Cloaca frass and guano plots #####
  
  guano_stock_data <- stock_data |>
    filter(cloaca == 1)
  guano_stock_data <- guano_stock_data |>
    filter(sample_type == "frass" | sample_type == "guano")
  # Selecting CNP in guano stock data
  cnp_gsd <- guano_stock_data |>
    filter(component_name == "C" |
             component_name == "N" | component_name == "P")
  
  # Keep only relevant rows
  
  cnp_gsd <- cnp_gsd |>
    select(species_latin_name_gbif,
           component_name,
           component_mean,
           body_mass,
           diet)
  
  # Average over species
  
  a_cnp_gsd <- cnp_gsd |>
    group_by(species_latin_name_gbif, component_name) |>
    dplyr::summarise(
      avg_component_mean = mean(component_mean),
      body_mass = first(body_mass),
      diet = first(diet)
    )
  
  # Compute the ratios
  species = unique(a_cnp_gsd$species_latin_name_gbif)
  
  for (i in species) {
    crow = which(a_cnp_gsd$species_latin_name_gbif == i &
                   a_cnp_gsd$component_name == "C")
    nrow = which(a_cnp_gsd$species_latin_name_gbif == i &
                   a_cnp_gsd$component_name == "N")
    prow = which(a_cnp_gsd$species_latin_name_gbif == i &
                   a_cnp_gsd$component_name == "P")
    cn_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/N",
      avg_component_mean = ifelse(
        any(crow) &&
          any(nrow),
        a_cnp_gsd$avg_component_mean[crow] / a_cnp_gsd$avg_component_mean[nrow],
        NA
      ),
      body_mass = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "diet"]
    )
    cp_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "C/P",
      avg_component_mean = ifelse(
        any(crow) &&
          any(prow),
        a_cnp_gsd$avg_component_mean[crow] / a_cnp_gsd$avg_component_mean[prow],
        NA
      ),
      body_mass = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "diet"]
    )
    np_row = data.frame(
      species_latin_name_gbif = i,
      component_name = "N/P",
      avg_component_mean = ifelse(
        any(nrow) &&
          any(prow),
        a_cnp_gsd$avg_component_mean[nrow] / a_cnp_gsd$avg_component_mean[prow],
        NA
      ),
      body_mass = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "body_mass"],
      diet = a_cnp_gsd[which(a_cnp_gsd$species_latin_name_gbif == i)[1], "diet"]
    )
    
    # Add Row using rbind()
    a_cnp_gsd = rbind(a_cnp_gsd, cn_row, cp_row, np_row)
  }
  
  # For each element, and each ratio, we do plots versus body mass and diet
  
  el_ra = c("C", "N", "P", "C/N", "C/P", "N/P")
  filenames = c("C", "N", "P", "CN", "CP", "NP")
  nb_elements = length(el_ra)
  units = c("%", "%", "%", "", "", "")
  variables = c("body_mass", "diet")
  nb_variables = length(variables)
  
  plots_bm = vector("list", nb_elements)
  names(plots_bm) = el_ra
  plots_diet = vector("list", nb_elements)
  names(plots_diet) = el_ra
  
  
  for (i in 1:length(el_ra)) {
    data_element = subset(a_cnp_gsd, a_cnp_gsd$component_name == el_ra[i])
    ylim_max = max(data_element$avg_component_mean, na.rm = T) + 0.2 * (
      max(data_element$avg_component_mean, na.rm = T) - min(data_element$avg_component_mean, na.rm = T)
    )
    
    # C, N, P and ratio in faeces versus body mass ####
    
    
    plots_bm[[i]] = ggplot2::ggplot(data_element ,
                                    aes(
                                      x = log10(body_mass),
                                      y = avg_component_mean,
                                      col = as.factor(diet)
                                    )) +
      ylim(NA, ylim_max) +
      geom_smooth(method = "lm", color = "black") +
      geom_point() +
      labs(x = "Body mass <br> (log<sub>10</sub> g)" ,
           y = paste(el_ra[i], units[i], "in faeces", sep = " ")) +
      theme(axis.title.x = element_markdown()) +
      scale_color_manual(name = 'Diet',
                         values = colours_diet) +
      theme(legend.position = "right")
    
    
    # Save each plot
    ggsave(
      filename = paste(filenames[i], "_&_bm_cloaca.pdf", sep = ""),
      plot = plots_bm[[i]],
      device = cairo_pdf,
      path = here::here("2_outputs", "2_figures"),
      scale = 1,
      width = 7,
      height = 4,
      units = "in"
    )
    
    # C, N, P and ratio in faeces versus diet ####
    
    
    plots_diet[[i]] = ggplot2::ggplot(data_element ,
                                      aes(
                                        x = diet,
                                        y = avg_component_mean,
                                        col = as.factor(diet)
                                      )) +
      ylim(NA, ylim_max) +
      geom_smooth(method = "lm", color = "black") +
      geom_boxplot() +
      labs(x = "Diet",
           y = paste(el_ra[i], units[i], "in wastes", sep = " ")) +
      scale_color_manual(name = 'Diet',
                         values = colours_diet) +
      theme(
        legend.position = "right",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    
    
    # Save each plot
    ggsave(
      filename = paste(filenames[i], "_&_diet_cloaca.pdf", sep = ""),
      plot = plots_diet[[i]],
      device = cairo_pdf,
      path = here::here("2_outputs", "2_figures"),
      scale = 1,
      width = 7,
      height = 4,
      units = "in"
    )
  }
  
  # Complete CNP plots ####
  
  complete_cnp_plot = ggpubr::ggarrange(
    plots_bm[[1]],
    plots_bm[[2]],
    plots_bm[[3]],
    NULL,
    NULL,
    NULL,
    plots_diet[[1]],
    plots_diet[[2]],
    plots_diet[[3]],
    ncol = 3,
    nrow = 3,
    labels = c("a.",
               "b.",
               "c.",
               "",
               "",
               "",
               "d.",
               "e.",
               "f."),
    label.y = 1.16,
    label.x = 0,
    heights = c(1, 0.05, 1),
    widths = c(1, 1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  complete_cnp_plot = ggpubr::annotate_figure(complete_cnp_plot,
                                              bottom = "",
                                              top = "")
  
  ggsave(
    filename = "cnp_cloaca.pdf",
    plot = complete_cnp_plot,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  complete_ratios_plot = ggpubr::ggarrange(
    plots_bm[[4]],
    plots_bm[[5]],
    plots_bm[[6]],
    NULL,
    NULL,
    NULL,
    plots_diet[[4]],
    plots_diet[[5]],
    plots_diet[[6]],
    ncol = 3,
    nrow = 3,
    labels = c("a.",
               "b.",
               "c.",
               "",
               "",
               "",
               "d.",
               "e.",
               "f."),
    label.y = 1.16,
    label.x = 0,
    heights = c(1, 0.05, 1),
    widths = c(1, 1, 1, 1),
    common.legend = TRUE,
    legend = "right"
  )
  
  complete_ratios_plot = ggpubr::annotate_figure(complete_ratios_plot,
                                                 bottom = "",
                                                 top = "")
  
  ggsave(
    filename = "ratios_cloaca.pdf",
    plot = complete_ratios_plot,
    device = cairo_pdf,
    path = here::here("2_outputs", "2_figures"),
    scale = 1,
    width = 7,
    height = 4,
    units = "in"
  )
  
  
  
  
}
