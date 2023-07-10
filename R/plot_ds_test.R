
plot_ds_test = function(data) {
  
# Step 1: Load necessary libraries
library(ggplot2)
library(dplyr)

# Step 3: Filter rows based on sample_type = faeces
faeces_data <- data |>
  filter(sample_type == "feces")

# Step 3: Filter rows based on component C
cnp_faeces <- faeces_data |>
  filter(component_name == "C")

# Step 3: Filter rows based on data type = stock
cnp_faeces_stock <- cnp_faeces |>
  filter(component_data_type == "stock")

# Step 4: Extract relevant columns
cnp_faeces_stock <- cnp_faeces_stock |>
  select(species_latin_name_gbif, component_name, component_mean, body_mass, diet)

# Step 5: Calculate averages per species
averaged_data <- cnp_faeces_stock |>
  group_by(species_latin_name_gbif, component_name) |>
  summarise(avg_component_mean = mean(component_mean),
            body_mass = first(body_mass),
            diet = first(diet))

# Step 7: Create separate plots for C, N, and P
component_plots <- ggplot(averaged_data, aes(x = log(body_mass), y = avg_component_mean)) +
  facet_wrap(~component_name, ncol = 3) +
  geom_point() +
  labs(x = "Log Body Mass", y = "Component Mean") +
  theme_bw()
}
  
