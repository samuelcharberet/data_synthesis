# Load necessary libraries
library(dplyr)

# Define file paths
arthropods_file <- "C:/Users/scharber/Documents/6_projects/3_data_synthesis/1_data/data_arthropods_species.csv"
mammals_file <- "C:/Users/scharber/Documents/6_projects/3_data_synthesis/1_data/data_mammals_species.csv"
sauropsids_file <- "C:/Users/scharber/Documents/6_projects/3_data_synthesis/1_data/data_sauropsids_species.csv"

# Load datasets
arthropods_data <- read.csv(arthropods_file)
mammals_data <- read.csv(mammals_file)
sauropsids_data <- read.csv(sauropsids_file)

# Function to count observations and component_name levels
analyze_dataset <- function(data, dataset_name) {
  num_observations <- length(which(!is.na(data$avg_component_mean)))
  component_name_counts <- data %>%
    filter(!is.na(avg_component_mean))%>%
    count(component_name) %>%
    arrange(desc(n))
  
  cat("Dataset:", dataset_name, "\n")
  cat("Number of observations:", num_observations, "\n")
  cat("Distribution of component_name levels:\n")
  print(component_name_counts)
  cat("\n")
}

# Analyze each dataset
analyze_dataset(arthropods_data, "Arthropods")
analyze_dataset(mammals_data, "Mammals")
analyze_dataset(sauropsids_data, "Sauropsids")
