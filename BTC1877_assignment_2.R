#' BTC1877 Assignment 2
#' Tenzin Gyaltsen

#### Preparation ####
# Install and load required packages.
install.packages("dplyr")
install.packages("tidyr")
install.packages("funModeling")
install.packages("ggplot2")
library(funModeling)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read data and briefly explore.
working <- read.csv("bc_data.csv", header = FALSE)
dim(working)
summary(working)
str(working)

# Assess and assign NAs.
which(working == "?")
working <- as.data.frame(lapply(working, function(x) 
  ifelse(x == "?", NA, x)))
status(working)
# Lymph node status is the only variable with zeroes and NAs.

# Rename variables.
names(working) <- c("id", "outcome", "time", "radius_mean", "texture_mean",
                    "perimeter_mean", "area_mean", "smoothness_mean", 
                    "compactness_mean", "concavity_mean", "concave_points_mean",
                    "symmetry_mean", "fractal_dimension_mean", "radius_se",
                    "texture_se", "perimeter_se", "area_se", "smoothness_se",
                    "compactness_se", "concavity_se", "concave_points_se",
                    "symmetry_se", "fractal_dimension_se", "radius_worst",
                    "texture_worst", "perimeter_worst", "area_worst",
                    "smoothness_worst", "compactness_worst", "concavity_worst",
                    "concave_points_worst", "symmetry_worst", "fractal_dimension_worst",
                    "tumour_size", "lymph_nodes")

#### Regression ####
# Consider only patients with recurrence.
workingr <- working[which(working$outcome == "R"),]
# Consider only mean feature values.
workingr <- workingr[,c(1:13,34,35)]
# Refactor number of axillary nodes.
workingr$lymph_nodes <- ifelse(is.na(workingr$lymph_nodes), NA, 
                        ifelse(workingr$lymph_nodes == "0", 0, 
                               ifelse(as.integer(workingr$lymph_nodes) < 4, "1-3",
                                      "4 or more")))

# Generate descriptive statistics for numerical variables.
num_variables <- c(3:14)
num_table <- workingr %>%
  summarise(across(all_of(num_variables), list(
    mean = ~mean(.x,na.rm = T), median = ~median(.x,na.rm = T),
    sd = ~sd(.x, na.rm = T), min = ~min(.x, na.rm = T),
    max = ~max(.x, na.rm = T), iqr = ~IQR(.x, na.rm = T)))) %>%
  pivot_longer(everything(), names_to = c("Variable", ".value"), 
               names_pattern = "(.*)_(.*)$")
num_table

# Generate descriptive statistics for one categorical variable.
cat_table <- workingr %>%
  count(lymph_nodes) %>%
  mutate(proportion = n/sum(n))
cat_table

#' Create histogram for each numerical variable and bar chart for each
#' categorical variable in data set.
for (var in names(workingr)) {
  if (is.numeric(workingr[[var]]) && var != "id") {
    histogram <- ggplot(workingr, aes(x = .data[[var]])) +
      geom_histogram(fill = "skyblue") +
      labs(title = var, x = var, y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 18))
    print(histogram)
  } else if (is.character(workingr[[var]]) && var != "outcome") {
    barchart <- ggplot(workingr, aes(x = .data[[var]])) +
      geom_bar(fill = "salmon") +
      labs(title = var, x = var, y = "Frequency") +
      theme_minimal() +
      theme(plot.title = element_text(, hjust = 0.5, size = 18))
    print(barchart)
  }
}
