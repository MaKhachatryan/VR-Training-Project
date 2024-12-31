source("environmentSetUp.R")


#correlation matrices for Dame and Linne cleaned files, using demographic and stress data
#input: dataframe
#output: dataframe, heatmap

#Split data by Subgroup
grouped_data <- split(combinedPMDAndDemographicsAndAnswers, combinedPMDAndDemographicsAndAnswers$Trainingsversion)

#Compute correlation matrices for each group
cor_matrices <- lapply(grouped_data, function(group) {
  numeric_data <- select_if(group, is.numeric)
  round(cor(numeric_data, use = "complete.obs"), 2)
})

# Create heatmaps for each correlation matrix
for (group_name in names(cor_matrices)) {
  cor_matrix <- cor_matrices[[group_name]]
  
  # Create the heatmap
  pheatmap(cor_matrix, 
           color = colorRampPalette(c("blue", "white", "red"))(50),
           display_numbers = TRUE,
           main = paste("Heatmap for Group:", group_name))
}



### Checking agegroups for both training versions

# Define age group bins
ageSubgroupsForEachTraining <- combinedPMDAndDemographicsAndAnswers %>%
  mutate(AgeGroup = cut(Age, breaks = c(-Inf, 20, 30, 40, Inf), 
                        labels = c("<20", "20-30", "30-40", "40+")))

# Split data by Trainingsversion (adaptive/non-adaptive) and AgeGroup
grouped_data <- split(ageSubgroupsForEachTraining, 
                      list(ageSubgroupsForEachTraining$Trainingsversion, 
                           ageSubgroupsForEachTraining$AgeGroup), 
                      drop = TRUE)

# Function to clean correlation matrix
clean_correlation_matrix <- function(cor_matrix) {
  cor_matrix[is.na(cor_matrix) | is.nan(cor_matrix) | is.infinite(cor_matrix)] <- 0
  return(cor_matrix)
}

# Modify the correlation calculation to handle single-row groups
cor_matrices <- lapply(grouped_data, function(group) {
  numeric_data <- select_if(group, is.numeric)
  if (nrow(numeric_data) > 1) {
    cor_matrix <- round(cor(numeric_data), 2)
    clean_correlation_matrix(cor_matrix)
  } else if (nrow(numeric_data) == 1) {
    # Create a matrix of 1's for single-row groups
    matrix(1, 
           nrow = ncol(numeric_data), 
           ncol = ncol(numeric_data), 
           dimnames = list(names(numeric_data), names(numeric_data)))
  } else {
    NULL
  }
})
# Create heatmaps for each correlation matrix
for (group_name in names(cor_matrices)) {
  cor_matrix <- cor_matrices[[group_name]]
  if (!is.null(cor_matrix) && nrow(grouped_data[[group_name]]) > 1) {
    pheatmap(cor_matrix, 
             color = colorRampPalette(c("blue", "white", "red"))(50),
             display_numbers = TRUE,
             main = paste("Heatmap for Group:", group_name))
  }
}

# Filter for 40+ age group and split by training version
group_40plus <- ageSubgroupsForEachTraining %>%
  filter(AgeGroup == "40+") %>%
  split(.$Trainingsversion)

# For each training version in 40+ group
for (version in names(group_40plus)) {
  numeric_data <- select_if(group_40plus[[version]], is.numeric)
  print(paste("Training Version:", version))
  print(numeric_data)
}



### Dividing the training version groups inot BMI subgroups

combinedPMDAndDemographicsAndAnswers <- calculateBMI(combinedPMDAndDemographicsAndAnswers)

# Define BMI categories
bmiSubgroups <- combinedPMDAndDemographicsAndAnswers %>%
  mutate(BMICategory = cut(BMI, 
                           breaks = c(-Inf, 18.4, 25, Inf), 
                           labels = c("<18.4", "18.4-25", "25+"), 
                           include.lowest = TRUE))

# Split data by Trainingsversion and BMICategory
grouped_data <- split(bmiSubgroups, 
                      list(bmiSubgroups$Trainingsversion, 
                           bmiSubgroups$BMICategory), 
                      drop = TRUE)

# Function to clean correlation matrix
clean_correlation_matrix <- function(cor_matrix) {
  cor_matrix[is.na(cor_matrix) | is.nan(cor_matrix) | is.infinite(cor_matrix)] <- 0
  return(cor_matrix)
}

# Compute correlation matrices for each subgroup
cor_matrices <- lapply(grouped_data, function(group) {
  numeric_data <- select_if(group, is.numeric)
  if (nrow(numeric_data) > 1) { # Ensure enough data for correlation
    cor_matrix <- round(cor(numeric_data, use = "complete.obs"), 2)
    clean_correlation_matrix(cor_matrix)
  } else {
    NULL
  }
})

# Create heatmaps for each correlation matrix
for (group_name in names(cor_matrices)) {
  cor_matrix <- cor_matrices[[group_name]]
  
  if (!is.null(cor_matrix)) {
    pheatmap(cor_matrix, 
             color = colorRampPalette(c("blue", "white", "red"))(50),
             display_numbers = TRUE,
             main = paste("Heatmap for Group:", group_name),
             cluster_rows = FALSE,  # Optional: Disable clustering
             cluster_cols = FALSE)
  }
}
