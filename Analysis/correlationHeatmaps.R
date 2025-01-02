source("environmentSetUp.R")


#correlation matrices for Dame and Linne cleaned files, using demographic data,
#physiological measurements and stress data
#input: dataframe
#output: dataframe, heatmap

# Select only numerical data from the dataset
numeric_data <- select_if(LinnePMDAndDemographicsAndAnswers, is.numeric)

# Compute the correlation matrix
cor_matrix <- round(cor(numeric_data, use = "complete.obs"), 2)

  
  # Create the heatmap
  pheatmap(cor_matrix, 
           color = colorRampPalette(c("blue", "white", "red"))(50),
           display_numbers = TRUE)




### Checking agegroups for both training versions

# Split by training
grouped_data <- split(LinnePMDAndDemographicsAndAnswers,
                                           LinnePMDAndDemographicsAndAnswers$Gender)
  
# Define age group bins
ageSubgroupsForEachTraining <- DamePMDAndDemographicsAndAnswers %>%
  mutate(AgeGroup = cut(Age, breaks = c(-Inf, 20, 30, 40, Inf), 
                        labels = c("<20", "20-30", "30-40", "40+")))

# Split data into demographic subgroups
grouped_data <- split(LinnePMDAndDemographicsAndAnswers, 
                      list(ageSubgroupsForEachTraining$Trainingsversion, 
                           
                           ageSubgroupsForEachTraining$Gender), 
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



### Dividing the training version groups into BMI subgroups

DamePMDAndDemographicsAndAnswers <- calculateBMI(DamePMDAndDemographicsAndAnswers)

# Filter for BMI ranges
filtered_data <- DamePMDAndDemographicsAndAnswers %>%
  filter(Age <= 26 ) %>%
  select(mean_HR, SDNN, RMSSD, Answer_Q1, Answer_Q2)


# Check if there is enough data for correlation
if (nrow(filtered_data) > 1) {
  # Compute the correlation matrix
  cor_matrix <- round(cor(filtered_data, use = "complete.obs"), 2)
  
  # Create a heatmap of the correlation matrix
  pheatmap(cor_matrix,
           color = colorRampPalette(c("blue", "white", "red"))(50),
           display_numbers = TRUE,
           main = "Correlation Matrix")
} else {
  message("Not enough data to calculate correlations.")
}


# Create a plot with BMI on the x-axis, stress (Q1) on the y-axis, and lines/dots for HR and SCR-amplitude
ggplot(DamePMDAndDemographicsAndAnswers, aes(x = BMI)) +
  # HR points
  geom_point(aes(y = Answer_Q1, color = "HR"), size = 3, alpha = 0.7) +
  # SCR-amplitude points
  geom_point(aes(y = Answer_Q1, color = "SCR-amplitude"), size = 3, alpha = 0.7) +
  # HR line
  geom_line(aes(y = mean_HR, color = "HR"), size = 1) +
  # SCR-amplitude line
  geom_line(aes(y = mean_SCR_amplitude_raw, color = "SCR-amplitude"), size = 1) +
  labs(
    title = "Stress vs. BMI with HR and SCR-amplitude",
    x = "BMI",
    y = "Stress Level (Q1)",
    color = "Metric"
  ) +
  scale_color_manual(values = c("HR" = "blue", "SCR-amplitude" = "red")) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )
