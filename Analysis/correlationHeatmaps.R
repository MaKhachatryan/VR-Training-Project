source("environmentSetUp.R")


#correlation matrices for Dame and Linne cleaned files, using demographic data,
#physiological measurements and stress data
#input: dataframe
#output: dataframe, heatmap



# Add the BMI column
combinedPMDAndDemographicsAndAnswers <- calculateBMI(combinedPMDAndDemographicsAndAnswers)
DamePMDAndDemographicsAndAnswers <- calculateBMI(DamePMDAndDemographicsAndAnswers)
LinnePMDAndDemographicsAndAnswers <- calculateBMI(LinnePMDAndDemographicsAndAnswers)

# Split by training
grouped_data <- split(combinedPMDAndDemographicsAndAnswers,
                      combinedPMDAndDemographicsAndAnswers$Trainingsversion)

# Function to select numeric columns from a data frame
select_numeric_columns <- function(df) {
  df %>% select(where(is.numeric))
}

# Apply the function to each data frame in the list
numeric_data_list <- lapply(grouped_data, select_numeric_columns)

# Compute the correlation matrix for each subset
cor_matrices <- lapply(numeric_data_list, function(df) {
  cor(df, use = "complete.obs")
})

# Create heatmaps for each correlation matrix
for (name in names(cor_matrices)) {
  pheatmap(
    cor_matrices[[name]],
    color = colorRampPalette(c("blue", "white", "red"))(50),
    display_numbers = TRUE,
    main = paste("Correlation Matrix for Training Version:", name)
  )
}


### Checking agegroups for both training versions
 
# Clean correlation matrix
clean_correlation_matrix <- function(cor_matrix) {
  cor_matrix[is.na(cor_matrix) | is.nan(cor_matrix) | is.infinite(cor_matrix)] <- 0
  return(cor_matrix)
}

# Split age subgroups into a list of data frames
ageSubgroups <- DamePMDAndDemographicsAndAnswers %>%
  mutate(AgeGroup = cut(Age, breaks = c(-Inf, 20, 30, 40, Inf), 
                        labels = c("15-30", "31-45", "45+"))) %>%
  group_by(AgeGroup) %>%
  group_split()

# Compute correlation matrices
cor_matrices <- lapply(grouped_data, function(group) {
  numeric_data <- select(group, where(is.numeric))
  if (nrow(numeric_data) > 1) {
    cor_matrix <- round(cor(scale(numeric_data)), 2)
    clean_correlation_matrix(cor_matrix)
  } else if (nrow(numeric_data) == 1) {
    matrix(1, 
           nrow = ncol(numeric_data), 
           ncol = ncol(numeric_data), 
           dimnames = list(names(numeric_data), names(numeric_data)))
  } else {
    NULL
  }
})

# Create heatmaps
for (i in seq_along(cor_matrices)) {
  cor_matrix <- cor_matrices[[i]]
  group_name <- levels(DamePMDAndDemographicsAndAnswers$AgeGroup)[i]
  if (!is.null(cor_matrix)) {
    pheatmap(cor_matrix, 
             color = colorRampPalette(c("blue", "white", "red"))(50),
             display_numbers = TRUE,
             main = paste("Heatmap for Group:", group_name))
  }
}



### Dividing the training version groups into BMI subgroups

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
