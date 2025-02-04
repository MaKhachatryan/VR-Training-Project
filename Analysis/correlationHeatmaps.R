source("environmentSetUp.R")


#correlation matrices for Dame and Linne cleaned files, using demographic data,
#physiological measurements and stress data
#input: dataframe
#output: dataframe, heatmap


### Heatmaps for Agegroups using RMSSD, Answer_Q1, Answer_Q2
 
# Clean correlation matrix
clean_correlation_matrix <- function(cor_matrix) {
  cor_matrix[is.na(cor_matrix) | is.nan(cor_matrix) | is.infinite(cor_matrix)] <- 0
  return(cor_matrix)
}

# Split age subgroups into a list of data frames
ageSubgroups <- combinedPMDAndDemographicsAndAnswers %>%
  mutate(AgeGroup = cut(Age, breaks = unique(c(-Inf, 20, 30, 40, 50, Inf)), 
                        labels = c("<20", "21-30", "31-40", "41-50", "51+"), include.lowest = TRUE, right=TRUE)) %>%
  filter(!is.na(AgeGroup)) %>%
  group_by(AgeGroup) %>%
  group_split()

# Compute correlation matrices
cor_matrices <- lapply(ageSubgroups, function(group) {
  # Select only the desired variables
  selected_data <- select(group, RMSSD, Answer_Q1, Answer_Q2)
  
  # Ensure only numeric columns are used
  numeric_data <- select(selected_data, where(is.numeric))
  
  if (nrow(numeric_data) > 1) {
    cor_matrix <- round(cor(scale(numeric_data)), 2)
    cor_matrix <- clean_correlation_matrix(cor_matrix)
    rownames(cor_matrix) <- colnames(cor_matrix) <- colnames(numeric_data)
    cor_matrix
  } else if (nrow(numeric_data) == 1) {
    matrix(1, 
           nrow = ncol(numeric_data), 
           ncol = ncol(numeric_data), 
           dimnames = list(colnames(numeric_data), colnames(numeric_data)))
  } else {
    NULL
  }
})

# Create heatmaps
for (i in seq_along(cor_matrices)) {
  cor_matrix <- cor_matrices[[i]]
  group_name <- levels(factor(sapply(ageSubgroups, 
                                     function(x) unique(x$AgeGroup))))[i]
  
  # Falls cor_matrix NULL oder NA-Werte enthält, überspringen
  if (is.null(cor_matrix) || any(is.na(cor_matrix)) || all(cor_matrix == cor_matrix[1])) {
    message("Skipping Heatmap for Age Group: ", group_name, " (invalid correlation matrix)")
    next
  }
  
  pheatmap(cor_matrix, 
           color = colorRampPalette(c("powderblue", "white", "pink"))(50),
           display_numbers = TRUE,
           main = paste("Heatmap for Age Group:", group_name))
}


### Heatmaps for Training versions or Gender, using mean_HR, Answer_Q1, Answer_Q2

# Split by training
grouped_data_by_training <- split(LinnePMDAndDemographicsAndAnswers,
                      LinnePMDAndDemographicsAndAnswers$Trainingsversion)
# Split by gender
grouped_data_by_gender <- split(DamePMDAndDemographicsAndAnswers,
                      DamePMDAndDemographicsAndAnswers$Gender)

# For training versions
for (training_version in names(grouped_data_by_training)) {
  # Filter for heart rate and perceived stress variables
  filtered_data <- grouped_data_by_training[[training_version]] %>%
    select(mean_HR, Answer_Q1, Answer_Q2)
  
  if (nrow(filtered_data) > 1) {
    cor_matrix <- round(cor(filtered_data, use = "complete.obs"), 2)
    
    pheatmap(cor_matrix,
             color = colorRampPalette(c("powderblue", "white", "pink"))(50),
             display_numbers = TRUE,
             main = paste("Linne Correlation Matrix - Training Version:", training_version))
  } else {
    message(paste("Not enough data for Training Version:", training_version))
  }
}

# For gender
for (gender in names(grouped_data_by_gender)) {
  filtered_data <- grouped_data_by_gender[[gender]] %>%
    select(mean_HR, Answer_Q1, Answer_Q2)
  
  if (nrow(filtered_data) > 1) {
    cor_matrix <- round(cor(filtered_data, use = "complete.obs"), 2)
    
    pheatmap(cor_matrix,
             color = colorRampPalette(c("powderblue", "white", "pink"))(50),
             display_numbers = TRUE,
             main = paste("Dame Correlation Matrix - Gender:", gender))
  } else {
    message(paste("Not enough data for Gender:", gender))
  }
}