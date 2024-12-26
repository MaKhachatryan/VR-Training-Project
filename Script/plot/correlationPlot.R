source("../../environmentSetUp.R")
# This function produces the visualization for the correlation table (demonstrate
# the relationship between the stress indicators (Answer Q1: cognitive load, 
# Answer Q2: physical load) and the physiological measurements (PMD)) of 
# 2 cohorts (Dame and Linne).
#
# Input: 
# 1. correlationTable: either correlationTableDame or correlationTableLinne
# 2. stressIndicator: either "Answer_Q1" or "Answer_Q2"
#
# Output: a bar chart shows the correlation between the stress indicators
# and the physiological measurements (PMD)


# Create function to process data and create the plot
plotCorrelation <- function(correlationTable, stressIndicator) {
  
  # Determine the subtitle based on the cohort
  cohortName <- ifelse(
    deparse(substitute(correlationTable)) == "correlationTableDame",
    "Dame",
    "Linne"
  )
  
  # Determine the subtitle based on the stress indicator
  subtitleText <- ifelse(
    stressIndicator == "Answer_Q1",
    "PMD & Cognitive Load",
    "PMD & Physical Load")
  
  # Change the structure of the correlation table
  correlationTable <- pivot_longer(correlationTable, -1, names_to = "PMD", 
                                   values_to = "Correlation") %>%
    filter(row_name == stressIndicator) %>%
    mutate(Correlation = as.numeric(Correlation))
  
  # Clean the variable names
  correlationTable$PMD <- str_replace_all(correlationTable$PMD, "_", " ")
  correlationTable$PMD <- str_replace_all(correlationTable$PMD, "raw|Raw", "")
  correlationTable$PMD <- str_replace_all(correlationTable$PMD, "mean", "Mean")
  
  # Create the plot
  ggplot(correlationTable, aes(x = reorder(PMD, Correlation, decreasing = TRUE), 
                               y = Correlation)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(
      x = "Physical Measurements",
      y = "Correlation with Stress Measurement",
      title = "Correlation between Physical measurement and Stress measurement",
      subtitle = sprintf("%s (%s)", subtitleText, cohortName)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


dameQ1 <- plotCorrelation(correlationTableDame, "Answer_Q1")
dameQ2 <- plotCorrelation(correlationTableDame, "Answer_Q2")
linneQ1 <- plotCorrelation(correlationTableLinne, "Answer_Q1")
linneQ2 <- plotCorrelation(correlationTableLinne, "Answer_Q2")
