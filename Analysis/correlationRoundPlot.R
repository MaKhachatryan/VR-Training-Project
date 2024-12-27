source("environmentSetUp.R")
# This function produces the plot to visualize how the relationship between 
# the stress indicators (stressIndicator Q1: cognitive load, stressIndicator Q2: physical load)
# and the physiological measurements (PMD)) of 2 cohorts (Dame and Linne) changes
# in every round. There are 9 rounds in Dame and and 11 rounds in Linne
#
# Input:
# 1. correlationTable: either correlationTableWithRoundsDame or correlationTableWithRoundsLinne
# 2. stressIndicator: either "Q1" or "Q2"


# Create function to process and create the plot
plotCorrelationRound <- function(correlationTable, stressIndicator) {
  
  # Determine the subtitle based on the cohort
  cohortName <- ifelse(
    deparse(substitute(correlationTable)) == "correlationTableWithRoundsDame",
    "Dame",
    "Linne"
  )
  
  # Determine the subtitle based on the stress indicator
  subtitleText <- ifelse(
    stressIndicator == "Q1",
    "PMD & Cognitive Load",
    "PMD & Physical Load")
  
  # Change the structure of the data frame
  correlationTable <- correlationTable %>%
    mutate(Round_number = as.numeric(Round_number)) %>%
    pivot_longer(
      cols = -Round_number, 
      names_to = "PMD", 
      values_to = "correlation"
    ) %>%
    filter(grepl(paste0("_", stressIndicator, "_"), PMD))  # Filter by stressIndicator Q1 or Q2
  
  # Ensure correlation is numeric
  correlationTable <- correlationTable %>%
    mutate(correlation = as.numeric(correlation))
  
  # Clean the variable names
  correlationTable$PMD <- str_replace_all(correlationTable$PMD, "_", " ")
  correlationTable$PMD <- str_replace_all(correlationTable$PMD, "raw|Raw", "")
  correlationTable$PMD <- str_replace_all(correlationTable$PMD, "mean", "Mean")
  correlationTable$PMD <- str_replace_all(correlationTable$PMD, "Q1", "")
  correlationTable$PMD <- str_replace_all(correlationTable$PMD, "Q2", "")
  correlationTable$PMD <- str_replace_all(correlationTable$PMD, "corr", "")
  
  # Create plot
  ggplot(correlationTable, aes(x = Round_number, y = correlation)) +
    geom_line(size = 1) + 
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
    facet_wrap(~ PMD, scales = "free_y", ncol = 3, strip.position = "top") +  
    scale_x_continuous(
      breaks = seq(min(correlationTable$Round_number), max(correlationTable$Round_number), 1),
    ) +  
    theme_minimal() +
    labs(
      x = "Round Number",
      y = "Correlation",
      title = "Correlation over the rounds between PMD and stress indicators",
      subtitle = sprintf("%s (%s)", subtitleText, cohortName)
    ) 
}

## Create plot with each stress indicator in each cohort
roundDameQ1 <- plotCorrelationRound(correlationTableWithRoundsDame, "Q1")
roundDameQ2 <- plotCorrelationRound(correlationTableWithRoundsDame, "Q2")
roundLinneQ1 <- plotCorrelationRound(correlationTableWithRoundsLinne, "Q1") 
roundLinneQ2 <- plotCorrelationRound(correlationTableWithRoundsLinne, "Q2")
