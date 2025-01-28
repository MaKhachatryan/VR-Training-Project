source("environmentSetUp.R")
# This function produces the visualization for the correlation table (demonstrate
# the relationship between the stress indicators (Answer Q1: cognitive load, 
# Answer Q2: physical load) and the physiological measurements (PMD)) of 
# the combined data between 2 cohorts.
# It will also filter out the PMDs, if their correlation values are too low
#
# Input: 
# 1. correlationTable: correlationTableCombined
# 2. minCorr: minimum of the absolute values of the correlation
#
# Output: a bar chart shows the correlation between the stress indicators
# and the physiological measurements (PMD)


# Create function
plotCorrelation <- function(correlationTable, minCorr) {
  
  # Change the structure of the data frame
  correlationTable <- correlationTable |>
    pivot_longer(cols = -row_name, names_to = "pmd", values_to = "correlation") |>
    filter(abs(as.numeric(correlation)) >= minCorr) 
  
  # Define measurements and their groups
  heartMeasurements <- c("HR", "RMSSD", "SDNN")
  skinMeasurements <- c("SCL", "SCR amplitude", "SCR frequency", "SCR rise time")
  blinkSaccadeMeasurements <- c("Blink rate last minute", "Saccade amplitude", "Saccade velocity")
  
  # Add a new column for the group based on measurement type
  correlationTable <- correlationTable |>
    filter(pmd != "IBI") |>
    mutate(
      pmd = str_replace_all(pmd, "_", " "),
      pmd = str_replace_all(pmd, "raw|Raw", ""),
      pmd = str_replace_all(pmd, "mean", ""),
      pmd = trimws(pmd),
      group = case_when(
        pmd %in% heartMeasurements ~ "Heart",
        pmd %in% skinMeasurements ~ "Skin",
        pmd %in% blinkSaccadeMeasurements ~ "Blink/Saccade",
      )
    )
  
  # Define group colors
  groupColors <- c(
    "Heart" = "#66c2a5",
    "Skin" = "#fc8d62",
    "Blink/Saccade" = "#7570b3"
  )
  
  # Create the plot
  ggplot(correlationTable, aes(
    x = reorder(pmd, correlation, decreasing = TRUE), 
    y = correlation, 
    fill = group
  )) +
    geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(
      x = "Physiological Measurements",
      y = "Correlation",
      title = "Correlation between Physiological Measurements and Stress Indicators",
      fill = "Group"
    ) +
    theme(strip.text = element_text(size = 16)) +
    scale_fill_manual(values = groupColors) +
    scale_x_discrete(labels = label_wrap(12)) +
    facet_wrap(~row_name, scales = "free_x")
}


# Create the plot with a modified minimum value for the correlation
correlation <- plotCorrelation(correlationTableCombined, 0.1)


# ## Export plots into Result folder
if (!file.exists("Result/Q2/correlation.png")) {
  ggsave("Result/Q2/correlation.png", correlation)
}
