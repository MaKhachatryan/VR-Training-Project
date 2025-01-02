source("environmentSetUp.R")
# This function produces the visualization for the correlation table (demonstrate
# the relationship between the stress indicators (Answer Q1: cognitive load, 
# Answer Q2: physical load) and the physiological measurements (PMD)) of 
# 2 cohorts (Dame and Linne). 
# It will also filter out the PMDs, if their correlation values are too low
#
# Input: 
# 1. correlationTable: either correlationTableDame or correlationTableLinne
# 2. min.corr: minimum of the absolute values of the correlation
#
# Output: a bar chart shows the correlation between the stress indicators
# and the physiological measurements (PMD)


# Create function to process data and create the plot
plotCorrelation <- function(correlationTable, min.corr) {
  
  # Determine the cohort name based on the input table
  cohortName <- ifelse(
    deparse(substitute(correlationTable)) == "correlationTableDame",
    "Dame",
    "Linne"
  )
  
  # Transform the correlation table
  correlationTable <- correlationTable %>%
    pivot_longer(cols = -row_name, names_to = "PMD", values_to = "Correlation") %>%
    filter(abs(as.numeric(Correlation)) >= min.corr) %>%
    mutate(
      Correlation = as.numeric(Correlation),
      PMD = str_replace_all(PMD, "_", " "),
      PMD = str_replace_all(PMD, "raw|Raw", ""),
      PMD = str_replace_all(PMD, "mean", "Mean")
    )
  
  # Create the plot with different colors for each stress indicator
  ggplot(correlationTable, aes(
    x = reorder(PMD, Correlation, decreasing = TRUE), 
    y = Correlation, 
    fill = row_name 
  )) +
    geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(
      x = "Physiological Measurements",
      y = "Correlation",
      title = "Correlation between Physiological measurements and Stress indicators",
      subtitle = sprintf("Cohort: %s", cohortName)
    ) +
    theme(strip.text = element_text(size = 16)) +
    scale_fill_manual(
      values = c(
        "Answer_Q1" = "#1f78b4",
        "Answer_Q2" = "#33a02c" 
      ),
      guide = "none" 
    ) + 
    scale_x_discrete(labels = label_wrap(12)) +
    facet_wrap(~row_name, scales = "free_x", labeller = as_labeller(c(
      "Answer_Q1" = "Cognitive Load",
      "Answer_Q2" = "Physical Load"
    )))
}


# Create the plot with 2 cohorts and 0.09 as a minimum value for the correlation
correlationDame <- plotCorrelation(correlationTableDame, 0.09)
correlationLinne <- plotCorrelation(correlationTableLinne, 0.09)


# ## Export plots into Result folder
if (!all(file.exists(c("Result/Q1/correlationDame.png",
                       "Result//Q1/correlationLinne.png")))) {
  ggsave("Result/Q1/correlationDame.png", correlationDame)
  ggsave("Result/Q1/correlationLinne.png", correlationLinne)
}




