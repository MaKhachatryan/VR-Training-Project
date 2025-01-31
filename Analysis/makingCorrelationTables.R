source("environmentSetUp.R")

# Using the computeAndSaveCorrelation function to generate correlation tables
# The generated CSV files will contain Spearman correlations between "Answer_Q1" (Cognitive Load) 
# and "Answer_Q2" (Physical Load) with other numeric variables in the dataset.

# Compute correlations across the entire dataset (not grouped by rounds)
computeAndSaveCorrelation(combinedPMDAndRoundLogs,
                          "Data/processedData/correlationTableCombined.csv")

# Compute correlations grouped by rounds
computeAndSaveCorrelation(combinedPMDAndRoundLogs,
                          "Data/processedData/correlationTableWithRoundsCombined.csv",
                          over_rounds = TRUE)
