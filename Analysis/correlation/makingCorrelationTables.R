#using the computeAndSaveCorrelation for making correlation tables
source("Script/utils/computeAndSaveCorrelation.R")
source("Script/cleanData/dataProcess.R")

compute_and_save_correlation(clean_dame_data, "Analysis/correlation/correlationTableDame.csv")
compute_and_save_correlation(clean_linne_data, "Analysis/correlation/correlationTableLinne.csv")
