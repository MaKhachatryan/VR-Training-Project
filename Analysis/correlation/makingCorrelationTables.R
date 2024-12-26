#using the computeAndSaveCorrelation for making correlation tables
source("Script/utils/computeAndSaveCorrelation.R")
source("Script/cleanData/dataProcess.R")

compute_and_save_correlation(clean_dame_data, "Analysis/correlation/correlationTableDame.csv")
compute_and_save_correlation(clean_linne_data, "Analysis/correlation/correlationTableLinne.csv")



# List of measurement columns (excluding Round_number, Answer_Q1, and Answer_Q2)
measurement_columns <- dame_linne_combined %>%
  select(where(is.numeric)) %>%
  select(-c(Answer_Q1, Answer_Q2, Round_number)) %>%
  colnames()


# Calculate correlations for each round
correlation_table_with_rounds <- dame_linne_combined %>%
  group_by(Round_number) %>%
  summarise(
    across(
      all_of(measurement_columns), 
      list(
        Q1_corr = ~ cor(.x, Answer_Q1, method = "spearman", use = "complete.obs"),
        Q2_corr = ~ cor(.x, Answer_Q2, method = "spearman", use = "complete.obs")
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()


write_csv(correlation_table_with_rounds, "Analysis/correlation/correlationTableWithRounds.csv")


