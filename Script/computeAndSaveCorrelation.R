# Function to compute correlations and save them to a specified file
# This function:
# - Calculates Spearman correlations between "Answer_Q1" (Cognitive Load) and "Answer_Q2" (Physical Load) 
#   and other numeric columns in the data frame (excluding specified columns).
# - If over_rounds = TRUE, computes correlations separately for each "Round_number".
# - If over_rounds = FALSE (default), computes correlations across the entire dataset.
# - Combines the results into a single table.
# - Saves the resulting correlations as a CSV file to a specified path.
# Input: 
#   - data: A data frame containing the numeric columns to compute correlations.
#   - full_path: The full file path (including file name) where the CSV will be saved.
#   - over_rounds: A logical value (default = FALSE). If TRUE, computes correlations per round.
# Output: A CSV file containing the computed correlations.

computeAndSaveCorrelation <- function(data, full_path, over_rounds = FALSE) {
  # Identify numeric measurement columns, excluding Answer_Q1, Answer_Q2, and Round_number
  measurement_columns <- data %>%
    select(where(is.numeric)) %>%
    select(-c(Answer_Q1, Answer_Q2, Round_number)) %>%
    colnames()
  
  if (over_rounds) {
    # Compute correlations grouped by Round_number
    correlation_table <- data %>%
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
    
  } else {
    # Compute correlations without grouping by round
    cor_Q1_PMD <- data %>%
      summarise(across(all_of(measurement_columns), ~ cor(Answer_Q1, .x, use = "complete.obs", method = "spearman")))
    
    cor_Q2_PMD <- data %>%
      summarise(across(all_of(measurement_columns), ~ cor(Answer_Q2, .x, use = "complete.obs", method = "spearman")))
    
    # Combine the results into a single table
    correlation_table <- bind_rows(
      cor_Q1_PMD %>% mutate(row_name = "Cognitive Load"),
      cor_Q2_PMD %>% mutate(row_name = "Physical Load")
    ) %>%
      select(row_name, everything()) # Move row_name to the first column
  }
  
  # Round numeric values to 3 decimal places
  correlation_table <- correlation_table %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  # Save the correlation table as a CSV file
  write.csv(correlation_table, full_path, row.names = FALSE)
}

