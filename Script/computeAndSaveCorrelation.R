# Function to compute correlations and save them to a specified folder
# This function:
# - Calculates Spearman correlations between "Answer_Q1" (Cognitive Load) and "Answer_Q2" (Physical Load) 
#   and other numeric columns in the data frame (excluding specified columns).
# - Combines the results into a single table.
# - Saves the resulting correlations as a CSV file to a specified path.
# Input: 
#   - data: A data frame containing the numeric columns to compute correlations.
#   - full_path: The full file path (including file name) where the CSV will be saved.
# Output: A CSV file containing the computed correlations.

computeAndSaveCorrelation <- function(data, full_path) {
  # Step 1: Identify numeric columns to be used for correlation calculations
  # - Excludes Answer_Q1, Answer_Q2, and Round_number from the analysis.
  measurement_columns <- data %>%
    select(where(is.numeric)) %>%
    select(-c(Answer_Q1, Answer_Q2, Round_number)) %>%
    colnames()
  
  # Step 2: Compute correlations with Answer_Q1 (Cognitive Load)
  # - Uses Spearman correlation and excludes missing values.
  cor_Q1_PMD <- data %>%
    summarise(across(all_of(measurement_columns), ~ cor(Answer_Q1, .x, use = "complete.obs", method = "spearman")))
  
  # Step 3: Compute correlations with Answer_Q2 (Physical Load)
  # - Uses Spearman correlation and excludes missing values.
  cor_Q2_PMD <- data %>%
    summarise(across(all_of(measurement_columns), ~ cor(Answer_Q2, .x, use = "complete.obs", method = "spearman")))
  
  # Step 4: Combine the results into a single table
  # - Adds a row identifier for Cognitive Load and Physical Load correlations.
  combined_tibble <- bind_rows(
    cor_Q1_PMD %>% mutate(row_name = "Cognitive Load"),
    cor_Q2_PMD %>% mutate(row_name = "Physical Load")
  ) %>%
    select(row_name, everything()) # Move row_name to the first column
  
  # Step 5: Round numeric values to 3 decimal places for better readability
  combined_tibble <- combined_tibble %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  # Step 6: Save the combined table to the specified path as a CSV file
  write.csv(combined_tibble, full_path, row.names = FALSE)
}
