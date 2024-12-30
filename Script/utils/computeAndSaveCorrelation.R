# Compute correlations and save to a specific folder
#input: data frame, path for saving
#output: .csv file
compute_and_save_correlation <- function(data, full_path) {
  # Select numeric columns (exclude Answer_Q1, Answer_Q2, and Round_number)
  measurement_columns <- data %>%
    select(where(is.numeric)) %>%
    select(-c(Answer_Q1, Answer_Q2, Round_number)) %>%
    colnames()
  
  print(measurement_columns)  # Display selected numeric columns
  
  # Compute correlations with Answer_Q1
  cor_Q1_PMD <- data %>%
    summarise(across(all_of(measurement_columns), ~ cor(Answer_Q1, .x, use = "complete.obs", method = "spearman")))
  
  # Compute correlations with Answer_Q2
  cor_Q2_PMD <- data %>%
    summarise(across(all_of(measurement_columns), ~ cor(Answer_Q2, .x, use = "complete.obs", method = "spearman")))
  
  # Combine results into a single tibble
  combined_tibble <- bind_rows(
    cor_Q1_PMD %>% mutate(row_name = "Answer_Q1"),
    cor_Q2_PMD %>% mutate(row_name = "Answer_Q2")
  ) %>%
    select(row_name, everything())
  
  combined_tibble <- combined_tibble %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
  
  # Save the combined tibble as a CSV file using the full path
  write.csv(combined_tibble, full_path, row.names = FALSE)
}
