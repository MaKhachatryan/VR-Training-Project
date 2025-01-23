# Reshape the Answer_Q... columns from RoundLogs to align with Round_number
# This function processes a data frame by:
# 1. Selecting columns containing answers for rounds (e.g., _R1 to _R9) while excluding others (e.g., _R10, _R11).
# 2. Transforming these columns from wide to long format using the round number as a separate column.
# Input: A data frame with columns named in the format "Answer_QX_RY".
# Output: A longer, pivoted data frame with a separate column for round numbers.

pivotedData <- function(data) {
  # Step 1: Exclude columns ending in _R10 or _R11 (if they exist in the dataset)
  data <- data %>%
    select(-matches("_R10$|_R11$"))  # Exclude columns ending in _R10 or _R11
  
  # Step 2: Pivot columns ending with _R1 to _R9 into a longer format
  data_long <- data %>%
    pivot_longer(
      # Match columns with names ending in _R1 to _R9
      cols = matches("_R[1-9]$"),    
      # Split the names into two parts: prefix and round number
      names_to = c(".value", "Round_number"), 
      # Regex to extract column prefix and round number
      names_pattern = "(.*)_R(\\d+)"  
    ) %>%
    # Convert round numbers to numeric for consistency
    mutate(Round_number = as.numeric(Round_number))  
  
  # Step 3: Return the reshaped data
  return(data_long)
}








# Function for selecting columns from a data frame by a given pattern
# This function:
# - Selects columns from a data frame whose names match the provided patterns.
# - Supports general usage but is particularly useful for selecting columns in round logs.
# Input: 
#   - data: A data frame to select columns from.
#   - patterns: A character vector of patterns to match column names.
# Output: A data frame with columns that match the provided patterns.

selectColumnsByPatterns <- function(data, patterns) {
  # Step 1: Validate that the input 'data' is a data frame
  checkmate::assertDataFrame(data)
  
  # Step 2: Validate that 'patterns' is a non-empty character vector
  checkmate::assertCharacter(patterns, any.missing = FALSE, min.len = 1)
  
  # Step 3: Use select() with matches() for pattern-based column selection
  # - 'any_of(patterns)': Selects columns with exact names in 'patterns'.
  # - 'matches(paste(patterns, collapse = "|"))': Selects columns whose names contain any of the patterns.
  selected_data <- data %>%
    select(any_of(patterns), matches(paste(patterns, collapse = "|")))
  
  # Step 4: Check if no columns matched the provided patterns
  if (ncol(selected_data) == 0) {
    stop("No columns matched the provided patterns.")
  }
  
  # Step 5: Return the data frame with selected columns
  return(selected_data)
}