#Reshape the Answer_Q... columns from RoundLogs to match Round_number
#taking the round number from the column names "_Rx" and making a separate column for it
#input: data frame
#output: longer pivoted data frame
pivotedData <- function(data) {
  # First, select only the columns that have '_R1' to '_R9' (exclude _R10 and _R11 columns)
  data <- data %>%
    select(-matches("_R10$|_R11$"))  # Exclude columns ending in _R10 or _R11
  
  # Now, apply the pivot_longer on the remaining columns (those with _R1 to _R9)
  data_long <- data %>%
    pivot_longer(
      cols = matches("_R[1-9]$"),  # Select columns ending with _R1 to _R9
      names_to = c(".value", "Round_number"),
      names_pattern = "(.*)_R(\\d+)"  # Extract column prefix and round number
    ) %>%
    mutate(Round_number = as.numeric(Round_number))  # Ensure round numbers are numeric
  
  # Return the transformed data
  return(data_long)
}








#function for selecting columns from a data frame by a given pattern
#will mostly be used on round logs but its general
selectColumnsByPatterns <- function(data, patterns) {
  # Validate input: Ensure data is a data frame
  checkmate::assertDataFrame(data)
  
  # Validate input: Ensure patterns is a character vector
  checkmate::assertCharacter(patterns, any.missing = FALSE, min.len = 1)
  
  # Use select() with matches() for pattern-based column selection
  selected_data <- data %>%
    select(any_of(patterns), matches(paste(patterns, collapse = "|")))
  
  # Check if no columns matched
  if (ncol(selected_data) == 0) {
    stop("No columns matched the provided patterns.")
  }
  
  return(selected_data)
}