# Function to replace all 0s with NA in a data frame
# This function:
# - Iterates through each column in the data frame.
# - Replaces any 0s in numeric columns with NA (making them easier to handle later).
# Input: A data frame.
# Output: A cleaned data frame with 0s replaced by NA in numeric columns.

cleanedData <- function(dataFrame) {
  # Iterate through each column of the data frame by name
  for (colName in names(dataFrame)) {
    # Check if the current column is numeric
    if (is.numeric(dataFrame[[colName]])) {
      # Extract the column for processing
      column <- dataFrame[[colName]]
      
      # Replace 0s and existing NAs with NA
      dataFrame[[colName]][column == 0 | is.na(column)] <- NA
    }
  }
  # Return the cleaned data frame
  return(dataFrame)
}