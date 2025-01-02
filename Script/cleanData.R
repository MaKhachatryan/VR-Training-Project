#function replaces all 0s with NA, which is easier to ignore later
cleanedData <- function(dataFrame) {
  # Iterate through each column of the data frame
  for (colName in names(dataFrame)) {
    # Check if the column is numeric
    if (is.numeric(dataFrame[[colName]])) {
      # Extract the column
      column <- dataFrame[[colName]]
      
      # Replace 0s with NA
      dataFrame[[colName]][column == 0 | is.na(column)] <- NA
    }
  }
  # Return the cleaned data frame
  return(dataFrame)
}