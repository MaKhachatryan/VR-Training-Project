dataCleaning <- function(dataFrame) {
  # Iterate through each column of the data frame
  for (colName in names(dataFrame)) {
    # Check if the column is numeric
    if (is.numeric(dataFrame[[colName]])) {
      # Extract the column
      column <- dataFrame[[colName]]
      
      # Check if the column is entirely zeros
      if (all(column == 0, na.rm = TRUE)) {
        next # Skip this column; leave it as is
      }
      
      # Calculate the mean, excluding 0 and NA
      columnMean <- mean(column[column != 0 & !is.na(column)], na.rm = TRUE)
      
      # Replace 0s and NAs with the mean
      dataFrame[[colName]][column == 0 | is.na(column)] <- columnMean
    }
  }
  # Return the cleaned data frame
  return(dataFrame)
}

