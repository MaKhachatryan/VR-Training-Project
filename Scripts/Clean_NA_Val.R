dataCleaning <- function(data_frame) {
  # Iterate through each column of the data frame
  for (col_name in names(data_frame)) {
    # Check if the column is numeric
    if (is.numeric(data_frame[[col_name]])) {
      # Extract the column
      column <- data_frame[[col_name]]
      
      # Check if the column is entirely zeros
      if (all(column == 0, na.rm = TRUE)) {
        next # Skip this column; leave it as is
      }
      
      # Calculate the mean, excluding 0 and NA
      column_mean <- mean(column[column != 0 & !is.na(column)], na.rm = TRUE)
      
      # Replace 0s and NAs with the mean
      data_frame[[col_name]][column == 0 | is.na(column)] <- column_mean
    }
  }
  # Return the cleaned data frame
  return(data_frame)
}

cleanDfLinne <- dataCleaning(roundLogs_Linne)
cleanDfDame <- dataCleaning(roundLogs_Dame)
is.na(cleanDfLinne)
