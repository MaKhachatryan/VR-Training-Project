# Combine data for MANOVA, but keep groups separate
#MANOVA tests whether there are significant differences between groups 
#across multiple dependent variables simultaneously. In our case, the dependent variables
#are likely age, height, and weight, and the independent variable is Group (Dame vs. Linne).
#Df (Degrees of Freedom)
#Pillai (Pillai's Trace)
#approx F (F-statistic)
#num Df and den Df
#Pr(>F) (p-value)
#There is no strong evidence to suggest that Dame and Linne differ significantly
#across the variables tested (e.g., age, height, and weight).

combined_data <- bind_rows(
  dame_user_demographics %>% mutate(Group = "Dame"),
  linne_user_demographics %>% mutate(Group = "Linne")
)

manova_results <- manova(cbind(Age, Height, Weight) ~ Group, data = combined_data)
summary(manova_results)


#Old NA cleaning function, used to calculate the mean of columns and insert the 
#value where there are NAs

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