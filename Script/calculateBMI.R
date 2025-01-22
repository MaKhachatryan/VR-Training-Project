# Function to calculate BMI and save it in a new column named "BMI"
# This function:
# - Checks for the presence of required columns: "Height" and "Weight".
# - Calculates BMI using the formula: Weight (kg) / [Height (m)]^2.
# - Adds the BMI column to the data frame.
# Input: A data frame containing "Height" (in cm) and "Weight" (in kg) columns.
# Output: A data frame with a new "BMI" column.

calculateBMI <- function(data) {
  # Check if the required columns "Height" and "Weight" exist in the data frame
  if (!all(c("Height", "Weight") %in% colnames(data))) {
    stop("The data frame must contain 'Height' and 'Weight' columns.")
  }
  
  # Add a BMI column to the data frame
  data <- data %>%
    mutate(
      Height_m = Height / 100,               # Convert height to meters if provided in cm
      BMI = Weight / (Height_m ^ 2)         # Calculate BMI using the standard formula
    ) %>%
    select(-Height_m)                       # Remove the temporary 'Height_m' column
  
  # Return the updated data frame
  return(data)
}