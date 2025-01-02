#calculate BMI and save it in a new column named "BMI"
#input: data frame
#output: data frame

calculateBMI <- function(data) {
  # Check if required columns exist
  if (!all(c("Height", "Weight") %in% colnames(data))) {
    stop("The data frame must contain 'Height' and 'Weight' columns.")
  }
  
  # Add the BMI column
  data <- data %>%
    mutate(
      Height_m = Height / 100,               # Convert height to meters if in cm
      BMI = Weight / (Height_m ^ 2)         # Calculate BMI
    ) %>%
    select(-Height_m)                        # Remove temporary column (optional)
  
  return(data)
}