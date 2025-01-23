# Functions used for processing data in the PMD folder

# Function to read the data and select necessary columns from the file
# This function:
# - Reads a delimited file with specified settings (e.g., decimal mark and trimming whitespace).
# - Selects only the required columns: User_ID, Round_number, and the fifth column in the dataset.
# Input: File path (string).
# Output: A data frame with selected columns.

readData <- function(file){
  
  # Read the data with custom settings for delimiters and decimal marks
  data <- read_delim(
    file, 
    delim = ";", 
    locale = locale(decimal_mark = ","),
    escape_double = FALSE, 
    trim_ws = TRUE, 
    show_col_types = FALSE
  )
  
  # Select only the necessary columns: User_ID, Round_number, and the fifth (measurement) column
  data <- data %>%
    select(User_ID, Round_number, colnames(data)[5])
  # Return the resulting data frame
  return(data)
}



# Function to calculate averages per round
# This function:
# - Groups the data by Round_number.
# - Calculates the average of the third column (measurement) for each round.
# - Dynamically assigns a name to the mean column based on the third column's name.
# Input: A data frame with columns User_ID, Round_number, and a measurement column.
# Output: A summarized data frame with averages per round.
averagePerRound <- function(data){
  
  # Generate a dynamic name for the calculated mean column
  mean_col_name <- paste0("mean_", colnames(data)[3])
  
  # Calculate the average per round
  avg_values_df <- data %>%
    mutate(
      Round_number = abs(as.numeric(Round_number)),  
    ) %>%
    group_by(Round_number) %>%
    summarise(
      !!mean_col_name := mean(.data[[colnames(data)[3]]], na.rm = TRUE), # Calculate the mean
      User_ID = first(User_ID),  # Retain the first User_ID for reference
      .groups = "drop"
    )
  
  # Return the resulting data frame with averages
  return(avg_values_df)
}



# Function to combine and clean a list of data frames
# This function:
# - Ensures all data frames in the list have a consistent structure (e.g., User_ID as character).
# - Combines all data frames into one.
# - Ensures each round (1 to 9) is present for every user.
# - Adjusts round numbers to start at 1 and removes rounds outside the range of 1 to 9.
# Input: A list of data frames.
# Output: A cleaned and combined data frame.
combinedDataList <- function(df_list){
  
  # Step 1: Ensure User_ID is a character column in all data frames
  df_list <- lapply(df_list, function(df) {
    if ("User_ID" %in% colnames(df)) {  # Check if User_ID column exists
      df$User_ID <- as.character(df$User_ID)
    }
    return(df)
  }) 
  
  # Step 2: Combine all data frames into a single data frame
  combine <- bind_rows(df_list)
  
  
  # Step 3: Clean and structure the combined data
  final_PMD <- combine %>%
    group_by(User_ID, Round_number) %>%
    summarise(across(everything(), ~ na.omit(.x)[1], .names = "{.col}"), .groups = "drop") %>%
    mutate(Round_number = Round_number + 1)  %>%  # Adjust round numbers to start at 1
    filter(Round_number >= 1 & Round_number <= 9)  # Retain only rounds 1 to 9
  
  # Return the cleaned and combined data frame
  return(final_PMD)
}