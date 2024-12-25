#Functions that were used on PMD folder

#function to read the data and select the necessary columns from the file
#input: file path
#output: data frame
readData <- function(file){
  
  # Read the data
  data <- read_delim(
    file, 
    delim = ";", 
    locale = locale(decimal_mark = ","),
    escape_double = FALSE, 
    trim_ws = TRUE, 
    show_col_types = FALSE
  )
  
  fifth_col_name <- colnames(data)[5]
  
  data <- data %>%
    select(User_ID, Round_number, colnames(data)[5])
  
  return(data)
}



#function for calculating avarages per round for a given data
#input: data frame
#output: data frame
averagePerRound <- function(data){
  
  # Dynamically generate the mean column name
  mean_col_name <- paste0("mean_", colnames(data)[3])
  
  #calculate the avarage per round
  avg_values_df <- data %>%
    mutate(
      Round_number = abs(as.numeric(Round_number)),  # Ensure 'Round_number' is numeric
    ) %>%
    group_by(Round_number) %>%
    summarise(
      !!mean_col_name := mean(.data[[colnames(data)[3]]], na.rm = TRUE),
      User_ID = first(User_ID),  # Keep User_ID in the summarised data
      .groups = "drop"
    )
  return(avg_values_df)
}



# Ensure all elements in the data list are data frames
# make sure every data frame has all rounds, and the rounds go from 1 till 9
# Combine all data frames into one and give it a nice structure
#input: data frame list
#output: data frame
combinedDataList <- function(df_list){
  
  # Ensure User_ID is character in all data frames
  df_list <- lapply(df_list, function(df) {
    if ("User_ID" %in% colnames(df)) {  # Check if User_ID column exists
      df$User_ID <- as.character(df$User_ID)
    }
    return(df)
  }) 
  
  combine <- bind_rows(df_list)
  
  
  # Combine all PMD columns by Round_number and User_ID
  final_PMD <- combine %>%
    group_by(User_ID, Round_number) %>%
    summarise(across(everything(), ~ na.omit(.x)[1], .names = "{.col}"), .groups = "drop") %>%
    mutate(Round_number = Round_number + 1)  # Adjust Round_number to start from 1
  
  return(final_PMD)
}