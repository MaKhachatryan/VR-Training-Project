install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("readr")
# Load necessary libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(cluster)



#1
#function to read the data we need from the file
readData <- function(file){
  
  # Extract the user ID from the file name
  file_name <- basename(file)  # Extract just the file name (without the path)
  user_id <- strsplit(file_name, "_")[[1]][1]  # Split by "_" and take the first part
  
  # Read the data
  data <- read_delim(file, delim = ";", escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
  
  # Clean and convert the 5th column
  data <- data %>%
    mutate(!!colnames(data)[5] := as.numeric(
      str_replace_all(
        str_replace_all(.data[[colnames(data)[5]]], ",", "."),  # Replace commas with periods
        "[^0-9eE.-]", ""  # Remove other unwanted characters
      )))
  
  data <- data %>%
    select(User_ID, Round_number, colnames(data)[5])
  
  return(data)
}



#2
#function for calculating avarages per round for a given data
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



#3
# Ensure all elements in the data list are data frames
# make sure every data frame has all rounds, and the rounds go from 1 till 9
# Combine all data frames into one and give it a nice structure
combinedDataList <- function(df_list){
  
  # Ensure User_ID is character in all data frames
  df_list <- lapply(df_list, function(df) {
    if ("User_ID" %in% colnames(df)) {  # Check if User_ID column exists
      df$User_ID <- as.character(df$User_ID)
    }
    return(df)
  }) 
  
  combine <- bind_rows(df_list)
  
  # Ensure all rounds (0 to 8) are present for each User_ID
  combine <- combine %>%
    group_by(User_ID) %>%
    complete(Round_number = 0:8, fill = list(User_ID = NA)) %>%  # Fill missing rounds for each user with NA
    ungroup()
  
  # Combine all PMD columns by Round_number and User_ID
  final_PMD <- combine %>%
    group_by(User_ID, Round_number) %>%
    summarise(across(everything(), ~ na.omit(.x)[1], .names = "{.col}"), .groups = "drop") %>%
    mutate(Round_number = Round_number + 1)  # Adjust Round_number to start from 1
  
  return(final_PMD)
}



#4
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




#5
#Reshape the Answer_Q... columns from RoundLogs to match Round_number
#taking the round number from the column names and making a separate column for it
pivotedData <- function(data){
  data_long <- data %>%
    pivot_longer(
      cols = matches("_R[1-9]$"),  # Select Answer columns with _R1 to _R9
      names_to = c(".value", "Round_number"),
      names_pattern = "(.*)_R(\\d+)"  # Extract column prefix and round number
    ) %>%
    mutate(Round_number = as.numeric(Round_number))# Ensure round numbers are numeric
  return(data_long)
}



#6
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


#all the functions should be kept separately




#the code below uses the functions to create the data frames



#EmotiBit Dame
#path to the EmotiBit folder from DameDaten
emotiBit_dame_path <- "Data/rawData/PMD/DameDaten/EmotiBit"

# List all files in the Emotibit fol folder
emotiBit_dame_file_list <- list.files(emotiBit_dame_path, full.names = TRUE)

# Initialize an empty list to store data frames
emotiBit_dame_data_list <- list()

for (file in emotiBit_dame_file_list) {
  
  #read the data from the file
  raw_data <- readData(file)
  
  #calculate average per round
  emotiBit_dame_avarage_per_round <- averagePerRound(raw_data)
  
  #make a data list from all the read data
  emotiBit_dame_data_list[[length(emotiBit_dame_data_list) + 1]] <- emotiBit_dame_avarage_per_round
}

#Bringing EmotiBit to a nice form and saving
emotiBit_dame <- combinedDataList(emotiBit_dame_data_list)


#EyeTracking Dame
#now doing the same steps for EyeTracking to have it nicely saved
#path to the EyeTracking folder from DameDaten
eyeTracking_dame_path <- "Data/rawData/PMD/DameDaten/Eye-Tracking"

eyeTracking_dame_file_list <- list.files(eyeTracking_dame_path, full.names = TRUE)

eyeTracking_dame_data_list <- list()

for(file in eyeTracking_dame_file_list) {
  
  raw_data <- readData(file)
  
  eyeTracking_dame_avarage_per_round <- averagePerRound(raw_data)
  
  eyeTracking_dame_data_list[[length(eyeTracking_dame_data_list) + 1]] <- eyeTracking_dame_avarage_per_round
}

eyeTracking_dame <- combinedDataList(eyeTracking_dame_data_list)



#working with Round Logs Dame
#saving the columns from the round logs Dame that we need, also using function to clean it
round_logs_dame <- cleanedData(selectColumnsByPatterns(roundLogs_Dame,
                                                       c("User_ID", "Answer_Q1", "Answer_Q2", "RMSSD", "SDNN")))

#pivoting it so it has a separate column for rounds, has more rows and fewer columns
round_logs_dame <- pivotedData(round_logs_dame)


#combining the Round Logs and PMD data frames i have for Dame to get a final data frame
# Perform a left join based on User_ID and Round_number
clean_dame_data <- round_logs_dame %>%
  left_join(emotiBit_dame, by = c("User_ID", "Round_number")) %>%
  left_join(eyeTracking_dame, by = c("User_ID", "Round_number"))

print(clean_dame_data)




#doing all the steps for Linne without comments cause i'm lazy :)

#EmotiBit Linne
emotiBit_linne_path <- "Data/rawData/PMD/LinneDaten/EmotiBit"

emotiBit_linne_file_list <- list.files(emotiBit_linne_path, full.names = TRUE)

emotiBit_linne_data_list <- list()

for (file in emotiBit_linne_file_list) {
  
  raw_data <- readData(file)
  
  emotiBit_linne_avarage_per_Round <- averagePerRound(raw_data)
  
  emotiBit_linne_data_list[[length(emotiBit_linne_data_list) + 1]] <- emotiBit_linne_avarage_per_round
}

emotiBit_linne <- combinedDataList(emotiBit_linne_data_list)

#EyeTracking Linne
eyeTracking_linne_path <- "Data/rawData/PMD/LinneDaten/Eye-Tracking"

eyeTracking_linne_file_list <- list.files(eyeTracking_linne_path, full.names = TRUE)

eyeTracking_linne_data_list <- list()

for(file in eyeTracking_linne_file_list) {
  
  raw_data <- readData(file)
  
  eyeTracking_linne_avarage_per_round <- averagePerRound(raw_data)
  
  eyeTracking_linne_data_list[[length(eyeTracking_linne_data_list) + 1]] <- eyeTracking_linne_avarage_per_round
}

eyeTracking_linne <- combinedDataList(eyeTracking_linne_data_list)
print(eyeTracking_linne)

#working with Round Logs Linne
round_logs_linne <- cleanedData(selectColumnsByPatterns(roundLogs_Linne,
                                                        c("User_ID", "Answer_Q1", "Answer_Q2", "RMSSD", "SDNN")))
#selecting only the first 9 rounds for Linne
round_logs_linne <- round_logs_linne %>%
  select(User_ID,
         matches("_R[1-9]$")
  )

round_logs_linne <- pivotedData(round_logs_linne)

#combining PMD data first and filtering only the first 9 rounds, cause the last two were test rounds
clean_linne_data <- emotiBit_linne %>%
  left_join(eyeTracking_linne, by = c("User_ID", "Round_number")) %>%
  filter(Round_number <= 9)

clean_linne_data <- round_logs_linne %>%
  left_join(clean_linne_data, by = c("User_ID", "Round_number"))
print(clean_linne_data)





#finally making a data frame with both Cohorts
dame_linne_combined <- bind_rows(
  clean_linne_data, clean_dame_data
)

print(dame_linne_combined)




#calculations for correlation
# Select numeric columns (exclude Answer_Q1 and Answer_Q2 temporarily, also round number)
measurement_columns <- dame_linne_combined %>%
  select(where(is.numeric)) %>%
  select(-c(Answer_Q1, Answer_Q2, Round_number)) %>% 
  colnames()
print(measurement_columns)

# Compute correlations between Answer_Q1 and each PMD column
cor_Q1_PMD <- dame_linne_combined %>%
  summarise(across(all_of(measurement_columns), ~ cor(Answer_Q1, .x, use = "complete.obs", method = "spearman")))
#now with Answer_Q2
cor_Q2_PMD <- dame_linne_combined %>%
  summarise(across(all_of(measurement_columns), ~ cor(Answer_Q2, .x, use = "complete.obs", method = "spearman")))

#making a tale to showcase the correlation results
combined_tibble <- bind_rows(
  cor_Q1_PMD %>% mutate(row_name = "Answer_Q1"),  # Add row name for tibble1
  cor_Q2_PMD %>% mutate(row_name = "Answer_Q2")   # Add row name for tibble2
)%>%
  select(row_name, everything())


#save it as a csv file
write.csv(combined_tibble, "tibble_output.csv", row.names = FALSE)  # Base R









#not final just trying some stuff, turned out it was pretty stupid

#for the 2nd question "over the rounds"
# Group by round_number and calculate correlation
correlation_per_round <- dame_linne_combined %>%
  group_by(Round_number) %>%
  summarise(
    across(all_of(measurement_columns), ~ cor(Answer_Q1, .x, use = "complete.obs", method = "spearman"))
  )

print(correlation_per_round)

# Reshape data to long format
data_long <- correlation_per_round %>%
  pivot_longer(
    cols = -Round_number,             # Keep Round_number as is
    names_to = "Measurement",         # Create a new column for measurement names
    values_to = "Value"               # Create a column for measurement values
  )

# Create the line plot
ggplot(data_long, aes(x = Round_number, y = Value, color = Measurement)) +
  geom_line(size = 1) +
  labs(
    title = "Measurement Trends Over Rounds",
    x = "Round Number",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = 1:9) +
  scale_color_brewer(palette = "Set1") 









#some analysis for question 3 and 4

#taking this from round logs bc its only there
linne_trainingversion <- roundLogs_Linne["Trainingsversion"]

linne_user_demographics <- bind_cols(
  selectColumnsByPatterns(userID_Linne, c("UserID", "Gender", "Age", "Weight", "Height")),
  linne_trainingversion,
)

linne_user_demographics

dame_user_demographics <- SimTLXAnswers_Dame[c("UserID", "Gender", "Age", "Weight", "Height", "Group")]

dame_user_demographics

# Boxplot for Ages
ggplot() +
  geom_boxplot(data = dame_user_demographics, aes(x = "Dame", y = Age, fill = "Dame"), alpha = 0.5) +
  geom_boxplot(data = linne_user_demographics, aes(x = "Linne", y = Age, fill = "Linne"), alpha = 0.5) +
  labs(title = "Age Distribution by Cohorts", x = "Cohort", y = "Age") +
  theme_minimal()


# Custom summary for key variables
dame_user_demographics %>%
  summarise(
    mean_dge = mean(Age, na.rm = TRUE),
    median_height = median(Height, na.rm = TRUE),
    mean_weight = mean(Weight, na.rm = TRUE),
    SD_weight = sd(Weight, na.rm = TRUE)
  )

linne_user_demographics %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    median_height = median(Height, na.rm = TRUE),
    mean_weight = mean(Weight, na.rm = TRUE),
    SD_weight = sd(Weight, na.rm = TRUE)
  )

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






# Check for NA values
colSums(is.na(dame_user_demographics))
#one NA in Weight column
cleaned_data <- na.omit(dame_user_demographics[, c("Age", "Height", "Weight", "Gender")])


#using clustering or group-based analyse to lsook for subgroups within Dame or Linne, 
# For Dame
dame_clusters <- kmeans(cleaned_data[, c("Age", "Height", "Weight")], centers = 2)
cleaned_data$Cluster <- dame_clusters$cluster


print(dame_clusters)
#two clusters: Cluster 1 contains 41 observations, and Cluster 2 contains 38 observations.
#Cluster 1 tends to have younger, shorter, and lighter participants.
#Cluster 2 tends to have older, taller, and heavier participants.
#Cluster 2 is more spread out, indicating more variability among its observations
#46.5% is moderate clustering performance.

table(cleaned_data$Cluster, cleaned_data$Gender)

# For Linne
linne_clusters <- kmeans(linne_user_demographics[, c("Age", "Height", "Weight")], centers = 2)
linne_user_demographics$Cluster <- linne_clusters$cluster

print(linne_clusters)

table(linne_user_demographics$Cluster, linne_user_demographics$Gender)


# Dame Cluster means:
#  Age   Height   Weight.                               F  M
#1 26.30233 170.8605 62.20930                         1 25 18
#2 28.58333 184.7500 86.08333                         2  2 34


#Linne Cluster means:
#  Age   Height   Weight                                F  M
#1 25.78571 181.8571 84.42857                        1  3  3
#2 24.66667 169.1667 61.00000                        2  1 13


#ig males are a bit taller and weight more in Dame, but does that really matter
#also mean age is almost the same, although from the graph we can see that there are more older people in Dame