library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(cluster)



#1
#function to read the data we need from the file
read_Data <- function(file){
  
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
average_Per_Round <- function(data){
  
  # Dynamically generate the mean column name
  mean_col_name <- paste0("mean_", colnames(data)[3])
  
  #calculate the avarage per round
  avg_Values_Df <- data %>%
    mutate(
      Round_number = abs(as.numeric(Round_number)),  # Ensure 'Round_number' is numeric
    ) %>%
    group_by(Round_number) %>%
    summarise(
      !!mean_col_name := mean(.data[[colnames(data)[3]]], na.rm = TRUE),
      User_ID = first(User_ID),  # Keep User_ID in the summarised data
      .groups = "drop"
    )
  return(avg_Values_Df)
}



#3
# Ensure all elements in the data list are data frames
# make sure every data frame has all rounds, and the rounds go from 1 till 9
# Combine all data frames into one and give it a nice structure
combined_Data_List <- function(df_list){
  
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
select_columns_by_patterns <- function(data, patterns) {
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
pivoted_Data <- function(data){
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
cleaned_Data <- function(dataFrame) {
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
EmotiBit_Dame_Path <- "Data/rawData/PMD/DameDaten/EmotiBit"

# List all files in the Emotibit fol folder
EmotiBit_Dame_File_List <- list.files(EmotiBit_Dame_Path, full.names = TRUE)

# Initialize an empty list to store data frames
EmotiBit_Dame_Data_List <- list()

for (file in EmotiBit_Dame_File_List) {
  
  #read the data from the file
   raw_Data <- read_Data(file)
   
   #calculate average per round
   EmotiBit_Dame_Avarage_Per_Round <- average_Per_Round(raw_Data)
   
   #make a data list from all the read data
   EmotiBit_Dame_Data_List[[length(EmotiBit_Dame_Data_List) + 1]] <- EmotiBit_Dame_Avarage_Per_Round
}

#Bringing EmotiBit to a nice form and saving
EmotiBit_Dame <- combined_Data_List(EmotiBit_Dame_Data_List)
print(EmotiBit_Dame)

#EyeTracking Dame
#now doing the same steps for EyeTracking to have it nicely saved
#path to the EyeTracking folder from DameDaten
EyeTracking_Dame_Path <- "Data/rawData/PMD/DameDaten/Eye-Tracking"

EyeTracking_Dame_File_List <- list.files(EyeTracking_Dame_Path, full.names = TRUE)

EyeTracking_Dame_Data_List <- list()

for(file in EyeTracking_Dame_File_List) {
  
  raw_Data <- read_Data(file)
  
  EyeTracking_Dame_Avarage_Per_Round <- average_Per_Round(raw_Data)
  
  EyeTracking_Dame_Data_List[[length(EyeTracking_Dame_Data_List) + 1]] <- EyeTracking_Dame_Avarage_Per_Round
}

EyeTracking_Dame <- combined_Data_List(EyeTracking_Dame_Data_List)



#working with Round Logs Dame
#saving the columns from the round logs Dame that we need, also using function to clean it
round_Logs_Dame <- cleaned_Data(select_columns_by_patterns(roundLogs_Dame,
                                              c("User_ID", "Answer_Q1", "Answer_Q2", "RMSSD", "SDNN")))
print(round_Logs_Dame, n=Inf)
#pivoting it so it has a separate column for rounds, has more rows and fewer columns
round_Logs_Dame <- pivoted_Data(round_Logs_Dame)


#combining the Round Logs and PMD data frames i have for Dame to get a final data frame
# Perform a left join based on User_ID and Round_number
clean_Dame_Data <- round_Logs_Dame %>%
  left_join(EmotiBit_Dame, by = c("User_ID", "Round_number")) %>%
  left_join(EyeTracking_Dame, by = c("User_ID", "Round_number"))

print(clean_Dame_Data)




#doing all the steps for Linne without comments cause i'm lazy :)

#EmotiBit Linne
EmotiBit_Linne_Path <- "Data/rawData/PMD/LinneDaten/EmotiBit"

EmotiBit_Linne_File_List <- list.files(EmotiBit_Linne_Path, full.names = TRUE)

EmotiBit_Linne_Data_List <- list()

for (file in EmotiBit_Linne_File_List) {
  
  raw_Data <- read_Data(file)
  
  EmotiBit_Linne_Avarage_Per_Round <- average_Per_Round(raw_Data)
  
  EmotiBit_Linne_Data_List[[length(EmotiBit_Linne_Data_List) + 1]] <- EmotiBit_Linne_Avarage_Per_Round
}

EmotiBit_Linne <- combined_Data_List(EmotiBit_Linne_Data_List)

#EyeTracking Linne
EyeTracking_Linne_Path <- "Data/rawData/PMD/LinneDaten/Eye-Tracking"

EyeTracking_Linne_File_List <- list.files(EyeTracking_Linne_Path, full.names = TRUE)

EyeTracking_Linne_Data_List <- list()

for(file in EyeTracking_Linne_File_List) {
  
  raw_Data <- read_Data(file)
  
  EyeTracking_Linne_Avarage_Per_Round <- average_Per_Round(raw_Data)
  
  EyeTracking_Linne_Data_List[[length(EyeTracking_Linne_Data_List) + 1]] <- EyeTracking_Linne_Avarage_Per_Round
}

EyeTracking_Linne <- combined_Data_List(EyeTracking_Linne_Data_List)
print(EyeTracking_Linne)

#working with Round Logs Linne
round_Logs_Linne <- cleaned_Data(select_columns_by_patterns(roundLogs_Linne,
                                                           c("User_ID", "Answer_Q1", "Answer_Q2", "RMSSD", "SDNN")))
#selecting only the first 9 rounds for Linne
round_Logs_Linne <- round_Logs_Linne %>%
  select(User_ID,
    matches("_R[1-9]$")
  )

round_Logs_Linne <- pivoted_Data(round_Logs_Linne)

#combining PMD data first and filtering only the first 9 rounds, cause the last two were test rounds
clean_Linne_Data <- EmotiBit_Linne %>%
  left_join(EyeTracking_Linne, by = c("User_ID", "Round_number")) %>%
  filter(Round_number <= 9)

clean_Linne_Data <- round_Logs_Linne %>%
  left_join(clean_Linne_Data, by = c("User_ID", "Round_number"))
print(clean_Linne_Data)





#finally making a data frame with both Cohorts
Dame_Linne_combined <- bind_rows(
  clean_Linne_Data, clean_Dame_Data
)

print(Dame_Linne_combined)




#calculations for correlation
# Select numeric columns (exclude Answer_Q1 and Answer_Q2 temporarily, also round number)
measurement_columns <- Dame_Linne_combined %>%
  select(where(is.numeric)) %>%
  select(-c(Answer_Q1, Answer_Q2, Round_number)) %>% 
  colnames()
print(measurement_columns)

# Compute correlations between Answer_Q1 and each PMD column
cor_Q1_PMD <- Dame_Linne_combined %>%
  summarise(across(all_of(measurement_columns), ~ cor(Answer_Q1, .x, use = "complete.obs", method = "spearman")))
#now with Answer_Q2
cor_Q2_PMD <- Dame_Linne_combined %>%
  summarise(across(all_of(measurement_columns), ~ cor(Answer_Q2, .x, use = "complete.obs", method = "spearman")))

#making a tale to showcase the correlation results
combined_tibble <- bind_rows(
  cor_Q1_PMD %>% mutate(Row_Name = "Answer_Q1"),  # Add row name for tibble1
  cor_Q2_PMD %>% mutate(Row_Name = "Answer_Q2")   # Add row name for tibble2
)%>%
  select(Row_Name, everything())


# View the final table, save it as a csv file
print(final_tibble)
write.csv(final_tibble, "tibble_output.csv", row.names = FALSE)  # Base R









#not final just trying some stuff, turned out it was pretty stupid

#for the 2nd question "over the rounds"
# Group by round_number and calculate correlation
correlation_Per_Round <- Dame_Linne_combined %>%
  group_by(Round_number) %>%
  summarise(
    across(all_of(measurement_columns), ~ cor(Answer_Q1, .x, use = "complete.obs", method = "spearman"))
  )

print(correlation_Per_Round)

# Reshape data to long format
data_long <- correlation_Per_Round %>%
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
Linne_Trainingversion <- roundLogs_Linne["Trainingsversion"]

Linne_User_Demographics <- bind_cols(
  select_columns_by_patterns(userID_Linne, c("UserID", "Gender", "Age", "Weight", "Height")),
  Linne_Trainingversion,
)

Linne_User_Demographics

Dame_User_Demographics <- SimTLXAnswers_Dame[c("UserID", "Gender", "Age", "Weight", "Height", "Group")]
                                                     

Dame_User_Demographics

# Boxplot for Ages
ggplot() +
  geom_boxplot(data = Dame_User_Demographics, aes(x = "Dame", y = Age, fill = "Dame"), alpha = 0.5) +
  geom_boxplot(data = Linne_User_Demographics, aes(x = "Linne", y = Age, fill = "Linne"), alpha = 0.5) +
  labs(title = "Age Distribution by Cohorts", x = "Cohort", y = "Age") +
  theme_minimal()


# Custom summary for key variables
Dame_User_Demographics %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Height = median(Height, na.rm = TRUE),
    Mean_Weight = mean(Weight, na.rm = TRUE),
    SD_Weight = sd(Weight, na.rm = TRUE)
  )

Linne_User_Demographics %>%
  summarise(
    Mean_Age = mean(Age, na.rm = TRUE),
    Median_Height = median(Height, na.rm = TRUE),
    Mean_Weight = mean(Weight, na.rm = TRUE),
    SD_Weight = sd(Weight, na.rm = TRUE)
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
  Dame_User_Demographics %>% mutate(Group = "Dame"),
  Linne_User_Demographics %>% mutate(Group = "Linne")
)

manova_results <- manova(cbind(Age, Height, Weight) ~ Group, data = combined_data)
summary(manova_results)






# Check for NA values
colSums(is.na(Dame_User_Demographics))
#one NA in Weight column
cleaned_data <- na.omit(Dame_User_Demographics[, c("Age", "Height", "Weight", "Gender")])


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
linne_clusters <- kmeans(Linne_User_Demographics[, c("Age", "Height", "Weight")], centers = 2)
Linne_User_Demographics$Cluster <- linne_clusters$cluster

print(linne_clusters)

table(Linne_User_Demographics$Cluster, Linne_User_Demographics$Gender)


# Dame Cluster means:
#  Age   Height   Weight.                               F  M
#1 26.30233 170.8605 62.20930                           25 18
#2 28.58333 184.7500 86.08333                           2 34


#Linne Cluster means:
#  Age   Height   Weight                                F  M
#1 25.78571 181.8571 84.42857                           3  3
#2 24.66667 169.1667 61.00000                           1 13


#ig males are a bit taller and weight more in Dame, but does that really matter
#also mean age is almost the same, although from the graph we can see that there are more older people in Dame

cleaned_data <- na.omit(Dame_User_Demographics[, c("UserID", "Age", "Height", "Weight", "Gender", "Group")])
cleaned_data <- cleaned_data %>%
  filter(Group != "Control") %>%
  rename(User_ID = UserID)


cleaned_data




# Summarize Dame
Dame_counts <- cleaned_data %>%
  group_by(Gender) %>%
  summarise(Count = n(), Cohort = "Dame", .groups = "drop")

# Summarize Linne
Linne_counts <- Linne_User_Demographics %>%
  group_by(Gender) %>%
  summarise(Count = n(), Cohort = "Linne", .groups = "drop")

# Combine summaries
gender_counts <- bind_rows(Dame_counts, Linne_counts)

# Plot
ggplot(gender_counts, aes(x = Cohort, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Gender Distribution by Group",
    x = "Group",
    y = "Number of Participants"
  ) +
  theme_minimal()+
  scale_y_continuous(breaks = seq(0, max(gender_counts$Count), by = 5)) 

#linne 3/13.   0.23
#dame 20/33.   0.6

dame_idk <- clean_Dame_Data %>%
  select("User_ID", "Round_number", "Answer_Q1", "Answer_Q2") %>%
   group_by(User_ID) %>%
  left_join(cleaned_data, by = "User_ID") %>%
  filter(!is.na(Gender))

#df has columns user ID, Round, number, answer Q1 and Q2 
dame_idk

#boxplot with genders and their physical load
ggplot(dame_idk, aes(x = Gender, y = Answer_Q2, fill = Gender)) +
  geom_boxplot(alpha = 0.7, outlier.size = 2) +
  labs(
    title = "Comparison of Physical Load Perception (Answer Q2) by Gender",
    x = "Gender",
    y = "Answer Q2"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("F" = "pink", "M" = "blue")) +
  theme(legend.position = "none")

#line plot about the physical load and gender 
ggplot(dame_idk, aes(x = Round_number, y = Answer_Q2, color = Gender, group = Gender)) +
  geom_line(stat = "summary", fun = mean, size = 1.5) +
  geom_point(stat = "summary", fun = mean, size = 2) +
  labs(
    title = "Physical Load Perception (Answer Q2) Over Rounds by Gender",  
    x = "Round Number",
    y = "Mean Answer Q2"
  ) +
  scale_x_continuous(breaks = 1:9)+
  theme_minimal() +
  scale_color_manual(values = c("F" = "pink", "M" = "blue")) +
  theme(legend.title = element_blank())




# Grouped Bar Plot, for gender, training type and answer q2
ggplot(dame_idk, aes(x = Gender, y = Answer_Q2, fill = Group)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +a
  labs(
    title = "Physical Load Perception (Answer Q2) by Gender and Training Type",
    x = "Gender",
    y = "Mean Answer Q2",
    fill = "Training Type"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Adaptive" = "yellow", "NonAdaptive" = "green"))


