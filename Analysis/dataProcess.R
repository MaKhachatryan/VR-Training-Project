source("environmentSetUp.R")
# This script processes physiological measurement data (PMD), cognitive and physical load responses, 
# and demographic information for two cohorts: Dame and Linne. 
# It loads, cleans, and merges data from multiple sources, calculates relevant metrics, 
# and saves the processed datasets for further analysis. 
# Final outputs include combined and cohort-specific datasets, with and without round-level data.  

# Load raw data for both cohorts (Dame and Linne)
roundLogs_Dame <- read_excel("Data/rawData/roundLogs/roundLogs_Dame.xlsx")
roundLogs_Linne <- read_delim("Data/rawData/roundLogs/roundLogs_Linne.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE,
                              locale = locale(decimal_mark = ","))
SimTLXAnswers_Dame <- read_excel("Data/rawData/SimTLXAnswers_Dame.xlsx")
userID_Linne <- read_delim("Data/rawData/userID_Linne.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)



##################################################################
# Process EmotiBit data for Dame

emotiBit_dame_path <- "Data/rawData/PMD/DameDaten/EmotiBit"

# List all files in the Emotibit fol folder
emotiBit_dame_file_list <- list.files(emotiBit_dame_path, full.names = TRUE)

# Initialize an empty list to store data frames
emotiBit_dame_data_list <- list()

for (file in emotiBit_dame_file_list) {
  raw_data <- readData(file)
  emotiBit_dame_data_list[[length(emotiBit_dame_data_list) + 1]] <- averagePerRound(raw_data)
}

emotiBit_dame <- combinedDataList(emotiBit_dame_data_list)




# Process Eye-Tracking data for Dame

eyeTracking_dame_path <- "Data/rawData/PMD/DameDaten/Eye-Tracking"

eyeTracking_dame_file_list <- list.files(eyeTracking_dame_path, full.names = TRUE)

eyeTracking_dame_data_list <- list()

for(file in eyeTracking_dame_file_list) {
  
  raw_data <- readData(file)
  eyeTracking_dame_data_list[[length(eyeTracking_dame_data_list) + 1]] <- averagePerRound(raw_data)
}

eyeTracking_dame <- combinedDataList(eyeTracking_dame_data_list)




# Process and clean round logs for Dame
round_logs_dame <- cleanedData(selectColumnsByPatterns(roundLogs_Dame,
                                                       c("User_ID", "Answer_Q1", "Answer_Q2", "RMSSD", "SDNN")))
round_logs_dame <- pivotedData(round_logs_dame)



# Merge round logs with EmotiBit and Eye-Tracking data
clean_dame_data <- round_logs_dame %>%
  left_join(emotiBit_dame, by = c("User_ID", "Round_number")) %>%
  left_join(eyeTracking_dame, by = c("User_ID", "Round_number")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))



##################################################################
# Repeat the same processing steps for Linne cohort

# Process EmotiBit data for Linne
emotiBit_linne_path <- "Data/rawData/PMD/LinneDaten/EmotiBit"

emotiBit_linne_file_list <- list.files(emotiBit_linne_path, full.names = TRUE)

emotiBit_linne_data_list <- list()

for (file in emotiBit_linne_file_list) {
  
  raw_data <- readData(file)
  emotiBit_linne_data_list[[length(emotiBit_linne_data_list) + 1]] <- averagePerRound(raw_data)
}

emotiBit_linne <- combinedDataList(emotiBit_linne_data_list)


# Process Eye-Tracking data for Linne
eyeTracking_linne_path <- "Data/rawData/PMD/LinneDaten/Eye-Tracking"

eyeTracking_linne_file_list <- list.files(eyeTracking_linne_path, full.names = TRUE)

eyeTracking_linne_data_list <- list()

for(file in eyeTracking_linne_file_list) {
  
  raw_data <- readData(file)
  eyeTracking_linne_data_list[[length(eyeTracking_linne_data_list) + 1]] <- averagePerRound(raw_data)
}

eyeTracking_linne <- combinedDataList(eyeTracking_linne_data_list)


# Process and clean round logs for Linne
round_logs_linne <- cleanedData(selectColumnsByPatterns(roundLogs_Linne,
                                                        c("User_ID", "Answer_Q1", "Answer_Q2", "RMSSD", "SDNN")))
round_logs_linne <- pivotedData(round_logs_linne)


# Merge round logs with EmotiBit and Eye-Tracking data
clean_linne_data <- round_logs_linne %>%
  left_join(emotiBit_linne, by = c("User_ID", "Round_number")) %>%
  left_join(eyeTracking_linne, by = c("User_ID", "Round_number"))%>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))



#########################################################################
# Combine both cohorts into a single dataset
dame_linne_combined <- bind_rows(clean_linne_data, clean_dame_data) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# Save the processed dataset
write_csv(dame_linne_combined, "Data/ProcessedData/combinedPMDAndRoundLogs.csv")



################################################################################
# Process and merge demographic data with cognitive load and physical load answers

# Extract and clean Dame demographics
dame_user_demographics <- SimTLXAnswers_Dame[c("UserID", "Gender", "Age", "Weight", "Height", "Group")]

dame_user_demographics <- na.omit(dame_user_demographics[, c("UserID", "Age", "Height", "Weight", "Gender", "Group")])
dame_user_demographics <- dame_user_demographics %>%
  rename(Trainingsversion = Group)

# Calculate mean cognitive and physical load per user for Dame
dame_demo_answers <- clean_dame_data %>%
  group_by(User_ID) %>%
  summarise(
    Mean_Answer_Q1 = mean(Answer_Q1, na.rm = TRUE),
    Mean_Answer_Q2 = mean(Answer_Q2, na.rm = TRUE)
  ) %>%
  inner_join(dame_user_demographics, by = c("User_ID" = "UserID")) %>%
  filter(Trainingsversion != "Control") %>%
  select(
    User_ID,
    Mean_Answer_Q1,
    Mean_Answer_Q2,
    Gender,
    Age,
    Weight,
    Height,
    Trainingsversion
  )

# Round all numeric columns to 3 decimal places
dame_demo_answers <- dame_demo_answers %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))




# Extract Linne demographics
linne_trainingversion <- roundLogs_Linne["Trainingsversion"]

linne_user_demographics <- bind_cols(
  selectColumnsByPatterns(userID_Linne, c("UserID", "Gender", "Age", "Weight", "Height")),
  linne_trainingversion,
) 

# Calculate mean cognitive and physical load per user for Linne
linne_demo_answers <- clean_linne_data %>%
  group_by(User_ID) %>%
  summarise(
    Mean_Answer_Q1 = mean(Answer_Q1, na.rm = TRUE),
    Mean_Answer_Q2 = mean(Answer_Q2, na.rm = TRUE)
  ) %>%
  inner_join(linne_user_demographics, by = c("User_ID" = "UserID")) %>%
  select(
    User_ID,
    Mean_Answer_Q1,
    Mean_Answer_Q2,
    Gender,
    Age,
    Weight,
    Height,
    Trainingsversion
  )

# Round all numeric columns to 3 decimal places
linne_demo_answers <- linne_demo_answers %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# Combine demographic and answer data for both cohorts
combined_demo_answers <- bind_rows(
  linne_demo_answers,
  dame_demo_answers 
) 


################################################################################
# Create datasets combining PMD, demographics, and cognitive load/physical load answers


# Filter out the unwanted rows from combined_demo_answers
filtered_demo_answers <- combined_demo_answers %>%
  select(-Mean_Answer_Q1, -Mean_Answer_Q2)

# Join the filtered data frame with dame_linne_combined by User_ID
combined_pmd_demo_answers_by_rounds <- dame_linne_combined %>%
  left_join(filtered_demo_answers, by = "User_ID")


# Aggregate PMD data per user, merging with demographic information
combined_pmd_demo_answers <- combined_pmd_demo_answers_by_rounds %>%
  group_by(User_ID) %>%  # Group by User_ID
  summarise(
    across(where(is.numeric) & !all_of("Round_number"), ~ mean(.x, na.rm = TRUE)), 
    across(where(is.character), ~ first(.x))  # Retain first non-NA value for character columns
  ) %>%
  ungroup()  

# Round all numeric columns to 3 decimal places
combined_pmd_demo_answers <- combined_pmd_demo_answers %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))


write_csv(combined_pmd_demo_answers,
          "Data/processedData/combinedPMDAndDemographicsAndAnswers.csv")


#creating data frames with PMD, Demographics and Answers for both Cohorts separately 

#-----------DAME----------#
dame_pmd_demo_answers_by_rounds <- dame_demo_answers %>%
  select(-Mean_Answer_Q1, -Mean_Answer_Q2) %>%
  inner_join(clean_dame_data, by = "User_ID")

dame_pmd_demo_answers <- dame_pmd_demo_answers_by_rounds %>%
  group_by(User_ID) %>%  
  summarise(
    across(where(is.numeric) & !all_of("Round_number"), ~ mean(.x, na.rm = TRUE)),  
    across(where(is.character), ~ first(.x))  
  ) %>%
  ungroup()  

# Round all numeric columns to 3 decimal places
dame_pmd_demo_answers <- dame_pmd_demo_answers %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write_csv(dame_pmd_demo_answers,
          "Data/processedData/DamePMDAndDemographicsAndAnswers.csv")


#-----------LINNE----------#
linne_pmd_demo_answers_by_rounds <- linne_demo_answers %>%
  select(-Mean_Answer_Q1, -Mean_Answer_Q2) %>%
  inner_join(clean_linne_data, by = "User_ID")

linne_pmd_demo_answers <- linne_pmd_demo_answers_by_rounds %>%
  group_by(User_ID) %>%  
  summarise(
    across(where(is.numeric) & !all_of("Round_number"), ~ mean(.x, na.rm = TRUE)),  
    across(where(is.character), ~ first(.x))  
  ) %>%
  ungroup() 

# Round all numeric columns to 3 decimal places
linne_pmd_demo_answers <- linne_pmd_demo_answers %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))


write_csv(linne_pmd_demo_answers,
          "Data/processedData/LinnePMDAndDemographicsAndAnswers.csv")



# making a combined data frame with this new created ones and adding a "Cohort" column
# for further combined analysis
combined_with_cohorts <- bind_rows(
  linne_pmd_demo_answers %>% mutate(Cohort = "Linne"),
  dame_pmd_demo_answers %>% mutate(Cohort = "Dame")
) 
combined_with_cohorts <- calculateBMI(combined_with_cohorts)

write_csv(combined_with_cohorts,
          "Data/processedData/combinedDataWithCohorts.csv")

combined_demo_with_cohorts <- bind_rows(
  linne_user_demographics %>% mutate(Cohort = "Linne"),
  dame_user_demographics %>% mutate(Cohort = "Dame")
) 
combined_demo_with_cohorts <- calculateBMI(combined_demo_with_cohorts)

write_csv(combined_demo_with_cohorts,
          "Data/processedData/combinedDemoWithCohorts.csv")
