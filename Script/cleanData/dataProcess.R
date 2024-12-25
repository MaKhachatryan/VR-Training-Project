#the code below uses the functions to create the data frames
install.packages("readxl")
library(readxl)


source("Script/utils/fixPMD.R")
source("Script/utils/pivotedData.R")
source("Script/utils/cleanData.R")

roundLogs_Dame <- read_excel("Data/rawData/roundLogs/roundLogs_Dame.xlsx")
roundLogs_Linne <- read_delim("Data/rawData/roundLogs/roundLogs_Linne.csv", 
                              delim = ";", escape_double = FALSE, trim_ws = TRUE)
SimTLXAnswers_Dame <- read_excel("Data/rawData/SimTLXAnswers_Dame.xlsx")
userID_Linne <- read_delim("Data/rawData/userID_Linne.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

combinedCohorts <- read_csv("Data/processedData/combinedDemographicsAndAnswers.csv")


##################################################################
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


clean_dame_data <- clean_dame_data %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))





#################################################################
#doing all the steps for Linne 

#EmotiBit Linne
emotiBit_linne_path <- "Data/rawData/PMD/LinneDaten/EmotiBit"

emotiBit_linne_file_list <- list.files(emotiBit_linne_path, full.names = TRUE)

emotiBit_linne_data_list <- list()

for (file in emotiBit_linne_file_list) {
  
  raw_data <- readData(file)
  
  emotiBit_linne_avarage_per_round <- averagePerRound(raw_data)
  
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





#working with Round Logs Linne
round_logs_linne <- cleanedData(selectColumnsByPatterns(roundLogs_Linne,
                                                        c("User_ID", "Answer_Q1", "Answer_Q2", "RMSSD", "SDNN")))

round_logs_linne <- pivotedData(round_logs_linne)



clean_linne_data <- round_logs_linne %>%
  left_join(emotiBit_linne, by = c("User_ID", "Round_number")) %>%
  left_join(eyeTracking_linne, by = c("User_ID", "Round_number"))

clean_linne_data <- clean_linne_data %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))




#########################################################################
#finally making a data frame with both Cohorts
dame_linne_combined <- bind_rows(
  clean_linne_data, clean_dame_data
)


# Round all numeric columns to 3 decimal places
dame_linne_combined <- dame_linne_combined %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))


write_csv(dame_linne_combined, "Data/ProcessedData/combinedPMDAndRoundLogs.csv")



################################################################################
#working with demographic and Answer_Q1, Answer_Q2 data

#importing the demographics to combine with pmd
#taking this from round logs bc its only there

dame_user_demographics <- SimTLXAnswers_Dame[c("UserID", "Gender", "Age", "Weight", "Height", "Group")]

dame_user_demographics <- na.omit(dame_user_demographics[, c("UserID", "Age", "Height", "Weight", "Gender", "Group")])
dame_user_demographics <- dame_user_demographics %>%
  filter(Group != "Control") %>%
  rename(Trainingsversion = Group)

dame_demo_answers <- clean_dame_data %>%
  group_by(User_ID) %>%
  summarise(
    Mean_Answer_Q1 = mean(Answer_Q1, na.rm = TRUE),
    Mean_Answer_Q2 = mean(Answer_Q2, na.rm = TRUE)
  ) %>%
  inner_join(dame_user_demographics, by = c("User_ID" = "UserID")) %>%
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
dame_demo_answers

write_csv(dame_demo_answers, "Data/processedData/DameDemographicsAndAnswers.csv")





################################################################################
linne_trainingversion <- roundLogs_Linne["Trainingsversion"]

linne_user_demographics <- bind_cols(
  selectColumnsByPatterns(userID_Linne, c("UserID", "Gender", "Age", "Weight", "Height")),
  linne_trainingversion,
) 

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

linne_demo_answers

write_csv(linne_demo_answers, "Data/processedData/LinneDemographicsAndAnswers.csv")



###############################################################################
combined_demo_answers <- bind_rows(
  linne_demo_answers, dame_demo_answers) 


write_csv(combined_demo_answers, "Data/processedData/combinedDemographicsAndAnswers.csv")


################################################################################
#creating data frames with PMD, Demographics and Answers
#with round numbers

# Filter out the unwanted rows from combined_demo_answers
filtered_demo_answers <- combined_demo_answers %>%
  filter(!User_ID %in% c("Mean_Answer_Q1", "Mean_Answer_Q2"))

# Join the filtered data frame with dame_linne_combined by User_ID
combined_pmd_demo_answers_by_rounds <- dame_linne_combined %>%
  left_join(filtered_demo_answers, by = "User_ID")



write_csv(combined_pmd_demo_answers_by_rounds,
          "Data/processedData/combinedPMDAndDemographicsAndAnswersByRounds.csv")


###########################
#without rounds
combined_pmd_demo_answers <- combined_pmd_demo_answers_by_rounds %>%
  group_by(User_ID) %>%  # Group by User_ID
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%  # Calculate mean for numeric columns
  ungroup()  # Remove grouping structure

# Round all numeric columns to 3 decimal places
combined_pmd_demo_answers <- combined_pmd_demo_answers %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))


write_csv(combined_pmd_demo_answers,
          "Data/processedData/combinedPMDAndDemographicsAndAnswers.csv")



