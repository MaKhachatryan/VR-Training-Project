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
