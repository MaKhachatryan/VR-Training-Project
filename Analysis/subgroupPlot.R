source("environmentSetUp.R")
# This function creates plots for analyzing subgroups within 
# both the Linne and Dame cohorts.
# Input: Cohort file and any required physiological measurements, as well as 
# percieved stress vars (Q1 , Q2)
# Output: Scatterplots 

# Function to create scatter plot for hrv
hrvScatterPlotFunction <- function(data, col_x, col_y, col_group, col_size, cohort) {
  
  # Create threshold for RMSSD and SDNN
  high_hrv_threshold <- quantile(data[[col_y]], 0.9, na.rm = TRUE)
  data <- data %>%
    mutate(high_hrv = ifelse(!!sym(col_y) >= high_hrv_threshold, "High HRV", "Normal HRV"))
  
  # Create the plot
  ggplot(data, aes(x = !!sym(col_x), y = !!sym(col_y), color = !!sym(col_group), size = !!sym(col_size))) +
    geom_point(alpha = 0.7) +
    scale_color_viridis(option = "plasma", name = "Stress Level (Q1)") +
    scale_size_continuous(range = c(2, 10), name = "SDNN") +
    labs(
      title = paste("High HRV Subgroup Across Age Groups", "(", cohort, ")"),
      x = "Age",
      y = "RMSSD"
    ) +
    theme_minimal() +
    theme(legend.position = "right") +
    geom_text(
      data = data %>% filter(high_hrv == "High HRV"),
      aes(x = !!sym(col_x), y = !!sym(col_y), label = "High HRV"),
      inherit.aes = FALSE,
      hjust = -0.2,
      vjust = 0.5,
      size = 3,
      color = "black"
    )
}

# Function to create scatter plot Males in both training versions, to show 
#different behaviours between percieved stress and meassured heartrate
hrVsQ1ScatterPlotFunction <- function(data, col_x, col_y, col_group, cohort) {
  
  data_males <- data %>%
    filter(Gender == "M")
  
  ggplot(data_males, aes(x = !!sym(col_x), y = !!sym(col_y), color = !!sym(col_group))) +
    geom_point(alpha = 0.7, size = 2.8) +
    scale_color_viridis(option = "plasma", discrete = TRUE) +
    theme_minimal() +
    labs(
      title = paste("Heart Rate vs Perceived Stress by Training Version in Males", "(", cohort, ")"),
      x = "Perceived Stress Level (Q1)",
      y = "Heart Rate (Bpm)",
      color = "Training Version"
    )
}



# Call Plots for HRV and percievd Stress throughout Agegroups
hrvQ1ByAgeDame <- hrvScatterPlotFunction(DamePMDAndDemographicsAndAnswers, "Age", "RMSSD", "Answer_Q1", "SDNN", "Dame")
hrvQ1ByAgeLinne <- hrvScatterPlotFunction(LinnePMDAndDemographicsAndAnswers, "Age", "RMSSD", "Answer_Q1", "SDNN", "Linne")
# Call Plots for HR and percievd Stress in Males for Training versions
hrQ1TrainingMalesDame <- hrVsQ1ScatterPlotFunction(DamePMDAndDemographicsAndAnswers, "Answer_Q1", "mean_HR", "Trainingsversion", "Dame")
hrQ1TrainingMalesLinne <- hrVsQ1ScatterPlotFunction(LinnePMDAndDemographicsAndAnswers, "Answer_Q1", "mean_HR", "Trainingsversion", "Linne")


## Export plots into Result folder
if (!all(file.exists(c("Result/hrQ1TrainingMalesDame.jpeg", "Result/hrQ1TrainingMalesLinne.jpeg",
                       "Result/hrvQ1ByAgeDame.jpeg", "Result/hrvQ1ByAgeLinne.jpeg")))) {
  ggsave("Result/hrQ1TrainingMalesDame.jpeg", hrQ1TrainingMalesDame)
  ggsave("Result/hrQ1TrainingMalesLinne.jpeg", hrQ1TrainingMalesLinne)
  ggsave("Result/hrvQ1ByAgeDame", hrvQ1ByAgeDame)
  ggsave("Result/hrvQ1ByAgeLinne.jpeg", hrvQ1ByAgeLinne)
}