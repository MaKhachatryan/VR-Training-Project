source("Script/utils/createScatterPlot.R")
source("Script/utils/calculateBMI.R")
source("Script/cleanData/dataProcess.R")




##-----using BMI and making plots-------##

combined_pmd_demo_answers <- calculateBMI(combined_pmd_demo_answers)

combined_pmd_demo_answers <- combined_pmd_demo_answers[!is.na(combined_pmd_demo_answers$BMI) 
                                                       & !is.nan(combined_pmd_demo_answers$BMI), ]


#########
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_HR", "Trainingsversion")
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_HR", "Gender")


scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_IBI", "Trainingsversion")
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_IBI", "Gender")

scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_SCL_Raw", "Trainingsversion")
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_SCL_Raw", "Gender")

scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_SCR_amplitude_raw", "Trainingsversion") #
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_SCR_amplitude_raw", "Gender")


scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_SCR_frequency_raw", "Trainingsversion")
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_SCR_frequency_raw", "Gender")

scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_SCR_rise_time_raw", "Trainingsversion")
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_SCR_rise_time_raw", "Gender")

scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_Blink_rate_last_minute", "Trainingsversion")
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_Blink_rate_last_minute", "Gender")

scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_Saccade_amplitude_raw", "Trainingsversion")
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_Saccade_amplitude_raw", "Gender")

scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_Saccade_velocity_raw", "Trainingsversion")
scatterPlotFunction(combined_pmd_demo_answers, "BMI", "mean_Saccade_velocity_raw", "Gender")










#######################for Linne###########################################


scatterPlotFunction(linne_pmd_demo_answers, "BMI", "Answer_Q1", "Gender")

scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_HR", "Trainingsversion")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_IBI", "Trainingsversion")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_SCL_Raw", "Trainingsversion")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_SCR_amplitude_raw", "Trainingsversion") #
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_SCR_frequency_raw", "Trainingsversion")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_SCR_rise_time_raw", "Trainingsversion")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_Blink_rate_last_minute", "Trainingsversion")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_Saccade_amplitude_raw", "Trainingsversion")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_Saccade_velocity_raw", "Trainingsversion")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "RMSSD", "Trainingsversion")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "SDNN", "Trainingsversion")


scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_HR", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_IBI", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_SCL_Raw", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_SCR_amplitude_raw", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_SCR_frequency_raw", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_SCR_rise_time_raw", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_Blink_rate_last_minute", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_Saccade_amplitude_raw", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "mean_Saccade_velocity_raw", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "RMSSD", "Gender")
scatterPlotFunction(linne_pmd_demo_answers, "BMI", "SDNN", "Gender")


###############################for dame#################################



scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_HR", "Trainingsversion")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_IBI", "Trainingsversion")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_SCL_Raw", "Trainingsversion")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_SCR_amplitude_raw", "Trainingsversion") #
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_SCR_frequency_raw", "Trainingsversion")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_SCR_rise_time_raw", "Trainingsversion")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_Blink_rate_last_minute", "Trainingsversion")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_Saccade_amplitude_raw", "Trainingsversion")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_Saccade_velocity_raw", "Trainingsversion")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "RMSSD", "Trainingsversion")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "SDNN", "Trainingsversion")


scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_HR", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_IBI", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_SCL_Raw", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_SCR_amplitude_raw", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_SCR_frequency_raw", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_SCR_rise_time_raw", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_Blink_rate_last_minute", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_Saccade_amplitude_raw", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "mean_Saccade_velocity_raw", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "RMSSD", "Gender")
scatterPlotFunction(dame_pmd_demo_answers, "BMI", "SDNN", "Gender")





###--------density plots----------##

# Density plot for BMI
ggplot(combined_with_cohorts, aes(x = BMI, color = Cohort, fill = Cohort)) +
  geom_density(alpha = 0.4) +
  labs(title = "BMI Density by Cohort", x = "BMI", y = "Density") +
  theme_minimal()

# Density plot for Age
ggplot(combined_with_cohorts, aes(x = Age, color = Cohort, fill = Cohort)) +
  geom_density(alpha = 0.4) +
  labs(title = "Age Density by Cohort", x = "Age", y = "Density") +
  theme_minimal()


# Combined plot for BMI
ggplot(combined_with_cohorts, aes(x = BMI, fill = Cohort)) +
  geom_density(alpha = 0.4, color = NA) +
  geom_boxplot(aes(x = BMI, group = Cohort), width = 0.2, position = position_dodge(0.8), color = "black") +
  labs(title = "BMI Distribution with Boxplot Overlay", x = "BMI", y = "Density") +
  theme_minimal()

# Combined plot for Age
ggplot(combined_with_cohorts, aes(x = Age, fill = Cohort)) +
  geom_density(alpha = 0.4, color = NA) +
  geom_boxplot(aes(x = Age, group = Cohort), width = 0.2, position = position_dodge(0.8), color = "black") +
  labs(title = "Age Distribution with Boxplot Overlay", x = "Age", y = "Density") +
  theme_minimal()


# Density plot for BMI by Training Version (faceted by Cohort)
ggplot(combined_with_cohorts, aes(x = BMI, color = Trainingsversion, fill = Trainingsversion)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ Cohort) +
  labs(title = "BMI Density by Training Version and Cohort", x = "BMI", y = "Density") +
  theme_minimal()

