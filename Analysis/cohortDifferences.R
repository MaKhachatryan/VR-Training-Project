#Maga
#plots for cohort differences
#plots are official, but anything that has to do with sourcing still needs
#to be changed a bit, i had a problem with sourceing the environment set up

source("Script/cleanData/dataProcess.R")
source("Script/utils/calculateBMI.R")


##---------plots for question 4-----------##
##---------cohort comparison--------------##
combined_with_cohorts <- calculateBMI(combined_with_cohorts)

## Violin plot for Age by Training Version (faceted by Cohort)
ageVariability <- ggplot(combined_demo_with_cohorts, aes(x = Trainingsversion, y = Age, fill = Trainingsversion)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  facet_wrap(~ Cohort, scales = "free_x") +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  labs(title = "Age Variability by Training Version and Cohort", x = "Training Version", y = "Age") +
  theme_minimal()+
  theme(
    strip.text = element_text(size = 14, face = "bold") # Adjust size and style of facet labels
  )




## Filter data to exclude "Control" group
filtered_combined_data <- combined_with_cohorts %>%
  filter(!(Trainingsversion == "Control" & Cohort == "Dame"))

# Faceted bar chart for gender distribution
genderDistribution <- ggplot(filtered_combined_data, aes(x = Trainingsversion, fill = Gender)) +
  geom_bar(position = "fill", color = "black") +  # Use "fill" for proportional bars
  facet_wrap(~ Cohort) +
  scale_fill_manual(values = c("M" = "#AEC6CF", "F" = "#FFB6C1")) +  # Same colors as violin plot
  labs(
    title = "Gender Distribution by Training Version and Cohort",
    x = "Training Version",
    y = "Proportion",
    fill = "Gender"
  ) +
  theme_minimal()+
  theme(
    strip.text = element_text(size = 14, face = "bold") # Adjust size and style of facet labels
  )



#---comparing the age group 20-30 between cohorts---#
# Filter data for 20–30 age range
combined_data_20_30 <- combined_with_cohorts %>% filter(Age >= 20 & Age <= 30)


# Density plot for BMI
BMI <- ggplot(combined_data_20_30, aes(x = BMI, color = Cohort, fill = Cohort)) +
  geom_density(alpha = 0.4) +
  labs(title = "BMI Density (20–30 Age Range)", x = "BMI", y = "Density") +
  theme_minimal()



#-----Violin plots for Answer Q1 and Answer Q2----#

# Violin plot for Cognitive Load (Q1)
AnswerQ1 <- ggplot(combined_data_20_30, aes(x = Cohort, y = Answer_Q1, fill = Cohort)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Cognitive Load (Q1) Distribution for 20–30 Age Range", x = "Cohort", y = "Cognitive Load (Q1)") +
  theme_minimal()

# Violin plot for Physical Stress (Q2)
AnswerQ2 <- ggplot(combined_data_20_30, aes(x = Cohort, y = Answer_Q2, fill = Cohort)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  labs(title = "Physical Stress (Q2) Distribution for 20–30 Age Range", x = "Cohort", y = "Physical Stress (Q2)") +
  theme_minimal()



#----Scatter plots for eye tracking measurements----#
# Scatter plot for Saccade Amplitude vs. Velocity
eyeMesuerements <- ggplot(combined_data_20_30, aes(x = mean_Saccade_amplitude_raw, y = mean_Saccade_velocity_raw, color = Trainingsversion)) +
  geom_point(size = 3, alpha = 0.7) +
  facet_wrap(~ Cohort) +
  labs(title = "Saccade Amplitude vs Velocity by Cohort and Training Version",
       x = "Saccade Amplitude (Raw)", y = "Saccade Velocity (Raw)") +
  theme_minimal()



#----using heatmaps for some PMD values in EmotiBit----#

# Subset data and calculate correlations for each cohort
cor_data <- combined_data_20_30 %>%
  select(Cohort, mean_SCL_Raw, mean_SCR_amplitude_raw, mean_SCR_frequency_raw, mean_SCR_rise_time_raw) %>%
  group_by(Cohort) %>%
  summarise(
    Correlations = list(
      cor(select(cur_data(), mean_SCL_Raw:mean_SCR_rise_time_raw), method = "spearman", use = "complete.obs")
    )
  )

# Convert correlation matrices into long format
facet_cor_data <- cor_data %>%
  rowwise() %>%
  mutate(Correlation = list(as.data.frame(as.table(Correlations)))) %>%
  unnest(Correlation) %>%
  mutate(
    Var1 = str_replace_all(Var1, c("^mean_" = "", "_" = " ")) %>% str_replace(" [^ ]+$", ""),
    Var2 = str_replace_all(Var2, c("^mean_" = "", "_" = " ")) %>% str_replace(" [^ ]+$", "")
  )


#heatmap
heatmaps <- ggplot(facet_cor_data, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "#E7A4B7", mid = "#F0F0F0", high = "#7BB274", midpoint = 0) +
  facet_wrap(~ Cohort) +
  labs(title = "Faceted Correlation Heatmap by Cohort",
       x = "Metric", y = "Metric", fill = "Correlation") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(angle = 0)
  )+
  theme(
    strip.text = element_text(size = 14, face = "bold") 
  )




# Boxplot for RMSSD by Cohort and Training Version
RMSSD <- ggplot(combined_data_20_30, aes(x = Trainingsversion, y = RMSSD, fill = Cohort)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  facet_wrap(~ Cohort) +
  labs(title = "RMSSD by Training Version and Cohort", x = "Training Version", y = "RMSSD (ms)") +
  theme_minimal()

# Boxplot for SDNN by Cohort and Training Version
SDNN <- ggplot(combined_data_20_30, aes(x = Trainingsversion, y = SDNN, fill = Cohort)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  facet_wrap(~ Cohort) +
  labs(title = "SDNN by Training Version and Cohort", x = "Training Version", y = "SDNN (ms)") +
  theme_minimal()


## Export plots into Result folder
if (!all(file.exists(c("Result/Q4/ageVariability.png", "Result/Q4/genderDistribution.png",
                       "Result/Q4/BMI.png", "Result/Q4/AnswerQ1.png", "Result/Q4/AnswerQ2.png",
                       "Result/Q4/eyeMesuerements.png", "Result/Q4/heatmaps.png", 
                       "Result/Q4/RMSSD.png", "Result/Q4/SDNN.png")))) {
  ggsave("Result/Q4/ageVariability.png", ageVariability)
  ggsave("Result/Q4/genderDistribution.png", genderDistribution)
  ggsave("Result/Q4/BMI.png", BMI)
  ggsave("Result/Q4/AnswerQ1.png", AnswerQ1)
  ggsave("Result/Q4/AnswerQ2.png", AnswerQ2)
  ggsave("Result/Q4/eyeMesuerements.png", eyeMesuerements)
  ggsave("Result/Q4/heatmaps.png", heatmaps)
  ggsave("Result/Q4/RMSSD.png", RMSSD)
  ggsave("Result/Q4/SDNN.png", SDNN)
}

