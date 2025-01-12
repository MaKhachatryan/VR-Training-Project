#Maga
#plots for cohort differences
#plots are official, but anything that has to do with sourcing still needs
#to be changed a bit, i had a problem with sourceing the environment set up
source("environmentSetUp.R")

##---------plots for question 4-----------##
##---------cohort comparison--------------##


## Boxplot for Age by Training Version (faceted by Cohort)
ageVariability <- ggplot(combinedDemoWithCohorts, aes(x = Trainingsversion, y = Age, fill = Trainingsversion)) +
  geom_boxplot(alpha = 0.8) +  
  facet_wrap(~ Cohort, scales = "free_x") +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  scale_fill_manual(values = c("Adaptive" = "#F8C471", "NonAdaptive" = "#82E0AA", "Control"= "#B0B0B0")) +
  labs(title = "Age Variability by Training Version and Cohort", x = "Training Version", y = "Age") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold")  # Adjust size and style of facet labels
  )




## Filter data to exclude "Control" group
filtered_combined_data <- combinedDataWithCohorts %>%
  filter(!(Trainingsversion == "Control" & Cohort == "Dame"))

# Faceted bar chart for gender distribution
genderDistribution <- ggplot(filtered_combined_data, aes(x = Trainingsversion, fill = Gender)) +
  geom_bar(position = "fill", color = "black", alpha = 0.8) +  # Use "fill" for proportional bars
  facet_wrap(~ Cohort) +
  scale_fill_manual(values = c("M" = "#7FB3D5", "F" = "#F1948A")) +  # Same colors as violin plot
  labs(
    title = "Gender Proportions by Training Version and Cohort",
    x = "Training Version",
    y = "Proportion",
    fill = "Gender"
  ) +
  theme_minimal()+
  theme(
    strip.text = element_text(size = 14, face = "bold") 
  )






# Density plot for BMI
BMI <- ggplot(combinedDataWithCohorts, aes(x = BMI, fill = Cohort, color = Cohort)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  scale_color_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  labs(title = "BMI Density", x = "BMI", y = "Density") +
  theme_minimal()




## Histogram for BMI
BMI_histogram <- ggplot(filtered_combined_data, aes(x = BMI)) +
  geom_histogram(aes(y = ..density.., fill = Cohort), bins = 50, alpha = 0.8) +
  facet_wrap(~ Cohort, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  labs(
    title = "BMI Histogram by Cohort with Age Gradient",
    x = "BMI",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )






#-----plots for Answer Q1 and Answer Q2----#

# Violin plot for Cognitive Load (Q1)
AnswerQ1 <- ggplot(filtered_combined_data, aes(x = Cohort, y = Answer_Q1, fill = Cohort)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  labs(title = "Cognitive Load Distribution", x = "Cohort", y = "Cognitive Load (Q1)") +
  theme_minimal()

# Violin plot for Physical Stress (Q2)
AnswerQ2 <- ggplot(filtered_combined_data, aes(x = Cohort, y = Answer_Q2, fill = Cohort)) +
  geom_violin(trim = FALSE, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  labs(title = "Physical Load Distribution") +
  theme_minimal()


# Boxplot for Cognitive Load (Q1)
AnswerQ1 <- ggplot(filtered_combined_data, aes(x = Cohort, y = Answer_Q1, fill = Cohort)) +
  geom_boxplot(alpha = 0.8, color = "black") +  # Replace violin plot with boxplot
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  labs(title = "Cognitive Load Distribution", x = NULL, y = NULL) +
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + 
  theme_minimal()



# Boxplot for Physical Stress (Q2)
AnswerQ2 <- ggplot(filtered_combined_data, aes(x = Cohort, y = Answer_Q2, fill = Cohort)) +
  geom_boxplot(alpha = 0.8, color = "black") +  # Replace violin plot with boxplot
  labs(title = "Physical Load Distribution", x = NULL, y= NULL) +
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + 
  theme_minimal()



Q1Q2Combined <- (AnswerQ1 | AnswerQ2) + 
  plot_layout(guides = "collect") +  # Combine legends
  theme(legend.position = "right")




# Boxplot for RMSSD by Cohort and Training Version
RMSSD <- ggplot(filtered_combined_data, aes(x = Trainingsversion, y = RMSSD, fill = Trainingsversion)) +
  geom_boxplot(alpha = 0.8, color = "black") +
  facet_wrap(~ Cohort) +
  labs(title = "RMSSD (measure of HRV)", x = NULL, y = NULL) +  # Remove x and y axis labels
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 1000)) +  # Set y-axis scale
  scale_fill_manual(values = c("Adaptive" = "#F8C471", "NonAdaptive" = "#82E0AA")) +  # Match colors
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "plain"),  # Adjust facet labels
    legend.position = "none"  # Remove legend for this plot (will combine later)
  )

# Boxplot for SDNN by Cohort and Training Version
SDNN <- ggplot(filtered_combined_data, aes(x = Trainingsversion, y = SDNN, fill = Trainingsversion)) +
  geom_boxplot(alpha = 0.5, color = "black") +
  facet_wrap(~ Cohort) +
  labs(title = "SDNN", x = NULL, y = NULL) +  # Remove x and y axis labels
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 1000)) +  # Set y-axis scale
  scale_fill_manual(values = c("Adaptive" = "#F8C471", "NonAdaptive" = "#82E0AA")) +  # Match colors
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "plain"),  # Adjust facet labels
    legend.position = "right"  # Position legend
  )

# Combine the two plots side by side with a single legend
RMSSD_SDNN_Combined <- (RMSSD | SDNN) +
  plot_layout(guides = "collect") +  # Combine legends
  theme(
    legend.position = "right"  # Position legend on the right
  )



## Export plots into Result folder
if (!all(file.exists(c("Result/Q4/ageVariability.png", "Result/Q4/genderDistribution.png",
                       "Result/Q4/BMI.png","Result/Q4/BMI_histogram.png", "Result/Q4/Q1Q2Combined.png",
                       "Result/Q4/RMSSD.png", "Result/Q4/SDNN.png")))) {
  ggsave("Result/Q4/ageVariability.png", ageVariability)
  ggsave("Result/Q4/genderDistribution.png", genderDistribution)
  ggsave("Result/Q4/BMI.png", BMI)
  ggsave("Result/Q4/BMI_histogram.png", BMI_histogram)
  ggsave("Result/Q4/Q1Q2Combined.png", Q1Q2Combined)
  ggsave("Result/Q4/RMSSD.png", RMSSD)
  ggsave("Result/Q4/SDNN.png", SDNN)
}

