source("environmentSetUp.R")
# Here we produce the plots to visualize the differences and similarities 
# between the two Cohorts.
# We analyse the variables: Age, gender, BMI, Cognitive and Physical load, 
# RMSSD and SDNN.

# Boxplot for Age by Training Version, faceted by Cohort
# Shows how age varies across training versions within each cohort.
ageVariability <- ggplot(combinedDemoWithCohorts, aes(x = Trainingsversion, y = Age, fill = Trainingsversion)) +
  geom_boxplot(alpha = 0.8) +  
  facet_wrap(~ Cohort, scales = "free_x") +
  scale_y_continuous(breaks = seq(0, 60, by = 10)) +
  scale_x_discrete(labels = c("NonAdaptive" = "Non Adaptive")) +
  scale_fill_manual(values = c("#F8C471", "#B0B0B0", "#82E0AA"),
                    labels = c("Adaptive", "Control", "Non Adaptive")) +
  labs(title = "Age Patterns Based on Training Versions and Cohort",
       x = "Training Version",
       y = "Age",
       fill= "Training Version") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 14, face = "bold") 
  )


# Filter data to exclude the "Control" group for the follow-up  plots
filtered_combined_data <- combinedDataWithCohorts %>%
  filter(!(Trainingsversion == "Control" & Cohort == "Dame"))


# Faceted bar chart for gender distribution
# Proportional bar chart to show gender distribution within training versions for each cohort.
genderDistribution <- ggplot(filtered_combined_data, aes(x = Trainingsversion, fill = Gender)) +
  geom_bar(position = "fill", color = "black", alpha = 0.8) +  
  facet_wrap(~ Cohort) +
  scale_x_discrete(labels = c("Adaptive" = "Adaptive",
                              "NonAdaptive" = "Non Adaptive")) +
  scale_fill_manual(values = c("M" = "#7FB3D5",
                               "F" = "#F1948A")) +  
  labs(title = "Gender Proportions Based on Training Versions and Cohort",
       x = "Training Version",
       y = "Proportion",
       fill = "Gender"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 14, face = "bold") 
  )



# Density plot for BMI
# Displays the distribution of BMI within each cohort.
# included in the original presentation.
BMI <- ggplot(filtered_combined_data, aes(x = BMI, fill = Cohort, color = Cohort)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  scale_color_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  labs(title = "BMI Density", x = "BMI", y = "Density") +
  theme_minimal()


# Histogram for BMI by Cohort
# Provides a detailed view of BMI distribution within cohorts, faceted by cohort.
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

# Boxplot for BMI
# Visualizes the distribution of BMI across cohorts
# included in the improved presentation 
BMI_boxplot <- ggplot(filtered_combined_data, aes(x = "", y = BMI, fill = Cohort)) +
  geom_boxplot(alpha = 0.8, color = "black") +  
  facet_grid(. ~ Cohort) +
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  labs(title = "BMI Across Between Dame and Linne Cohorts",
       caption = "BMI = weight (kg) / [height (m)]^2",
       x = NULL) +
  scale_y_continuous(limits = c(16, 34), breaks = seq(16, 34, by = 4)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 14),  # Bold facet labels
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    panel.spacing = unit(1, "lines") # Add spacing between facets
  )


# Visualizations for Cognitive Load (Answer Q1) and Physical Load (Answer Q2).

# Boxplot for Cognitive Load 
# Visualizes the distribution of cognitive load scores across cohorts
AnswerQ1 <- ggplot(filtered_combined_data, aes(x = Cohort, y = Answer_Q1, fill = Cohort)) +
  geom_boxplot(alpha = 0.8, color = "black") +  
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  labs(title = "Cognitive Load Distribution",
       y = "Cognitive Load score",
       x = NULL) +
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + 
  theme_minimal()



# Boxplot for Physical Load 
# Visualizes the distribution of Physical load scores across cohorts
AnswerQ2 <- ggplot(filtered_combined_data, aes(x = Cohort, y = Answer_Q2, fill = Cohort)) +
  geom_boxplot(alpha = 0.8, color = "black") +  
  labs(title = "Physical Load Distribution",
       y = "Physical Load score",
       x = NULL) +
  scale_fill_manual(values = c("Dame" = "#A1887F", "Linne" = "#F5B7B1")) +
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, by = 1)) + 
  theme_minimal()


# Combined plot for Q1 and Q2
# Merges the boxplots for Q1 and Q2 side-by-side, sharing a common legend on the right side
Q1Q2Combined <- (AnswerQ1 | AnswerQ2) + 
  plot_layout(guides = "collect", heights = c(1, 1.1)) + 
  plot_annotation(title = "Cognitive and Physical Load Across the Cohorts",
                  theme = theme(plot.title = element_text(face = "bold", size = 18))) +
  theme(legend.position = "right") 
  




# Boxplot for RMSSD by Cohort and Training Version
# RMSSD measures heart rate variability. Here, the comparison is done by cohort and training version.
RMSSD <- ggplot(filtered_combined_data, aes(x = Trainingsversion, y = RMSSD, fill = Trainingsversion)) +
  geom_boxplot(alpha = 0.8, color = "black") +
  facet_wrap(~ Cohort) +
  labs(title = "RMSSD Across Training Versions in the Cohorts", 
       x = NULL,
       fill= "Training Version") +  
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 1000)) +
  scale_x_discrete(labels = c("Adaptive" = "Adaptive",
                              "NonAdaptive" = "Non Adaptive")) +
  scale_fill_manual(values = c("#F8C471", "#82E0AA"),
                    labels = c("Adaptive", "Non Adaptive")) +  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "right"
    )


# Boxplot for SDNN by Cohort and Training Version (not included in the presentation)
# SDNN measures heart rate variability. Here, the comparison is done by cohort and training version.
SDNN <- ggplot(filtered_combined_data, aes(x = Trainingsversion, y = SDNN, fill = Trainingsversion)) +
  geom_boxplot(alpha = 0.8, color = "black") +
  facet_wrap(~ Cohort) +
  labs(title = "SDNN",
       x = NULL,
       fill= "Training Version") +  
  scale_y_continuous(limits = c(0, 4000), breaks = seq(0, 4000, by = 1000)) +  
  scale_x_discrete(labels = c("Adaptive" = "Adaptive",
                              "NonAdaptive" = "Non Adaptive")) +
  scale_fill_manual(values = c("#F8C471", "#82E0AA"),
                    labels = c("Adaptive", "Non Adaptive")) +  
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),  
    legend.position = "right"  
  )



## Export plots into Result folder
if (!all(file.exists(c("Result/Q1/ageVariability.png", "Result/Q1/genderDistribution.png",
                       "Result/Q1/BMI.png","Result/Q1/BMI_boxplot.png", 
                       "Result/Q1/BMI_histogram.png", "Result/Q1/Q1Q2Combined.png",
                       "Result/Q1/RMSSD.png", "Result/Q1/SDNN.png")))) {
  ggsave("Result/Q1/ageVariability.png", ageVariability)
  ggsave("Result/Q1/genderDistribution.png", genderDistribution)
  ggsave("Result/Q1/BMI.png", BMI)
  ggsave("Result/Q1/BMI_histogram.png", BMI_histogram)
  ggsave("Result/Q1/BMI_boxplot.png", BMI_boxplot)
  ggsave("Result/Q1/Q1Q2Combined.png", Q1Q2Combined)
  ggsave("Result/Q1/RMSSD.png", RMSSD)
  ggsave("Result/Q1/SDNN.png", SDNN)
}

