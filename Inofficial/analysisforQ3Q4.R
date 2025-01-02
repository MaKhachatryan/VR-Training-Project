library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
source("Script/utils/pivotedData.R")
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
ggplot(combinedCohorts, aes(x = Trainingsversion, y = Mean_Answer_Q1, fill = Trainingsversion)) +
  geom_boxplot(alpha = 0.7, outlier.size = 2) +
  labs(
    title = "Comparison of Mental Load Perception (Answer Q1) by Training Group",
    x = "Training Groups",
    y = "Answer Q1",
  ) +
  theme_minimal() +
  scale_fill_viridis_d(option = "plasma") +
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




# Grouped Bar Plot, for demographic meassurement and answer Q1 / Q2
ggplot(DameDemographicsAndAnswers, aes(x = Trainingsversion, y = Mean_Answer_Q1, fill = Gender)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
labs(
  title = "Mental Load Perception (Answer Q1) by Gender and Training Type",
  x = "Training Type",
  y = "Mean Answer Q1",
  fill = "Gender"
) +
  theme_minimal() +
  scale_fill_viridis_d(option = "viridis")




##Iris: for the tests between Group Q1 and Q2, can be used for other matches
# Reshape the data to a long format
long_data <- combinedCohorts %>%
  pivot_longer(
    cols = c(Mean_Answer_Q1, Mean_Answer_Q2), # Columns to pivot
    names_to = "Question",                   # New column for the question
    values_to = "Mean_Answer"                # New column for the values
  )

# Plot both Q1 and Q2
ggplot(long_data, aes(x = Trainingsversion, y = Mean_Answer, fill = Trainingsversion)) +
  geom_boxplot(alpha = 0.7, outlier.size = 2) +
  facet_wrap(~ Question, labeller = as_labeller(c(
    Mean_Answer_Q1 = "Q1 (Mental Load)",
    Mean_Answer_Q2 = "Q2 (Physical Load)"
  ))) +
  labs(
    title = "Mental and Physical Load Perception by Training Type",
    x = "Training Type",
    y = "Mean Answer",
    fill = "Group"
  ) +
  theme_minimal() +
  scale_fill_viridis_d(option = "viridis")


#####Scatterplots for the different Subgroup Combinations###################

ggplot(DamePMDAndDemographicsAndAnswers, 
       aes(x = mean_HR, 
           y = mean_SCL_Raw, 
           color = Answer_Q1)) +
  geom_point(alpha = 0.7) +  
  scale_color_viridis(name = "Stress level (Q1)") + 
  theme_minimal() +
  labs(
    title = "Cognitive Load vs. Heart Rate",
    x = "Heart Rate (mean_HR)",
    y = "Skin Conductivity (mean_SCL_Raw)"
  ) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )



# Scatter plot for BMI and percieved / meassured stress 
ggplot(bmiSubgroups, aes(x = mean_HR, y = mean_SCR_amplitude_raw, color = Answer_Q1)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis(option = "plasma", name = "Stress Level (Q1)") +
  facet_wrap(~ BMICategory) +
  theme_minimal() +
  labs(
    title = "Relationship Between Perceived Stress, Heart Rate, and SCR Amplitude ",
    x = "Heart Rate (mean_HR)",
    y = "Saccade Amplitude"
  ) +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14)
  )



# Create a boxplot for Q1 by BMI Category to show distributions and outliers
ggplot(data, aes(x = BMI_Category, y = Answer_Q1, fill = BMI_Category)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +  # Highlight outliers in red
  theme_minimal() +
  labs(
    title = "Perceived Stress Level (Q1) by BMI Category",
    x = "BMI Category",
    y = "Perceived Stress Level (Q1)"
  ) +
  scale_fill_manual(values = c("Underweight" = "lightblue", "Normal Weight" = "green", "Overweight" = "red"))
