# ----- LOADING -----
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
correlationTable <- read_csv("Analysis/correlationTable.csv")

# ----- CHANGE STRUCTURE -----
correlationQ1 <- pivot_longer(correlationTable, -1, names_to = "PMD",
                              values_to = "Correlation") |>
  filter(Row_Name == "Answer_Q1") |>
  mutate(Correlation = as.numeric(Correlation))

correlationQ2 <- pivot_longer(correlationTable, -1, names_to = "PMD",
                              values_to = "Correlation") |>
  filter(Row_Name == "Answer_Q2")

correlation <- pivot_longer(correlationTable, -1, names_to = "PMD",
                            values_to = "Correlation")

# ----- CHOSEN PLOT FOR QUESTION 1 -----
## Clean the variable names:
correlationQ1$PMD <- str_replace_all(correlationQ1$PMD, "_", " ")
correlationQ1$PMD <- str_replace_all(correlationQ1$PMD, "raw|Raw", "")
correlationQ1$PMD <- str_replace_all(correlationQ1$PMD, "mean", "Mean")
correlationQ2$PMD <- str_replace_all(correlationQ1$PMD, "_", " ")
correlationQ2$PMD <- str_replace_all(correlationQ1$PMD, "raw|Raw", "")
correlationQ2$PMD <- str_replace_all(correlationQ1$PMD, "mean", "Mean")

## plot
# Q1
ggplot(correlationQ1, aes(x = reorder(PMD, Correlation, decreasing = TRUE), y = Correlation)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Bar chart
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Baseline at zero
  theme_minimal() +
  labs(x = "Physical Measurements", 
       y = "Correlation with Stress Measurement", 
       title = "Correlation between Physical measurement and Stress measurement",
       subtitle = "(PMD & Answer Q1)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Q2
ggplot(correlationQ2, aes(x = reorder(PMD, Correlation, decreasing = TRUE), y = Correlation)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Bar chart
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Baseline at zero
  theme_minimal() +
  labs(x = "Physical Measurements", 
       y = "Correlation with Stress Measurement", 
       title = "Correlation between Physical measurement and Physical load",
       subtitle = "(PMD & Answer Q2)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# --------------------------------------












# ----- Test plot -----
## Plot 1: bubble plot -----
ggplot(correlation, aes(x = PMD, y = Row_Name, size = abs(Correlation), fill = Correlation)) +
  geom_point(shape = 21, color = "black") +  # Bubbles with black borders
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # Diverging color scale
  scale_size(range = c(3, 10)) +  # Bubble size for correlation strength
  theme_minimal() +
  labs(x = "Physical Measurements", 
       y = "Questions (Stress Measurements)", 
       size = "Correlation Strength", 
       fill = "Correlation") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10)
  )

## Plot 2: bar chart goes below and above -----
# Q1
ggplot(correlationQ1, aes(x = PMD, y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Bar chart
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # Diverging color scale
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Baseline at zero
  theme_minimal() +
  labs(x = "Physical Measurements", 
       y = "Correlation with Stress Measurement", 
       title = "Correlation Bar Chart") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# facette both
ggplot(correlation, aes(x = PMD, y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +  # Bar chart
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +  # Diverging color scale
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Baseline at zero
  theme_minimal() +
  labs(x = "Physical Measurements", 
       y = "Correlation with Stress Measurement", 
       title = "Correlation Bar Chart") +
  facet_wrap(~Row_Name) +  # Facet by the stress measurement (Answer_Q1, Answer_Q2, etc.)
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Plot 3: bar chart P2 but compare between 2 Answer -----
ggplot(correlation, aes(x = PMD, y = Correlation, fill = Row_Name)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  scale_fill_manual(values = c("blue", "red"), name = "Stress Measurement") +  # Set colors for Q1 and Q2
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Add baseline
  theme_minimal() +
  labs(x = "Physical Measurements", 
       y = "Correlation", 
       title = "Grouped Bar Chart: Correlations with Stress Measurements") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ----- QUESTION 2 ------
## example df
set.seed(42)

# Generate example data
rounds <- 1:9  # 9 rounds
physical_measurements <- paste0("PMD_", 1:11)  # 11 physical measurements
stress_measures <- c("Answer_Q1")

# Create a DataFrame with random correlation values for each combination
example_df <- expand.grid(PMD = physical_measurements, 
                          Row_Name = stress_measures, 
                          Round = rounds)
example_df$Correlation <- round(runif(nrow(example_df), -1, 1), 2)


## ------ TEST Plot -----
ggplot(example_df, aes(x = Round, y = Correlation, color = PMD, group = PMD)) +
  geom_line(size = 1) +  # Line plot with PMD-specific colors
  geom_point(size = 2) +  # Points for clarity
  facet_wrap(~ PMD, ncol = 1, strip.position = "left", scales = "free_y") +  # Vertical facets
  scale_x_continuous(breaks = 1:9) +  # Ensure x-axis shows fixed values 1 to 9
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Add a horizontal line at y=0
  scale_color_viridis_d(name = "Physical Measurement") +  # Use a distinctive color palette
  theme_minimal() +
  labs(
    x = "Round Number", 
    y = "Correlation", 
    title = "Correlation Changes Across Rounds (Answer_Q1)"
  ) +
  theme(
    strip.text.y = element_text(angle = 0, size = 10),  # Adjust facet labels
    strip.placement = "outside",  # Move labels outside the plot
    panel.spacing = unit(1, "lines"),  # Increase spacing between panels
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "none"  # Remove legend since PMD is already in facets
  )

