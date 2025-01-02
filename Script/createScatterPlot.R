# Function to create scatter plot with mean line and annotation
scatterPlotFunction <- function(data, col_x, col_y, col_group) {
  # Check if the columns exist in the data frame
  if (!all(c(col_x, col_y, col_group) %in% names(data))) {
    stop("One or more specified columns do not exist in the data frame.")
  }
  
  # Calculate the mean of the x-column
  x_mean <- mean(data[[col_x]], na.rm = TRUE)
  
  # Dynamically create the title
  plot_title <- paste("Analyzing Relationship Between", col_x, "and", col_y)
  
  # Create the scatter plot
  ggplot(data, aes_string(x = col_x, y = col_y, color = col_group)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_vline(xintercept = x_mean, color = "black", linetype = "dashed", linewidth = 1) +  # Add mean line
    annotate("text", x = max(data[[col_x]], na.rm = TRUE), 
             y = max(data[[col_y]], na.rm = TRUE), 
             label = paste("Mean of", col_x, "=", round(x_mean, 2)), 
             hjust = 1, vjust = 1, size = 4, color = "black") +  # Add mean text with axis name
    labs(
      title = plot_title,
      x = col_x,
      y = col_y,
      color = col_group  # Legend title for the group
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")  # Use chosen color palette
}



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

# Call Plots for HRV and percievd Stress throughout Agegroups
hrvQ1ByAgeDame <- hrvScatterPlotFunction(DamePMDAndDemographicsAndAnswers, "Age", "RMSSD", "Answer_Q1", "SDNN", "Dame")
hrvQ1ByAgeLinne <- hrvScatterPlotFunction(LinnePMDAndDemographicsAndAnswers, "Age", "RMSSD", "Answer_Q1", "SDNN", "Dame")


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

# Call Plots for HR and percievd Stress in Males for Training versions
hrQ1TrainingMalesDame <- hrVsQ1ScatterPlotFunction(DamePMDAndDemographicsAndAnswers, "Answer_Q1", "mean_HR", "Trainingsversion", "Dame")
hrQ1TrainingMalesLinne <- hrVsQ1ScatterPlotFunction(LinnePMDAndDemographicsAndAnswers, "Answer_Q1", "mean_HR", "Trainingsversion", "Linne")
