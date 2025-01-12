source("environmentSetUp.R")
# This function creates plots for analyzing subgroups within 
# both the Linne and Dame cohorts.
# Input: Cohort file and any required physiological measurements, as well as 
# percieved stress vars (Q1 , Q2)
# Output: Scatterplots 


# Function to create scatter plot for RMSSD and Q1 by Agegroups
hrvScatterPlotFunction <- function(data, col_x, col_y, col_group, cohort) {
  
  # Remove rows with NA values in relevant columns
  data <- data %>%
    filter(!is.na(!!sym(col_x)) & !is.na(!!sym(col_y)) & !is.na(!!sym(col_group)))
  
  # If col_group is "Age", group the age into larger categories
  if (col_group == "Age") {
    data <- data %>%
      mutate(age_group = cut(Age, breaks = c(-Inf, 20, 30, 40, 50, Inf), 
                             labels = c("<20", "21-30", "31-40", "41-50", "51+"))) %>%
      mutate(age_group = factor(age_group, levels = c("<20", "21-30", "31-40", "41-50", "51+")))  # Convert to factor explicitly
    col_group <- "age_group"  # Use the newly created age_group column for col_group
  }
  
  # Create high HRV threshold based on RMSSD values
  high_hrv_threshold <- quantile(data[[col_y]], 0.9, na.rm = TRUE)
  data <- data %>%
    mutate(high_hrv = ifelse(!!sym(col_y) >= high_hrv_threshold, "High HRV", "Normal HRV"))
  
  # Dynamically assign a color palette based on unique levels of col_group
  palette <- brewer.pal(n = 5, name = "YlGnBu") # Yellow-Green-Blue palette
  names(palette) <- c("<20", "21-30", "31-40", "41-50", "51+")
  
  # Calculate dynamic axis limits
  x_limits <- range(data[[col_x]], na.rm = TRUE)
  y_limits <- range(data[[col_y]], na.rm = TRUE)
  
  # Create the plot
  ggplot(data, aes(x = !!sym(col_x), y = !!sym(col_y), fill = !!sym(col_group))) +
    geom_point(alpha = 1, size = 2.8, shape = 21, stroke = 0.3, color = "black") + 
    scale_fill_manual(values = palette, name = "Age groups") + 
    labs(
      title = paste("High Heart rate variability (HRV) Subgroup", "(",cohort,")"),
      x = "Cognitive Load",
      y = "RMSSD",
      fill = "Age Group"
    ) +
    coord_cartesian(xlim = c(x_limits[1], x_limits[2]), ylim = c(y_limits[1], y_limits[2])) +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 14, hjust = 0.5)
    ) 
}


# Function to create scatter plot Males in both training versions, to show 
#different behaviours between percieved stress and meassured heartrate
hrQ1ScatterPlotFunction <- function(data, col_x, col_y, col_group, cohort, gender) {
  
  # Optionally filter the data based on gender
  if (!is.null(gender)) {
    data <- data.frame(data) %>%
      filter(Gender == gender)
  }
  
  # Dynamically set colors based on col_group
  if (col_group == "Trainingsversion") {
    custom_colors <- c("Adaptive" = "#F8C471", "NonAdaptive" = "#82E0AA")
  } else if (col_group == "Gender") {
    custom_colors <- c("M" = "#7FB3D5", "F" = "#F1948A")
  } else {
    custom_colors <- NULL # Default colors
  }
  
  # Dynamic title based on cohort
  gender_title <- if (!is.null(gender) && gender == "M") {
    "Males"
  } else if (!is.null(gender) && gender == "F") {
    "Females"
  } else {
    NULL
  }
  
  plot_title <- if (cohort == "Linne") {
    ifelse(
      is.null(gender),
      paste("Relationship between Heart Rate and Cognitive Load by Training Version (",cohort,")", sep = ""),
      paste("Relationship between Heart Rate and Cognitive Load by Training Version in", gender_title, "(",cohort,")")
    )
  } else if (cohort == "Dame") {
    paste("Relationship between Heart Rate and Cognitive Load by Gender in", "(",cohort,")")
  } else {
    ifelse(
      is.null(gender),
      paste("Relationship between Heart Rate and Cognitive Load by", col_group, "(",cohort,")", sep = " "),
      paste("Relationship between Heart Rate and Cognitive Load by", col_group, "in", gender_title, "(",cohort,")")
    )
  }
  
  # Plot
  plot <- ggplot(data, aes_string(x = col_x, y = col_y, fill = col_group)) +
    geom_point(alpha = 1, size = 2.8, shape = 21, stroke = 0.3, color = "black") +
    theme_minimal() +
    theme(
      legend.position = "right",
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 14, hjust = 0.5)
    ) +
    labs(
      title = plot_title,
      x = "Cognitive Load",
      y = "Heart Rate (Bpm)",
      fill = col_group
    ) +
    coord_cartesian(xlim = c(1.5, 4.7), ylim = c(50, 105)) 
  
  # Apply custom colors to fill
  if (!is.null(custom_colors)) {
    plot <- plot + scale_fill_manual(values = custom_colors)
  }
  
  # Add a purple trend line if only males are selected
  if (!is.null(gender) && gender == "M" && cohort == "Linne") {
    adaptive_data <- data %>%
      filter(Trainingsversion == "Adaptive") # Filter for Adaptive group
    
    plot <- plot +
      geom_smooth(
        data = adaptive_data,
        aes_string(x = col_x, y = col_y),
        method = "loess", 
        color = "#F8C471",
        linetype = "dashed",
        se = FALSE 
      )
  }
  
  # Add a vertical line if cohort is "Dame"
  if (cohort == "Dame") {
    plot <- plot +
      geom_vline(
        xintercept = 4,
        color = "black",
        linetype = "dashed",
        size = 1
      ) 
  }
  
  return(plot)
}


# Call Plots for HRV and percievd Stress throughout Agegroups
hrvQ1ByAgeCombined <- hrvScatterPlotFunction(combinedPMDAndDemographicsAndAnswers, "Answer_Q1", "RMSSD", "Age", "Combined cohorts")

# Call Plots for HR and percievd Stress in Males for Training versions
hrQ1TrainingGenderDame <- hrQ1ScatterPlotFunction(DamePMDAndDemographicsAndAnswers, "Answer_Q1", "mean_HR", "Gender", "Dame", NULL)

hrQ1TrainingMalesLinne <- hrQ1ScatterPlotFunction(LinnePMDAndDemographicsAndAnswers, "Answer_Q1", "mean_HR", "Trainingsversion", "Linne", "M")
hrQ1TrainingFemalesLinne <- hrQ1ScatterPlotFunction(LinnePMDAndDemographicsAndAnswers, "Answer_Q1", "mean_HR", "Trainingsversion", "Linne", "F")
hrQ1TrainingLinne <- hrQ1ScatterPlotFunction(LinnePMDAndDemographicsAndAnswers, "Answer_Q1", "mean_HR", "Trainingsversion", "Linne", NULL)

## Export plots into Result folder
if (!all(file.exists(c("Result/Q3/hrQ1TrainingGenderDame.jpeg", "Result/Q3/hrQ1TrainingMalesLinne.jpeg", "Result/Q3/hrQ1TrainingFemalesLinne.jpeg", 
                       "Result/Q3/hrQ1TrainingLinne.jpeg", "Result/Q3/hrvQ1ByAgeCombined.jpeg")))) {
  ggsave("Result/Q3/hrQ1TrainingGenderDame.jpeg", hrQ1TrainingGenderDame)
  ggsave("Result/Q3/hrQ1TrainingMalesLinne.jpeg", hrQ1TrainingMalesLinne)
  ggsave("Result/Q3/hrQ1TrainingFemalesLinne.jpeg", hrQ1TrainingFemalesLinne)
  ggsave("Result/Q3/hrQ1TrainingLinne.jpeg", hrQ1TrainingLinne)
  ggsave("Result/Q3/hrvQ1ByAgeCombined.jpeg", hrvQ1ByAgeCombined)
}
