source("environmentSetUp.R")
# This function produces the plot to visualize how the relationship between 
# the stress indicators (Answer Q1: cognitive load, Answer Q2: physical load)
# and the physiological measurements (PMD)) of 2 cohorts (Dame and Linne) changes
# in every round. There are 9 rounds in total.

# Define Measurements and Colors
heartMeasurements <- c("Mean HR", "RMSSD", "SDNN")
heartColor <- "#66c2a5"

skinMeasurements <- c("Mean SCL", "Mean SCR amplitude", "Mean SCR frequency", "Mean SCR rise time")
skinColor <- "#fc8d62"

blinkSaccadeMeasurements <- c("Mean Blink rate last minute", "Mean Saccade amplitude", "Mean Saccade velocity")
blinkSaccadeColor <- "#7570b3"

# Function to preprocess data
prepareData <- function(data, question, measurements, group) {
  data %>%
    mutate(Round_number = as.numeric(Round_number)) %>%
    pivot_longer(
      cols = -Round_number, 
      names_to = "PMD", 
      values_to = "correlation"
    ) %>%
    filter(grepl(paste0("_", question, "_"), PMD)) %>%
    mutate(correlation = as.numeric(correlation)) %>%
    mutate(PMD = str_replace_all(PMD, "_", " ")) %>%
    mutate(PMD = str_replace_all(PMD, "raw|Raw", "")) %>%
    mutate(PMD = str_replace_all(PMD, "mean", "Mean")) %>%
    mutate(PMD = str_replace_all(PMD, question, "")) %>%
    mutate(PMD = str_replace_all(PMD, "corr", "")) %>%
    mutate(PMD = str_trim(PMD)) %>%
    filter(PMD %in% measurements) %>%
    mutate(Group = group)  # Add group for legend
}

# Function to create a single plot
createPlot <- function(data, color, x_label = NULL, y_label = NULL) {
  ggplot(data, aes(x = Round_number, y = correlation, color = Group)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 2, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
    scale_x_continuous(breaks = seq(min(data$Round_number), max(data$Round_number), 1)) + 
    scale_y_continuous(limits = c(-0.4, 0.4)) +
    scale_color_manual(
      name = NULL,  # Remove the legend title
      values = c("Heart" = heartColor, "Skin" = skinColor, "Blink/Saccade" = blinkSaccadeColor)
    ) +
    facet_wrap(~ PMD, scales = "free_y", nrow = 1) +
    theme_minimal() +
    labs(x = x_label, y = y_label) +
    theme(
      legend.position = "right",  # Position legend to the right
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 10),
      axis.title.x = if (!is.null(x_label)) element_text(size = 12, face = "bold") else element_blank(),
      axis.title.y = if (!is.null(y_label)) element_text(size = 12, face = "bold") else element_blank(),
      strip.text.x = element_text(size = 10, face = "bold")
    )
}

# Function to generate combined plot with a single legend
generateCombinedPlot <- function(question, title, subtitle) {
  heartData <- prepareData(correlationTableWithRoundsCombined, question, heartMeasurements, "Heart")
  skinData <- prepareData(correlationTableWithRoundsCombined, question, skinMeasurements, "Skin")
  blinkSaccadeData <- prepareData(correlationTableWithRoundsCombined, question, blinkSaccadeMeasurements, "Blink/Saccade")
  
  heartPlot <- createPlot(heartData, heartColor)
  skinPlot <- createPlot(skinData, skinColor, y_label = "Correlation")
  blinkSaccadePlot <- createPlot(blinkSaccadeData, blinkSaccadeColor, x_label = "Round Number")
  
  # Combine plots and ensure one legend
  combinedPlot <- (heartPlot / skinPlot / blinkSaccadePlot) +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      theme = theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 14, face = "plain", hjust = 0)
      )
    ) +
    plot_layout(guides = "collect") & theme(legend.position = "right")  # Ensure shared legend to the right
  
  return(combinedPlot)
}

# Generate Q1 and Q2 plots
roundCombinedQ1 <- generateCombinedPlot(
  "Q1",
  "Correlation over the rounds between Physiological measurement and Stress indicators",
  "PMD & Cognitive Load"
)

roundCombinedQ2 <- generateCombinedPlot(
  "Q2",
  "Correlation over the rounds between Physiological measurement and Stress indicators",
  "PMD & Physical Load"
)

# Display the plots
roundCombinedQ1
roundCombinedQ2

# Export the plots
if (!all(file.exists(c("Result/Q3/roundCombinedQ1.png", "Result/Q3/roundCombinedQ2.png")))) {
  ggsave("Result/Q3/roundCombinedQ1.png", roundCombinedQ1, width = 12, height = 7, dpi = 300)
  ggsave("Result/Q3/roundCombinedQ2.png", roundCombinedQ2, width = 12, height = 7, dpi = 300)
}
