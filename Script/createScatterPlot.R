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
