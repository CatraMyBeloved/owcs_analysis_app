# Define custom dark theme
library(ggplot2)

owcs_theme <- function() {
  theme_minimal() +
    theme(
      # Plot background
      plot.background = element_rect(fill = "#2D2D2D", color = NA),
      panel.background = element_rect(fill = "#2D2D2D", color = NA),
      
      # Grid lines
      panel.grid.major = element_line(color = "#d8d8d8",  linewidth = 0.3),
      panel.grid.minor = element_line(color = "#c6c4c4", linewidth = 0.1),
      
      # Text elements
      text = element_text(color = "#FFFFFF"),
      plot.title = element_text(color = "#FFFFFF", face = "bold"),
      axis.title = element_text(color = "#FFFFFF"),
      axis.text = element_text(color = "#DDDDDD"),
      
      # Legend
      legend.background = element_rect(fill = "#2D2D2D", color = NA),
      legend.key = element_rect(fill = "#2D2D2D", color = NA),
      legend.text = element_text(color = "#FFFFFF"),
      legend.title = element_text(color = "#FFFFFF", face = "bold")
    )
}

# Set as default theme
theme_set(owcs_theme())

