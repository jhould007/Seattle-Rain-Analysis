# Look for a correlation between temperature and precipitation levels
  # Add new column for average temperature
  averageByMonth <- mutate(averageByMonth, average_temp = (average_tmax + average_tmin)/2)
  
  # Create scatterplot of temperature and precipitation level to look for correlation
  
    # Add month_name column and rearrange for readability 
    months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    averageByMonth <- mutate(averageByMonth, month_name = months)

    # Add colors to each month's point for visual appeal
    month_colors <- c("#59acd9", "#59acd9", "#3ea852", "#3ea852", "#3ea852", "#d68f24", "#d68f24", "#d68f24", "#cc3f10", "#cc3f10","#59acd9","#59acd9")
    # averageByMonth <- mutate(averageByMonth, month_color = c("#59acd9", "#59acd9", "#3ea852", "#3ea852", "#3ea852", "#d68f24", "#d68f24", "#d68f24", "#cc3f10", "#cc3f10","#59acd9","#59acd9"))
    
    # Generate the plot
    ggplot(averageByMonth, aes(x = average_temp, y = average_prcp, label = month_name)) +
      geom_point(aes(color = month_colors)) +
      geom_smooth(method = "lm", se = FALSE, color="black") +
      scale_color_identity() + 
      geom_text(aes(vjust = -0.5), size = 3) + 
      labs(title = "Average Temperature vs. Precipitation Levels", x = "Average Temperature (°F)", y = "Average Precipitation (in)")
    
    # Find the regression coefficient
    month_regression <- lm(average_prcp ~ average_temp, data = averageByMonth)
    r_squared <- summary(month_regression)$r.squared    
    