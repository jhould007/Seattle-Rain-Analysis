# Find out which day of the week it rains the most

  # Add a column to rainData for day of the week
  install.packages("lubridate")
  library(lubridate)
  rainData <- mutate(rainData, day_of_week = weekdays(date))

  # Ensure each day occurs with roughly the same frequency (error checking)
  day_counts <- rainData %>%
    group_by(day_of_week = weekdays(date)) %>%
    summarise(count = n())
  
  # Filter rainy days
  rainy_days <- rainData %>%
    filter(rain == TRUE)
  
  # Count the number of rainy days for each day of the week
  rainy_day_counts <- rainy_days %>%
    group_by(day_of_week = weekdays(date)) %>%
    summarise(rainy_count = n())
  
  # Create a bar chart of rainy day counts using ggplot2
  ggplot(rainy_day_counts, aes(x = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = rainy_count, fill=day_of_week)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#3280ad", "#B3E0FF", "#80C9FF", "#4DA6FF", "#1A83FF", "#0066CC", "#004C99")) +
    labs(title = "Rainfall Frequencies by Weekday", x = "Day of the Week", y = "Total Number of Rainy Days") + 
    guides(fill="none")