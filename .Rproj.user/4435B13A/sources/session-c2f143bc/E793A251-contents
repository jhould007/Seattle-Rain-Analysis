# Find % of days that were rainy
num_days <- nrow(rainData)
num_rainy_days <- nrow(sqldf("SELECT * FROM rainData WHERE RAIN == TRUE"))
percent_rainy_days <- (num_rainy_days / num_days) * 100

# Find the average precipitation level, max temp, and min temp for each month
  # Add new columns for year and month
  rainData2 <- rainData
  rainData <- mutate(rainData, year = substr(date, 1, 4))
  rainData <- mutate(rainData, month = substr(date, 6, 7))

  # Reorder columns so year and month are next to date
  rainData <- rainData[, c(1,6,7,2,3,4,5)]
  
  # Find the average values for each month
  averageByMonth <- rainData %>%
    group_by(month) %>%
    summarize(average_prcp = mean(prcp, na.rm = TRUE), average_tmax = mean(tmax, na.rm = TRUE), average_tmin = mean(tmin, na.rm = TRUE))
  
  # Graph average precipitation level for each month
  ggplot(data = averageByMonth) +
    geom_line(aes(x = month, y = average_prcp), size = 1.5, color = "blue", linetype = "solid", group=1) +
    labs(title = "Average Precipitation by Month",
         x = "Month",
         y = "Average Precipitation (in)") +
    theme_minimal()
  
  # Graph average max and min temperatures for each month
  ggplot(data = averageByMonth) +
    geom_line(aes(x = month, y = average_tmax, color = "Max temperatures"), size = 1.5, linetype = "solid", group = 1) +
    geom_line(aes(x = month, y = average_tmin, color = "Min temperatures"), size = 1.5, linetype = "solid", group = 2) +
    labs(title = "Average Max and Min Temperatures by Month",
         x = "Month",
         y = "Average Temperatures (°F)") +
    scale_color_manual(name = "Legend", values = c("Max temperatures" = "#F4A438", "Min temperatures" = "#30E1CB")) +
    theme_minimal()
  
# Find average values for each year
  averageByYear <- rainData %>%
    group_by(year) %>%
    summarize(average_prcp = mean(prcp, na.rm = TRUE), average_tmax = mean(tmax, na.rm = TRUE), average_tmin = mean(tmin, na.rm = TRUE))
  
# Find driest and wettest year on record
  driestYear <- sqldf("SELECT year, MIN(average_prcp) FROM averageByYear")
  wettestYear <- sqldf("SELECT year, MAX(average_prcp) FROM averageByYear")
  
# Graph average precipitation for each year
    ggplot(data = averageByYear) +
    geom_line(aes(x = 1948:2017, y = average_prcp), size = 1.5, color = "black", linetype = "solid", group=1) +
    geom_hline(yintercept=sqldf("SELECT AVG(average_prcp) FROM averageByYear")[1,1], linetype="dashed",color="red") +
    labs(title = "Average Yearly Precipitation",
         x = "Year",
         y = "Average Precipitation (in)") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
    scale_x_continuous(breaks = seq(min(averageByYear$year), max(averageByYear$year), by = 5))
  
# Graph average precipitation from 1948 to 1955
  averageByYear48to55 <- head(averageByYear, 8)
  ggplot(data = averageByYear48to55) +
    geom_line(aes(x = 1948:1955, y = average_prcp), size = 1.5, color = "black", linetype = "solid") +
    labs(title = "Average Yearly Precipitation, 1948-1955",
         x = "Year",
         y = "Average Precipitation (in)") +
    geom_vline(xintercept = c(1950,1953), color = "red", linetype = "solid") + 
    annotate(geom="label", 1951.5, 0.15, label="Duration of El Niño", fill="red", color="white")
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    
# Graph average precipitation from 1990 to 2001
  averageByYear90to01 <- sqldf("SELECT * FROM averageByYear WHERE CAST(year AS REAL) >= 1990 AND CAST(year AS REAL) <= 2001")
  ggplot(data = averageByYear90to01) +
    geom_line(aes(x = 1990:2001, y = average_prcp), linewidth = 1.5, color = "black", linetype = "solid") +
    labs(title = "Average Yearly Precipitation, 1993-1999",
        x = "Year",
        y = "Average Precipitation (in)") +
    geom_vline(xintercept = c(1993,1999), color = "red", linetype = "solid") + 
    annotate(geom="label", 1996, 0.15, label="Duration of La Niña", fill="red", color="white") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
# Find hottest and coldest years on record
  hottestYear <- sqldf("SELECT year, average_tmax, average_tmin, SUM(average_tmax + average_tmin) AS sum FROM averageByYear GROUP BY year ORDER BY sum DESC LIMIT 1")
  coldestYear <- sqldf("SELECT year, average_tmax, average_tmin, SUM(average_tmax + average_tmin) AS sum FROM averageByYear GROUP BY year ORDER BY sum ASC LIMIT 1")
  
  
  