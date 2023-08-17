# Look for correlation between avg wind speed and precipitation level
windSample <- sample_n(windData, 200)
windSample <- sqldf("SELECT date, awnd, wdfg, wsfg, prcp, rain FROM windSample")

# Create the scatterplot
ggplot(windSample, aes(x = awnd, y = prcp)) +
  geom_point(color="blue") +
  geom_smooth(method = "lm", se = FALSE, color="black") +
  labs(title = "Wind Speed vs. Precipitation Levels", x = "Average Wind Speed (mph)", y = "Precipitation (in)")

# Find the regression coefficient
r_squared_windSpeed <- summary(lm(awnd ~ prcp, data = windSample))$r.squared

# Find how many entries have a wdfg value that is not a mult of 45
notMultOf45 <- sqldf("SELECT * FROM windSample WHERE wdfg%45 != 0")
rm(notMultOf45)

# Find average precipitation for each wind direction
prcpPerWindDirection <- sqldf("SELECT wdfg, AVG(prcp) AS avg_prcp FROM windSample GROUP BY wdfg")
prcpPerWindDirection <- mutate(prcpPerWindDirection, dir=c("NE", "E", "SE", "S", "SW", "W", "NW", "N"))
prcpPerWindDirection <- prcpPerWindDirection[, c(1,3,2)]

# Show data in a bar graph
ggplot(prcpPerWindDirection, aes(x = dir, y = avg_prcp)) +
  geom_bar(stat = "identity", fill = "#34e8eb") +
  labs(title = "Average Precipitation by Wind Direction", x = "Wind Direction", y = "Average Precipitation (in)") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))