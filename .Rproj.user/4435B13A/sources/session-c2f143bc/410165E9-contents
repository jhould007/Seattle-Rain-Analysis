# Analyze entries with missing values
entriesWithMissingValues <- rainData %>%
  filter(rowSums(is.na(.)) > 0)
rm(entriesWithMissingValues)

# Remove entries with missing values
rainData <- na.omit(rainData)
windData <- sqldf("SELECT DATE, AWND, WDFG, WSFG FROM windData WHERE DATE NOT NULL AND AWND NOT NULL AND WDFG NOT NULL AND WSFG NOT NULL")

# Make all dataframe titles lowercase
colnames(rainData) <- tolower(colnames(rainData))
colnames(windData) <- tolower(colnames(windData))

# Remove duplicates
rainData <- distinct(rainData)
windData <- distinct(windData)

# Merge the rainData and windData dataframes
rainData <- merge(windData, rainData, by="date", all=TRUE)

# Filter windData 
windData <- filter(rainData, date >= as.Date("1984-01-01") & date <= as.Date("1996-09-30"))

# Find the max and min PRCP, TMAX, and TMIN
max_values <- sqldf("SELECT MAX(prcp), MAX(tmax), MAX(tmin) FROM rainData")
min_values <- sqldf("SELECT MIN(prcp), MIN(tmax), MIN(tmin) FROM rainData")
max_prcp <- sqldf("SELECT date, MAX(prcp) FROM rainData")
min_prcp <- sqldf("SELECT date, MIN(prcp) FROM rainData")
max_tmax <- sqldf("SELECT date, MAX(tmax) FROM rainData")
min_tmax <- sqldf("SELECT date, MIN(tmax) FROM rainData")
max_tmin <- sqldf("SELECT date, MAX(tmin) FROM rainData")
min_tmin <- sqldf("SELECT date, MIN(tmin) FROM rainData")

# Remove all max and min values
rm(max_values)
rm(min_values)
rm(max_prcp)
rm(min_prcp)
rm(max_tmax)
rm(min_tmax)
rm(max_tmin)
rm(min_tmin)

# Check for negative values
negativeValue <- sqldf("SELECT * FROM rainData WHERE prcp < 0 OR tmax < 0 OR tmin < 0")
rm(negativeValue)

# Reset dataframe to clean, original version
rainData <- rainData_backup

# Update rainData backup to latest version
rainData_backup <- rainData
