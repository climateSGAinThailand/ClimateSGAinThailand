################################################################################
#
# Data Cleaning and Preparation script 1
# 
################################################################################

#############
# Section 1: Weekly average temperature for all subjects in ANC raw data 
#            (those gestational weeks != NA)
#

# Load the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the data
weatherdata <- read_excel("MaeSot_WeatherStatio_1986-2020.xlsx")
ANCdata <- read_dta("Demodel_ayemin_011222.dta") # ANC raw data
ANCdata <- ANCdata %>% 
  mutate(gestational_weeks = lengthpreg_days %/% 7)

ANCdata <- ANCdata %>%
  mutate(lmp_month = month(lmp), lmp_day = day(lmp)) %>%
  mutate(Season = case_when(
    (lmp_month == 2 & lmp_day >= 16) | (lmp_month >= 3 & lmp_month <= 4) | (lmp_month == 5 & lmp_day <= 15) ~ "Summer",
    (lmp_month == 5 & lmp_day >= 16) | (lmp_month >= 6 & lmp_month <= 9) | (lmp_month == 10 & lmp_day <= 15) ~ "Rainy",
    (lmp_month == 10 & lmp_day >= 16) | (lmp_month == 11) | (lmp_month == 12) | (lmp_month == 1 & lmp_day <= 15) ~ "Winter"
  ))


# Step 1: Create a data frame with all dates during pregnancy ####
# Extract columns anc_code, lmp, gestational_weeks from the dataset ANCdataclean
pregnant_data <- ANCdata %>% 
  select(anc_code, lmp, gestational_weeks) %>%
  filter(!is.na(gestational_weeks) & !is.na(lmp))

pregnant_dates <- pregnant_data %>%
  rowwise() %>%
  mutate(Dates = list(seq(lmp, by = "day", length.out = gestational_weeks * 7))) %>%
  tidyr::unnest(cols = Dates)

rm(pregnant_data)


# Step 2: Assign each date a gestational week label ####
# # Method 1: assigning according to the real calendar
# pregnant_dates <- pregnant_dates %>%
#   mutate(Week = week(Dates) - week(lmp) + 1)
# Method 2: assigning according to the gestational weeks for each anc_code
pregnant_dates <- pregnant_dates %>%
  group_by(anc_code) %>%
  mutate(Week = rep(seq_len(gestational_weeks[1]), each = 7))
# Week start from week 0 (LMP - LMP+6)
pregnant_dates <- pregnant_dates %>%
  mutate(Week = Week - 1)


# Step 3: Merge daily temperature dataset by date to create a new column called daily temperature
# Convert the DATE column in the weatherdata dataset to a Date object
weatherdata_selected <- weatherdata %>%
  mutate(DATE = as.Date(DATE, format = "%m/%d/%Y")) %>%
  select(DATE, TMAX, TAVG, TMIN)
merged_data <- left_join(pregnant_dates, weatherdata_selected, by = c("Dates" = "DATE"))
write.csv(merged_data, "merged_data.csv") 
# this dataset can be used to do the humidity and heat data without operating the former steps

rm(weatherdata_selected)
rm(pregnant_dates)

# Step 4: Summarize to calculate the weekly average temperature and weekly 
# standard deviation of temperature for each gestational week for each pregnant woman
weekly_temp <- merged_data %>%
  group_by(anc_code, Week) %>%
  summarise(weekly_avg_temp = mean(TAVG, na.rm = TRUE),
            weekly_sd_temp_Celsius = sd(((TAVG- 32) * 5/9), na.rm = TRUE),
            weekly_max_temp = mean(TMAX, na.rm = TRUE))
write.csv(weekly_temp, "weekly_temp_new.csv") 
# this dataset is useful for merging with the ANCdata - rowwise


# weekly_temp <- weekly_temp[, -ncol(weekly_temp)] # long format data
# Reshape the dataset
wide_temp_data <- weekly_temp %>%
  pivot_wider(names_from = Week, values_from = c(weekly_sd_temp_Celsius, weekly_avg_temp, weekly_max_temp))


# merge with the ANCdata
ANC_weekly_temp <- inner_join(ANCdata, wide_temp_data, by = "anc_code")
#write.csv(ANC_weekly_temp, "WeeklyAverageTemperature.csv")



######################################################################
# Section 2: Daily average humidity for all subjects in ANC raw data
######################################################################

# Load the data
humidity_data <- read_excel("MaeSotWeatherStation_humidity_new_1986-2020.xlsx")
# change the first column date format from %y-%m-%d to %y-%m
humidity_data$`Month/Year` <- format(as.Date(humidity_data$`Month/Year`, format = "%y-%m-%d"), "%Y-%m")
# change the column name of column 2 to 31 to day1 to day31
colnames(humidity_data)[2:32] <- paste0("day", 1:31)
colnames(humidity_data)[1] <- "year_month"
humidity_data <- humidity_data %>% select(year_month, day1:day31)

# Convert the wide format to long format
humidity_long <- humidity_data %>%
  gather(key = "day", value = "humidity", -year_month) %>%
  mutate(day = as.integer(sub("day", "", day))) %>%
  mutate(date = ymd(paste(year_month, day, sep = "-")))

# Select the relevant columns and arrange by date
humidity_long <- humidity_long %>%
  select(date, humidity) %>%
  arrange(date)

#write.csv(humidity_long, "daily_humidity_long.csv")


############################
# daily heat index (HI)
############################
# combine the two datasets
daily_temp_humid <- cbind(temp_long, humidity_long)
daily_temp_humid <- daily_temp_humid[, -3]
daily_temp_humid <- daily_temp_humid %>%
  filter(!is.na(temp) & !is.na(humidity)) # remove missing values

# calculate the heat index (in Fahrenheit)
daily_temp_humid <- daily_temp_humid %>%
  mutate(temp_F = temp * 9/5 + 32) %>%
  mutate(HI = -42.379 + 2.04901523 * temp_F + 10.14333127 * humidity - 
           0.22475541 * temp_F * humidity - 6.83783e-3 * temp_F^2 - 
           5.481717e-2 * humidity^2 + 1.22874e-3 * temp_F^2 * humidity + 
           8.5282e-4 * temp_F * humidity^2 - 1.99e-6 * temp_F^2 * humidity^2)

#write.csv(daily_temp_humid, "daily_temp_humid_heat.csv")


################################################################################
# Final combination of weekly weather data - avg temp, max temp, humidity, HI
################################################################################

# load the data
merged_data <- read.csv("merged_data.csv")
colnames(daily_temp_humid)[1] <- "Dates"
merged_data$Dates <- as.Date(merged_data$Dates)

# merge the data
weekly_weather_data <- merged_data %>%
  left_join(daily_temp_humid, by = "Dates")
colnames(weekly_weather_data)[10] <- "TAVG_new"
weekly_weather_data <- weekly_weather_data[, -1]

# write.csv(weekly_weather_data, "merged_data.csv")

weekly_temp <- weekly_weather_data %>%
  group_by(anc_code, Week) %>%
  summarise(weekly_avg_temp = mean(TAVG, na.rm = TRUE),
            weekly_sd_temp_Celsius = sd(((TAVG- 32) * 5/9), na.rm = TRUE),
            weekly_max_temp = mean(TMAX, na.rm = TRUE),
            weekly_avg_temp_new = mean(TAVG_new, na.rm = TRUE),
            weekly_sd_temp_Celsius_new = sd(TAVG_new, na.rm = TRUE),
            weekly_humidity = mean(humidity, na.rm = TRUE),
            weekly_HI = mean(HI, na.rm = TRUE))
weekly_climate <- weekly_temp


weekly_climate <- weekly_climate %>%
  select(anc_code, Week, weekly_sd_temp_Celsius, weekly_avg_temp,
         weekly_max_temp, weekly_avg_temp_new, weekly_sd_temp_Celsius_new, weekly_humidity, weekly_HI)

# Reshape the dataset to wide format
wide_climate_data <- weekly_climate %>%
  pivot_wider(names_from = Week, values_from = c(weekly_sd_temp_Celsius, weekly_avg_temp,
                                                 weekly_max_temp, weekly_avg_temp_new, 
                                                 weekly_sd_temp_Celsius_new, weekly_humidity, 
                                                 weekly_HI))

# merge with the ANCdata
ANC_weekly_climate <- left_join(ANCdata, wide_climate_data, by = "anc_code")
write.csv(ANC_weekly_climate, "ANCdata_climate_final.csv")
#ANCdata_wrong <- read.csv("ANCdata_climate_final.csv")
