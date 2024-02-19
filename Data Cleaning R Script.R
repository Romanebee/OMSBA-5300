###Romane Beeharry
###OMSBA 5300
###Week 7 Data Exploration Challenge

#Data Cleaning Portion

#Read the Google Trends Data
#Load the necessary libraries
library(tidyverse)
library(dplyr)
library(rio)
library(stringr)
library(lubridate)
library(fixest)

#Read the Google Trends Files
google_trends_files <- list.files(path = "C:/Users/roman/OneDrive/Documents/OMSBA 5300 Econometrics/Data Exploration Challenge/Week 7 Data Exploration Challenge/Lab3_Rawdata", 
pattern = "trends_up_to_", full.names = TRUE)

#Bind the files into a single data set
google_trends_data <- import_list(google_trends_files, rbind = TRUE)

#get the first ten characters out of the "monthorweek" variable
google_trends_data$monthorweek <- as.Date(ymd(str_sub(google_trends_data$monthorweek, 1, 10)))

#Aggregate the dates to get months instead of weeks
google_trends_data$month <- floor_date(google_trends_data$monthorweek, unit = "month")

#Standardize the index variable by schoolname and keyword
google_trends_data <- google_trends_data %>%
  group_by(schname, keyword) %>%
  mutate(standardized_index = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE))

#Aggregate the standardized index to the school-month level
school_month_level <- google_trends_data %>%
  group_by(schname, month) %>%
  summarize(
    mean_standardized_index = mean(standardized_index, na.rm = TRUE))

#Read the scorecard data
scorecard_data <- import("C:/Users/roman/OneDrive/Documents/OMSBA 5300 Econometrics/Data Exploration Challenge/Week 7 Data Exploration Challenge/Lab3_Rawdata/Most+Recent+Cohorts+(Scorecard+Elements).csv")

#Read the id name and link data 
id_name_link_data <- import("C:/Users/roman/OneDrive/Documents/OMSBA 5300 Econometrics/Data Exploration Challenge/Week 7 Data Exploration Challenge/Lab3_Rawdata/id_name_link.csv")

#Merge the Scorecard Data to the Trends Data
#Count how many times each school name appears in id_name_link
school <- id_name_link_data %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  ungroup()

# Filter out school names that appear more than once
id_name_link_filtered <- school %>%
  filter(n <= 1)%>%
  select(schname, unitid, opeid)

# Merge the data into a final dataset that is limited to schools that predominantly grant Bachelor's Degrees
Final_trend_data <- school_month_level %>%
  inner_join(id_name_link_filtered, by = "schname") %>%
  inner_join(scorecard_data, by = c("unitid" = "UNITID", "opeid" = "OPEID")) %>%
  filter(PREDDEG == 3)

# Subset the data to only include the variables needed for the regression analysis
Analysis_df <- data.frame(
  SchoolName = Final_trend_data$INSTNM,
  City = Final_trend_data$CITY,
  State = Final_trend_data$STABBR,
  Month = Final_trend_data$month,
  StandardizedIndex = Final_trend_data$mean_standardized_index,
  SchoolURL = Final_trend_data$INSTURL,
  NPCURL = Final_trend_data$NPCURL,
  PREDDEG = Final_trend_data$PREDDEG,
  SchoolType = Final_trend_data$CONTROL,
  Locale = Final_trend_data$LOCALE,
  AverageSATAll = Final_trend_data$SAT_AVG_ALL,
  EarningsAfter10Years = Final_trend_data$'md_earn_wne_p10-REPORTED-EARNINGS')

#Define a threshold in order to classify earnings into high-earnings and low-earnings
threshold <- median(Analysis_df$EarningsAfter10Years)

#Create a binary variable for high-earning and low-earning
Analysis_df$HighEarning <- ifelse(Analysis_df$EarningsAfter10Years >= threshold, 1, 0)

# Define the release date of the College Scorecard
scorecard_release_date <- as.Date("2015-09-01")

# Create the binary variable PostScorecard
Analysis_df$PostScorecard <- ifelse(Analysis_df$Month >= scorecard_release_date, 1, 0)


#Export the clean data for analysis into a csv file
rio::export(Analysis_df, "ScorecardTrendsAnalysis_data.csv")
