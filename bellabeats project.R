library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
DailyActivity <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
head(DailyActivity)
colnames(DailyActivity)
view(DailyActivity)
#change ActivityDate to Date
DailyActivity <- rename(DailyActivity, Date = ActivityDate)
#sort by date
DailyActivity <- DailyActivity[order(as.Date(DailyActivity$Date, format = "%m/%d/%Y")),]
view(DailyActivity)
#check for null values
map(DailyActivity, ~sum(is.na(.)))
#check for total number of distinct values
n_distinct(DailyActivity$Id)
#check for duplicated values
sum(duplicated(DailyActivity))
DailyCalories <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
view(DailyCalories)
#change ActivityDay to Date
DailyCalories <- rename(DailyCalories, Date = ActivityDay)
#sort by date
DailyCalories <- DailyCalories[order(as.Date(DailyCalories$Date, format = "%m/%d/%Y")),]
view(DailyCalories)
#check for null values
map(DailyCalories, ~sum(is.na(.)))
#check for total number of distinct values
n_distinct(DailyCalories$Id)
#check for duplicated values
sum(duplicated(DailyCalories))
DailyIntensities <- read_csv("Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
view(DailyIntensities)
#change ActivityDay to Date
DailyIntensities <- rename(DailyIntensities, Date = ActivityDay)
#sort by Date
DailyIntensities <- DailyIntensities[order(as.Date(DailyIntensities$Date, format = "%m/%d/%Y")),]
view(DailyIntensities)
#check for null values
map(DailyIntensities, ~sum(is.na(.)))
#check for total number of distinct values
n_distinct(DailyCalories$Id)
#check for duplicated values
n_distinct(DailyIntensities$Id)
sum(duplicated(DailyIntensities))
DailySteps <- read_csv("Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
view(DailySteps)
#change ActivityDay to Date
DailySteps <- rename(DailySteps, Date = ActivityDay)
#sort by date
DailySteps <- DailySteps[order(as.Date(DailySteps$Date, format = "%m/%d/%Y")),]
view(DailySteps)
#check for null values
map(DailySteps, ~sum(is.na(.)))
#check for total number of distinct values
n_distinct(DailySteps$Id)
#check for duplicated values
sum(duplicated(DailySteps))
DailySleep <- read_csv("Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
view(DailySleep)
#separate SleepDay column to Date and Time
DailySleep <- separate(DailySleep, SleepDay, c("Date", "Time"), sep = " ")
#sort by Date
DailySleep <- DailySleep[order(as.Date(DailySleep$Date, format = "%m/%d/%Y")),]
view(DailySleep)
#check for null values
map(DailySleep, ~sum(is.na(.)))
#check for total number of distinct values
n_distinct(DailySleep$Id)
#check for duplicated values
sum(duplicated(DailySleep))
#remove duplicates
DailySleep <- DailySleep %>% distinct() %>% drop_na()
sum(duplicated(DailySleep))
#merge DailyActivity and DailySleep
DailyActivity <- merge(DailySleep, DailyActivity, by = c("Id", "Date"))
view(DailyActivity)
DailyActivity %>% select(TotalSteps, TotalDistance, Calories) %>% summary()
DailyActivity %>% select(TotalTimeInBed, TotalMinutesAsleep) %>% summary()
DailyActivity %>% select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>% summary()
#plot correlation between values
ggplot(data = DailyActivity) + geom_smooth(mapping = aes(x = TotalSteps, y = TotalDistance)) + geom_point(mapping = aes(x = TotalSteps, y = TotalDistance)) + labs(title = "Total Steps vs Total Distance")
ggplot(data = DailyActivity) + geom_smooth(mapping = aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) + geom_point(mapping = aes(x = TotalMinutesAsleep, y = TotalTimeInBed)) + labs(title = "Total minutes asleep vs Total Time In bed")
ggplot(data = DailyActivity) + geom_smooth(mapping = aes(x = SedentaryMinutes, y = TotalSteps)) + geom_point(mapping = aes(x = SedentaryMinutes, y = TotalSteps)) + labs(title = "Sedentary minutes vs Total Steps")
ggplot(data = DailyActivity) + geom_smooth(mapping = aes(x = SedentaryMinutes, y = TotalMinutesAsleep)) + geom_point(mapping = aes(x = SedentaryMinutes, y = TotalMinutesAsleep)) + labs(title = "Sedentary minutes vs Total minutes asleep")
