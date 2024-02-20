# Final Project - data wrangling 
# Group Member: Esabel Zhang
# relationship between sleep quality and depression 

library(dplyr)
library(stringr)
library(testthat)

sleep_health_lifestyle_dy <- read.csv("Sleep_health_and_lifestyle_dataset.csv")
sleepStudy_dy <- read.csv("SleepStudy.csv")

head(sleep_health_lifestyle_dy) 
head(sleepStudy_dy) 

# 1. sleep_health_and_lifestyle_dataset
# Dataset Columns:
# Person ID: An identifier for each individual.
# Gender: The gender of the person (Male/Female).
# Age: The age of the person in years.
# Occupation: The occupation or profession of the person.
# Sleep Duration (hours): The number of hours the person sleeps per day.
# Quality of Sleep (scale: 1-10): A subjective rating of the quality of sleep, ranging from 1 to 10.
# Physical Activity Level (minutes/day): The number of minutes the person engages in physical activity daily.
# Stress Level (scale: 1-10): A subjective rating of the stress level experienced by the person, ranging from 1 to 10.
# BMI Category: The BMI category of the person (e.g., Underweight, Normal, Overweight).
# Blood Pressure (systolic/diastolic): The blood pressure measurement of the person, indicated as systolic pressure over diastolic pressure.
# Heart Rate (bpm): The resting heart rate of the person in beats per minute.
# Daily Steps: The number of steps the person takes per day.
# Sleep Disorder: The presence or absence of a sleep disorder in the person (None, Insomnia, Sleep Apnea).

# 2.sleepstudy
# Gender: The gender of the participant (0 might indicate one gender, and another number might indicate another, though the encoding is not specified).
# ClassYear: The academic year of the participant (e.g., 1 for freshman, 2 for sophomore, etc.).
# LarkOwl: Self-identification as a morning person ("Lark"), evening person ("Owl"), or neither.
# NumEarlyClass: Number of classes before 9 a.m.
# EarlyClass: Indicator of whether they have early classes (1) or not (0).
# GPA: Grade point average of the participant.
# ClassesMissed: Number of classes missed by the participant.
# CognitionZscore: Standardized score relating to cognitive performance.
# PoorSleepQuality: A measure of sleep quality, likely based on a survey or questionnaire.
# DepressionScore: A score indicating the level of depression symptoms, likely based on a standardized depression assessment tool.
# AnxietyScore: Similar to DepressionScore, this likely measures anxiety levels.
# StressScore: A measure of perceived stress.
# AlcoholUse: Categorization of alcohol consumption (e.g., Light, Moderate).
# Drinks: Number of alcoholic drinks consumed in a typical week.
# WeekdayBed: Average bedtime on weekdays (possibly in hours, with decimal points indicating parts of an hour).
# WeekdayRise: Average rise time on weekdays.
# WeekdaySleep: Average amount of sleep on weekdays.
# WeekendBed: Average bedtime on weekends.
# WeekendRise: Average rise time on weekends.
# WeekendSleep: Average amount of sleep on weekends.
# AverageSleep: Overall average amount of sleep.
# AllNighter: Indicator of whether the participant has stayed up all night (e.g., for studying) during the period of study (1 for yes, 0 for no).

# !!!!Traits:
# 1.sleep_health: stress-level - ranke it normal / high
#                 sleep_disorder: none/ Insomnia/ Sleep Apnea
# sleep duration:
# quality of sleep
# 2. sleepstudy: depression stastus: normal/ moderate / severe
# stress: normal/ high
# anxiety: normal/ moderate / severe
# may study the relationship between stress and anxiety and depression 
# poorsleep quality ----- quality of sleep
# avarage sleep ---- sleep duration

# combined two dataset by stress level

# add a new column to show the stress level
sleep_health_lifestyle_dy <- sleep_health_lifestyle_dy  %>%
                            mutate( "Stress" = case_when(
                              Stress.Level >= 4 ~ "high",
                              TRUE ~ "normal"  # This acts as the 'else' condition
                            ))

names(sleepStudy_dy)[names(sleepStudy_dy) == "Gender"] <- "Gender.x"
sleepStudy_dy$Gender.x <- ifelse(sleepStudy_dy$Gender.x == 1, "male", "female")

# sleepStudy_dy <- sleepStudy_dy %>%
#                  mutate("Age" =17)


# combined the data by stress level 
combined_dy <- merge(sleep_health_lifestyle_dy, sleepStudy_dy, by = "Stress", all = TRUE)

#delete unusaful column
combined_dy <- combined_dy %>% 
                select(-c("LarkOwl", 
                          "NumEarlyClass", 
                          "EarlyClass", 
                          "AllNighter",
                          "ClassesMissed" ,
                          "Happiness",
                          "AlcoholUse",
                          "Drinks",
                          "WeekdayBed",
                          "WeekdayRise",
                          "WeekdaySleep",
                          "WeekendBed",
                          "WeekendRise",
                          "WeekendSleep"))

combined_dy <- combined_dy %>% 
                select(-c("ClassYear",
                          "DepressionScore",
                          "AnxietyScore",
                          "StressScore"))

combined_dy <- combined_dy %>% 
                select(-c("Person.ID",
                          "Stress.Level"))

#to do list 
# create new variable 
# import one column variabel into the other so that the page will be cleanner 



# Must create at least one summarization data frame 

#quality of sleep - 4-9
#average sleep - 4.95-10.62

# Must create at least one new categorical variable
# Age group:
# Teenagers (14-17 years): 8-10 hours per day.
# Young adults (18-25 years): 7-9 hours per night.
# Adults (26-64 years): 7-9 hours per night.
# Older adults (65+ years): 7-8 hours per night.


combined_dy <- combined_dy %>%
  mutate(AgeGroup = case_when(
    Age >= 14 & Age <= 17 ~ "Teenagers",
    Age >= 18 & Age <= 25 ~ "Young adults",
    Age >= 26 & Age <= 64 ~ "Adults",
    Age >= 65 ~ "Older adults",
    #TRUE ~ "Other"  # This line is optional for handling ages outside the specified ranges
  ))

# Question:
# age group doesn't make sense
# why so many obs


# Must create at least one new continuous/numerical variable
combined_dy <- combined_dy %>%
               
