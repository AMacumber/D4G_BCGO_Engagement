### Engagement Status
## Author: Andrew Macumber
###

rm(list = ls())

### load required libraries
library(janitor)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
###


### Import data files
attendance_df <- read.table("Attendance.txt", sep = "|", header = T, stringsAsFactors = F) %>%
  clean_names()

member_df <- read.table("Member.txt", sep = "|", header = T, fill = T, stringsAsFactors = F) %>%
  clean_names()

program_df <- read.table("Program.txt", sep = "|", header = T, stringsAsFactors = F) %>%
  clean_names()
###


### Data Wrangling

starting_obs <- nrow(attendance_df)

## select check-in info from program_df
program_or_check_in_df <- program_df %>%
  select(d4g_program_id, d4g_program_or_check_in)

## Table: All attributes filtered by:
# d4g_program_or_check_in: "Check-In Only"
# mem_types: M - Member; U - Trying Out; Q - Aquatic Child
# age_group: J - Juvenile; I - Intermediate; S - Senior
# check_in_year: range(2009, 2019) ; last ten years
# check_in_period: Summer (July, August) ; else School

check_in_attendance_df <- attendance_df %>%
  
  # use check-in info from program_df to filter attendance_d for check-in data
  left_join(program_or_check_in_df, by = c("d4g_program_id" = "d4g_program_id")) %>%
  filter(d4g_program_or_check_in == "Check-In Only") %>%
  select(-d4g_program_or_check_in) %>%
  
  # remove volunteers
  filter(age_category != "V") %>%
  
  # remove mem_type aquatic adult ('A')
  filter(mem_type != "A") %>%
  
  # remove d4g_member_id == -1; seems to be given to many members
  filter(d4g_member_id != -1) %>%
  
  # expand date info to several columns
  # create dim: "check_in_period" to label Summer and School Periods
  mutate(
    check_in_date = as.Date(date_attended),
    check_in_year = year(as.Date(date_attended)),
    check_in_month = month(as.Date(date_attended)),
    check_in_week = week(as.Date(date_attended)),
    check_in_period = ifelse(check_in_month == 7 | check_in_month == 8, "Summer", "School")
  ) %>%
    
  # remove pre-school year 2009 | summer 2019
  filter(check_in_date >= "2009-09-01") %>%
  filter(check_in_date < "2019-07-01") %>%
  
  unite(Cohort, check_in_period, check_in_year, sep = ".")

rm(program_or_check_in_df)
##

## Return change in dataset dimensions
filtered_obs <- nrow(check_in_attendance_df)
(filtered_obs/starting_obs)*100

## Table: number of weeks per Cohort (period & year)
no_weeks_cohort <- check_in_attendance_df %>%
  
  group_by(Cohort) %>%
  
  summarize(no_weeks = max(check_in_week))
##

## Table: Member Attributes by Cohort
member_dim_cohort <- check_in_attendance_df %>%
  
  select(d4g_member_id, member_location, sex, member_age, age_category, Cohort) %>%
  
  group_by(d4g_member_id, Cohort)
##

## Function to categorize engagement level based on average visits per week
engagementFunction <- function(x) {
  if(x >= 2.00) {"1. Ideal"
  } else if(x >= 1.00) {"2. Fair"
  } else if(x >= 0.25) {"3. Light"
  } else if(x >= 0.125) {"4. Poor"
  } else if(x >= 0.001) {"5. Limited"
  } else {"6. None"}
}
##

## Table: Member Return weekly counts and averages w/ member attributes
check_in_Cohort_stats <- check_in_attendance_df %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id, Cohort) %>%
  
  # Return week counts
  summarize(check_in_count_Cohort = n()) %>%
  
  # Add number of weeks
  left_join(no_weeks_cohort, by = ("Cohort" = "Cohort")) %>%
  
  # Create dim: weekly average for members by cohort
  # Create dim: engagement status for member by cohort
  mutate(
    check_in_wk_avg_Cohort = round(check_in_count_Cohort / no_weeks, 3),
    Eng = engagementFunction(check_in_wk_avg_Cohort)
    ) %>%
  
  # Add member attributes
  left_join(member_dim_cohort, by = c("d4g_member_id" = "d4g_member_id", "Cohort" = "Cohort"))
##

## Table: Member Engagement Status by Age_Group
eng_age_group <- check_in_Cohort_stats %>%
  
  # Group by: ID, Age_category
  group_by(d4g_member_id, age_category) %>%
  
  # Return weekly mean for cohort
  summarize(Eng = mean(check_in_wk_avg_Cohort)) %>%
  
  # Create dim: Eng Status for Cohort
  mutate(
    Eng_Level = engagementFunction(Eng)
  ) %>%
  
  # Subset the dataframe with columns of interest
  select(d4g_member_id, age_category, Eng_Level) %>%
  
  # Create new dims; "variable" = Eng_Level; "value" = Eng_Level values
  pivot_longer(c(-d4g_member_id, -age_category), names_to = "variable", values_to = "value") %>%
  
  # New dim: combination of "variable" and "age_category" (ex., Eng_Level.S)
  unite(variableT, variable, age_category, sep=".") %>%
  
  # breaks "variableT" into new columns
  spread(variableT, value)
##


## Return change in dataset dimensions
group_obs <- nrow(eng_age_group)
(group_obs/starting_obs)*100
(group_obs/filtered_obs)*100


## alluvial prep
datAlluvial <-
  
  # start a pipeline
  eng_age_group %>%
  
  # dep16 is a binary indicator of depression
  group_by(Eng_Level.J, Eng_Level.I, Eng_Level.S) %>%
  
  # create dim: "n" count of each of unique combos of dep16 at time 1,2,3,4
  summarise(n = n()) #%>%
##


## Return change in dataset dimensions
alluvial_obs <- sum(datAlluvial$n)
(alluvial_obs/starting_obs)*100
(alluvial_obs/filtered_obs)*100
(alluvial_obs/group_obs)*100  # no change


## Alluvial diagram

# Load library and create display window
library(alluvial)
windows(10,7)
#

# Change 'na' to "6. None"
datAlluvial[is.na(datAlluvial)] <- "6. None"
#

# if at Junior or Intermediate Ideal, Fair or Light highlight Green, Yellow, Blue
alluvial(datAlluvial[,1:3],  # Eng_Level at J, I, S
         freq=datAlluvial$n,  # counts of each unique combination
         col = ifelse(datAlluvial$Eng_Level.J == "1. Ideal", "green", 
                      ifelse(datAlluvial$Eng_Level.I == "1. Ideal", "green",
                             ifelse(datAlluvial$Eng_Level.S == "1. Ideal", "green",
                                    ifelse(datAlluvial$Eng_Level.J == "2. Fair", "yellow", 
                                           ifelse(datAlluvial$Eng_Level.I == "2. Fair", "yellow",
                                                  ifelse(datAlluvial$Eng_Level.S == "2. Fair", "yellow",
                                                         ifelse(datAlluvial$Eng_Level.J == "3. Light", "sky blue", 
                                                                ifelse(datAlluvial$Eng_Level.I == "3. Light", "sky blue",
                                                                       ifelse(datAlluvial$Eng_Level.S == "3. Light", "sky blue", "#D3D3D3"))))))))),
         axis_labels = c("Junior", "Intermediate", "Senior"),
         hide = datAlluvial$n < 10,
         cex = 0.7)
##
###