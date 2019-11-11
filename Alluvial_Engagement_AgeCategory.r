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


### Data Exploration

unique(attendance_df$mem_type)
error_raw_neg1 <- filter(attendance_df, d4g_member_id == -1)

min(as.Date(attendance_df$date_attended))

unique(check_in_attendance_df$mem_type)
unique(check_in_attendance_df$age_category)
sort(unique(check_in_attendance_df$check_in_week))

min(as.Date(check_in_attendance_df$date_attended))

unique(check_in_Cohort_stats$Eng)

error_neg1 <- filter(member_dim_cohort, d4g_member_id == -1)
###


### Data Wrangling

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
  if(x >= 2.00) {"Ideal"
  } else if(x >= 1.00) {"Fair"
  } else if(x >= 0.25) {"Light"
  } else if(x >= 0.125) {"Poor"
  } else if(x >= 0.001) {"Limited"
  } else {"None"}
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
  
  group_by(d4g_member_id, age_category) %>%
  
  summarize(Eng = mean(check_in_wk_avg_Cohort)) %>%
  
  mutate(
    Eng_Level = engagementFunction(Eng)
  ) %>%
  
### ATTEMPTING TO CREATE ENG.J, ENG.I, ENG.S FOR EACH MEMBER  
  
eng_age_group <-
  
  # start a pipeline
  check_in_Cohort_stats %>%
  
  #  select variables of interest
  select(d4g_member_id, age_category, depress, dep16) %>%
  
  # organise by ID, by treatment, by time
  # create dim: "variable" (l: depressed, not depressed)
  # create dim: "value" (l: depress value)
  pivot_longer(c(-ID, -w1group, -time), names_to = "variable", values_to = "value") %>%
  
  # create dim: "variableT"; 
  # pastes together "variable" and "time"
  # pastes together "value" and "time"
  unite(variableT, variable, time, sep=".") %>%
  
  # breaks "variableT" into new columns
  spread(variableT, value)
##


## Reshape check_in_groupings_df; add in additional metrics for complete year, engagement levels
check_in_groupings_wide_df <- check_in_groupings_df %>%
  spread(key = check_in_period, value = number_check_ins) %>%
  rename(
    school_total_visits = School,
    summer_total_visits = Summer
  ) %>%
  mutate(
    school_total_visits = ifelse(is.na(school_total_visits), 0, school_total_visits),
    summer_total_visits = ifelse(is.na(summer_total_visits), 0, summer_total_visits),
    year_total_visits = school_total_visits + summer_total_visits,
    school_visits_per_week = round(school_total_visits / 40, digits = 2),
    summer_visits_per_week = round(summer_total_visits / 8, digits = 2),
    year_visits_per_week = round(year_total_visits / 48, digits = 2),
    school_engagement_lvl = engagementFunction(school_visits_per_week),
    summer_engagement_lvl = engagementFunction(summer_visits_per_week),
    year_engagement_lvl = engagementFunction(year_visits_per_week),
    ideal_engagement_flag = if(school_visits_per_week >= 2.00 | summer_visits_per_week >= 2.00) {TRUE
    } else {FALSE},
    highest_engagement_period = if(school_visits_per_week >= summer_visits_per_week) {"School"
    } else {"Summer"},
    #lowest_engagement_period = if(school_visits_per_week < summer_visits_per_week) {"School"
    #} else {"Summer"},
    year_engagement_profile = if(school_total_visits == 0) {"Summer Only"
    } else if(summer_total_visits == 0) {"School Only"
    } else {"Complete Year"},
    higest_engagement_level = if(school_visits_per_week >= summer_visits_per_week) {
      school_engagement_lvl
    } else if (school_visits_per_week <= summer_visits_per_week) {
      summer_engagement_lvl}
  )
##

## Remove numeric columns for final table format
check_in_groupings_final_df <- check_in_groupings_wide_df %>%
  select(-school_total_visits, -summer_total_visits, -year_total_visits,
         -school_visits_per_week, -summer_visits_per_week, -year_visits_per_week) %>%
  mutate(
    record_count = 1,
    higest_engagement_level = factor(higest_engagement_level, c("Ideal", "Average", "Light", "Poor", "Limited", "None"))
  )
##
###