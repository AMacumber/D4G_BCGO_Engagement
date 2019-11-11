### Engagement Levels by Member and Year
## Author: Alex Campbell
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

## Join attendance_df and program_df to determine check-in records; filter to only include these records from 2014-2018; add new data points
program_or_check_in_df <- program_df %>%
  select(d4g_program_id, d4g_program_or_check_in)

check_in_attendance_df <- attendance_df %>%
  left_join(program_or_check_in_df, by = c("d4g_program_id" = "d4g_program_id")) %>%
  filter(d4g_program_or_check_in == "Check-In Only") %>%
  select(-d4g_program_or_check_in) %>%
  filter(age_category != "V") %>%
  mutate(
    check_in_year = year(as.Date(date_attended)),
    check_in_month = month(as.Date(date_attended)),
    check_in_period = ifelse(check_in_month == 7 | check_in_month == 8, "Summer", "School")
  ) %>%
  filter(between(check_in_year, 2014, 2018))
##

## Group and summarize check_in_attendance_df by member, year, period
check_in_groupings_df <- check_in_attendance_df %>%
  group_by(d4g_member_id, check_in_year, check_in_period) %>%
  summarize(number_check_ins = n())
###

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


### Create a stacked bar chart to visually represent the engagement levels from 2014-2018
ggplot(check_in_groupings_final_df, aes(fill = year_engagement_profile, y = record_count, x = higest_engagement_level)) + 
  geom_bar(position="stack", stat="identity") +
  theme_set(theme_minimal()) +
  theme(axis.title.x = element_text(colour="#939598", size = 8, margin = margin(t = 8)),
        axis.text.y = element_text(colour="#939598", size = 8),
        axis.text.x = element_text(colour="#939598", size = 8),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(colour="#939598", size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(colour="#939598", size = 8),
        title = element_text(colour="#939598", size = 10)
  ) +
  scale_fill_manual(values=c("#89C349", "#DCDCDC", "#AA87BC")) +
  labs(title = "Distribution of Member Engagement Levels",
       subtitle = "All Check-Ins from 2014-2018",
       x = "Highest Engagement Level in Year Measured",
       y = "Total Occurrences of Engagement Level")