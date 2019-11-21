### Engagement Status
## Authors: Andrew Macumber
###

## Goals
# Create an alluvial diagram that shows a members journey from junior to senior
##

## Data requirements
# Check-In Data with Member ID (remove -1)
# Time Frame: Fiscal Calendar (2009-2018)
# Follow Junior Journeys (must have junior engagement level)
##


### Prepare Work Space

## Empty work space
rm(list = ls())
##

## load required libraries
library(janitor)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
##

## Import data files
attendance_df <- read.table("Attendance.txt", sep = "|", header = T, stringsAsFactors = F) %>%
  clean_names()

member_df <- read.table("Member.txt", sep = "|", header = T, fill = T, stringsAsFactors = F) %>%
  clean_names()

program_df <- read.table("Program.txt", sep = "|", header = T, stringsAsFactors = F) %>%
  clean_names()
##
###


### Data Wrangling

## select check-in info from program_df
program_or_check_in_df <- program_df %>%
  select(d4g_program_id, d4g_program_or_check_in)

## Table: Member Check-ins
# d4g_program_or_check_in: "Check-In Only"
# mem_types: M - Member; U - Trying Out; Q - Aquatic Child
# age_group: J - Juvenile; I - Intermediate; S - Senior
# check_in_year: range(2009, 2019) ; last ten years
# check_in_period: Summer (July, August) ; else School

member_checkin <- attendance_df %>%
  
  # use check-in info from program_df to filter attendance_d for check-in data
  left_join(program_or_check_in_df, by = c("d4g_program_id" = "d4g_program_id")) %>%
  filter(d4g_program_or_check_in == "Check-In Only") %>%
  select(-d4g_program_or_check_in) %>%
  
  # remove volunteers
  filter(age_category != "V") %>%
  
  # remove mem_type aquatic adult ('A')
  filter(mem_type != "A") %>%
  
  # remove d4g_member_id == -1; represents non-members
  filter(d4g_member_id != -1) %>%
  
  # expand date info to several columns
  # create dim: "check_in_period" to label Summer and School Periods
  mutate(
    check_in_date = as.Date(date_attended),
    check_in_year = year(as.Date(date_attended)),
    check_in_month = month(as.Date(date_attended)),
    check_in_week = week(as.Date(date_attended)),
    check_in_period = ifelse(check_in_month == 7 | check_in_month == 8, "Summer", "School")
  )

rm(program_or_check_in_df, program_df)
##

## Table: Member Check-Ins For Fiscal Period (2009 to 2018)
#
member_checkin_fiscal <- member_checkin %>%
  
  # Fiscal Years 2009 to 2018
  filter(check_in_year >= "2009") %>%
  filter(check_in_year < "2018") %>%
  
  # Select relevant dimensions
  select(d4g_member_id, sex, member_age, age_category, check_in_date, check_in_year, check_in_month, check_in_week)

# Remove objects
rm(member_checkin)
##

## Table: Number of weeks per year
#
no_weeks_fiscal <- member_checkin_fiscal %>%
  
  group_by(check_in_year) %>%
  
  summarize(no_weeks = max(check_in_week))
# Note: For only two years (2009 and 2016) does a member check-in for all 52 weeks
##

## Table Engagement Stats
member_engagement_stats <- member_checkin_fiscal %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id, check_in_year, age_category) %>%
  
  # Return week counts
  summarize(checkin_total = n()) %>%
  
  # Ungroup after calculatoin
  ungroup() %>%
  
  # Add number of weeks
  left_join(no_weeks_fiscal, by = ("check_in_year")) %>%
  
  # Create dim: weekly average for members by cohort
  mutate(
    checkin_avg = round(checkin_total / no_weeks, 3)
  )
##

## Table Engagement Stats by Age Category

member_engagement_level <- member_engagement_stats %>%
  
  # Select variables of interest
  select(d4g_member_id, age_category, checkin_avg) %>%
  
  # Group by d4g_member_id, age_category
  group_by(d4g_member_id, age_category) %>%
  
  # Calculate weekly average for age category
  summarize(checkin_avg_age = mean(checkin_avg)) %>%
  
  ungroup() %>%
  
  # Create dim: Eng_Level based on checkin_avg_age
  mutate(
    Eng_Level = ifelse(checkin_avg_age >= 2, "A. Ideal (2+)", 
                       ifelse(checkin_avg_age >= 0.5, "B. Moderate (0.5-2)",
                              ifelse(checkin_avg_age > 0, "C. Low (< 0.5)",
                                     "D. None (0)")))) %>%
  
  # Select dim of interest
  select(-checkin_avg_age)
##

## Table Append: "Never Attended"
member_engagement <- member_engagement_level %>%
  
  # Subset the dataframe with columns of interest
  select(d4g_member_id, age_category, Eng_Level) %>%
  
  # Create new dims; "variable" = Eng_Level; "value" = Eng_Level values
  pivot_wider(names_from = age_category, values_from = Eng_Level) %>%
  
  # replace any NaN with NA
  mutate_at(vars(-d4g_member_id), funs(ifelse(is.na(.), "Never Attended", .))) %>%
  
  # convert from wide to long
  pivot_longer(c(J, I, S),
               names_to = "age_category",
               values_to = "Eng_Level")
##

### Alluvial Diagram (function ggalluvial)

## Set up work space
library(ggalluvial)
windows(10,7)
##

## Alluvial Diagram Prep

plot_data <- member_engagement

# Plotting program requires Eng_Level to be factor type
plot_data$Eng_Level <- as.factor(plot_data$Eng_Level)
#

# Change J, I, S to Junior, Intermediate and Senior
plot_data <- within(plot_data, age_category[age_category=="J"] <- "A-Junior")
plot_data <- within(plot_data, age_category[age_category=="I"] <- "B-Intermediate")
plot_data <- within(plot_data, age_category[age_category=="S"] <- "C-Senior")
#
##

## Alluvial Plot
# Colour Palette
ggplot(plot_data,
       aes(x = age_category,
           stratum = Eng_Level,
           alluvium = d4g_member_id,
           fill = Eng_Level,
           label = Eng_Level)) +
  theme_bw() +  # sets background to white and grey grid lines
  scale_y_continuous(breaks = seq(0, 15000, by = 2500)) +
  scale_x_discrete(expand = c(.1, .1)) +  # widens the spread and column width
  geom_flow() +
  geom_stratum() +
  scale_fill_manual(values= c("#F1484B", "#f99f1b", "#90aad8", "grey80")) +
  theme(legend.position = "bottom") +  # add a legend at bottom
  # geom_text(stat = "stratum", size = 3) +  # add labels to blocks
  labs(y="Count of Members (n)", x = "") +  # add y-axis label and remove x-axis label
  ggtitle("Engagement Journeys (n = 13082)")
##
###