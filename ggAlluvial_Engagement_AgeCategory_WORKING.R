### Engagement Status
## Authors: Andrew Macumber, Alex Campbell
###


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


## Function to categorize engagement level based on average visits per week
engagementFunction <- function(x) {
  if(x >= 2.00) {"A. Ideal (2+)"
  } else if(x >= 1.00) {"B. Typical (1-2)"
  } else if(x >= 0) {"C. Poor (0-1)"
  } else {"D. None (0)"}
}
##

## WORKING Table: Member attributes, Cohort check-in counts and weekly averages
check_in_Cohort_stats <- check_in_attendance_df %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id, Cohort, age_category) %>%
  
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
  
  unite(Cohort.Member, d4g_member_id, Cohort, sep ='.')
  
  # Add member attributes
  #left_join(member_df, by = "d4g_member_id")
##

test <- check_in_Cohort_stats[!duplicated(check_in_Cohort_stats[,'Cohort.Member']),]


### Alluvial Diagram (function ggalluvial)

## Set up work space
library(ggalluvial)
set.seed(2564)
windows(10,7)
##

## WORKING Table: Member Engagement Status (Ideal, Typical, Poor, None) by Age_Group (Junior, Intermediate, Senior)
eng_age_group <- test %>%
  
  # Group by: ID, Age_category
  group_by(Cohort.Member, age_category) %>%
  
  # Return weekly mean for cohort
  summarize(Eng = mean(check_in_wk_avg_Cohort)) %>%
  
  # Create dim: Eng Status for Cohort
  mutate(
    Eng_Level = engagementFunction(Eng)
  ) %>%
  
  # Subset the dataframe with columns of interest
  select(Cohort.Member, age_category, Eng_Level) #%>%
##

## Table: Member Engagement Status (Ideal, Typical, Poor, None) by Age_Group (Junior, Intermediate, Senior)
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
  select(d4g_member_id, age_category, Eng_Level) #%>%
##

## Plotting program requires Eng_Level to be factor type
eng_age_group$Eng_Level <- as.factor(eng_age_group$Eng_Level)
##

# Change J, I, S to Junior, Intermediate and Senior
eng_age_group <- within(eng_age_group, age_category[age_category=="J"] <- "A-Junior")
eng_age_group <- within(eng_age_group, age_category[age_category=="I"] <- "B-Intermediate")
eng_age_group <- within(eng_age_group, age_category[age_category=="S"] <- "C-Senior")

# Resampling indexes 
index <- sample(1:nrow(eng_age_group), 12000)
#

# Sets
plot_data <- eng_age_group  # full set
plot_data <- eng_age_group[index,]  # subset
#

## Alluvial Plot
ptm <- proc.time()
ggplot(plot_data,
       aes(x = age_category,
           stratum = Eng_Level,
           alluvium = Cohort.Member,
           fill = Eng_Level,
           label = Eng_Level)) +
  scale_fill_brewer(type = "qual",
                    palette = "Set2") +
  geom_flow(stat = "alluvium",
            lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "none") +
  ggtitle("Member Engagement Journeys by Age Category (n = 19311)") +
  geom_text(stat = "stratum", size = 3) +
  labs(y="Count of Members (n)", x = "")
proc.time() - ptm
##
###