# Step 02 Prep Engagement Summer
# Author: Andrew Macumber, Heather Woods
# Pre-requisites: Step 01

#
## Filter for Summer Year period
member_visits_calendar <- member_visits %>%
  
  # Summer Years 2009 to 2018
  filter(check_in_year >= "2009") %>%
  filter(check_in_year < "2018") %>%
  
  # Filter for School Period
  filter(check_in_period == "School") %>%
  
  # Create dim: Eng_Level based on checkin_avg_age
  mutate(
    school_year = ifelse(check_in_month > 8, check_in_year, check_in_year - 1)) %>%
  
  # Select relevant dimensions
  select(d4g_member_id, mem_type, sex, member_age, age_category, check_in_date, check_in_year, check_in_month, check_in_week)

##
#

#
## Number of weeks: Fiscal (48), School (40), Summer (8)
no_weeks <- 40
##
#

#
## Calculate stats: visits per week, average visits per week
member_engagement <- member_visits_calendar %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id, check_in_year, age_category) %>%
  
  # Return week counts
  summarize(checkin_total = n()) %>%
  
  # Ungroup after calculation
  ungroup() %>%
  
  # Create dim: weekly average for members by cohort
  mutate(
    checkin_avg = round(checkin_total / no_weeks, 3)
  )

##
#

#
## Translate engagement stats to categories
member_engagement_levels <- member_engagement %>%
  
  # Select variables of interest
  select(d4g_member_id, age_category, checkin_avg) %>%
  
  # Group by d4g_member_id, age_category
  group_by(d4g_member_id, age_category) %>%
  
  # Calculate weekly average for age category
  summarize(checkin_avg_age = mean(checkin_avg)) %>%
  
  ungroup() %>%
  
  # Create dim: Eng_Level based on checkin_avg_age
  mutate(
    Eng_Level = ifelse(checkin_avg_age >= 1, "Ideal (1+)", 
                       ifelse(checkin_avg_age > 0, "Limited (<1)",
                              "Absent"))) %>%
  
    # Select dim of interest
  select(-checkin_avg_age)
##
#