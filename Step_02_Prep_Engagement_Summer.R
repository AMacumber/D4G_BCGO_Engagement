# Step 02 Prep Engagement Summer
# Author: Andrew Macumber, Heather Woods
# Pre-requisites: Step 01

#
## Filter for Summer Year period
member_visits_summer <- member_visits %>%
  
  # Summer Years 2009 to 2018
  filter(check_in_year >= "2009") %>%
  filter(check_in_year < "2018") %>%
  filter(check_in_period == "Summer") %>%
  
  # Select relevant dimensions
  select(d4g_member_id, mem_type, sex, member_age, age_category, check_in_date, check_in_year, check_in_month, check_in_week)

##
#

#
## Calculate the number of weeks in a period
no_weeks <- member_visits_summer %>%
  
  group_by(check_in_year) %>%
  
  summarize(no_weeks = max(check_in_week)-min(check_in_week))

##
#

#
## Calculate stats: visits per week, average visits per week
member_engagement_summer <- member_visits_summer %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id, check_in_year, age_category) %>%
  
  # Return week counts
  summarize(checkin_total = n()) %>%
  
  # Ungroup after calculation
  ungroup() %>%
  
  # Add number of weeks
  left_join(no_weeks, by = ("check_in_year")) %>%
  
  # Create dim: weekly average for members by cohort
  mutate(
    checkin_avg = round(checkin_total / no_weeks, 3)
  )

##
#

#
## Translate engagement stats to categories
member_engagement_summer <- member_engagement_summer %>%
  
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
#