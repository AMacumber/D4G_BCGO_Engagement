# Step 02 Prep Engagement Fiscal
# Author: Andrew Macumber, Alex Campbell
# Pre-requisites: Step 01

#
## Filter for Fiscal Year period
member_visits_calendar <- member_visits %>%
  
  # Fiscal Years 2009 to 2018
  filter(check_in_year >= "2009") %>%
  filter(check_in_year <= "2018") %>%
  
  # Select relevant dimensions
  select(d4g_member_id, mem_type, sex, member_age, age_category, check_in_date, check_in_year, check_in_month, check_in_week)

##
#

#
## Calculate first year of engagement by d4g_member_id
Member_Year1 <- member_visits_calendar %>%
  group_by(d4g_member_id) %>%
  summarise(
    first_year_checked_in = min(check_in_year),
    first_year = first_year_checked_in * 1 # Added to resolve calculation issue in next data frame
  )
##
#

#
## Merge Year1 values and calculate relative year of engagement, filter to first five years
member_visits_5year <- member_visits_calendar %>%
  
  # Add Year1 columns
  left_join(Member_Year1, by = "d4g_member_id") %>%
  
  # Add a relative year column
  mutate(
    relative_year = (check_in_year - first_year) + 1) %>%
  
  # Keep only the first five years
  filter(relative_year <= 5) %>%
  
  # Change to Y1:Y5
  mutate(
    relative_year = paste("Y", relative_year, sep = "")
  )
##
#

#
## Number of weeks: Fiscal (48), School (40), Summer (8)
no_weeks <- 48
##
#

#
## Calculate stats: visits per week, average visits per week
member_engagement <- member_visits_5year %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id, relative_year) %>%
  
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
## Define: "Never Attended" & "Not Old Enough"
member_engagement_levels <- member_engagement %>%
  
  # Subset the dataframe with columns of interest
  select(d4g_member_id, relative_year, checkin_avg) %>%
  
  # Create new dims; "variable" = Eng_Level; "value" = Eng_Level values
  pivot_wider(names_from = relative_year, values_from = checkin_avg) %>%
  
  # Replace NA with 0
  replace_na(list(Y1=0, Y2=0, Y3=0, Y4=0, Y5=0)) %>%
  
  # Add Engagment Levels
  mutate(
    Y1 = ifelse(Y1 >= 2, "Ideal (2+)", 
               ifelse(Y1 >= 0.5, "Moderate (0.5-2)",
                      ifelse(Y1 > 0, "Limited (< 0.5)",
                             "Did Not Attend"))),

    Y2 = ifelse(Y2 >= 2, "Ideal (2+)", 
                ifelse(Y2 >= 0.5, "Moderate (0.5-2)",
                       ifelse(Y2 > 0, "Limited (< 0.5)",
                              "Did Not Attend"))),
    
    Y3 = ifelse(Y3 >= 2, "Ideal (2+)", 
                ifelse(Y3 >= 0.5, "Moderate (0.5-2)",
                       ifelse(Y3 > 0, "Limited (< 0.5)",
                              "Did Not Attend"))),
    
    Y4 = ifelse(Y4 >= 2, "Ideal (2+)", 
                ifelse(Y4 >= 0.5, "Moderate (0.5-2)",
                       ifelse(Y4 > 0, "Limited (< 0.5)",
                              "Did Not Attend"))),
    
    Y5 = ifelse(Y5 >= 2, "Ideal (2+)", 
                ifelse(Y5 >= 0.5, "Moderate (0.5-2)",
                       ifelse(Y5 > 0, "Limited (< 0.5)",
                              "Did Not Attend"))) )
##
#
