# Step 04 Create Labeled Dataset for Churn Analysis
# Author: Andrew Macumber
# Pre-requisites: Step 01, Step 02, Step 03

#
## Rationale
# We are interested in Y1_Limiteds that become Y2_Ideals (engaged) and will compare them to
# Y1_Limiteds who become Y2_Limiteds (static).
##
#


#
## Feature Isolation

### Isolate total visits and visits per week for y1
y1_visits <- member_engagement %>%
  
  # Keep only year 1 numbers
  filter(relative_year == 'Y1') %>%
  
  select(-relative_year)
###

### Visit distribution over Y1
Y1_visits_distribution <- member_visits_5year %>%
  
  # Select columns
  select(d4g_member_id, check_in_month, relative_year) %>%
  
  # Keep only year 1 numbers
  filter(relative_year == 'Y1') %>%
    
  # remove relative year
  select(-relative_year) %>%
  
  # Group months
  # Add Age Categories
  mutate(
    season = ifelse(check_in_month < 8, "fall_visits", "winter_visits")
  ) %>%
    
  # remove check_in_month
  select(-check_in_month) %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id, season) %>%
  
  # Return week counts
  summarize(season_total = n()) %>%
  
  # Create new dims; "variable" = fall, winter; "value" = season_total
  pivot_wider(names_from = season, values_from = season_total) %>%
  replace_na(list(fall_visits=0, winter_visits=0)) %>%
  
  transmute(
    fall_frac = fall_visits / (fall_visits + winter_visits),
    winter_frac = winter_visits / (fall_visits + winter_visits)
  )
###
##
#


#
## Create Labeled Dataset & Feature engineering
labeled_filter <- member_engagement_levels %>%
  
  # Only Y1_Limited that are at least Y2_Limited
  filter(Y1 == 'Limited (<1)', Y2 != 'Absent') %>%
  
  # Label the dataset
  mutate(
    label = ifelse(Y2 == 'Limited (<1)', 'static', 'engaged')) %>%
  
  # Select only member ID and label column
  select(d4g_member_id, label) %>%
  
  # Add Year 1 column
  left_join(Member_Year1, by = "d4g_member_id") %>%
  
  # Remove duplicate column
  select(-first_year_checked_in) %>%
  
  # Add member info dimensions
  left_join(member_df, by = "d4g_member_id") %>%
  
  # Create Y1_Age dimension
  mutate(
    Y1_Age = first_year - birth_year
  ) %>%
  
  # Add total visits and average visits for year 1
  left_join(y1_visits, by = "d4g_member_id") %>%
  
  # Add Age Categories
  mutate(
    age_category = ifelse(Y1_Age >= 18, "mature",
                          ifelse(Y1_Age > 14, "senior",
                          ifelse(Y1_Age > 10, "intermediate",
                          ifelse(Y1_Age >=6, "junior",
                          "infant"))))
    ) %>%
  
  # Add fall / winter break in visits
  left_join(Y1_visits_distribution, by = "d4g_member_id")
##
#

