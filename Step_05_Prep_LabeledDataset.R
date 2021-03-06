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
  
  # Filter out CAMP only members (CAMP and outlier)
  filter(member_location != "CAMP") %>%
  
  # Create Y1_Age dimension
  mutate(
    Y1_Age = first_year - birth_year
  ) %>%
  
  # Add total visits and average visits for year 1
  left_join(feature_y1_visits_stats, by = "d4g_member_id") %>%
  
  # Add Age Categories
  mutate(
    age_category = ifelse(Y1_Age >= 18, "mature",
                          ifelse(Y1_Age >= 14, "senior",
                          ifelse(Y1_Age > 10, "intermediate",
                          ifelse(Y1_Age >=6, "junior",
                          "infant"))))
    ) %>%
  
  # Add fall / winter break in visits
  left_join(feature_y1_season_most, by = "d4g_member_id") %>%
  
  # Add feature_clubhouse_distance
  left_join(feature_clubhouse_distance, by = 'd4g_member_id') %>%
  
  # Remove outlier
  filter(club_km < 100) %>%
  
  # Remove irrelevant distance features
  select(-c(club_min_name, club_min_km, diff_club_min))
##
#
#
## Write csv file
write.csv(labeled_filter, file = "D4G_BGCO_Engage_Labeled_v003.csv", row.names = FALSE)
##
#
