# Step 04 - Feature Engineering
# Author: Andrew Macumber, Bruno Afonso
# Pre-requisites: Step 01, Step 02, Step 03
## some features are also based on selection of the final labeled dataset (step 05)
#################################################################################
#
## Rationale
# We are interested in Y1_Limiteds that become Y2_Ideals (engaged) and will compare them to
# Y1_Limiteds who become Y2_Limiteds (static).
##
#
#################################################################################
#
## Isolate total visits and visits per week for y1
feature_y1_visits_stats <- member_engagement %>%
  
  # Keep only year 1 numbers
  filter(relative_year == 'Y1') %>%
  
  select(-relative_year)
##
#
#################################################################################
#
## Season Most: Fall (9-12) vs Winter (1-3)
feature_y1_season_most <- member_visits_5year %>%
  
  # Select columns
  select(d4g_member_id, check_in_month, relative_year) %>%
  
  # Keep only year 1 numbers
  filter(relative_year == 'Y1') %>%
  
  # remove relative year
  select(-relative_year) %>%
  
  # Group months
  # Add Age Categories
  mutate(
    season = ifelse(check_in_month < 8, "fall_visits", "winter_visits")  # wondering if further subdivision is required
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
  
  mutate(
    season_most = ifelse(fall_visits > winter_visits, "fall", "winter")
  ) %>%
  
  select(d4g_member_id, season_most)
##
#
#################################################################################
#
## How many clubhouses do Members visit?

# create a list of members included in analysis
final_members <- labeled_filter$d4g_member_id

# filter visits for those members only
final_members_visits <- member_visits_calendar[member_visits_calendar$d4g_member_id %in% final_members,]

# do members have more than one member_location? no
clubhouse_variety <- final_members_visits %>%
  
  select(d4g_member_id, member_location) %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id, member_location) %>%
  
  # Return number of clubhouses
  summarize(clubhouse_visits = n()) %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id) %>%
  
  # Return number of clubhouses
  summarize(clubhouse_sum = n())

min(clubhouse_variety$clubhouse_sum) # 1
max(clubhouse_variety$clubhouse_sum) # 1

# do members have more than one program_location?
clubhouse_variety <- final_members_visits %>%
  
  select(d4g_member_id, program_location) %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id, program_location) %>%
  
  # Return number of clubhouses
  summarize(clubhouse_visits = n()) %>%
  
  # Group by ID, Cohort
  group_by(d4g_member_id) %>%
  
  # Return number of clubhouses
  summarize(clubhouse_sum = n())

min(clubhouse_variety$clubhouse_sum)  # 1
max(clubhouse_variety$clubhouse_sum)  # 6

# Frequency of program_locations by member
hist(clubhouse_variety$clubhouse_sum)

feature_clubhouse_number <- clubhouse_variety %>%
  
  # Create a feature, visits multiple clubhouses
  mutate(clubhouse_number = ifelse(clubhouse_sum > 1, 'Multi', 'Single')) %>%
  
  select(d4g_member_id, clubhouse_number)
##
#
#################################################################################
#
## Add distance to clubhouse
feature_distance2clubhouse <- read.csv('Member_Dist_to_Clubhouses_BrunoAfonso.csv')
##
#
#################################################################################




