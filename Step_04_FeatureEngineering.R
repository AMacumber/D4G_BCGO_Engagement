# Step 04 - Feature Engineering
# Author: Andrew Macumber, Bruno Afonso
# Pre-requisites: Step 01, Step 02, Step 03
## use final_members_filter (step 05)
#################################################################################
#
## Rationale
# We are interested in Y1_Limiteds that become Y2_Ideals (engaged) and will compare them to
# Y1_Limiteds who become Y2_Limiteds (static).
##
#
#################################################################################
#
## List of members in the analysis
final_members_filter <- member_engagement_levels %>%
  
  # Only Y1_Limited that are at least Y2_Limited
  filter(Y1 == 'Limited (<1)', Y2 != 'Absent') %>%
  
  select(d4g_member_id)
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
## How many clubhouses do Members visit? In general only one

# Filter for year 1 visits only
member_visits_year1 <- member_visits_5year %>% filter(relative_year == 'Y1')

# filter visits for those members only
final_members_visits_year1 <- member_visits_year1[member_visits_year1$d4g_member_id %in% final_members_filter$d4g_member_id,]

# do members have more than one member_location? no
clubhouse_variety <- final_members_visits_year1 %>%
  
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
clubhouse_variety <- final_members_visits_year1 %>%
  
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
max(clubhouse_variety$clubhouse_sum)  # 4

# Frequency of program_locations by member
hist(clubhouse_variety$clubhouse_sum)

# In general, members only visited a single location

##
#
#################################################################################
#
## What is a member's distance to their clubhouse, minimum distance to clubhouse?

# Read in distance data
feature_distance2clubhouse <- read.csv('Member_Dist_to_Clubhouses_BrunoAfonso.csv')

# select only clubhouse distances for each member
clubhouse_distance <- feature_distance2clubhouse[, c(2, 15:30)]

# select member clubhouses from member df
member_clubhouse <- member_df[,c(1,5)]

# keep only those members that are part of final analysis
final_members_clubhouse_distance <- final_members_filter %>% 
  
  left_join(clubhouse_distance, by = c('d4g_member_id' = 'D4G_MemberId')) %>%
  
  # keep only those rows without na
  filter(complete.cases(.)) %>%
  
  # add member clubhouse
  left_join(member_clubhouse, by = 'd4g_member_id')

# Feature: distance to member clubhouse (member_df)
# Feature: minimum distance to clubhouse
# Feature: difference btw min and clubhouse
for (member in seq(1, nrow(final_members_clubhouse_distance))) {
  
  final_members_clubhouse_distance$club_km[member] <- as.numeric(final_members_clubhouse_distance[member, grep(final_members_clubhouse_distance$member_location[member], colnames(final_members_clubhouse_distance))])

  min_value <- 1000
  
  for (col in seq(2, 17)) {
    col_value <- as.numeric(final_members_clubhouse_distance[member, col])
    
    if (col_value < min_value) {
      min_value <- col_value
      min_name <- unlist(strsplit(colnames(final_members_clubhouse_distance[,col]), "_"))[3]
    }
    }
  
  final_members_clubhouse_distance$club_min_km[member] <- min_value 
  final_members_clubhouse_distance$club_min_name[member] <- min_name
}

# reduce the number of decimal places
is.num <- sapply(final_members_clubhouse_distance, is.numeric)
final_members_clubhouse_distance[is.num] <- lapply(final_members_clubhouse_distance[is.num], round, 2)

# remove member with difference between club_km and club_min_km > 200
feature_clubhouse_distance <- final_members_clubhouse_distance %>%
  
  # remove dist to
  select(-c(dist_to_ADM, dist_to_BAY, dist_to_BL, dist_to_BRC, dist_to_BRIT, 
            dist_to_CAMP, dist_to_HEA, dist_to_HGT, dist_to_MC, dist_to_MYC,
            dist_to_PAL, dist_to_PWH, dist_to_PYC, dist_to_RGM, dist_to_RID,
            dist_to_ROC, member_location)) %>%
  
  mutate(diff_club_min = club_km - min_value) %>%
  
  # found that negative numbers were between the same clubhouses
  mutate(diff_club_min = ifelse(diff_club_min < 0, 0, diff_club_min)) %>%
  
  filter(diff_club_min < 500)

# there are two members with differenes greater than 500 km
# there are 11 members with differences greater than 150 km
# there are 16 members with differences less than 0 km
table(feature_clubhouse_distance$diff_club_min)
hist(feature_clubhouse_distance$diff_club_min)
boxplot(feature_clubhouse_distance$diff_club_min, horizontal = TRUE, 
        main = "Distance to Clubhouse in member_df and closest Clubhouse",
        xlab = "distance (km)")
##
#
