# Engagement Journeys

#
## Meta Details
# Author: Andrew Macumber (Data Ambassador)
# Project: Phase II Boys & Girls Club Project
# Data: Protected
##
#

#
## Objectives
# Alluvial plot showing Engagement Journeys
##
#

#
## Datasets
# see "Import_Wrangle"
# att_checkin_fiscal_overlap
##
#

#
## Calculate: Engagement Stats

# Table: Number of weeks per year
no_weeks_fiscal <- att_checkin_fiscal_overlap %>%
  
  group_by(check_in_year) %>%
  
  summarize(no_weeks = max(check_in_week))
# Note: For only two years (2009 and 2016) does a member check-in for all 52 weeks
#

# Table Engagement Stats
att_engagement_stats <- att_checkin_fiscal_overlap %>%
  
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
#

# Table Engagement Stats by Age Category
att_engagement_level <- att_engagement_stats %>%
  
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
#

# Define: "Never Attended" & "Not Old Enough"

## Member ages for defining "Not Old Enough"
member_ages <- member_filt_ages %>%
  select(d4g_member_id, age)

## Define: "Never Attended" & "Not Old Enough"
member_engagement <- att_engagement_level %>%
  
  # Subset the dataframe with columns of interest
  select(d4g_member_id, age_category, Eng_Level) %>%
  
  # Create new dims; "variable" = Eng_Level; "value" = Eng_Level values
  pivot_wider(names_from = age_category, values_from = Eng_Level) %>%
  
  # Append the Members information
  inner_join(member_ages, by = "d4g_member_id") %>%
  
  # replace any NaN with NA
  mutate_at(vars(-d4g_member_id), funs(ifelse(is.na(.), ifelse(age <= 10, "Not Old Enough",
                                                               ifelse(age <= 13, "Not Old Enough", "Never Attended")), .))) %>%
  
  # Remove age information
  select(-age) %>%
  
  # convert from wide to long
  pivot_longer(c(J, I, S),
               names_to = "age_category",
               values_to = "Eng_Level")
##
#

#
## Alluvial Diagram (function ggalluvial)

# Set up work space
library(ggalluvial)
windows(10,7)
#

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
  scale_fill_manual(values= c("#F1484B", "#f99f1b", "#90aad8", "grey80", "grey95")) +
  theme(legend.position = "bottom") +  # add a legend at bottom
  # geom_text(stat = "stratum", size = 3) +  # add labels to blocks
  labs(y="Count of Members (n)", x = "") +  # add y-axis label and remove x-axis label
  ggtitle("Engagement Journeys (n = 4994)")
##
#