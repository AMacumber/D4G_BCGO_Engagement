# Step 03 Alluvial Summer
# Author: Andrew Macumber, Heather Woods

#
## Convert NAs to "Never Attended" or "Not Old Enough"

# Define: "Never Attended" & "Not Old Enough"
member_engagement_summer <- member_engagement_summer %>%
  
  # Subset the dataframe with columns of interest
  select(d4g_member_id, age_category, Eng_Level) %>%
  
  # Create new dims; "variable" = Eng_Level; "value" = Eng_Level values
  pivot_wider(names_from = age_category, values_from = Eng_Level) %>%
  
  # Append the Members information
  inner_join(member_filtered, by = "d4g_member_id") %>%
  
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
## Prep according to Alluvial specifications

# Set up work space
library(ggalluvial)
windows(10,7)

# Plotting program requires Eng_Level to be factor type
member_engagement_summer$Eng_Level <- as.factor(member_engagement_summer$Eng_Level)

# Default is alphabetically ordering
member_engagement_summer <- within(member_engagement_summer, age_category[age_category=="J"] <- "A-Junior")
member_engagement_summer <- within(member_engagement_summer, age_category[age_category=="I"] <- "B-Intermediate")
member_engagement_summer <- within(member_engagement_summer, age_category[age_category=="S"] <- "C-Senior")

##
#

#
## Alluvial Plot
ggplot(member_engagement_summer,
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
  ggtitle("Summer Engagement Journeys (n = 3797)")
##
#