# Investigate number of unique members

#
## Meta Details
# Author: Andrew Macumber (Data Ambassador)
# Project: Phase II Boys & Girls Club Project
# Data: Protected
##
#

#
## Objectives
# How many unique members are found in both the Attendance_df and Members_df
##
#

#
## Data Analysis

# Count: Distinct Members in Attendance
member_attendance = att_checkin_fiscal %>% 
  select("d4g_member_id") %>%
  distinct()

att <- length(member_attendance[,1])  # 12918

# Count: Distinct Members in Members
member_members = mem_filtered %>%
  select("d4g_member_id") %>%
  distinct()

mem <- length(member_members[,1])  # 7998

# Count: Distinct members only in Attendance_df
member_attendance_discrepancy = member_attendance %>% 
  anti_join(member_members, by = "d4g_member_id")

att_only <- length(member_attendance_discrepancy[,1])  # 7924

# Count: Distinct members only in Member_df
member_member_discrepancy = member_members %>% 
  anti_join(member_attendance, by = "d4g_member_id")

mem_only <- length(member_member_discrepancy[,1])  # 3004

# Count: Distinct members found in both Attendance_df and Members_df
overlap <- sum(att,mem,-att_only,-mem_only)

# Count: Total number of distinct members across Attendance_df and Members_df
total <- sum(att_only, mem_only, overlap)
##
#

#
## Table Creation

categories <- c("att", "mem", "att_Only", "mem_Only", "overlap", "total")

unique_mems <- c(att, mem, att_only, mem_only, overlap, total)

member_totals <- data.frame(categories, unique_mems)

member_stats <- member_totals %>%
  
  mutate(
    Percent_Total = round(totals/total, 3) * 100
  )
##
#