# Step 01 Wrangle
# Author: Andrew Macumber, Alex Campbell

#
## Set-Up Work Environment (Libraries, Data)

# Empty work space
rm(list = ls())

# load required libraries
library(janitor)
library(dplyr)
library(lubridate)
library(tidyr)

# Import data files
attendance_df <- read.table("Attendance.txt", sep = "|", header = T, stringsAsFactors = F) %>%
  clean_names()

member_df <- read.table("Member.txt", sep = "|", header = T, fill = T, stringsAsFactors = F) %>%
  clean_names()

program_df <- read.table("Program.txt", sep = "|", header = T, stringsAsFactors = F) %>%
  clean_names()

##
#

#
## Filter for visits only

# select visits info from program_df
program_visits <- program_df %>%
  select(d4g_program_id, d4g_program_or_check_in)

# filter for clubhouse visits
visits <- attendance_df %>%
  
  # use check-in info from program_df to filter attendance_d for check-in data
  left_join(program_visits, by = c("d4g_program_id" = "d4g_program_id")) %>%
  filter(d4g_program_or_check_in == "Check-In Only") %>%
  select(-d4g_program_or_check_in)
##
#

#
## Filter non-relevant member types and non-members
visits <- visits %>%
  
  # remove mem_type aquatic adult ('A')
  filter(mem_type != "U", mem_type != "A", mem_type != "V") %>%
  
  # remove d4g_member_id == -1; represents non-members
  filter(d4g_member_id != -1)
##
#

#
## Create new columns based on date of visit
visits <- visits %>%
  
  # expand date info to several columns
  mutate(
    check_in_date = as.Date(date_attended),
    check_in_year = year(as.Date(date_attended)),
    check_in_month = month(as.Date(date_attended)),
    check_in_week = week(as.Date(date_attended)),
    check_in_period = ifelse(check_in_month == 7 | check_in_month == 8, "Summer", "School")
  )
##
#

#
## Filter for members that are also in member_df

# Filter out irrelevant members from member_df
member_filtered <- member_df %>%
  
  # remove incorrect member types
  # mem_types: M - Member;  X - Aged Out; U - Trying Out; Q - Aquatic Child;
  ## A - Aquatic Adult; V - Volunteer Vetted; C - Volunteer Candidate; "" - Empty
  filter(mem_type != "U", mem_type != "X", mem_type != "A", mem_type != "V", mem_type != "C", mem_type != "") %>%
  
  # remove d4g_member_id == -1; represents non-members
  filter(d4g_member_id != -1) %>%
  
  select(d4g_member_id, age)

# Filter out members not found in member_df
member_visits <- visits %>%
  
  inner_join(member_filtered, by = "d4g_member_id")
  
##
#
