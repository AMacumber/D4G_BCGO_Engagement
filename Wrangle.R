# Data Wrangling

## Meta Details
# Author: Andrew Macumber (Data Ambassador)
# Project: Phase II Boys & Girls Club Project
# Data: Protected
##


## Datasets
# att_checkin_fiscal: Check-in, M/X/Q, !V, !-1, Fiscal
# mem_filtered: mem_type = M/Q, id != -1
# att_checkin_fiscal_overlap : found in member_df
##

## Notes
# mem_type = "P" is program check-in
# mem_type = "X" only exists in 2006 and 2007
##

## Prepare Work Space

# Empty work space
rm(list = ls())
#

# load required libraries
library(janitor)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
#

# Import data files
attendance_df <- read.table("Attendance.txt", sep = "|", header = T, stringsAsFactors = F) %>%
  clean_names()

member_df <- read.table("Member.txt", sep = "|", header = T, fill = T, stringsAsFactors = F) %>%
  clean_names()

program_df <- read.table("Program.txt", sep = "|", header = T, stringsAsFactors = F) %>%
  clean_names()
#
##


## Data Wrangling

### att_checkin_fiscal

# select check-in info from program_df
program_or_check_in_df <- program_df %>%
  select(d4g_program_id, d4g_program_or_check_in)

# Table: Member Check-ins
# d4g_program_or_check_in: "Check-In Only"
# mem_types: M - Member; U - Trying Out; Q - Aquatic Child
# age_group: J - Juvenile; I - Intermediate; S - Senior
# check_in_year: range(2009, 2019) ; last ten years
# check_in_period: Summer (July, August) ; else School

att_checkin <- attendance_df %>%
  
  # use check-in info from program_df to filter attendance_d for check-in data
  left_join(program_or_check_in_df, by = c("d4g_program_id" = "d4g_program_id")) %>%
  filter(d4g_program_or_check_in == "Check-In Only") %>%
  select(-d4g_program_or_check_in) %>%
  
  # remove volunteers
  #filter(age_category != "V") %>%
  
  # remove mem_type aquatic adult ('A')
  filter(mem_type != "U", mem_type != "A", mem_type != "V") %>%
  
  # remove d4g_member_id == -1; represents non-members
  filter(d4g_member_id != -1) %>%
  
  # expand date info to several columns
  # create dim: "check_in_period" to label Summer and School Periods
  mutate(
    check_in_date = as.Date(date_attended),
    check_in_year = year(as.Date(date_attended)),
    check_in_month = month(as.Date(date_attended)),
    check_in_week = week(as.Date(date_attended)),
    check_in_period = ifelse(check_in_month == 7 | check_in_month == 8, "Summer", "School")
  )
#

# Table: Member Check-Ins For Fiscal Period (2009 to 2018)

att_checkin_fiscal <- att_checkin %>%
  
  # Fiscal Years 2009 to 2018
  filter(check_in_year >= "2009") %>%
  filter(check_in_year < "2018") %>%
  
  # Select relevant dimensions
  select(d4g_member_id, mem_type, sex, member_age, age_category, check_in_date, check_in_year, check_in_month, check_in_week)
#
##

### mem_fiscal

# Table: Member Check-ins
mem_filtered <- member_df %>%
  
  # remove incorrect member types
  # mem_types: M - Member;  X - Aged Out; U - Trying Out; Q - Aquatic Child;
  ## A - Aquatic Adult; V - Volunteer Vetted; C - Volunteer Candidate; "" - Empty
  filter(mem_type != "U", mem_type != "X", mem_type != "A", mem_type != "V", mem_type != "C", mem_type != "") %>%
  
  # remove d4g_member_id == -1; represents non-members
  filter(d4g_member_id != -1)
###

### att_checkin_fiscal_overlap

# Table: Member Ages
member_filt_ages <- mem_filtered %>% select(d4g_member_id, age)

# Table: Member Check-ins
att_checkin_fiscal_overlap <- att_checkin_fiscal %>%
  
  inner_join(member_filt_ages, by = "d4g_member_id")
  
###
##
#
