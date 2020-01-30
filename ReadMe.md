# Organisation: **Data For Good (D4G)**
# Client: **Boys and Girls Club of Ottawa (BGCO)**
# Project: **Engagement Journeys (June Refresh)**

## Description
We used Sankey diagrams to show how members engagement (visits per week) levels change over time. These will be organised by two perspectives: a member's first five years and by age categories (Junior, Intermediate, Senior).

The first five years perspective showed that there was a group of Year01 members with limited engagement (< 1 visit per week) who had ideal engagement (>1 visit per week) in Year02. We can compare them to all other Year01 members with limited engagement.

See other branches for age category perspectives and by different calendars (Fiscal, School, Summer).

Other notable results would be a table showing the percent loss of members over time.

## Data
Protected. Provided by D4G. Contact Alex Campbell.

## Active Scripts

**Step 01 Prep Visits**
* Set-Up Work Environment (Libraries, Data)
* Filter for visits only
* Filter out: volunteers, -1 and aquatic adult
* Create new columns: date, year, month, week, period
* Include only members that are also in Members_df

**Step 02 Prep Year**
* Filter for 2009 to 2019 and School Period
* Calculate first year of engagement (relative year)
* Keep only the first five years
* Calculate engagement stats
* Translate engagement stats to categories

**Step 03 Sankey Year**
* Define 'Left'
* Create 'Links' and 'Nodes' tables for Sankey
* Plot Sankey Diagram

**Step 04 Prep Feature Engineering**
* Calculate Year 1 checkin total, and weekly average
* Calculate if visited more during Fall or Winter
* Did they visit one clubhouse (single) or more (multi)?
* Read in distance to clubhouse data (Bruno Afonso)

**Step 05 Prep Label Engage Dataset**
* Filter for Y1 == Limited and Y2 != Absent
* Add label column; static is no change in engagement, engaged is a change
* Join members_df features
* Create a dim for age at Year 01
* Add total Y1 checkins, weekly average
* Add age category at Y1
* Add season they most visited in (Fall/Winter)
* Add if they visited one or more clubhouses (Single/Multi)
* Add distance to clubhouse data (Bruno Afonso)

**Step 06 EDA Feature Evaluation**
* filter for: unique, redundant, missing, imbalanced features
* Data: D4G_BGCO_Engage_Labeled_v001: prior to postal code data
* Data: D4G_BGCO_Engage_Labeled_v002: includes postal code data

**Step 07 Classification**
* look at numerical and categorical variables, and against 'churn'
* encode data
* XGBoost and evaluation


**Test_Members_Unique_Stats**
* Small analysis to compare unique members found in Attendance and Member data

## Definitions
* Age Groups: J - Junior, I - Intermediate, S - Senior
* Member Type: M - Member, U - Trying Out, Q - Aquatic Child, A - Aquatic Adult, V - Volunteer, X - Aged Out
* Member Type: X - Aged Out only exists in years 2006, 2007
* Member Type: P - Clubhouse Check-In, found in Programs_df
* Member ID: -1 is assigned to visits that do not have  membership
* Time Period of Interest: 2009 to 2019
* Time Period of Interest: 2019 is incomplete, January to July
* Time Period of Interest: Fiscal, 2009-2018, Jan to Dec.
* Time Period of Interest: School, 2008-2019, Sept to June
* Time Period of Interest: Summer, 2009-2018, July-August
