# Organisation: **Data For Good (D4G)**
# Client: **Boys and Girls Club of Ottawa (BGCO)**
# Project: **Engagement Journeys (June Refresh)**

## Description
We will use Alluvial/Sankey diagrams to show how members engagement (visits per week) levels change over time. These will be organised by two perspectives: a member's first five years and by age categories (Junior, Intermediate, Senior).

The goal is to highlight 'groups of members' that would be of interest to the client. This interest could be due to the loss (or churn) of members or an increase in engagement levels.

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

**Step 02 Prep Engagement Fiscal**
* Filter for Fiscal Period Only
* Calculate number of weeks in Period
* Calculate engagement stats
* Translate engagement stats to categories

**Step 03 Alluvial Fiscal**
* Define 'Never Attended' and 'Not Old Enough'
* Prep according to ggAlluvial specifications
* Plot

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
