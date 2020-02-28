#############################################################################################################
# Step 08 Data Show
## Dataset: Boys and Girls Club of Ottawa (Data For Good)
## Author: Andrew Macumber
#############################################################################################################
#
## Load Data 

# Load libraries/working directory
library(dplyr)  # needed for pipes
library(stringr)  # working with strings
library(ggplot2)  # viz

# Read in the data, and change column names
df_000 <- read.csv(file = "D4G_BGCO_Engage_Labeled_ForEDA_v003.csv", stringsAsFactors = FALSE)
##
#
#############################################################################################################
#
## Version Control

#df_000  # read csv

##
#
#############################################################################################################
#
## Histogram - Count of Engaged Members by Distance

# Keep only engaged members
df_engaged <- df_000 %>%
  
  filter(label == "engaged") %>%
  filter(club_km < 20)

ggplot(df_engaged, aes(club_km)) +
  geom_histogram( binwidth=1.25, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  annotate("text", x=5, y=133, label= "Bars are roughly every 1 km") + 
  annotate("text", x=5, y=133, label= "Bars are roughly every 1 km") + 
  labs(title = "How far Engaged Members travel", x="Distance to Clubhouse(km)", y="Number of Engaged Members", fill = "Engaged")
##
#
#############################################################################################################
#
## Stacked Barplot - Proportion of Static vs Engaged by visits
# need to bin visits
# show proportion of static/engaged by visit bin

# Plot the basic frame of the stacked bar chart.
ggplot(df_000, aes(x = PTA_Names, y = Count_Programs, fill = Data_QA)) + 
  theme_blackborder_empty() +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values=c("#098553", "#4ca64c", "#99cc99", "#cccccc")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = paste("Program Data Quality by Region for", year, sep = " "), x="", y="", fill = "") +
  theme(legend.position="top") +
  coord_flip()
##
#
#############################################################################################################
#
# Histogram - Count of engaged vs Age