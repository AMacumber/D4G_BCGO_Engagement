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
## Set Theme

# Create a new theme
theme_blackborder_empty <- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "white"),
      panel.grid.minor  = element_line(color = "white"),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black")
    )
}
##
#
#############################################################################################################
#
## Histogram - Count of Engaged Members by Distance

# Keep only engaged members
df_distance <- df_000 %>%
  
  filter(label == "engaged") %>%
  filter(club_km < 20)

ggplot(df_distance, aes(club_km)) +
  geom_histogram( binwidth=1.25, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  annotate("text", x=5, y=133, label= "Bar widths are roughly every 1 km") + 
  labs(title = "How far Engaged Members travel", x="Distance to Clubhouse(km)", y="Number of Engaged Members", fill = "Engaged") +
  theme_blackborder_empty()
##
#
#############################################################################################################
#
## Stacked Barplot - Proportion of Static vs Engaged by visits

# Convert number of visits to a categorical variable
hist(df_000$checkin_total, breaks = 8)

df_visits <- df_000 %>%
  
  mutate(visit_bins = cut(checkin_total, breaks = 8, labels = c("0", "5", "10", "15", "20", "25", "30", "35"))) %>%
  
  group_by(visit_bins, label) %>%
  summarise(Count_Members =n())

# Plot the basic frame of the stacked bar chart.
ggplot(df_visits, aes(x = visit_bins, y = Count_Members, fill = label)) + 
  theme_blackborder_empty() +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  #scale_fill_manual(values=c("#098553", "#4ca64c", "#99cc99", "#cccccc")) +
  #guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Year 1 Clubhouse Visits", x="Number of Visits", y="", fill = "Year Two Status:") +
  theme(legend.position="top")
  #coord_flip()
##
#
#############################################################################################################
#
# ECDF - Count of engaged vs Age
# add lines to show that engaged members tend to be younger

ggplot(df_000, aes(x= Y1_Age, fill=label)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 20) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  labs(fill="")

df_age <- df_000 %>% filter(label == 'engaged')

ggplot(df_000, aes(Y1_Age, colour = label)) + 
  theme_blackborder_empty() +
  stat_ecdf(geom = "line", size = 2) +
  guides(fill=guide_legend(title="Year Two Status:")) +
  labs(title = "Member Age Distributions", x="Age of Member During Year One", y="") +
  theme(legend.position="top")
