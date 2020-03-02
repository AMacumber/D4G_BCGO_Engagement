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
  theme_blackborder_empty() +
  geom_histogram( bins = 16, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  annotate("text", x=7, y=120, label= "The majority (57%) of Engaged Members \nare within one km.", size = 5) + 
  labs(title = "Distance of Engaged Members from their Clubhouses", 
       x="Distance from Clubhouse (km) to Postal Code", y="Number of Engaged Members", fill = "Engaged") +
  theme(plot.title = element_text(size=18))
  
 
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
  theme(plot.title = element_text(size=20)) +
  labs(title = "How many times did Members visit in the first year?") +
  labs( x="", y="", fill = "Year Two Status:") +
  geom_hline(yintercept = 0.5, colour = "white", size = 1) +
  theme(legend.position="top")
  #coord_flip()
##
#
#############################################################################################################
#
# ECDF - Count of engaged vs Age

df_age <- df_000 %>% filter(label == 'engaged')

ggplot(df_000, aes(Y1_Age, colour = label)) + 
  theme_blackborder_empty() +
  stat_ecdf(geom = "line", size = 4) +
  guides(fill=guide_legend(title="Year Two Status:")) +
  labs(title = "Engaged Members tend to be younger.", x="Member Age in First Year", y="Fraction of Members") +
  theme(plot.title = element_text(size=20)) +
  theme(legend.position="top") +
  geom_hline(yintercept = 0.5, colour = "#cccccc", size = 1, linetype="dotted") +
  geom_vline(xintercept = 8.7, colour = "#cccccc", size = 1) +
  #geom_segment(data = df_000, aes(x = 10.25, y = -Inf, xend = 10.25, yend = 0.5)) +
  #geom_segment(aes(x = -Inf, y = 0.5, xend = 10.25, yend = 0.5))+
  #geom_segment(aes(x = 8.75, y = -Inf, xend = 8.75, yend = 0.5))
  geom_vline(xintercept = 10.23, colour = "#cccccc", size = 1)

##
#
#############################################################################################################
