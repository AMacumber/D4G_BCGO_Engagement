# Sankey Diagram
# Tutorial URL - https://www.r-bloggers.com/creating-custom-sankey-diagrams-using-r/
# Tutorial URL - https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram
# Author: Andrew Macumber, Heather Woods, Alex Campbell

#
## Goal
# Display engagement journeys using Sankey diagrams
##
#

#
## Define: "Never Attended" & "Not Old Enough"
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
  
  # Remove age information and member id
  select(J, I, S)
##
#

#
## Count: All Journeys
journeys_all <- member_engagement_summer %>%
  
  # Group and count journeys
  group_by(J, I, S) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  
  # 'Left' == (Junior Engaged to Int,Sen == 'Never Attended')
  within(I[I=="Never Attended" & S == "Never Attended"] <- 'Left') %>%
  within(S[S=="Never Attended"] <- 'Left') %>%
  
  # 'Not Old Enough' Intermediates, should be same for Senior
  within(S[I=="Not Old Enough"] <- 'Not Old Enough') %>%
  
  # Remove 'Not Old Enough' Juniors
  filter(J != 'Not Old Enough') %>%
  
  # 'Skipped' == (Junior, Senior Engaged but Int 'Never Attended')
  within(I[I=="Never Attended" & J != 'Never Attended' & S != 'Never Attended'] <- 'Skip Int.')

#
## Count: Junior to Intermediate Journeys
journeys_J_I <- journeys_all %>%
  
  # Select columns of interest
  select(J, I, Freq) %>%
  
  # Group and count journeys
  group_by(J, I) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Remove non-Juniors,Intermediates
  filter(J != "Never Attended" | I != "Never Attended") %>%
  
  # Assign "Never Attended" Juniors to "Skip Jun."
  within(J[J=="Never Attended"] <- "Skip Jun.") %>%
  
  # Convert Junior cateories to numbers
  within(J[J=="A. Ideal (2+)"] <- "Ideal_Jun.") %>%
  within(J[J=="B. Moderate (0.5-2)"] <- "Moderate_Jun.") %>%
  within(J[J=="C. Limited (< 0.5)"] <- "Limited_Jun.") %>%
  within(J[J=="Skip Jun."] <- "Skip_Jun.") %>%
  
  # Convert Intermediate categories to numbers
  within(I[I=="A. Ideal (2+)"] <- "Ideal_Int.") %>%
  within(I[I=="B. Moderate (0.5-2)"] <- "Moderate_Int.") %>%
  within(I[I=="C. Limited (< 0.5)"] <- "Limited_Int.") %>%
  within(I[I=="Not Old Enough"] <- "Young_Int.") %>%
  within(I[I=="Left"] <- "Left_Jun.") %>%
  within(I[I=="Skip Int."] <- "Skip_Int.")
##
#

#
## Count: Intermediate to Senior journeys
journeys_I_S <- journeys_all %>%
  
  # Select Intermediate and Senior
  select(I, S, Freq) %>%
  
  # Group and count
  group_by(I, S) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Remove unchanged categories
  #filter(I != 'Not Old Enough' | S != 'Not Old Enough') %>%
  #filter(I != 'Left' | S != 'Left') %>%
  
  # Assign "Never Attended" Intermediate to "Skip Int."
  within(I[I=="Never Attended"] <- "Skip Int.") %>%

  within(I[I=="A. Ideal (2+)"] <- "Ideal_Int.") %>%
  within(I[I=="B. Moderate (0.5-2)"] <- "Moderate_Int.") %>%
  within(I[I=="C. Limited (< 0.5)"] <- "Limited_Int.") %>%
  within(I[I=="Not Old Enough"] <- "Young_Int.") %>%
  within(I[I=="Left"] <- "Left_Jun.") %>%
  within(I[I=="Skip Int."] <- "Skip_Int.") %>%
  
  within(S[S=="A. Ideal (2+)"] <- "Ideal_Sen.") %>%
  within(S[S=="B. Moderate (0.5-2)"] <- "Moderate_Sen.") %>%
  within(S[S=="C. Limited (< 0.5)"] <- "Limited_Sen.") %>%
  within(S[S=="Not Old Enough"] <- "Young_Sen.") %>%
  within(S[S=='Left'] <- "Left_Int.")
##
#

#
## Rename columns for sankey function
names(journeys_J_I) <- c("source", "target", "value")
names(journeys_I_S) <- c("source", "target", "value")
##
#

#
## Create Sankey Plot

# Load Libraries
library(networkD3)
library(dplyr)

# Create your links
links <- rbind(as.data.frame(journeys_J_I), as.data.frame(journeys_I_S))

# Create your nodes
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
  )

# Create a group column in nodes
nodes$group <- c("Ideal", "Moderate", "Limited", "Not_Engaged", "Ideal", "Moderate", "Limited", "Not_Engaged", "Not_Engaged", "Not_Engaged", "Ideal", "Moderate", "Limited", "Not_Engaged", "Not_Engaged")

# Create your Link Groups
links$group <- nodes$group[match(links$source, nodes$name)]

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["Ideal", "Moderate","Limited", "Not_Engaged"]) .range(["#89C349", "#AA87BC", "#DCDCDC","#656666"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale=my_color, LinkGroup="group", NodeGroup="group", fontSize = 12)
p
##
#