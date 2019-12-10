# Sankey Diagram
# Tutorial URL - https://www.r-bloggers.com/creating-custom-sankey-diagrams-using-r/
# Tutorial URL - https://www.r-graph-gallery.com/322-custom-colours-in-sankey-diagram
# Author: Andrew Macumber, Alex Campbell
# Pre-requisites: Step 01, Step 02

#
## Goal
# Display engagement journeys using Sankey diagrams
##
#

#
## ReDefine NA values
member_eng_lvl_na <- member_engagement_levels %>%
  
  # Subset the dataframe with columns of interest
  select(d4g_member_id, age_category, Eng_Level) %>%
  
  # Create new dims; "variable" = Eng_Level; "value" = Eng_Level values
  pivot_wider(names_from = age_category, values_from = Eng_Level) %>%
  
  # Append the Members information
  inner_join(member_filtered, by = "d4g_member_id") %>%
  
  # replace any NaN with NA
  mutate_at(vars(-d4g_member_id), funs(ifelse(is.na(.), ifelse(age <= 10, "Young_Int",
                                                               ifelse(age <= 13, "Young_Sr", "Absent")), .))) %>%
  
  # Remove age information and member id
  select(J, I, S)
##
#

#
## Count: All Journeys
journeys_all <- member_eng_lvl_na %>%
  
  # Group and count journeys
  group_by(J, I, S) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  
  # 'Left' == (Junior Engaged to Int,Sen == 'Absent')
  within(I[I=="Absent" & S == "Absent"] <- 'LeftAfter_Jnr') %>%
  within(S[I=="LeftAfter_Jnr"] <- "LeftAfter_Jnr") %>%
  within(S[S=="Absent"] <- 'LeftAfter_Int') %>%
  
  # 'JoinedAs'
  within(I[J=="Absent" & I == "Absent"] <- "JoinedAs_Sr") %>%
  within(J[I=="JoinedAs_Sr"] <- "JoinedAs_Sr") %>%
  within(J[J=="Absent"] <- "JoinedAs_Int") %>%
  
  # Remove immature members
  filter(J != 'Young_Int', J != 'Young_Sr', I != 'Young_Int', I != 'Young_Sr', S != 'Young_Int', S != 'Young_Sr') %>%
  
  # Remove 'Absent' intermediates
  filter(I != "Absent")

#
## Count: Junior to Intermediate Journeys
journeys_J_I <- journeys_all %>%
  
  # Select columns of interest
  select(J, I, Freq) %>%
  
  # Group and count journeys
  group_by(J, I) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Filter out non junior and intermediates
  filter(J != "JoinedAs_Sr") %>%
  
  # Append an age qualifier
  transform(
    J = sprintf('Jnr_%s', J),
    I = sprintf('Int_%s', I))
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
  
  # Append an age qualifier
  transform(
    I = sprintf('Int_%s', I),
    S = sprintf('Sr_%s', S))
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
nodes$group <- c("Ideal", "Absent", "Limited", "Ideal", "Absent", "Left", "Limited", "Ideal", "Left", "Limited", "Left")

# Create your Link Groups
links$group <- nodes$group[match(links$source, nodes$name)]

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["Ideal", "Limited", "Absent", "Left"]) .range(["#89C349", "#AA87BC", "#DCDCDC", "#656666"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", LinkGroup="group", NodeGroup="group",
                   colourScale = my_color, 
                   fontSize = 48,
                   nodeWidth = 30)
p
##
#