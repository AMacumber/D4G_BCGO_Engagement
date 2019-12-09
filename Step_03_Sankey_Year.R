# Sankey Diagram
# Tutorial URL - https://www.r-bloggers.com/creating-custom-sankey-diagrams-using-r/
# Author: Andrew Macumber and Alex Campbell
# Pre-requisites: Step 01 and 02

#
## Goal
# Display engagement journeys using Sankey diagrams
##
#

#
## Count: All Journeys
journeys_all <- member_engagement_levels %>%
  
  # Group and count journeys
  group_by(Y1, Y2, Y3, Y4, Y5) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  
  # Define 'Left' as consecutive years 'Absent'
  within(Y2[Y2=="Absent" & Y3 == "Absent" & Y4 == "Absent" & Y5 == "Absent"] <- 'Left') %>%
  within(Y3[Y3 == "Absent" & Y4 == "Absent" & Y5 == "Absent"] <- 'Left') %>%
  within(Y4[Y4 == "Absent" & Y5 == "Absent"] <- 'Left') %>%
  within(Y5[Y5 == "Absent"] <- 'Left') %>%
  
  # Remove Absent
  filter(Y2 != 'Absent', Y3 != 'Absent', Y4 != 'Absent')

#
## Count: Junior to Intermediate Journeys
journeys_Y1_Y2 <- journeys_all %>%
  
  # Select columns of interest
  select(Y1, Y2, Freq) %>%
  
  # Group and count journeys
  group_by(Y1, Y2) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Append a year qualifier
  transform(
    Y1 = sprintf('Y1_%s', Y1),
    Y2 = sprintf('Y2_%s', Y2))
##
#

#
## Count: Junior to Intermediate Journeys
journeys_Y2_Y3 <- journeys_all %>%
  
  # Select columns of interest
  select(Y2, Y3, Freq) %>%
  
  # Group and count journeys
  group_by(Y2, Y3) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Append a year qualifier
  transform(
    Y2 = sprintf('Y2_%s', Y2),
    Y3 = sprintf('Y3_%s', Y3))
##
#

#
## Count: Junior to Intermediate Journeys
journeys_Y3_Y4 <- journeys_all %>%
  
  # Select columns of interest
  select(Y3, Y4, Freq) %>%
  
  # Group and count journeys
  group_by(Y3, Y4) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Append a year qualifier
  transform(
    Y3 = sprintf('Y3_%s', Y3),
    Y4 = sprintf('Y4_%s', Y4))
##
#

#
## Count: Junior to Intermediate Journeys
journeys_Y4_Y5 <- journeys_all %>%
  
  # Select columns of interest
  select(Y4, Y5, Freq) %>%
  
  # Group and count journeys
  group_by(Y4, Y5) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Append a year qualifier
  transform(
    Y4 = sprintf('Y4_%s', Y4),
    Y5 = sprintf('Y5_%s', Y5))
##
#

#
## Rename columns for sankey function
names(journeys_Y1_Y2) <- c("source", "target", "value")
names(journeys_Y2_Y3) <- c("source", "target", "value")
names(journeys_Y3_Y4) <- c("source", "target", "value")
names(journeys_Y4_Y5) <- c("source", "target", "value")
##
#

#
## Load Libraries
library(networkD3)
##
#

#
## Format for Plotting

# Create your links
links <- rbind(as.data.frame(journeys_Y1_Y2),
               as.data.frame(journeys_Y2_Y3),
               as.data.frame(journeys_Y3_Y4),
               as.data.frame(journeys_Y4_Y5))

# Create your nodes
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# Create a group column in nodes
nodes$group <- c("Ideal", "Limited", "Ideal", "Left", "Limited", "Ideal", "Left", "Limited", "Ideal", "Left", "Limited", "Ideal", "Left", "Limited")

# Create your Link Groups
links$group <- nodes$group[match(links$source, nodes$name)]

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["Ideal", "Limited", "Left"]) .range(["#89C349", "#AA87BC", "#656666"])'
##
#

#
## Make the network. Plot.
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", LinkGroup="group", NodeGroup="group",
                   colourScale = my_color, 
                   fontSize = 56,
                   nodeWidth = 30)
p
##
#