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
journeys_all <- member_engagement_level %>%
  
  # Group and count journeys
  group_by(Y1, Y2, Y3, Y4, Y5) %>%
  summarise(Freq = n()) %>%
  ungroup() %>%
  
  # Define 'Left' as consecutive years 'Did Not Attend'
  within(Y2[Y2=="Did Not Attend" & Y3 == "Did Not Attend" & Y4 == "Did Not Attend" & Y5 == "Did Not Attend"] <- 'Left') %>%
  within(Y3[Y3 == "Did Not Attend" & Y4 == "Did Not Attend" & Y5 == "Did Not Attend"] <- 'Left') %>%
  within(Y4[Y4 == "Did Not Attend" & Y5 == "Did Not Attend"] <- 'Left') %>%
  within(Y5[Y5 == "Did Not Attend"] <- 'Left')

#
## Count: Junior to Intermediate Journeys
journeys_Y1_Y2 <- journeys_all %>%
  
  # Select columns of interest
  select(Y1, Y2, Freq) %>%
  
  # Group and count journeys
  group_by(Y1, Y2) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  
  # Convert Junior cateories to numbers
  within(Y1[Y1=="Ideal (2+)"] <- 0) %>%
  within(Y1[Y1=="Moderate (0.5-2)"] <- 1) %>%
  within(Y1[Y1=="Limited (< 0.5)"] <- 2) %>%
  
  # Convert Intermediate categories to numbers
  within(Y2[Y2=="Ideal (2+)"] <- 3) %>%
  within(Y2[Y2=="Moderate (0.5-2)"] <- 4) %>%
  within(Y2[Y2=="Limited (< 0.5)"] <- 5) %>%
  within(Y2[Y2=="Did Not Attend"] <- 6) %>%
  within(Y2[Y2=="Left"] <- 7)
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
  
  # Change Left flow to 0
  #within(Freq[Y2 == 'Left'] <- 0) %>%
  
  # Convert Intermediate categories to numbers
  within(Y2[Y2=="Ideal (2+)"] <- 3) %>%
  within(Y2[Y2=="Moderate (0.5-2)"] <- 4) %>%
  within(Y2[Y2=="Limited (< 0.5)"] <- 5) %>%
  within(Y2[Y2=="Did Not Attend"] <- 6) %>%
  within(Y2[Y2=="Left"] <- 7) %>%
  
  # Convert Intermediate categories to numbers
  within(Y3[Y3=="Ideal (2+)"] <- 8) %>%
  within(Y3[Y3=="Moderate (0.5-2)"] <- 9) %>%
  within(Y3[Y3=="Limited (< 0.5)"] <- 10) %>%
  within(Y3[Y3=="Did Not Attend"] <- 11) %>%
  within(Y3[Y3=="Left"] <- 12)
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
  
  # Remove 'Left'
  #filter(Y3 != 'Left') %>%
  
  # Convert Intermediate categories to numbers
  within(Y3[Y3=="Ideal (2+)"] <- 8) %>%
  within(Y3[Y3=="Moderate (0.5-2)"] <- 9) %>%
  within(Y3[Y3=="Limited (< 0.5)"] <- 10) %>%
  within(Y3[Y3=="Did Not Attend"] <- 11) %>%
  within(Y3[Y3=="Left"] <- 12) %>%
  
  # Convert Intermediate categories to numbers
  within(Y4[Y4=="Ideal (2+)"] <- 13) %>%
  within(Y4[Y4=="Moderate (0.5-2)"] <- 14) %>%
  within(Y4[Y4=="Limited (< 0.5)"] <- 15) %>%
  within(Y4[Y4=="Did Not Attend"] <- 16) %>%
  within(Y4[Y4=="Left"] <- 17)
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
  
  # Remove 'Left'
  #filter(Y4 != 'Left') %>%
  
  # Convert Intermediate categories to numbers
  within(Y4[Y4=="Ideal (2+)"] <- 13) %>%
  within(Y4[Y4=="Moderate (0.5-2)"] <- 14) %>%
  within(Y4[Y4=="Limited (< 0.5)"] <- 15) %>%
  within(Y4[Y4=="Did Not Attend"] <- 16) %>%
  within(Y4[Y4=="Left"] <- 17) %>%
  
  # Convert Intermediate categories to numbers
  within(Y5[Y5=="Ideal (2+)"] <- 18) %>%
  within(Y5[Y5=="Moderate (0.5-2)"] <- 19) %>%
  within(Y5[Y5=="Limited (< 0.5)"] <- 20) %>%
  within(Y5[Y5=="Left"] <- 21)
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
## Create final dataframe, convert to numeric
sankey_data <- rbind(as.data.frame(journeys_Y1_Y2),
                     as.data.frame(journeys_Y2_Y3),
                     as.data.frame(journeys_Y3_Y4),
                     as.data.frame(journeys_Y4_Y5))
i <- c(1, 2, 3)
sankey_data[ , i] <- apply(sankey_data[ , i], 2,  # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
##
#

#
## Create Sankey Plot

# Load Libraries
library(networkD3)

# Create your nodes
nodes = data.frame("name" = 
                     c("Ideal (2+)", # Node 0
                       "Moderate (0.5-2)", # Node 1
                       "Limited (< 0.5)", # Node 2
                       "Ideal (2+)", # Node 3
                       "Moderate (0.5-2)", # Node 4
                       "Limited (< 0.5)", # Node 5
                       "Did Not Attend", # Node 6
                       "Left",  # Node 7
                       "Ideal (2+)", # Node 8
                       "Moderate (0.5-2)", # Node 9
                       "Limited (< 0.5)", # Node 10
                       "Did Not Attend", # Node 11
                       "Left",  # Node 12
                       "Ideal (2+)", # Node 13
                       "Moderate (0.5-2)", # Node 14
                       "Limited (< 0.5)", # Node 15
                       "Did Not Attend", # Node 16
                       "Left",  # Node 17
                       "Ideal (2+)", # Node 18
                       "Moderate (0.5-2)", # Node 19
                       "Limited (< 0.5)", # Node 20
                       "Left"  # Node 21
                     ))

# Plot
sankeyNetwork(Links = sankey_data,
              Nodes = nodes,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "name",
              fontSize= 14,
              nodeWidth = 15)
##
#