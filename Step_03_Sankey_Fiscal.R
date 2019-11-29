# Sankey Diagram
# Tutorial URL - https://www.r-bloggers.com/creating-custom-sankey-diagrams-using-r/
# Author: Andrew Macumber

#
## Goal
# Display engagement journeys using Sankey diagrams
##
#

#
## Define: "Never Attended" & "Not Old Enough"
member_engagement_fiscal <- member_engagement_fiscal %>%
  
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
journeys_all <- member_engagement_fiscal %>%
  
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
  within(J[J=="A. Ideal (2+)"] <- 0) %>%
  within(J[J=="B. Moderate (0.5-2)"] <- 1) %>%
  within(J[J=="C. Low (< 0.5)"] <- 2) %>%
  within(J[J=="Skip Jun."] <- 12) %>%
  
  # Convert Intermediate categories to numbers
  within(I[I=="A. Ideal (2+)"] <- 3) %>%
  within(I[I=="B. Moderate (0.5-2)"] <- 4) %>%
  within(I[I=="C. Low (< 0.5)"] <- 5) %>%
  within(I[I=="Not Old Enough"] <- 6) %>%
  within(I[I=="Left"] <- 10) %>%
  within(I[I=="Skip Int."] <- 11)
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
  filter(I != 'Not Old Enough' | S != 'Not Old Enough') %>%
  filter(I != 'Left' | S != 'Left') %>%
  
  # Assign "Never Attended" Intermediate to "Skip Int."
  within(I[I=="Never Attended"] <- "Skip Int.") %>%

  within(I[I=="A. Ideal (2+)"] <- 3) %>%
  within(I[I=="B. Moderate (0.5-2)"] <- 4) %>%
  within(I[I=="C. Low (< 0.5)"] <- 5) %>%
  within(I[I=="Not Old Enough"] <- 6) %>%
  within(I[I=="Skip Int."] <- 11) %>%
  
  within(S[S=="A. Ideal (2+)"] <- 7) %>%
  within(S[S=="B. Moderate (0.5-2)"] <- 8) %>%
  within(S[S=="C. Low (< 0.5)"] <- 9) %>%
  within(S[S=="Not Old Enough"] <- 6) %>%
  within(S[S=='Left'] <- 10)
##
#

#
## Rename columns for sankey function
names(journeys_J_I) <- c("source", "target", "value")
names(journeys_I_S) <- c("source", "target", "value")
##
#

#
## Create final dataframe, convert to numeric
sankey_data <- rbind(as.data.frame(journeys_J_I), as.data.frame(journeys_I_S))
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
                     c("Ideal Jun.", # Node 0
                       "Moderate Jun.", # Node 1
                       "Low Jun.", # Node 2
                       "Ideal Int", # Node 3
                       "Moderate Int",  # Node 4
                       "Low Int",  # Node 5
                       "Not Old Enough",  # Node 6
                       "Ideal Sen", # Node 7
                       "Moderate Sen",  # Node 8
                       "Low Sen",  # Node 9
                       "Left", # Node 10
                       "Skip Int.",  # Node 11
                       "Skip Jun." # Node 12
                       ))

# Plot
sankeyNetwork(Links = sankey_data,
              Nodes = nodes,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "name",
              fontSize= 18,
              nodeWidth = 30)
##
#