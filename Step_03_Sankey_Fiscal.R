# Sankey Diagram
# URL - https://www.r-bloggers.com/creating-custom-sankey-diagrams-using-r/

# Step 03 Alluvial Fiscal
# Author: Andrew Macumber

#
## Convert NAs to "Never Attended" or "Not Old Enough"

# Define: "Never Attended" & "Not Old Enough"
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
  select(-d4g_member_id, -age) #%>%

# Count: Junior to Intermediate Journeys
Jun_Int <- member_engagement_fiscal %>%
  
  select(J, I) %>%
  
  group_by(J, I) %>%
  summarise(Freq = n()) %>%
  
  filter(J != "Not Old Enough") %>%
  
  within(J[J=="A. Ideal (2+)"] <- 0) %>%
  within(J[J=="B. Moderate (0.5-2)"] <- 1) %>%
  within(J[J=="C. Low (< 0.5)"] <- 2) %>%
  within(J[J=="Never Attended"] <- 3) %>%
  
  within(I[I=="A. Ideal (2+)"] <- 4) %>%
  within(I[I=="B. Moderate (0.5-2)"] <- 5) %>%
  within(I[I=="C. Low (< 0.5)"] <- 6) %>%
  within(I[I=="Never Attended"] <- 7) %>%
  within(I[I=="Not Old Enough"] <- 8)

names(Jun_Int) <- c("source", "target", "value")
  

# Count: Intermediate to Senior
Int_Sen <- member_engagement_fiscal %>%
  
  select(I, S) %>%
  
  group_by(I, S) %>%
  summarise(Freq = n()) %>%
  
  filter(Freq > 2) %>%
  
  within(I[I=="A. Ideal (2+)"] <- 4) %>%
  within(I[I=="B. Moderate (0.5-2)"] <- 5) %>%
  within(I[I=="C. Low (< 0.5)"] <- 6) %>%
  within(I[I=="Never Attended"] <- 7) %>%
  within(I[I=="Not Old Enough"] <- 8) %>%
  
  within(S[S=="A. Ideal (2+)"] <- 9) %>%
  within(S[S=="B. Moderate (0.5-2)"] <- 10) %>%
  within(S[S=="C. Low (< 0.5)"] <- 11) %>%
  within(S[S=="Never Attended"] <- 12) %>%
  within(S[S=="Not Old Enough"] <- 13)

names(Int_Sen) <- c("source", "target", "value")

sankey_data <- rbind(as.data.frame(Jun_Int), as.data.frame(Int_Sen))

# Check the object class
sapply(sankey_data, class)

# Convert to numeric
i <- c(1, 2, 3)

sankey_data[ , i] <- apply(sankey_data[ , i], 2,  # Specify own function within apply
                    function(x) as.numeric(as.character(x)))


#
## Prep Workspace

# Load Libraries
library(networkD3)

# Create your nodes
nodes = data.frame("name" = 
                     c("Ideal Jun.", # Node 0
                       "Moderate Jun.", # Node 1
                       "Low Jun.", # Node 2
                       "Never Attended Jun.",  # Node 3
                       "Ideal Int", # Node 4
                       "Moderate Int",  # Node 5
                       "Low Int",  # Node 6
                       "Never Attended Int",  # Node 7
                       "Not Old Enough Int",  # Node 8
                       "Ideal Sen", # Node 9
                       "Moderate Sen",  # Node 10
                       "Low Sen",  # Node 11
                       "Never Attended Sen",  # Node 12
                       "Not Old Enough Sen"))  # Node 13

# Create your links
links = sankey_data
##
#

#
## Plot
windows()
sankeyNetwork(Links = links,
              Nodes = nodes,
              Source = "source",
              Target = "target",
              Value = "value",
              NodeID = "name",
              fontSize= 12,
              nodeWidth = 30)
##
#