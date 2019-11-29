# Sankey Diagram
# URL - https://www.r-bloggers.com/creating-custom-sankey-diagrams-using-r/

#
## Prep Workspace

# Load Libraries
library(networkD3)

# Create your nodes
nodes = data.frame("name" = 
                     c("Node A", # Node 0
                       "Node B", # Node 1
                       "Node C", # Node 2
                       "Node D"))# Node 3

# Create your links
links = as.data.frame(matrix(c(
  0, 1, 10, # Each row represents a link. The first number
  0, 2, 20, # represents the node being conntected from. 
  1, 3, 30, # the second number represents the node connected to.
  2, 3, 40),# The third number is the value of the node
  byrow = TRUE, ncol = 3))

# Name your links
names(links) = c("source", "target", "value")
##
#

#
## Plot
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)
##
#