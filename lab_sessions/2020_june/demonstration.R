library(igraph)
setwd('~/Downloads/network-analysis-course/')
nodes <- read.csv("data/star-wars-network-nodes.csv")
nodes
edges <- read.csv("data/star-wars-network-edges.csv")
edges
g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
plot(g)
nodes
V(g)
vertex_attr(g)
nodes$name
g$name
V(g)$name
dark_side <- c("DARTH VADER", "MOTTI", "TARKIN")
light_side <- c("R2-D2", "CHEWBACCA", "C-3PO", "LUKE", "CAMIE", "BIGGS",
                "LEIA", "BERU", "OWEN", "OBI-WAN", "HAN", "DODONNA",
                "GOLD LEADER", "WEDGE", "RED LEADER", "RED TEN", "GOLD FIVE")
neutral <- c("GREEDO", "JABBA")

V(g)$color <- NA
vertex_attr(g)
V(g)$color[V(g)$name %in% dark_side] <- "red"
V(g)$color
V(g)$color[V(g)$name %in% light_side] <- "gold" # set the light side color name to gold
plot(g)
V(g)$color
V(g)$color[V(g)$name %in% neutral] <- "green" # set the color of neutral characters to green
plot(g)
dark_side_graph <- induced_subgraph(g, dark_side) # Using the dark_side variable from above
dark_side_graph
V(dark_side_graph)
plot(dark_side_graph)
light_side_graph <- induced_subgraph(g, light_side) # Using the light_side variable
plot(light_side_graph)
plot(light_side_graph)

# Post break

head(edges)
d <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
plot(d)
d

E(g)
E(d)
E(g)$weight
edge_attr(g)
edge_attr(d)
E(g)$color <- "blue"
edge_attr(g)
E(g)$color[E(g)$weight >= 5] <- "red"
edge_attr(g)
plot(g)
g[]
d[]
#plot(d, layout=layout_with_sugiyama, main="Sugiyama Layout")
plot(d, layout=layout_with_fr, main="FR Layout")
V(d)$color <- NA
V(d)$color[V(d)$name %in% dark_side] <- "red"
V(d)$color[V(d)$name %in% light_side] <- "gold"
V(d)$color[V(d)$name %in% neutral] <- "green"
V(d)
plot(d, layout=layout_with_fr, main="FR Layout")
color_var <- V(d)$color
color_var
