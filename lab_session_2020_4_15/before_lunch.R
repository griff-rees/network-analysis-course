if (!require(igraph)) {
    install.packages("igraph", repos="http://cran.uk.r-project.org")
}
setwd("~/Downloads/network-analysis-course/")
nodes <- read.csv("data/star-wars-network-nodes.csv")
edges <- read.csv("data/star-wars-network-edges.csv")
g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
plot(g)


dark_side <- c("DARTH VADER", "MOTTI", "TARKIN")
light_side <- c("R2-D2", "CHEWBACCA", "C-3PO", "LUKE", "CAMIE", "BIGGS",
                "LEIA", "BERU", "OWEN", "OBI-WAN", "HAN", "DODONNA",
                "GOLD LEADER", "WEDGE", "RED LEADER", "RED TEN", "GOLD FIVE")
neutral <- c("GREEDO", "JABBA")

V(g)$color <- NA

vertex_attr(g)

V(g)$color[V(g)$name %in% dark_side] <- "red"
V(g)$color

V(g)$color[V(g)$name %in% light_side] <- "gold"
V(g)$color[V(g)$name %in% neutral] <- "green"


dark_side_graph <- induced_subgraph(g, dark_side)

V(dark_side_graph)
