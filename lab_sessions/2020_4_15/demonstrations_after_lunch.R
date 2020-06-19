# Only install igraph if not currently installed, and load it either way

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
plot(dark_side_graph)

light_side_graph <- induced_subgraph(g, light_side)
plot(light_side_graph)

head(edges)
d <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
plot(d)
g
E(g)
d
E(d)
head(edges$weight)
head(E(d))
head(E(d)$weight)
edge_attr(g)

E(g)$color <- "blue"
E(g)$weight
E(g)$color[E(g)$weight >= 5] <- "red"
head(E(g)$color)
edge_attr(g)
plot(g)
g[]
d[]

summary(g)
betweenness(g)
page_rank(g)
centr_betw(g)
max(E(g)$weight)
E(g)$weight
edge_attr(g)
E(g)
head(edges)
tail(edges,)
