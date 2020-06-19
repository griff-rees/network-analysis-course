library(igraph)

nodes <- read.csv("data/congress-twitter-network-nodes.csv")
edges <- read.csv("data/congress-twitter-network-edges.csv")


# Amirali Emami's example posted at 11:03 19/6/2020
g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
# top in-followers
in_degrees = data.frame(degree(g, mode='in', normalized=FALSE))
in_degrees$name <- V(g)$name
names(in_degrees) <- c('degree','name')
in_degrees <- in_degrees %>% arrange(desc(degree))
head(in_degrees)
hist(in_degrees$degree)


# most follow
out_degrees = data.frame(degree(g, mode='out', normalized=FALSE))
out_degrees$name <- V(g)$name
names(out_degrees) <- c('degree','name')
out_degrees <- out_degrees %>% arrange(desc(degree))
head(out_degrees)
hist(out_degrees$degree)

# reciprocity
reciprocity(g, ignore.loops = TRUE, mode ="default")
