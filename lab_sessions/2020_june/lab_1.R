library(igraph)


# Group 1
```{r}
library(dplyr)
library(igraph)
edges <- read.csv('../data/congress-twitter-network-edges.csv')
nodes <- read.csv('../data/congress-twitter-network-nodes.csv')
popular <- nodes[nodes['followers_count'] > 100000,]
edges <- edges[which( edges$source %in% popular$id_str) ,]
edges <- edges[which( edges$target %in% popular$id_str) ,]
g <- graph_from_data_frame(d=edges, vertices=popular, directed=TRUE)
V(g)$color <- popular$party
plot(g)
```

# Group 2


# Group 3


# Group 4
