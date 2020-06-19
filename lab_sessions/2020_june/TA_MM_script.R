library(igraph)
library(tidyverse)

## loading nodes and edges -----------------------------------------

nodes <- read.csv("data/star-wars-network-nodes.csv")
nodes

edges <- read.csv("data/star-wars-network-edges.csv")
edges

## making graph and plotting ---------------------------------------

g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)

plot(g)

## light and dark side ---------------------------------------------

h <- g
dark_side <- c("DARTH VADER", "MOTTI", "TARKIN")
light_side <- c("R2-D2", "CHEWBACCA", "C-3PO", "LUKE", "CAMIE", "BIGGS",
                "LEIA", "BERU", "OWEN", "OBI-WAN", "HAN", "DODONNA",
                "GOLD LEADER", "WEDGE", "RED LEADER", "RED TEN", "GOLD FIVE")
neutral <- c("GREEDO", "JABBA")

# Add the color attribute to the network nodes
V(h)$color <- NA
V(h)$color[V(h)$name %in% dark_side] <- "red"
V(h)$color[V(h)$name %in% light_side] <- "gold"
V(h)$color[V(h)$name %in% neutral] <- "green"

plot(h, layout=layout_with_fr, main="Force-directed")
legend(x=.75, y=.75, legend=c("Dark side", "Light side", "Neutral"), 
       pch=21, pt.bg=c("red", "gold", "green"), pt.cex=2, bty="n")

# lowest degree between chosen nodes
characters <- c("R2-D2","GOLD FIVE","LEIA","JABBA","GREEDO")
sort(degree(g)[V(g)$name %in% characters])

## Nodes -----------------------------------------------------------

# V function to look at nodes
V(g)

# look at node attributes
vertex_attr(g)

# attribute name
V(g)$name

# adding an attribute

dark_side <- c("DARTH VADER", "MOTTI", "TARKIN")
light_side <- c("R2-D2", "CHEWBACCA", "C-3PO", "LUKE", "CAMIE", "BIGGS",
                "LEIA", "BERU", "OWEN", "OBI-WAN", "HAN", "DODONNA",
                "GOLD LEADER", "WEDGE", "RED LEADER", "RED TEN", "GOLD FIVE")
neutral <- c("GREEDO", "JABBA")

V(g)$color <- NA
V(g)$color[V(g)$name %in% dark_side] <- "red" # set the dark side color name to red
V(g)$color[V(g)$name %in% light_side] <- "gold" # set the light side color name to gold
V(g)$color[V(g)$name %in% neutral] <- "green" # set the color of neutral characters to green
V(g)$color

# tidyverse alternative to add a node attribute ------------------------------

# first, create variable in original dataframe using mutate
nodes <- nodes %>%
  mutate(
    color = ifelse(name %in% dark_side, "red",
                   ifelse(name %in% light_side, "gold", "green"))
  )
# second, add vertex attribute
g <- g %>%
  set_vertex_attr(name = "color",
                  value = nodes %>% select(color) %>% as_vector())

## subgraphs -------------------------------------------

dark_side_graph <- induced_subgraph(g, dark_side) # Using the dark_side variable from above
V(dark_side_graph)
plot(dark_side_graph)

light_side_graph <- induced_subgraph(g, light_side)
plot(light_side_graph)

## edges ------------------------------------------------

# default is to have directed = T, but shown here for emphasis
d <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)

plot(d)

# DNW stands for Directed, Named, Weighted
d

# looking at edges
E(d)

# edge ids
as_ids(E(d))

# looking at weights
E(d)$weight

# binding with edge ids
data.frame(weight = E(d)$weight, 
       nodes = as_ids(E(d)))

# into adjacency matrix
as_adjacency_matrix(d)

# into adjacency matrix with weights instead of edges
as_adjacency_matrix(d, attr = "weight")

# colour for edges
E(d)$color <- "blue"
E(d)$color[E(d)$weight >= 5] <- "red"

edge_attr(d)

plot(d)

## adjacency matrix -----------------------------------------------

g[] # with weights

mean(as.matrix(g[]) == t(as.matrix(g[]))) # checking symmetry, 1 = symmetrical

as_adjacency_matrix(g, attr = "weight") # equivalent to g[] command

mean(as.matrix(g[]) == as.matrix(as_adjacency_matrix(g, attr = "weight"))) # shows they are equivalent

## plotting -------------------------------------------------------

plot(d, layout = layout_with_fr, main = "fr layout")

## lab -------------------------------------------------------------

congress_edges <- read.csv("data/congress-twitter-network-edges.csv")
congress_nodes <- read.csv("data/congress-twitter-network-nodes.csv")

head(congress_edges)
head(congress_nodes)

congress <- graph_from_data_frame(d = congress_edges, 
                           vertices = congress_nodes,
                           directed = TRUE)

lay_out <- layout_nicely(congress)
# plot is commented out due to time to run
# plot(congress, layout = lay_out, vertex.label = NA)

# removing isolates
isolated <- which(degree(congress) == 0)
congress_filt <- delete.vertices(congress, isolated)
plot(congress_filt, vertex.label = NA)

## random sample: note this is not recommended!!!! --------------------

# done for illustrative purposes, but not recommended
set.seed(123)
samp_size <- floor(length(V(congress_filt)$followers_count) * 0.2)
samp <- sample(V(congress_filt)$followers_count, samp_size)
subgroup <- V(congress_filt)$name[V(congress_filt)$followers_count %in% samp]
congress_sample <- induced_subgraph(congress_filt,subgroup)
plot(congress_sample, vertex.label = NA)

## plotting senators for closeness --------------------------------

# subgraph for senators
senator_names <- V(congress)$name[V(congress)$chamber == "sen"]
senators <- induced_subgraph(congress,senator_names)

# removing isolates
isolated_sen <- which(degree(senators)==0)
senators2 <- delete.vertices(senators, isolated_sen)

# calculating closeness
V(senators2)$closeness_in <- closeness(senators2, mode = "in")

# colour for party
V(senators2)$color <- ifelse(V(senators2)$party == "Republican",
                               "tomato","lightblue")

# 95th percentile for closeness
ninety_five <- quantile(V(senators2)$closeness_in, 0.95)

# setting layout
m1 <- layout_nicely(senators2)

# plotting
plot(senators2,
     vertex.label = ifelse(V(senators2)$closeness_in >= ninety_five, 
                           V(senators2)$name, NA),
     vertex.label.color = "black",
     layout = m1,
     main = "Senators by Party and Closeness")

## Nathan's excellent ggplot and tidyverse solution ----------------------------------------

install.packages("network")
library(network)
install.packages("ggnetwork")
library(ggnetwork)

gg_senators <- ggnetwork(
  senators2
)

ggplot(fortify(gg_senators), 
       aes(x, y, xend = xend, yend = yend)) +
  geom_edges(alpha = 0.2) +
  geom_nodes(aes(colour = party, size = followers_count)) +
  theme_blank() +
  scale_colour_manual(values = c("blue","green","red"))

## who's the independent senator? -------------------------------

V(congress)$name[V(congress)$party == "Independent"] # It's Bernie!
