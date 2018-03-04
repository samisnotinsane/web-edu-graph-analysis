rm(list=ls())

library(igraph)
library("poweRlaw")
library("ggplot2")

list <- read.csv("web-edu.csv", sep = " ", header = FALSE)
G <- graph.data.frame(list)

# List of degrees
G.degrees <- degree(G)

# List of vertices
G.vertices <- V(G)

# List of edges
G.edges <- E(G)

# Global clustering coefficient
G.globalClusterCoeff <- transitivity(graph = G, type = "global", vids = NULL, weights = NULL)

# Local clustering coefficient (not needed?) 
G.localClusterCoeff <- transitivity(graph = G, type = "local", vids = NULL, weights = NULL)

# Avg of local/network clustering coefficient
G.avgLocalClusterCoeff <- transitivity(graph = G, type = "localaverage", vids = NULL, weights = NULL)

# Count frequency of each degree and create log-log graph.
G.degree.histogram <- as.data.frame(table(G.degrees))
G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])
ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(nodes with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Degree Distribution (log-log)") +
  theme_bw()

# A more simplistic plot to the one above.
plot(degree.distribution(G), xlab="node degree")

# Visualise whole graph.
plot.igraph(G, vertex.size=0, vertex.label=NA)

plot(G, edge.arrow.size=0, vertex.color="red", vertex.size=0,
     vertex.label.color="black", 
     vertex.label.cex=0, vertex.label=NA, edge.curved=0.2)

# Generate G(n,m) model of Erdos-Renyi.
G2 <- sample_gnm(n = 3031, m = 6474, directed = FALSE, loops = FALSE)

# List of degrees
G2.degrees <- degree(G2)

# List of vertices
G2.vertices <- V(G2)

# List of edges
G2.edges <- E(G2)

# Clustering coefficients
G2.globalClusterCoeff <- transitivity(graph = G2, type = "global", vids = NULL, weights = NULL)
G2.localClusterCoeff <- transitivity(graph = G2, type = "local", vids = NULL, weights = NULL)
G2.avgLocalClusterCoeff <- transitivity(graph = G2, type = "localaverage", vids = NULL, weights = NULL)

# Count frequency of each degree and create log-log graph.
G2.degree.histogram <- as.data.frame(table(G2.degrees))
G2.degree.histogram[,1] <- as.numeric(G2.degree.histogram[,1])
ggplot(G2.degree.histogram, aes(x = G2.degrees, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(nodes with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Degree Distribution of G(n,m) model (log-log)") +
  theme_bw()

plot.igraph(G2, vertex.size=0, vertex.label=NA)

plot(G2, edge.arrow.size=0, vertex.color="red", vertex.size=0,
     vertex.label.color="black", 
     vertex.label.cex=0, vertex.label=NA, edge.curved=0.2)

# Export E-R network to gnm.csv
write_graph(graph = G2, file = "gnm.csv", format = "edgelist")

# ----
dgraph <- read.table("web-edu.csv", sep = " ", header = FALSE)
dgraph.network <- graph.data.frame(d = dgraph, directed = TRUE)

degree(dgraph.network, v = V(dgraph.network))

