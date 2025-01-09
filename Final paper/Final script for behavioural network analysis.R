### Impact of Female Mating Status on Male Courtship Behaviour in the Sexually 
### Cannibalistic New Zealand Fishing Spider Dolomedes minor (Araneae, Pisauridae)
### Bastien Clémot, 2023
### Behavioural network analysis

#### Packages and data ####
## Packages
library(data.table)
library(tidyverse)
library(igraph)
library(rqdatatable)
## Data
group1 <- read.csv("transition_group1.csv")
group2 <- read.csv("transition_group2.csv")

#### Generation of behavioural networks

## Generation of the observed adjacency matrix 
graph1 <- graph.data.frame(group1, directed = T)
graph2 <- graph.data.frame(group2, directed = T)
adjacency.matrix1 <- get.adjacency(graph1, sparse = F)
adjacency.matrix2 <- get.adjacency(graph2, sparse = F)

## Permutation test
# Number of permutations
m<-10000
# Empty list for the results
results1<-as.list(NA)
results2<-as.list(NA)
# Randomly shuffle the second column m times and keep each new adjacency matrix in a list
# Group1
for (i in 1:m) {
  perm <- group1
  perm[, 2] <- perm[sample(1:nrow(perm), replace = FALSE), 2]
  perm.graph <- graph.data.frame(perm, directed = T)
  perm.adjacency.matrix <- get.adjacency(perm.graph, sparse = F)
  results1[[i]] <- perm.adjacency.matrix
}
# Group2
for (i in 1:m) {
  perm <- group2
  perm[, 2] <- perm[sample(1:nrow(perm), replace = FALSE), 2]
  perm.graph <- graph.data.frame(perm, directed = T)
  perm.adjacency.matrix <- get.adjacency(perm.graph, sparse = F)
  results2[[i]] <- perm.adjacency.matrix
}

## Reorganize list
listVec1 <- lapply(results1, c, recursive=TRUE)
x1 <- do.call(cbind, listVec1)
listVec2 <- lapply(results2, c, recursive=TRUE)
x2 <- do.call(cbind, listVec2)

## Calculate quantiles 5 and 95%
quantiles1<-matrix(NA,nrow=nrow(x1),ncol=2)
for (i in 1:nrow(x1)){
  q<-quantile(x1[i,],probs=c(0.05,0.95))
  quantiles1[i,]<-q
}
quantiles2<-matrix(NA,nrow=nrow(x2),ncol=2)
for (i in 1:nrow(x2)){
  q<-quantile(x2[i,],probs=c(0.05,0.95))
  quantiles2[i,]<-q
}

## Save matrices for each of the low and high quantile data
low1<-matrix(quantiles1[,1],nrow=14,byrow=F)
high1<-matrix(quantiles1[,2],nrow=14,byrow=F)
low2<-matrix(quantiles2[,1],nrow=14,byrow=F)
high2<-matrix(quantiles2[,2],nrow=14,byrow=F)

## Rename the low and high quantile datasets to match the observed adjacency matrix
colnames(low1)<-colnames(adjacency.matrix1)
rownames(low1)<-colnames(adjacency.matrix1)
colnames(high1)<-colnames(adjacency.matrix1)
rownames(high1)<-colnames(adjacency.matrix1)
colnames(low2)<-colnames(adjacency.matrix2)
rownames(low2)<-colnames(adjacency.matrix2)
colnames(high2)<-colnames(adjacency.matrix2)
rownames(high2)<-colnames(adjacency.matrix2)

## Calculate transition probabilities
#divide each cell by the row sum
trans.prob1<-round(adjacency.matrix1/rowSums(adjacency.matrix1),2) 
trans.prob1[is.nan(trans.prob1)]<-0 #replace any NaN values with 0
trans.prob1
trans.prob2<-round(adjacency.matrix2/rowSums(adjacency.matrix2),2) 
trans.prob2[is.nan(trans.prob2)]<-0 #replace any NaN values with 0
trans.prob2
# Simplify to the matrix of significant behaviors
keep1<-ifelse(adjacency.matrix1>=high1,trans.prob1,0)
keep2<-ifelse(adjacency.matrix2>=high2,trans.prob2,0)

## Graphical representation
# Creation of a network object 
network1<-graph.adjacency(keep1,weighted=TRUE,diag=TRUE)
network2<-graph.adjacency(keep2,weighted=TRUE,diag=TRUE)
# Vertex sizes
vertex_sizes1<-degree(adjacency.matrix1,rescale=TRUE)
vertex_sizes2<-degree(adjacency.matrix2,rescale=TRUE)
# Set edge (arrow) weights to be equal to 5 categories of edge weight (# of transitions)
edge_weights1<-as.numeric(cut(E(network1)$weight,c(0,.1,.25,.5,.75,1),labels=c(1:5)))
edge_weights2<-as.numeric(cut(E(network2)$weight,c(0,.1,.25,.5,.75,1),labels=c(1:5)))
# Plot network
plot1<-tkplot(network1)
plot2<-tkplot(network2)
# Coordinates
coords1<-tkplot.getcoords(plot1)
coords2<-tkplot.getcoords(plot2)
# Final
plot.igraph(network1,vertex.label=V(network1)$name, vertex.label.cex = 1.25, layout=coords1,vertex.size=vertex_sizes1*150, edge.width=edge_weights1/4, edge.arrow.size=edge_weights1/4)
plot.igraph(network2,vertex.label=V(network2)$name, vertex.label.cex = 1.25, layout=coords2,vertex.size=vertex_sizes2*150, edge.width=edge_weights2/4, edge.arrow.size=edge_weights2/4)

