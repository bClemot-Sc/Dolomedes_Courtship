############################################################
#### code to permute and plot behavioural sequence data ####
############ used in Green & Patek 2017 ####################
############################################################

#Load required packages
library('sand')
library('igraph')
library('sna')
library('diagram')

#load data
data<-read.csv('Data group1.csv',header=F)
#inspect data
head(data)
#remove the first row (column descriptors) and the last two columns (contest descriptors)
data<-data[-c(1),-c(3,4)]
#the resulting dataset should have only 2 columns.

#generate the observed adjacency matrix
graph<-graph.data.frame(data,directed = T)
adjacency.matrix<-get.adjacency(graph,sparse=F)
#check that this is a 14x14 matrix for Green & Patek 2017
#if using your own data, the matrix should be n x n for an ethogram with n possible behaviours
adjacency.matrix 

#permutation test
#define the number of permutations and create an empty list array to be filled
m<-10000
results<-as.list(NA)
#the loop below resamples the second column of the dataset w/o replacement to permute it.
#each iteration is saved as a matix in a list object (results[[i]])
for (i in 1:m){
  perm<-data
  perm[,2]<-perm[sample(1:nrow(perm),replace=FALSE),2]
  perm.graph<-graph.data.frame(perm,directed = T)
  perm.adjacency.matrix<-get.adjacency(perm.graph,sparse=F)
  results[[i]]<-perm.adjacency.matrix
}
#inspect one sample from results; it should look like a 14x14 adjacency matrix
results[[3456]]

#reorganize list structure of results 
listVec <- lapply(results, c, recursive=TRUE)
x <- do.call(cbind, listVec)
#for Green & Patek 2017, the structure of "x" should be 196 rows each with 10,000 columns.
#each row in x is organized moving down the adjacency matrices, 
#e.g. row 15 in x has the permutation values for row 1, col 2 (x-->t transtition) in adjacency matrix

#if you want, plot a histogram of a row of x of interest
#this is row 15: the x-->t transition 
hist(x[15,])
#the observed RM dataset has 44 x-->t transitions.
abline(v=44)
#If the observed transition occurred more frequently than expected, the observed value should be toward the edge of the permuted distribution.
#can do the same for the BLM dataset.

#calculate 5% and 95% quantile for permuted data
#Green & Patek 2017 uses only the 95% quantile, but the 5% may be useful if interested in which behaviours occurred less frequently than expected.
quantiles<-matrix(NA,nrow=nrow(x),ncol=2)
for (i in 1:nrow(x)){
  q<-quantile(x[i,],probs=c(0.05,0.95))
  quantiles[i,]<-q
}

#check structure of quantiles
head(quantiles)
str(quantiles)
#each row should show the low (column 1) and high (column 2) quantile for a transition (196 total rows)

#save matrices for each of the low and high quantile data
low<-matrix(quantiles[,1],nrow=14,byrow=F)
high<-matrix(quantiles[,2],nrow=14,byrow=F)

#rename the low and high quantile datasets to match the observed adjacency matrix
colnames(low)<-colnames(adjacency.matrix)
rownames(low)<-colnames(adjacency.matrix)
colnames(high)<-colnames(adjacency.matrix)
rownames(high)<-colnames(adjacency.matrix)

#inspect low, high, and observed matrices
low
high
adjacency.matrix

#save the low and high quantile datasets
write.csv(low,file='low_quantiles.csv')
write.csv(high,file='high_quantiles.csv')
#save the observed adjacency matrix
write.csv(adjacency.matrix,file='observed.csv')

##########################################################################
#### calculating transitional probabilities from the observed dataset ####
##########################################################################
#pull out the transitional probability values from the observed matrix
#divide each cell by the row sum
trans.prob<-round(adjacency.matrix/rowSums(adjacency.matrix),2) 
trans.prob[is.nan(trans.prob)]<-0 #replace any NaN values with 0
trans.prob
#simplify the dataset to keep only those cell values where adjacency matrix is greater or equal to high quantile
#keep the original transitional probabilities, i.e. before simplification
keep<-ifelse(adjacency.matrix>=high,trans.prob,0)
#inspect the "keep" matrix
keep
#save the transitonal probability dataset
write.csv(trans.prob,file='transitional_probability.csv')

##################################################
#### visualising the transitions using igraph ####
##################################################
#create a network object from the significant transitional probability matrix (transitions more frequent than expected)
network<-graph.adjacency(keep,weighted=TRUE,diag=TRUE)
#set vertex (circle) sizes to be equal to scaled degree of adjacency matrix
vertex_sizes<-degree(adjacency.matrix,rescale=TRUE)
#set edge (arrow) weights to be equal to 5 categories of edge weight (# of transitions)
edge_weights<-as.numeric(cut(E(network)$weight,c(0,.1,.25,.5,.75,1),labels=c(1:5)))
#plot the network - will open another window
#tkplot may not work with a Mac, but there may be other options available. 
plot<-tkplot(network)
#move the vertices around to make an organized figure
#save the coordinates from this plot
#you can also load coordinates for each vertex as a matrix with n rows (one for each vertex) and 2 columns with x (col 1) and y (col 2) position
coords<-tkplot.getcoords(plot)
#finally, create a network plot using the above coordinates and weights.
plot.igraph(network,vertex.label=V(network)$name, vertex.label.cex = 1.25, layout=coords,vertex.size=vertex_sizes*150, edge.width=edge_weights/4, edge.arrow.size=edge_weights/4)
#note that some factors are multiplied or divided (e.g., vertex_sizes*150) to make them easier to see.