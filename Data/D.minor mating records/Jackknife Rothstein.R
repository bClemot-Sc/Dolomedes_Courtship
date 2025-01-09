### Packages:
library(tidyverse)
library(Hmisc)
library(car)

## Importation of an example:
Exa <- read.csv("Jackknife test.csv",header=T,sep=";")
data.test <- matrix(nrow = 38*2,ncol=2)
data.test[,1] <- c(rep("before",38),rep("after",38))
data.test[,2] <- c(Exa[,2],Exa[,3])
data.test <- as.data.frame(data.test)
ggplot(data.test) +
  geom_boxplot(aes(x=V1,y=as.numeric(V2)))
var(Exa[,2])
var(Exa[,3])

## Jackknife code:
# Calculate var of Y
varY <- 0
for (i in 1:nrow(Exa)) {
  varY <- varY + (Exa[i,3] - mean(Exa[,3]))^2
}
# Caclculate var of X
varX <- 0
for (i in 1:nrow(Exa)) {
  varX <- varX + (Exa[i,2] - mean(Exa[,2]))^2
}
# Calculate y
y <- varY / varX
# Calculate all y-i
Exa$yi <- NA
for (i in 1:nrow(Exa)) {
  varXi <- 0
  varYi <- 0
  Exai <- Exa[-i,]
  for (j in 1:(nrow(Exa)-1)) {
    varXi <- varXi + (Exai[j,2] - mean(Exai[,2]))^2
    varYi <- varYi + (Exai[j,3] - mean(Exai[,3]))^2
  }
  Exa[i,4] <- varYi / varXi
}
# Logarithmic transformation
Exa$Li <- nrow(Exa) * log(y) - (nrow(Exa)-1) * log(Exa$yi)
# Calculate mean of the distribution
meanL <- mean(Exa$Li)
# Calculate variance of distribution
vLi <- 0
for (i in 1:nrow(Exa)) {
  vLi <- vLi + (Exa[i,5] - mean(Exa[,5]))^2
}
VarLi <- vLi / ( nrow(Exa) * (nrow(Exa)-1))
# Calculate 
Q <- meanL / sqrt(VarLi)

## Application of the Jackknife to my data:
subset1 <- read.table("subset1_Bastien_original_weight.csv",sep=",",header=T)
subset2 <- read.table("subset2_Bastien_original_weight.csv",sep=",",header=T)
Exa <- subset1[,c("Female","number.of.courtships")]
colnames(Exa)[2] <- "before"
Exa <- left_join(Exa,subset2[,c("Female","number.of.courtships")])
colnames(Exa)[3] <- "after"
# Calculate var of Y
varY <- 0
for (i in 1:nrow(Exa)) {
  varY <- varY + (Exa[i,3] - mean(Exa[,3]))^2
}
# Caclculate var of X
varX <- 0
for (i in 1:nrow(Exa)) {
  varX <- varX + (Exa[i,2] - mean(Exa[,2]))^2
}
# Calculate y
y <- varY / varX
# Calculate all y-i
Exa$yi <- NA
for (i in 1:nrow(Exa)) {
  varXi <- 0
  varYi <- 0
  Exai <- Exa[-i,]
  for (j in 1:(nrow(Exa)-1)) {
    varXi <- varXi + (Exai[j,2] - mean(Exai[,2]))^2
    varYi <- varYi + (Exai[j,3] - mean(Exai[,3]))^2
  }
  Exa[i,4] <- varYi / varXi
}
# Logarithmic transformation
Exa$Li <- nrow(Exa) * log(y) - (nrow(Exa)-1) * log(Exa$yi)
# Calculate mean of the distribution
meanL <- mean(Exa$Li)
# Calculate variance of distribution
vLi <- 0
for (i in 1:nrow(Exa)) {
  vLi <- vLi + (Exa[i,5] - mean(Exa[,5]))^2
}
VarLi <- vLi / ( nrow(Exa) * (nrow(Exa)-1))
# Calculate 
Q <- meanL / sqrt(VarLi)

## Test de le faire à l'envers pour avoir comme dans l'exemple:
# Calculate var of Y
varY <- 0
for (i in 1:nrow(Exa)) {
  varY <- varY + (Exa[i,2] - mean(Exa[,2]))^2
}
# Caclculate var of X
varX <- 0
for (i in 1:nrow(Exa)) {
  varX <- varX + (Exa[i,3] - mean(Exa[,3]))^2
}
# Calculate y
y <- varY / varX
# Calculate all y-i
Exa$yi <- NA
for (i in 1:nrow(Exa)) {
  varXi <- 0
  varYi <- 0
  Exai <- Exa[-i,]
  for (j in 1:(nrow(Exa)-1)) {
    varXi <- varXi + (Exai[j,3] - mean(Exai[,3]))^2
    varYi <- varYi + (Exai[j,2] - mean(Exai[,2]))^2
  }
  Exa[i,4] <- varYi / varXi
}
# Logarithmic transformation
Exa$Li <- nrow(Exa) * log(y) - (nrow(Exa)-1) * log(Exa$yi)
# Calculate mean of the distribution
meanL <- mean(Exa$Li)
# Calculate variance of distribution
vLi <- 0
for (i in 1:nrow(Exa)) {
  vLi <- vLi + (Exa[i,5] - mean(Exa[,5]))^2
}
VarLi <- vLi / ( nrow(Exa) * (nrow(Exa)-1))
# Calculate 
Q <- meanL / sqrt(VarLi)


# Test similaire avec total.courtship.duration:
Exa <- subset1[,c("Female","total.courtship.duration")]
colnames(Exa)[2] <- "before"
Exa <- left_join(Exa,subset2[,c("Female","total.courtship.duration")])
colnames(Exa)[3] <- "after"
# Encore à l'envers c'est plus simple:
# Calculate var of Y
varY <- 0
for (i in 1:nrow(Exa)) {
  varY <- varY + (Exa[i,2] - mean(Exa[,2]))^2
}
# Caclculate var of X
varX <- 0
for (i in 1:nrow(Exa)) {
  varX <- varX + (Exa[i,3] - mean(Exa[,3]))^2
}
# Calculate y
y <- varY / varX
# Calculate all y-i
Exa$yi <- NA
for (i in 1:nrow(Exa)) {
  varXi <- 0
  varYi <- 0
  Exai <- Exa[-i,]
  for (j in 1:(nrow(Exa)-1)) {
    varXi <- varXi + (Exai[j,3] - mean(Exai[,3]))^2
    varYi <- varYi + (Exai[j,2] - mean(Exai[,2]))^2
  }
  Exa[i,4] <- varYi / varXi
}
# Logarithmic transformation
Exa$Li <- nrow(Exa) * log(y) - (nrow(Exa)-1) * log(Exa$yi)
# Calculate mean of the distribution
meanL <- mean(Exa$Li)
# Calculate variance of distribution
vLi <- 0
for (i in 1:nrow(Exa)) {
  vLi <- vLi + (Exa[i,5] - mean(Exa[,5]))^2
}
VarLi <- vLi / ( nrow(Exa) * (nrow(Exa)-1))
# Calculate 
Q <- meanL / sqrt(VarLi)


# Same for mean.courthsip.length
# Test similaire avec total.courtship.duration:
Exa <- subset1[,c("Female","mean.courtship.length")]
colnames(Exa)[2] <- "before"
Exa <- left_join(Exa,subset2[,c("Female","mean.courtship.length")])
colnames(Exa)[3] <- "after"
# Encore à l'envers c'est plus simple:
# Calculate var of Y
varY <- 0
for (i in 1:nrow(Exa)) {
  varY <- varY + (Exa[i,2] - mean(Exa[,2]))^2
}
# Caclculate var of X
varX <- 0
for (i in 1:nrow(Exa)) {
  varX <- varX + (Exa[i,3] - mean(Exa[,3]))^2
}
# Calculate y
y <- varY / varX
# Calculate all y-i
Exa$yi <- NA
for (i in 1:nrow(Exa)) {
  varXi <- 0
  varYi <- 0
  Exai <- Exa[-i,]
  for (j in 1:(nrow(Exa)-1)) {
    varXi <- varXi + (Exai[j,3] - mean(Exai[,3]))^2
    varYi <- varYi + (Exai[j,2] - mean(Exai[,2]))^2
  }
  Exa[i,4] <- varYi / varXi
}
# Logarithmic transformation
Exa$Li <- nrow(Exa) * log(y) - (nrow(Exa)-1) * log(Exa$yi)
# Calculate mean of the distribution
meanL <- mean(Exa$Li)
# Calculate variance of distribution
vLi <- 0
for (i in 1:nrow(Exa)) {
  vLi <- vLi + (Exa[i,5] - mean(Exa[,5]))^2
}
VarLi <- vLi / ( nrow(Exa) * (nrow(Exa)-1))
# Calculate 
Q <- meanL / sqrt(VarLi)

