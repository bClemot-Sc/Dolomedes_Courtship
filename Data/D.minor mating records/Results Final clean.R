#### Packages and data ####

# Packages:
library(tidyverse)
library(Hmisc)
library(car)
library(lme4)
library(performance)
library(AICcmodavg)
library(gridExtra)

# Jackknife function:
# Function for the jackknife:
jackknife <- function(dataset) {
  # Calculate variance of variable X
  varX <- 0
  for (i in 1:nrow(dataset)) {
    varX <- varX + (dataset[i, 3] - mean(dataset[, 3])) ^ 2
  }
  # Calculate variance of variable Y
  varY <- 0
  for (i in 1:nrow(dataset)) {
    varY <- varY + (dataset[i, 2] - mean(dataset[, 2])) ^ 2
  }
  # Calculate y, ratio of variances
  y <- varY / varX
  # Calculate all y-i
  dataset$yi <- NA
  for (i in 1:nrow(dataset)) {
    varXi <- 0
    varYi <- 0
    dataseti <- dataset[-i, ]
    for (j in 1:(nrow(dataset) - 1)) {
      varXi <- varXi + (dataseti[j, 3] - mean(dataseti[, 3])) ^ 2
      varYi <- varYi + (dataseti[j, 2] - mean(dataseti[, 2])) ^ 2
    }
    dataset[i, 4] <- varYi / varXi
  }
  # Logarithmic transformation of all y-i
  dataset$Li <- nrow(dataset) * log(y) - (nrow(dataset) - 1) * log(dataset$yi)
  # Calculate mean of the distribution (mean of all y-i)
  meanL <- mean(dataset$Li)
  # Calculate variance of distribution (variance of all y-1)
  vLi <- 0
  for (i in 1:nrow(dataset)) {
    vLi <- vLi + (dataset[i, 5] - mean(dataset[, 5])) ^ 2
  }
  VarLi <- vLi / (nrow(dataset) * (nrow(dataset) - 1))
  # Calculate Q the statistical value
  Q <- meanL / sqrt(VarLi)
  return(Q)
}

# Datasets:
subset1 <- read.table("subset1_Bastien_original_weight.csv",sep=",",header=T)
subset2 <- read.table("subset2_Bastien_original_weight.csv",sep=",",header=T)
data <- rbind(subset1,subset2)

#### Influence of mating status on courtship duration ####

## Total courtship duration ~ mating status
# Wilcoxon paired test:
wilcox.test(subset1$total.courtship.duration,subset2$total.courtship.duration,paired=TRUE)
# Jackknife variance test:
dataset <- subset1[,c("Female","total.courtship.duration")]
colnames(dataset)[2] <- "virgin"
dataset <- left_join(dataset,subset2[,c("Female","total.courtship.duration")])
colnames(dataset)[3] <- "mated"
jackknife(dataset)

## Number of courtship segments ~ mating status
# Wilcoxon paired test:
wilcox.test(subset1$number.of.courtships,subset2$number.of.courtships,paired=TRUE)
# Jackknife variance test:
dataset <- subset1[,c("Female","number.of.courtships")]
colnames(dataset)[2] <- "virgin"
dataset <- left_join(dataset,subset2[,c("Female","number.of.courtships")])
colnames(dataset)[3] <- "mated"
jackknife(dataset)

## Mean duration of courtship segments ~ mating status
# Wilcoxon paired test:
wilcox.test(subset1$mean.courtship.length,subset2$mean.courtship.length,paired=TRUE)
# Jackknife variance test:
dataset <- subset1[,c("Female","mean.courtship.length")]
colnames(dataset)[2] <- "virgin"
dataset <- left_join(dataset,subset2[,c("Female","mean.courtship.length")])
colnames(dataset)[3] <- "mated"
jackknife(dataset)

## Graphical representation:
plot1 <- ggplot(data, aes(x=femalematedbefore, y=total.courtship.duration, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("") +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Total courtship duration (s)") +
  theme_light(base_size = 16) +
  theme(legend.position="none")
plot1
plot2 <- ggplot(data, aes(x=femalematedbefore, y=number.of.courtships, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("Female mating status") +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Number of courtship attempts") +
  theme_light(base_size = 16) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0,10),breaks = c(0,2,4,6,8,10))
plot3 <- ggplot(data, aes(x=femalematedbefore, y=mean.courtship.length, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("") +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Mean duration of courtship attempts (s)") +
  theme_light(base_size = 16) +
  theme(legend.position="none")
grid.arrange(plot1,plot2,plot3,ncol=3)

## Total courtship duration ~ Mating status + Number of attacks 
mod.tot <- lmer(total.courtship.duration ~ femalematedbefore*number.of.attacks + (1|Female),data=data)
summary(mod.tot)
Anova(mod.tot,type='II')

## Number of courtship segments ~ Mating status + Number of attacks
mod.nb <- glmer(number.of.courtships ~ number.of.attacks + (1|Female),family = poisson,data=data)
summary(mod.nb)
Anova(mod.nb,type='II')

## Mean duration of courtship segments ~ Mating status + Number of attacks
mod.mean <- lmer(mean.courtship.length ~ femalematedbefore*number.of.attacks + (1|Female),data = data)
summary(mod.mean)
Anova(mod.mean,type='II')

#### Influence of courtship on mating outcomes ####

## Latency to mount ~ courtship 
mod.mount <- lmer(latency.to.mount ~ scale(total.courtship.duration)*scale(number.of.courtships) + scale(total.courtship.duration)*scale(mean.courtship.length)  + (1|Female),data=data)
summary(mod.mount)
Anova(mod.mount,type='II')

## Latency to attack ~ courtship 
mod.attk <- lmer(latency.to.attack ~ scale(total.courtship.duration)*scale(number.of.courtships) + scale(total.courtship.duration)*scale(mean.courtship.length) + (1|Female), data=data)
summary(mod.attk)
Anova(mod.attk,type='II')

## Latency to copulate ~ courtship
mod.cop <- lmer(latency.to.copulate ~ scale(total.courtship.duration)*scale(number.of.courtships) + scale(total.courtship.duration)*scale(mean.courtship.length) + (1|Female), data=data)
summary(mod.cop)
Anova(mod.cop,type='II')

## Latency to mount
