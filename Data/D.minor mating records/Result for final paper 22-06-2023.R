## Packages and data
# Packages:
library(tidyverse)
library(Hmisc)
library(car)
library(lme4)
library(performance)
library(AICcmodavg)
library(gridExtra)
library(plotrix)


# Datasets:
subset1 <- read.table("subset1_Bastien_original_weight.csv",sep=",",header=T)
subset2 <- read.table("subset2_Bastien_original_weight.csv",sep=",",header=T)
data <- rbind(subset1,subset2)

## Total courtship duration ~ Female mating status
# Test
A <- subset1[,c("Female","total.courtship.duration")]
B <- subset2[,c("Female","total.courtship.duration")]
colnames(B) <- c("Female","total.courtship.duration2")
testC <- left_join(A,B)
wilcox.test(C$total.courtship.duration,C$total.courtship.duration2,paired=TRUE)
# Graphical representation
ggplot(data, aes(x=femalematedbefore, y=total.courtship.duration, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("") +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Total courtship duration (s)") +
  theme_light(base_size = 16) +
  theme(legend.position="none")

## Latency to mount ~ Female mating status
# Test
A <- subset1[,c("Female","latency.to.mount")]
B <- subset2[,c("Female","latency.to.mount")]
colnames(B) <- c("Female","latency.to.mount2")
C <- left_join(A,B)
wilcox.test(C$latency.to.mount,C$latency.to.mount2,paired=TRUE)
# Graphical representation
ggplot(data, aes(x=femalematedbefore, y=latency.to.mount, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("") +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Latency to mount (s)") +
  theme_light(base_size = 16) +
  theme(legend.position="none")

## Number of attacks ~ Female mating status
# Test
A <- subset1[,c("Female","number.of.attacks")]
B <- subset2[,c("Female","number.of.attacks")]
colnames(B) <- c("Female","number.of.attacks2")
C <- left_join(A,B)
C <- C[-6,]
wilcox.test(C$number.of.attacks,C$number.of.attacks2,paired=TRUE)
# Graphical representation
ggplot(data, aes(x=femalematedbefore, y=number.of.attacks, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("") +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Number of female attacks") +
  theme_light(base_size = 16) +
  theme(legend.position="none")

## Total mount duration ~ Female mating status
# Test
A  <- subset1[,c("Female","total.mount.duration")]
B <- subset2[,c("Female","total.mount.duration")]
colnames(B) <- c("Female","total.mount.duration2")
C <- left_join(A,B)
C <- C[-6,]
wilcox.test(C$total.mount.duration,C$total.mount.duration2,paired=TRUE)
# Graphical representation
ggplot(data, aes(x=femalematedbefore, y=total.mount.duration, fill=femalematedbefore)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("") +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Total mount duration (s)") +
  theme_light(base_size = 16) +
  theme(legend.position="none")
