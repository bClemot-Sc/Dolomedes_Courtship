### Impact of Female Mating Status on Male Courtship Behaviour in the Sexually 
### Cannibalistic New Zealand Fishing Spider Dolomedes minor (Araneae, Pisauridae)
### Bastien Clémot, 2023
### Global analysis

#### Packages and data ####

### Packages
library(readxl)
library(tidyverse)
library(gridExtra)
library(plotrix)

### Data
data <- read_excel("Final dataframe.xlsx")
data$latency.to.mount <- as.numeric(data$latency.to.mount)
data$first.courtship.duration <- as.numeric(data$first.courtship.duration)
data$first.mounting.duration <- as.numeric(data$first.mounting.duration)

#### Global analysis ####

### First courtship duration ~ Female mating status
## Normality test
before <- data %>% filter(female.mated.before==FALSE)
shapiro.test(before$first.courtship.duration)
after <- data %>% filter(female.mated.before==TRUE)
shapiro.test(after$first.courtship.duration)
## Statistical test 
wilcox.test(before$first.courtship.duration,after$first.courtship.duration,paired=TRUE)
## Graphical representation
plot1 <- ggplot(data, aes(x=female.mated.before, y=first.courtship.duration, fill=female.mated.before)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Courtship duration (s)") +
  xlab("") +
  theme_light(base_size = 20) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0,5000),breaks = c(0,1000,2000,3000,4000,5000))
plot1

### First mounting duration ~ Female mating status
## Normality test
shapiro.test(before$first.mounting.duration)
shapiro.test(after$first.mounting.duration)
## Statistical test 
wilcox.test(before$first.mounting.duration,after$first.mounting.duration,paired=TRUE)
## Graphical representation
plot2 <- ggplot(data, aes(x=female.mated.before, y=first.mounting.duration, fill=female.mated.before)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  xlab("") +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Mounting duration (s)") +
  xlab("Female mating status") +
  theme_light(base_size = 20) +
  theme(legend.position="none")
plot2

### Latency to mount ~ Female mating status
## Normality test
shapiro.test(before$latency.to.mount)
shapiro.test(after$latency.to.mount)
## Statistical test 
wilcox.test(before$latency.to.mount,after$latency.to.mount,paired=TRUE)
## Graphical representation
plot3 <- ggplot(data, aes(x=female.mated.before, y=latency.to.mount, fill=female.mated.before)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Latency to mount (s)") +
  xlab("") +
  theme_light(base_size = 20) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(0,12000),breaks = c(0,3000,6000,9000,12000))
plot3



### Fuse all three results
grid.arrange(plot1,plot2,plot3,ncol=3)

#### Female aggressiveness ####
## Normality test
shapiro.test(before$attacks.per.hour)
shapiro.test(after$attacks.per.hour)
## Statistical test
wilcox.test(before$attacks.per.hour,after$attacks.per.hour,paires=TRUE)
## Graphical representation
ggplot(data, aes(x=female.mated.before, y=attacks.per.hour, fill=female.mated.before)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  scale_fill_manual(values=c("#F2F2F2","gray75")) +
  scale_x_discrete(labels = c("Unmated","Mated")) +
  ylab("Number of attacks per hour") +
  xlab("Female mating status") +
  theme_light(base_size = 20) +
  theme(legend.position="none")

#### Significance check up ####

means <- c(mean(before$first.courtship.duration,na.rm=TRUE),mean(after$first.courtship.duration,na.rm=TRUE))
st.errors <- c(std.error(before$first.courtship.duration),std.error(after$first.courtship.duration))
df.tot <- data.frame(group = c('unmated','mated'),mean = means, se = st.errors)
plotA <- ggplot(df.tot) +
  geom_bar(aes(x=group, y=mean),stat="identity") +
  scale_x_discrete(limits = c('unmated','mated')) +
  geom_errorbar( aes(x=group, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3)
plotA

means <- c(mean(before$first.mounting.duration,na.rm=TRUE),mean(after$first.mounting.duration,na.rm=TRUE))
st.errors <- c(std.error(before$first.mounting.duration),std.error(after$first.mounting.duration))
df.tot <- data.frame(group = c('unmated','mated'),mean = means, se = st.errors)
plotB <- ggplot(df.tot) +
  geom_bar(aes(x=group, y=mean),stat="identity") +
  scale_x_discrete(limits = c('unmated','mated')) +
  geom_errorbar( aes(x=group, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3)
plotB

means <- c(mean(before$latency.to.mount,na.rm=TRUE),mean(after$latency.to.mount,na.rm=TRUE))
st.errors <- c(std.error(before$latency.to.mount),std.error(after$latency.to.mount))
df.tot <- data.frame(group = c('unmated','mated'),mean = means, se = st.errors)
plotC <- ggplot(df.tot) +
  geom_bar(aes(x=group, y=mean),stat="identity") +
  scale_x_discrete(limits = c('unmated','mated')) +
  geom_errorbar( aes(x=group, ymin=mean-se, ymax=mean+se), width=0.4, colour="orange", alpha=0.9, size=1.3)
plotC
