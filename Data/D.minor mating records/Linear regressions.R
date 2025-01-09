### Packages:
library(tidyverse)
library(Hmisc)
library(car)

### Open data:
subset1 <- read.table("subset1_Bastien_original_weight.csv",sep=",",header=T)
subset2 <- read.table("subset2_Bastien_original_weight.csv",sep=",",header=T)

## Bind both data frames together :
data <- rbind(subset1,subset2)

##### First investigation #####
## Total courtship duration:
ggplot(data) +
  geom_boxplot(aes(x=femalematedbefore,y=total.courtship.duration))

shapiro.test(subset1$total.courtship.duration)
shapiro.test(subset2$total.courtship.duration)

var(subset1$total.courtship.duration)
var(subset2$total.courtship.duration)

t.test(subset1$total.courtship.duration,subset2$total.courtship.duration,data= Tableau,var.equal=TRUE,paired = T)

## number of courtships
ggplot(data) +
  geom_violin(aes(x=femalematedbefore,y=number.of.courtships))

shapiro.test(subset1$number.of.courtships) # not normal
shapiro.test(subset2$number.of.courtships)

wilcox.test(subset1$number.of.courtships,subset2$number.of.courtships,paired=TRUE)

var.test(subset1$number.of.courtships,subset2$number.of.courtships)

# See attempt to make a jackknife test for comparing variances

## Mean of courtship:
ggplot(data) +
  geom_boxplot(aes(x=femalematedbefore,y=mean.courtship.length))

shapiro.test(subset1$mean.courtship.length)
shapiro.test(subset2$mean.courtship.length) # not normal

wilcox.test(subset1$mean.courtship.length,subset2$mean.courtship.length,paired=TRUE)

### With all data:

complete_data <- read.table("data_mating_weight.csv",sep=",",header=T)

## Total courtship duration:
ggplot(complete_data) +
  geom_boxplot(aes(x=femalematedbefore,y=total.courtship.duration))

## number of courtships
ggplot(complete_data) +
  geom_boxplot(aes(x=femalematedbefore,y=number.of.courtships))

## Mean of courtship:
ggplot(complete_data) +
  geom_boxplot(aes(x=femalematedbefore,y=mean.courtship.length))

## Mated state and Copulation success :
table(subset1$copulation,subset2$copulation,dnn = list("virgin","mated"))
Copulation <- matrix(c(0, 11, 0, 4),nrow = 2,dimnames = list("1st Survey" = c("False", "True"),"2nd Survey" = c("False", "True")))
Copulation
mcnemar.test(Copulation)

## Mated state and attempted.copulation :
Attempted.copulation <- table(subset1$attempted.copulation,subset2$attempted.copulation,dnn = list("virgin","mated"))
mcnemar.test(Attempted.copulation)

## Mated state and post.copulatory.kill :
table(subset1$post.copulatory.kill,subset2$post.copulatory.kill,dnn = list("virgin","mated"))
Post.copulatory.kill <- matrix(c(12, 3, 0, 0),nrow = 2,dimnames = list("1st Survey" = c("False", "True"),"2nd Survey" = c("False", "True")))
Post.copulatory.kill
mcnemar.test(Post.copulatory.kill)

## Mated state and pre.copulatory.kill :
table(subset1$pre.copulatory.kill,subset2$pre.copulatory.kill,dnn = list("virgin","mated"))
Pre.copulatory.kill <- matrix(c(14, 0, 1, 0),nrow = 2,dimnames = list("1st Survey" = c("False", "True"),"2nd Survey" = c("False", "True")))
Pre.copulatory.kill
mcnemar.test(Pre.copulatory.kill)

## Cannibalism
Cannibalism <- table(subset1$cannibalism,subset2$cannibalism,dnn = list("virgin","mated"))
Cannibalism
mcnemar.test(Cannibalism)

## Palp.broken
Palp.broken <- table(subset1$Palp.Broken,subset2$Palp.Broken,dnn = list("virgin","mated"))
Palp.broken
mcnemar.test(Palp.broken)

##### Some linear regressions #####

### Question around risk of being cannibalized:

## Number of attacks
lm3 <- lm(number.of.attacks ~ (number.of.courtships + mean.courtship.length + Female.last.weight + Male.last.weight) * femal, data = subset1)
Anova(lm3,type='II')
vif(lm3)

lm4 <- lm(number.of.attacks ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset2)
Anova(lm4,type='II')

## Latency to attack
lm5 <- lm(latency.to.attack ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset1)
Anova(lm5,type = 'II')

lm6 <- lm(latency.to.attack ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset2)
Anova(lm6,type='II')

## Number of flees
lm6a <- lm(number.of.flees ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset1)
Anova(lm6a,type='II')

lm6b <- lm(number.of.flees ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset2)
Anova(lm6b,type='II')

### Question around male implication:

## Number of mounts
lm7 <- lm(number.of.mounts ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset1)
Anova(lm7, type='II')

lm8 <- lm(number.of.mounts ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset2)
Anova(lm8,type="II")

# Total mount duration
lm9 <- lm(total.mount.duration ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset1)
Anova(lm9, type='II')

lm10 <- lm(total.mount.duration ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset2)
Anova(lm10,type="II")

# Mean mount duration
lm11 <- lm(mean.mount.length ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset1)
Anova(lm11, type='II')

lm12 <- lm(mean.mount.length ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset2)
Anova(lm12,type="II")

# Latency to mount 
lm13 <- lm(latency.to.mount ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset1)
Anova(lm13, type='II')

lm14 <- lm(latency.to.mount ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset2)
Anova(lm14,type="II")

# Latency to touch
lm15 <- lm(latency.to.touch ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset1)
Anova(lm15, type='II')

lm16 <- lm(latency.to.touch ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset2)
Anova(lm16,type="II")

# Latency to copulate
lm17 <- lm(latency.to.copulate ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset1)
Anova(lm17, type='II')

lm18 <- lm(latency.to.copulate ~ number.of.courtships + total.courtship.duration + mean.courtship.length + Female.last.weight + Male.last.weight, data = subset2)
Anova(lm18,type="II")

##### Assess if there's a link between the male being killed and copulation failure/palp.broken

table(subset1$Palp.Broken, subset1$pre.copulatory.kill|subset1$post.copulatory.kill)
chisq.test(subset1$Palp.Broken, subset1$pre.copulatory.kill|subset1$post.copulatory.kill)

table(subset2$Palp.Broken, subset2$pre.copulatory.kill|subset2$post.copulatory.kill)
chisq.test(subset2$Palp.Broken, subset2$pre.copulatory.kill|subset2$post.copulatory.kill)

table(subset1$copulation, subset1$pre.copulatory.kill|subset1$post.copulatory.kill)
chisq.test(subset1$copulation, subset1$pre.copulatory.kill|subset1$post.copulatory.kill)

table(subset2$copulation, subset2$pre.copulatory.kill|subset2$post.copulatory.kill)
chisq.test(subset2$copulation, subset2$pre.copulatory.kill|subset2$post.copulatory.kill)
