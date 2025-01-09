##### Results update 30/06/23 #####

# Packages:
library(tidyverse)
library(Hmisc)
library(car)
library(lme4)
library(performance)
library(AICcmodavg)
library(gridExtra)
library(plotrix)

# Dataset
subset1 <- read.table("subset1_Bastien_original_weight.csv",sep=",",header=T)
subset2 <- read.table("subset2_Bastien_original_weight.csv",sep=",",header=T)
data <- rbind(subset1,subset2)

# Part 1: Global courtship parameters

## Data for total courtship duration should only be restrained to the first 
## courtship + mounting
## Look at my notes, I'm fkin lost

# Part 2: Female agressiveness ~ mating status

## Need to work with nb attacks/time ??? Look at what Simon told me about the total
## courtship duration to know if it could be used as a time parameter.