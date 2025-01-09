## To read all of my datas quickly

collection <- read.table("2020 Lab Season 28-08-21 _ rene_ up2date.csv",sep=",",header=T,quote = "\"")
complete_data <- read.table("data_mating_weight.csv",sep=",",header=T)
subset1 <- read.table("subset1_Bastien_original.csv",sep=",",header=T)
subset2 <- read.table("subset2_Bastien_original.csv",sep=",",header=T)
data2 <- rbind(subset1,subset2)

## Information of the spiders of my subsets:
# For the females
fem.list <- subset1$Female
subfem.collect <- collection %>% filter(Spider.No. %in% fem.list)
# For the males
mal.list <- unique(c(unique(subset1$Male),unique(subset2$Male)))
submal.collect <- collection %>% filter(Spider.No. %in% mal.list)

length(unique(c(subset2$Male,subset1$Male)))
sort(subset1$Male)
length(unique(subset2$Male))

library(tidyverse)

blabla <- collection %>% filter(Species == "Dolomedes minor")
unique(blabla$Site)

# Information of pedipalp used:

table(subset1$copulation.pedipalps.used)
