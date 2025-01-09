##### Information #####
# There's two different versions.

### The first one will just take the selected columns of the weight data frame, 
# which here are only the different dates, but it can easily be changed,
# and it will merge them with the big data frame.

### The second one is a bit weirder 
# it will only keep the last known weight of the spider and its date,
# (as weights were made before or just after mating trials, I considered the
# most recent weight as the best approximation possible of the weight during the trial)
# and it will merge them with the big data frame too.

### PLEASE run the two different versions separately.


##### Importation of datasets #####
# The most complete dataset:
data <- read.table("Minor mating trials Merged V2.csv", sep = ",", header = T)
# The dataset with the data that need to be added:
weight <- read.table("2020 Lab Season 28-08-21 _ rene_ up2date.csv", sep = ",", header = T, quote = "\"")


##### Packages #####
library(tidyverse)


##### V1: Merging of all weights #####
# Exportation of all females and males ID separately:
# Will be used to locate lines in the second data frame
femID <- unique(data$Female)
malID <- unique(data$Male)
# Get the rows for each sex with the ID and the parameters you want to keep:
# Here you can just change the n° of the columns to keep whatever you want
weightFem <- weight[, c(1, 11:29)] %>% filter(Spider.No. %in% femID)
weightMal <- weight[, c(1, 11:29)] %>% filter(Spider.No. %in% malID)
# Just changing the column names to:
# - Have the same name for the ID column between the two data frames
colnames(weightFem)[1] <- "Female"
colnames(weightMal)[1] <- "Male"
# - Distinguish weight column names between both sexes and make it more readable
# The substr() part is only for the aesthetic, you might need to remove or change it
colnames(weightFem)[-1] <- paste0("Female.Weight.",substr(colnames(weightFem)[-1],9,16),".g") #Not really important
colnames(weightMal)[-1] <- paste0("Male.Weight.",substr(colnames(weightMal)[-1],9,16),".g") #Not really important
# Merging the data frames by matching the spiders ID:
data <- left_join(data, weightFem)
data <- left_join(data, weightMal)
# I left the weight data at the end of the the data frame, but of course they can be moved


##### V2: Keeping only the most recent weight before the mating #####
# Keeping only dates in weight column names:
colnames(weight)[11:29] <- substr(colnames(weight)[11:29], 9, 16)
# Making a data frame with only ID, dates and weights:
weight <- weight[, c(1, 11:29)]
weight <- gather(weight, "Date", "Weight.g", 2:20)
# Remove all lines with NAs or blanks:
weight <- weight[!(weight$Weight.g == "" | is.na(weight$Weight.g)), ]
# Changing notation of dates to easily sort them (YYYY-MM-DD):
weight$Date <- paste0("20",substr(weight$Date,7,8),"-",substr(weight$Date,4,5),"-",substr(weight$Date,1,2))
# Keeping the most recent weight measurement 
weight <- weight %>% group_by(Spider.No.) %>% top_n(1, Date)
# Exportation of all females and males ID separately:
# Will be used to locate lines in the weight data frame
femID <- unique(data$Female)
malID <- unique(data$Male)
# Get one data frame for each sex:
weightFem <- weight %>% filter(Spider.No. %in% femID)
weightMal <- weight %>% filter(Spider.No. %in%  malID)
# Just changing the column names to:
# - Have the same name for the ID column between the two data frames
colnames(weightFem)[1] <- "Female"
colnames(weightMal)[1] <- "Male"
# - Distinguish weight column names between both sexes and make it more readable
colnames(weightFem)[2:3] <- c("Female.date.last.weight","Female.last.weight")
colnames(weightMal)[2:3] <- c("Male.date.last.weight","Male.last.weight")
# Merging the data frames by matching the spiders ID:
data <- left_join(data, weightFem)
data <- left_join(data, weightMal)
# I left the weight data at the end of the the data frame, but of course they can be moved


##### Exportation of the data frame as a .csv #####
write.csv(data, "C:\\Users\\bclem\\OneDrive\\Documents\\Formation\\Stage\\WAIKATO\\Data\\D.minor mating records\\data_mating_weight.csv", row.names=FALSE)
