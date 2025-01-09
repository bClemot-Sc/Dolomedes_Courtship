## Open Simon's dataset :

data <- read.table("data_mating_weight.csv",
             sep = ",",
             header = T)
data
summary(data)

library(Hmisc)
Hmisc::describe(data[, c(
  "Species.cross",
  "Backed.up.on.University.server",
  "number.of.courtships",
  "copulation",
  "malematedbefore",
  "femalematedbefore"
)])

## First subset (virgin females that have copulated)

# Filter Backed up, copulation & femalematedbefore
library(tidyverse)
subset1 <-
  data %>% filter(Backed.up.on.University.server == TRUE,
                  copulation == TRUE,
                  femalematedbefore == FALSE)

# Only keep experiments with focal females
subset1b <- subset1[(str_split(subset1$experiment, " ", simplify = TRUE))[, 3] == "Focals", ]

subset1b
summary(subset1b)
Hmisc::describe(subset1b)

## Second subset (same females, their second mating, have courted)

# Filter Backed up, number.of.courtships, femalematedbefore
subset2 <- data %>% filter(
  Backed.up.on.University.server == TRUE,
  number.of.courtships > 0,
  femalematedbefore == TRUE
)

# Filter by only keeping females that were in the first subset
subset1.fem <- (str_split(subset1b$experiment, " ", simplify = TRUE))[, 4]
subset1.fem

subset2b <- subset2[(str_split(subset2$experiment, " ", simplify = TRUE))[, 4] %in% subset1.fem, ]

# When females had more than 2 mating trials (more than one line with the same female in subset2b)
# then randomly keep one
subset2c <- subset2b %>% group_by(Female) %>% sample_n(1)

subset2c
summary(subset2c)
Hmisc::describe(subset2c)

write.csv(subset1b, "C:\\Users\\bclem\\OneDrive\\Documents\\Formation\\Stage\\WAIKATO\\Data\\D.minor mating records\\subset1_Bastien_V2.csv", row.names=FALSE)
write.csv(subset2c, "C:\\Users\\bclem\\OneDrive\\Documents\\Formation\\Stage\\WAIKATO\\Data\\D.minor mating records\\subset2_Bastien_V2.csv", row.names=FALSE)

## Randomly select 10 mating trials

# Keep 10 randomly selected females in the first subset
subset1b_only10 <- subset1b[sample(nrow(subset1b), 10), ]
# Keep the 10 same females in the second subset
subset1.only10fem <- subset1b_only10$Female
subset2c_only10 <- subset2c[subset2c$Female %in% subset1.only10fem, ]

# Check if everything's good :
Hmisc::describe(subset1b_only10[, c(
  "Species.cross",
  "Backed.up.on.University.server",
  "number.of.courtships",
  "copulation",
  "malematedbefore",
  "femalematedbefore"
)])

Hmisc::describe(subset2c_only10[, c(
  "Species.cross",
  "Backed.up.on.University.server",
  "number.of.courtships",
  "copulation",
  "malematedbefore",
  "femalematedbefore"
)])

sort(subset1b_only10$Female)
sort(subset2c_only10$Female)
