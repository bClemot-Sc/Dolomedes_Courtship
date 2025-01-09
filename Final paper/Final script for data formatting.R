### Impact of Female Mating Status on Male Courtship Behaviour in the Sexually 
### Cannibalistic New Zealand Fishing Spider Dolomedes minor (Araneae, Pisauridae)
### Bastien Clémot, 2023
### Data formatting

#### Packages and data ####
## Packages
library(tidyverse)
## Data, as binary tables of behaviours
g1.G26xJ203 <- as.data.frame(fread("G26 x J203_No focal subject.tsv"))
g1.G30xJ210 <- as.data.frame(fread("G30 x J210_No focal subject.tsv"))
g1.H30xH71 <- as.data.frame(fread("H30 x H71_No focal subject.tsv"))
g1.H45xH65 <- as.data.frame(fread("H45 x H65_No focal subject.tsv"))
g1.H62xJ132 <- as.data.frame(fread("H62 x J132_No focal subject.tsv"))
g1.J101xJ204 <- as.data.frame(fread("J101 x J204_No focal subject.tsv"))
g1.J102xJ206 <- as.data.frame(fread("J102 x J206_No focal subject.tsv"))
g1.J110xH67 <- as.data.frame(fread("J110 x H67_No focal subject.tsv"))
g1.J124xG42 <- as.data.frame(fread("J124 x G42_No focal subject.tsv"))
g1.J144xG39 <- as.data.frame(fread("J144 x G39_No focal subject.tsv"))
g1.J183xH26 <- as.data.frame(fread("J183 x H26_No focal subject.tsv"))
g1.J189xJ112 <- as.data.frame(fread("J189 x J112_No focal subject.tsv"))
g1.J190xJ42 <- as.data.frame(fread("J190 x J42_No focal subject.tsv"))
g1.J191xJ68 <- as.data.frame(fread("J191 x J68_No focal subject.tsv"))
g1.J201xG43 <- as.data.frame(fread("J201 x G43_No focal subject.tsv"))
g2.G30xH52 <- as.data.frame(fread("G30 x H52_No focal subject.tsv"))
g2.H30xG38 <- as.data.frame(fread("H30 x G38_No focal subject.tsv"))
g2.H62xG33 <- as.data.frame(fread("H62 x G33_No focal subject.tsv"))
g2.J101xH76 <- as.data.frame(fread("J101 x H76_No focal subject.tsv"))
g2.J102xJ161 <- as.data.frame(fread("J102 x J161_No focal subject.tsv"))
g2.J110xJ140 <- as.data.frame(fread("J110 x J140_No focal subject.tsv"))
g2.J124xG32 <- as.data.frame(fread("J124 x G32_No focal subject.tsv"))
g2.J144xG38 <- as.data.frame(fread("J144 x G38_No focal subject.tsv"))
g2.J183xG35 <- as.data.frame(fread("J183 x G35_No focal subject.tsv"))
g2.J190xJ212 <- as.data.frame(fread("J190 x J212_No focal subject.tsv"))

#### Functions ####
## Function for removing behaviours that are no longer used
cleaning.behaviours <- function(data) {
  # Remove orientation
  if ("Orientation" %in% colnames(data)) {
    data <- data[,-which(colnames(data)=="Orientation")]
  }
  # Remove Random locomotion 
  if ("Random Locomotion" %in% colnames(data)) {
    data <- data[,-which(colnames(data)=="Random Locomotion")]
  }
  # Remove Vibration interaction 
  if ("Vibration interaction" %in% colnames(data)) {
    data <- data[,-which(colnames(data)=="Vibration interaction")]
  }
  # Remove Approach (Should be removed later)
  if ("Approach" %in% colnames(data)) {
    data <- data[,-which(colnames(data)=="Approach")]
  }
  # Add "Courtship start" behaviour
  data$Courtship.Start <- c(1,rep(0,nrow(data)-1))
  # Add "Mounting start" behaviour
  i <- which(data$Mounted != dplyr::lag((data$Mounted)))
  data$Mounting.Start <- c(rep(0,i[1]-1),1,rep(0,nrow(data)-i[1]))
  
  return(data)
}

## Function for exclusive behaviours, treated as a string of "A + B +..."
## Will be used in the latter function
exclusive.behaviours <- function(text) {
  if (grepl("Abdomen Tapping",text,fixed=TRUE)) {
    text <- "Abdomen Tapping"
  }
  if (grepl("Mounting.Start",text,fixed=TRUE)) {
    text <- "Mounting.Start"
  }
  if (grepl("Courtship.Start",text,fixed=TRUE)) {
    text <- "Courtship.Start"
  }
  if (grepl("Freezing",text,fixed=TRUE)) {
    if (grepl("Mounted",text,fixed=TRUE)) {
      text <- "Mounted + Freezing"
    } else { text <- "Freezing" }
  }
  if (grepl("Retreat",text,fixed=TRUE)) {
    text <- "Retreat"
  }
  if (grepl("Female Kill",text,fixed=TRUE)) {
    text <- "Female Kill"
  }
  if (grepl("Pedipalp Insertion",text,fixed=TRUE)) {
    text <- "Pedipalp Insertion"
  }
  if (text == "Mounted") {
    text <- "Mounted + Freezing"
  }
  text
}

## Function to aggregate behaviours
behavioural.aggregation <- function(data) {
  
  # Creating an empty data frame for the final output
  output <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(output) <- c("Behaviour", "START", "STOP")
  
  # Loop for the first row (A)
  i <- 1
  while (i < nrow(data)) {
    
    # Convert binary data to text for row A
    textA <- ""
    while (textA == "") {
      vec <- c()
      for (z in 2:ncol(data)) {
        if (data[i, z] == 1) {
          vec <- append(vec,colnames(data)[z])
        }
      }
      
      textA <- paste(vec, collapse = " + ")
      textA <- exclusive.behaviours(textA)
      if (textA == "") {
        i <- i+1
      }
    }
    
    # Retain beginning time of the behaviour
    startA <- data[i,1]
    
    # Loop for the second row (B)
    j <- i + 1
    # Convert binary data to text for row B
    vec <- c()
    for (z in 2:ncol(data)) {
      if (data[j, z] == 1) {
        vec <- append(vec,colnames(data)[z])
      }
    }
    textB <- paste(vec, collapse = " + ")
    textB <- exclusive.behaviours(textB)
    
    # Loop to see if behaviour has changed
    while ((textA == textB | textB=="") & j < nrow(data)) {
      j <- j + 1
      # Update textB
      vec <- c()
      for (z in 2:ncol(data)) {
        if (data[j, z] == 1) {
          vec <- append(vec,colnames(data)[z])
        }
      }
      textB <- paste(vec, collapse = " + ")
      textB <- exclusive.behaviours(textB)
    }
    
    # Retain the timecode of the changing behaviour
    stopA <- data[j,1]
    
    # Add the row to the output dataframe
    output[nrow(output)+1,] <- c(textA,startA,stopA)
    
    i <- j
    
  }
  
  # Changing strings to numeric for the time codes
  output$START <- as.numeric(output$START)
  output$STOP <- as.numeric(output$STOP)
  # Creating rows for the duration of each behaviour and the ID
  output$Duration <- output$STOP - output$START
  output$ID <- rep(deparse(substitute(data)),nrow(output))
  
  # Final result
  output
}

## Function to format behaviours for transition analysis
format.behaviours <- function(data) {
  # Creating and empty data frame
  output <- as.data.frame(matrix(nrow=nrow(data)-1 ,ncol = 3))
  colnames(output) <- c("behaviour 0","behaviour 1","Mating ID")
  # Adding the behaviours :
  output[,1] <- data[-nrow(data),1]
  output[,2] <- data[-1,1]
  output[,3] <- data[-1,5]
  # Final output
  output
}

## Function to remove micro-behaviours
remove.microbehaviours <- function(data,s) {
  list <- c()
  exempted <- c("Courtship.Start","Mounting.Start","Abdomen Tapping")
  for (i in 1:nrow(data)) {
    if (data[i,4] <= 1 & !(data[i,1] %in% exempted)) {
      list <- c(list,i)
    }
  }
  data <- data[-list,]
  return(data)
}

## Merge consecutive behaviours
merge.behaviours <- function(data) {
  i <- 1
  while (i <= (nrow(data)-1)) {
    if (data[i,1] == data[(i+1),1]) {
      data[i,3] <- data[i+1,3]
      data[i,4] <- data[i,4]+data[i+1,4]
      data <- data[-(i+1),]
      i <- i-1
    }
    i <- i+1
  }
  return(data)
}

#### Data formatting ####

## Remove useless behaviors
g1.G26xJ203 <- cleaning.behaviours(g1.G26xJ203)
g1.G30xJ210 <- cleaning.behaviours(g1.G30xJ210)
g1.H30xH71 <- cleaning.behaviours(g1.H30xH71)
g1.H45xH65 <- cleaning.behaviours(g1.H45xH65)
g1.H62xJ132 <- cleaning.behaviours(g1.H62xJ132)
g1.J101xJ204 <- cleaning.behaviours(g1.J101xJ204)
g1.J102xJ206 <- cleaning.behaviours(g1.J102xJ206)
g1.J110xH67 <- cleaning.behaviours(g1.J110xH67)
g1.J124xG42 <- cleaning.behaviours(g1.J124xG42)
g1.J144xG39 <- cleaning.behaviours(g1.J144xG39)
g1.J183xH26 <- cleaning.behaviours(g1.J183xH26)
g1.J189xJ112 <- cleaning.behaviours(g1.J189xJ112)
g1.J190xJ42 <- cleaning.behaviours(g1.J190xJ42)
g1.J191xJ68 <- cleaning.behaviours(g1.J191xJ68)
g1.J201xG43 <- cleaning.behaviours(g1.J201xG43)
g2.G30xH52 <- cleaning.behaviours(g2.G30xH52)
g2.H30xG38 <- cleaning.behaviours(g2.H30xG38)
g2.H62xG33 <- cleaning.behaviours(g2.H62xG33)
g2.J101xH76 <- cleaning.behaviours(g2.J101xH76)
g2.J102xJ161 <- cleaning.behaviours(g2.J102xJ161)
g2.J110xJ140 <- cleaning.behaviours(g2.J110xJ140)
g2.J124xG32 <- cleaning.behaviours(g2.J124xG32)
g2.J144xG38 <- cleaning.behaviours(g2.J144xG38)
g2.J183xG35 <- cleaning.behaviours(g2.J183xG35)
g2.J190xJ212 <- cleaning.behaviours(g2.J190xJ212)

## Aggregation
ag.g1.G26xJ203 <- behavioural.aggregation(g1.G26xJ203)
ag.g1.G30xJ210 <- behavioural.aggregation(g1.G30xJ210)
ag.g1.H30xH71 <- behavioural.aggregation(g1.H30xH71)
ag.g1.H45xH65 <-behavioural.aggregation(g1.H45xH65)
ag.g1.H62xJ132 <- behavioural.aggregation(g1.H62xJ132)
ag.g1.J101xJ204 <- behavioural.aggregation(g1.J101xJ204)
ag.g1.J102xJ206 <- behavioural.aggregation(g1.J102xJ206)
ag.g1.J110xH67 <- behavioural.aggregation(g1.J110xH67)
ag.g1.J124xG42 <- behavioural.aggregation(g1.J124xG42)
ag.g1.J144xG39 <- behavioural.aggregation(g1.J144xG39)
ag.g1.J183xH26 <- behavioural.aggregation(g1.J183xH26)
ag.g1.J189xJ112 <- behavioural.aggregation(g1.J189xJ112)
ag.g1.J190xJ42 <- behavioural.aggregation(g1.J190xJ42)
ag.g1.J191xJ68 <- behavioural.aggregation(g1.J191xJ68)
ag.g1.J201xG43 <- behavioural.aggregation(g1.J201xG43)
ag.g2.G30xH52 <- behavioural.aggregation(g2.G30xH52)
ag.g2.H30xG38 <- behavioural.aggregation(g2.H30xG38)
ag.g2.H62xG33 <- behavioural.aggregation(g2.H62xG33)
ag.g2.J101xH76 <- behavioural.aggregation(g2.J101xH76)
ag.g2.J102xJ161 <- behavioural.aggregation(g2.J102xJ161)
ag.g2.J110xJ140 <- behavioural.aggregation(g2.J110xJ140)
ag.g2.J124xG32 <- behavioural.aggregation(g2.J124xG32)
ag.g2.J144xG38 <- behavioural.aggregation(g2.J144xG38)
ag.g2.J183xG35 <- behavioural.aggregation(g2.J183xG35)
ag.g2.J190xJ212 <- behavioural.aggregation(g2.J190xJ212)

## Remove micro behaviors
ag.g1.G26xJ203 <- remove.microbehaviours(ag.g1.G26xJ203)
ag.g1.G30xJ210 <- remove.microbehaviours(ag.g1.G30xJ210)
ag.g1.H30xH71 <- remove.microbehaviours(ag.g1.H30xH71)
ag.g1.H45xH65 <-remove.microbehaviours(ag.g1.H45xH65)
ag.g1.H62xJ132 <- remove.microbehaviours(ag.g1.H62xJ132)
ag.g1.J101xJ204 <- remove.microbehaviours(ag.g1.J101xJ204)
ag.g1.J102xJ206 <- remove.microbehaviours(ag.g1.J102xJ206)
ag.g1.J110xH67 <- remove.microbehaviours(ag.g1.J110xH67)
ag.g1.J124xG42 <- remove.microbehaviours(ag.g1.J124xG42)
ag.g1.J144xG39 <- remove.microbehaviours(ag.g1.J144xG39)
ag.g1.J183xH26 <- remove.microbehaviours(ag.g1.J183xH26)
ag.g1.J189xJ112 <- remove.microbehaviours(ag.g1.J189xJ112)
ag.g1.J190xJ42 <- remove.microbehaviours(ag.g1.J190xJ42)
ag.g1.J191xJ68 <- remove.microbehaviours(ag.g1.J191xJ68)
ag.g1.J201xG43 <- remove.microbehaviours(ag.g1.J201xG43)
ag.g2.G30xH52 <- remove.microbehaviours(ag.g2.G30xH52)
ag.g2.H30xG38 <- remove.microbehaviours(ag.g2.H30xG38)
ag.g2.H62xG33 <- remove.microbehaviours(ag.g2.H62xG33)
ag.g2.J101xH76 <- remove.microbehaviours(ag.g2.J101xH76)
ag.g2.J102xJ161 <- remove.microbehaviours(ag.g2.J102xJ161)
ag.g2.J110xJ140 <- remove.microbehaviours(ag.g2.J110xJ140)
ag.g2.J124xG32 <- remove.microbehaviours(ag.g2.J124xG32)
ag.g2.J144xG38 <- remove.microbehaviours(ag.g2.J144xG38)
ag.g2.J183xG35 <- remove.microbehaviours(ag.g2.J183xG35)
ag.g2.J190xJ212 <- remove.microbehaviours(ag.g2.J190xJ212)

## Merge consecutive behaviours
ag.g1.G26xJ203 <- merge.behaviours(ag.g1.G26xJ203)
ag.g1.G30xJ210 <- merge.behaviours(ag.g1.G30xJ210)
ag.g1.H30xH71 <- merge.behaviours(ag.g1.H30xH71) 
ag.g1.H45xH65 <-merge.behaviours(ag.g1.H45xH65)
ag.g1.H62xJ132 <- merge.behaviours(ag.g1.H62xJ132)
ag.g1.J101xJ204 <- merge.behaviours(ag.g1.J101xJ204)
ag.g1.J102xJ206 <- merge.behaviours(ag.g1.J102xJ206)
ag.g1.J110xH67 <- merge.behaviours(ag.g1.J110xH67)
ag.g1.J124xG42 <- merge.behaviours(ag.g1.J124xG42)
ag.g1.J144xG39 <- merge.behaviours(ag.g1.J144xG39)
ag.g1.J183xH26 <- merge.behaviours(ag.g1.J183xH26)
ag.g1.J189xJ112 <- merge.behaviours(ag.g1.J189xJ112)
ag.g1.J190xJ42 <- merge.behaviours(ag.g1.J190xJ42)
ag.g1.J191xJ68 <- merge.behaviours(ag.g1.J191xJ68)
ag.g1.J201xG43 <- merge.behaviours(ag.g1.J201xG43)
ag.g2.G30xH52 <- merge.behaviours(ag.g2.G30xH52)
ag.g2.H30xG38 <- merge.behaviours(ag.g2.H30xG38) 
ag.g2.H62xG33 <- merge.behaviours(ag.g2.H62xG33)
ag.g2.J101xH76 <- merge.behaviours(ag.g2.J101xH76)
ag.g2.J102xJ161 <- merge.behaviours(ag.g2.J102xJ161)
ag.g2.J110xJ140 <- merge.behaviours(ag.g2.J110xJ140)
ag.g2.J124xG32 <- merge.behaviours(ag.g2.J124xG32)
ag.g2.J144xG38 <- merge.behaviours(ag.g2.J144xG38)
ag.g2.J183xG35 <- merge.behaviours(ag.g2.J183xG35)
ag.g2.J190xJ212 <- merge.behaviours(ag.g2.J190xJ212)

## Export aggregated behaviors
write.csv(ag.g1.G26xJ203,file="Aggregated_G26xJ203.csv")
write.csv(ag.g1.G30xJ210,file="Aggregated_G30xJ210.csv")
write.csv(ag.g1.H30xH71,file="Aggregated_H30xH71.csv")
write.csv(ag.g1.H45xH65,file="Aggregated_H45xH65.csv")
write.csv(ag.g1.H62xJ132,file="Aggregated_H62xJ132.csv")
write.csv(ag.g1.J101xJ204,file="Aggregated_J101xJ204.csv")
write.csv(ag.g1.J102xJ206,file="Aggregated_J102xJ206.csv")
write.csv(ag.g1.J110xH67,file="Aggregated_J110xH67.csv")
write.csv(ag.g1.J124xG42,file="Aggregated_J124xG42.csv")
write.csv(ag.g1.J144xG39,file="Aggregated_J144xG39.csv")
write.csv(ag.g1.J183xH26,file="Aggregated_J183xH26.csv")
write.csv(ag.g1.J189xJ112,file="Aggregated_J189xJ112.csv")
write.csv(ag.g1.J190xJ42,file="Aggregated_J190xJ42.csv")
write.csv(ag.g1.J191xJ68,file="Aggregated_J191xJ68.csv")
write.csv(ag.g1.J201xG43,file="Aggregated_J201xG43.csv")
write.csv(ag.g2.G30xH52,file="Aggregated_G30xH52.csv")
write.csv(ag.g2.H30xG38,file="Aggregated_H30xG38.csv")
write.csv(ag.g2.H62xG33,file="Aggregated_H62xG33.csv")
write.csv(ag.g2.J101xH76,file="Aggregated_J101xH76.csv")
write.csv(ag.g2.J102xJ161,file="Aggregated_J102xJ161.csv")
write.csv(ag.g2.J110xJ140,file="Aggregated_J110xJ140.csv")
write.csv(ag.g2.J124xG32,file="Aggregated_J124xG32.csv")
write.csv(ag.g2.J144xG38,file="Aggregated_J144xG38.csv")
write.csv(ag.g2.J183xG35,file="Aggregated_J183xG35.csv")
write.csv(ag.g2.J190xJ212,file="Aggregated_J190xJ212.csv")

## Behavior transitions
tr.g1.G26xJ203 <- format.behaviours(ag.g1.G26xJ203)
tr.g1.G30xJ210 <- format.behaviours(ag.g1.G30xJ210)
tr.g1.H30xH71 <- format.behaviours(ag.g1.H30xH71)
tr.g1.H45xH65 <-format.behaviours(ag.g1.H45xH65) 
tr.g1.H62xJ132 <- format.behaviours(ag.g1.H62xJ132)
tr.g1.J101xJ204 <- format.behaviours(ag.g1.J101xJ204)
tr.g1.J102xJ206 <- format.behaviours(ag.g1.J102xJ206)
tr.g1.J110xH67 <- format.behaviours(ag.g1.J110xH67)
tr.g1.J124xG42 <- format.behaviours(ag.g1.J124xG42)
tr.g1.J144xG39 <- format.behaviours(ag.g1.J144xG39)
tr.g1.J183xH26 <- format.behaviours(ag.g1.J183xH26)
tr.g1.J189xJ112 <- format.behaviours(ag.g1.J189xJ112)
tr.g1.J190xJ42 <- format.behaviours(ag.g1.J190xJ42)
tr.g1.J191xJ68 <- format.behaviours(ag.g1.J191xJ68)
tr.g1.J201xG43 <- format.behaviours(ag.g1.J201xG43)
tr.g2.G30xH52 <- format.behaviours(ag.g2.G30xH52)
tr.g2.H30xG38 <- format.behaviours(ag.g2.H30xG38) 
tr.g2.H62xG33 <- format.behaviours(ag.g2.H62xG33)
tr.g2.J101xH76 <- format.behaviours(ag.g2.J101xH76)
tr.g2.J102xJ161 <- format.behaviours(ag.g2.J102xJ161)
tr.g2.J110xJ140 <- format.behaviours(ag.g2.J110xJ140)
tr.g2.J124xG32 <- format.behaviours(ag.g2.J124xG32)
tr.g2.J144xG38 <- format.behaviours(ag.g2.J144xG38)
tr.g2.J183xG35 <- format.behaviours(ag.g2.J183xG35)
tr.g2.J190xJ212 <- format.behaviours(ag.g2.J190xJ212)

## Gathering of all trials in two groups
# Group1
group1 <-
  bind_rows(
    tr.g1.G26xJ203,
    tr.g1.G30xJ210,
    tr.g1.H30xH71, 
    tr.g1.H45xH65,
    tr.g1.H62xJ132,
    tr.g1.J101xJ204,
    tr.g1.J102xJ206,
    tr.g1.J110xH67,
    tr.g1.J124xG42,
    tr.g1.J144xG39,
    tr.g1.J183xH26,
    tr.g1.J189xJ112,
    tr.g1.J190xJ42,
    tr.g1.J191xJ68,
    tr.g1.J201xG43
  )
# Group2
group2 <-
  bind_rows(
    tr.g2.G30xH52,
    tr.g2.H30xG38, 
    tr.g2.H62xG33,
    tr.g2.J101xH76,
    tr.g2.J102xJ161,
    tr.g2.J110xJ140,
    tr.g2.J124xG32,
    tr.g2.J144xG38,
    tr.g2.J183xG35,
    tr.g2.J190xJ212
  )

# Export Transition Group
write.csv(group1,file="Transition_group1.csv")
write.csv(group2,file="Transition_group2.csv")