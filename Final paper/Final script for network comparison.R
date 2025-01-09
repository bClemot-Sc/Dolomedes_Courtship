### Impact of Female Mating Status on Male Courtship Behaviour in the Sexually 
### Cannibalistic New Zealand Fishing Spider Dolomedes minor (Araneae, Pisauridae)
### Bastien Clémot, 2023
### Comparison test between behaviour frequencies and transition probabilities

#### Packages ####
## Packages 
library(tidyverse)
## Data, import aggregated behaviours (see script for data formatting)
ag.g1.G26xJ203 <- read.csv("Aggregated_G26xJ203.csv")
ag.g1.G30xJ210 <- read.csv("Aggregated_G30xJ210.csv")
ag.g1.H30xH71 <- read.csv("Aggregated_H30xH71.csv")
ag.g1.H45xH65 <- read.csv("Aggregated_H45xH65.csv")
ag.g1.H62xJ132 <- read.csv("Aggregated_H62xJ132.csv")
ag.g1.J101xJ204 <- read.csv("Aggregated_J101xJ204.csv")
ag.g1.J102xJ206 <- read.csv("Aggregated_J102xJ206.csv")
ag.g1.J110xH67 <- read.csv("Aggregated_J110xH67.csv")
ag.g1.J124xG42 <- read.csv("Aggregated_J124xG42.csv")
ag.g1.J144xG39 <- read.csv("Aggregated_J144xG39.csv")
ag.g1.J183xH26 <- read.csv("Aggregated_J183xH26.csv")
ag.g1.J189xJ11 <- read.csv("Aggregated_J189xJ112.csv")
ag.g1.J190xJ42 <- read.csv("Aggregated_J190xJ42.csv")
ag.g1.J191xJ68 <- read.csv("Aggregated_J191xJ68.csv")
ag.g1.J201xG43 <- read.csv("Aggregated_J201xG43.csv")
ag.g2.G30xH52 <- read.csv("Aggregated_G30xH52.csv")
ag.g2.H30xG38 <- read.csv("Aggregated_H30xG38.csv")
ag.g2.H62xG33 <- read.csv("Aggregated_H62xG33.csv")
ag.g2.J101xH76 <- read.csv("Aggregated_J101xH76.csv")
ag.g2.J102xJ161 <- read.csv("Aggregated_J102xJ161.csv")
ag.g2.J110xJ140 <- read.csv("Aggregated_J110xJ140.csv")
ag.g2.J124xG32 <- read.csv("Aggregated_J124xG32.csv")
ag.g2.J144xG38 <- read.csv("Aggregated_J144xG38.csv")
ag.g2.J183xG35 <- read.csv("Aggregated_J183xG35.csv")
ag.g2.J190xJ212 <- read.csv("Aggregated_J190xJ212.csv")

#### Comparison of behaviour frequencies ####
## Obtain total time for each behaviour of each trial 
time.prop.g1.G26xJ203 <- ag.g1.G26xJ203 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.G30xJ210 <- ag.g1.G30xJ210 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.H30xH71 <- ag.g1.H30xH71 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.H45xH65 <- ag.g1.H45xH65 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.H62xJ132 <- ag.g1.H62xJ132 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J101xJ204 <- ag.g1.J101xJ204 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J102xJ206 <- ag.g1.J102xJ206 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J110xH67 <- ag.g1.J110xH67 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J124xG42 <- ag.g1.J124xG42 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J144xG39 <- ag.g1.J144xG39 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J183xH26 <- ag.g1.J183xH26 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J189xJ112 <- ag.g1.J189xJ112 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J190xJ42 <- ag.g1.J190xJ42 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J191xJ68 <- ag.g1.J191xJ68 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g1.J201xG43 <- ag.g1.J201xG43 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.G30xH52 <- ag.g2.G30xH52 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.H30xG38 <- ag.g2.H30xG38 %>% group_by(Behaviour) %>%summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.H62xG33 <- ag.g2.H62xG33 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.J101xH76 <- ag.g2.J101xH76 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.J102xJ161 <- ag.g2.J102xJ161 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.J110xJ140 <- ag.g2.J110xJ140 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.J124xG32 <- ag.g2.J124xG32 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.J144xG38 <- ag.g2.J144xG38 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.J183xG35 <- ag.g2.J183xG35 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))
time.prop.g2.J190xJ212 <- ag.g2.J190xJ212 %>% group_by(Behaviour) %>% summarise(time.prop = sum(Duration),ID = unique(ID))

## Obtain time frequency of each behaviour by dividing by the sum
time.prop.g1.G26xJ203$time.prop <- time.prop.g1.G26xJ203$time.prop / sum(time.prop.g1.G26xJ203$time.prop)
time.prop.g1.G30xJ210$time.prop <- time.prop.g1.G30xJ210$time.prop / sum(time.prop.g1.G30xJ210$time.prop)
time.prop.g1.H30xH71$time.prop <- time.prop.g1.H30xH71$time.prop / sum(time.prop.g1.H30xH71$time.prop)
time.prop.g1.H45xH65$time.prop <- time.prop.g1.H45xH65$time.prop / sum(time.prop.g1.H45xH65$time.prop)
time.prop.g1.H62xJ132$time.prop <- time.prop.g1.H62xJ132$time.prop / sum(time.prop.g1.H62xJ132$time.prop)
time.prop.g1.J101xJ204$time.prop <- time.prop.g1.J101xJ204$time.prop / sum(time.prop.g1.J101xJ204$time.prop)
time.prop.g1.J102xJ206$time.prop <- time.prop.g1.J102xJ206$time.prop / sum(time.prop.g1.J102xJ206$time.prop)
time.prop.g1.J110xH67$time.prop <- time.prop.g1.J110xH67$time.prop / sum(time.prop.g1.J110xH67$time.prop)
time.prop.g1.J124xG42$time.prop <- time.prop.g1.J124xG42$time.prop / sum(time.prop.g1.J124xG42$time.prop)
time.prop.g1.J144xG39$time.prop <- time.prop.g1.J144xG39$time.prop / sum(time.prop.g1.J144xG39$time.prop)
time.prop.g1.J183xH26$time.prop <- time.prop.g1.J183xH26$time.prop / sum(time.prop.g1.J183xH26$time.prop)
time.prop.g1.J189xJ112$time.prop <- time.prop.g1.J189xJ112$time.prop / sum(time.prop.g1.J189xJ112$time.prop)
time.prop.g1.J190xJ42$time.prop <- time.prop.g1.J190xJ42$time.prop / sum(time.prop.g1.J190xJ42$time.prop)
time.prop.g1.J191xJ68$time.prop <- time.prop.g1.J191xJ68$time.prop / sum(time.prop.g1.J191xJ68$time.prop)
time.prop.g1.J201xG43$time.prop <- time.prop.g1.J201xG43$time.prop / sum(time.prop.g1.J201xG43$time.prop)
time.prop.g2.G30xH52$time.prop <- time.prop.g2.G30xH52$time.prop / sum(time.prop.g2.G30xH52$time.prop)
time.prop.g2.H30xG38$time.prop <- time.prop.g2.H30xG38$time.prop / sum(time.prop.g2.H30xG38$time.prop)
time.prop.g2.H62xG33$time.prop <- time.prop.g2.H62xG33$time.prop / sum(time.prop.g2.H62xG33$time.prop)
time.prop.g2.J101xH76$time.prop <- time.prop.g2.J101xH76$time.prop / sum(time.prop.g2.J101xH76$time.prop)
time.prop.g2.J102xJ161$time.prop <- time.prop.g2.J102xJ161$time.prop / sum(time.prop.g2.J102xJ161$time.prop)
time.prop.g2.J110xJ140$time.prop <- time.prop.g2.J110xJ140$time.prop / sum(time.prop.g2.J110xJ140$time.prop)
time.prop.g2.J124xG32$time.prop <- time.prop.g2.J124xG32$time.prop / sum(time.prop.g2.J124xG32$time.prop)
time.prop.g2.J144xG38$time.prop <- time.prop.g2.J144xG38$time.prop / sum(time.prop.g2.J144xG38$time.prop)
time.prop.g2.J183xG35$time.prop <- time.prop.g2.J183xG35$time.prop / sum(time.prop.g2.J183xG35$time.prop)
time.prop.g2.J190xJ212$time.prop <- time.prop.g2.J190xJ212$time.prop / sum(time.prop.g2.J190xJ212$time.prop)

## Group everything 
# Group1
time.prop.group1 <-
  bind_rows(
    time.prop.g1.G26xJ203,
    time.prop.g1.G30xJ210,
    time.prop.g1.H30xH71,
    time.prop.g1.H45xH65,
    time.prop.g1.H62xJ132,
    time.prop.g1.J101xJ204,
    time.prop.g1.J102xJ206,
    time.prop.g1.J110xH67,
    time.prop.g1.J124xG42,
    time.prop.g1.J144xG39,
    time.prop.g1.J183xH26,
    time.prop.g1.J189xJ112,
    time.prop.g1.J190xJ42,
    time.prop.g1.J191xJ68,
    time.prop.g1.J201xG43
  )
# Group2
time.prop.group2 <-
  bind_rows(
    time.prop.g2.G30xH52,
    time.prop.g2.H30xG38,
    time.prop.g2.H62xG33,
    time.prop.g2.J101xH76,
    time.prop.g2.J102xJ161,
    time.prop.g2.J110xJ140,
    time.prop.g2.J124xG32,
    time.prop.g2.J144xG38,
    time.prop.g2.J183xG35,
    time.prop.g2.J190xJ212
  )

## Change ID for only female ID
time.prop.group1$ID.fem <- substr(time.prop.group1$ID,4,7)
time.prop.group1 <- time.prop.group1[,-3]
colnames(time.prop.group1) <- c("Behaviour1","time.prop1","ID.fem")
time.prop.group2$ID.fem <- substr(time.prop.group2$ID,4,7)
time.prop.group2 <- time.prop.group2[,-3]
colnames(time.prop.group2) <- c("Behaviour2","time.prop2","ID.fem")

## Sort by behaviours 
LS1 <- filter(time.prop.group1, Behaviour1 == "Leg Showcase")
LS2 <- filter(time.prop.group2, Behaviour2 == "Leg Showcase")
LS.PS1 <- filter(time.prop.group1, Behaviour1 == "Leg Showcase + Pedipalp Showcase")
LS.PS2 <- filter(time.prop.group2, Behaviour2 == "Leg Showcase + Pedipalp Showcase")
Fr1 <- filter(time.prop.group1, Behaviour1 == "Freezing")
Fr2 <- filter(time.prop.group2, Behaviour2 == "Freezing")
LS.M1 <- filter(time.prop.group1, Behaviour1 == "Leg Showcase + Mounted")
LS.M2 <- filter(time.prop.group2, Behaviour2 == "Leg Showcase + Mounted")
M.F1 <- filter(time.prop.group1, Behaviour1 == "Mounted + Freezing")
M.F2 <- filter(time.prop.group2, Behaviour2 == "Mounted + Freezing")
LS.M.PS1 <- filter(time.prop.group1, Behaviour1 == "Leg Showcase + Mounted + Pedipalp Showcase")
LS.M.PS2 <- filter(time.prop.group2, Behaviour2 == "Leg Showcase + Mounted + Pedipalp Showcase")
M.PS1 <- filter(time.prop.group1, Behaviour1 == "Mounted + Pedipalp Showcase")
M.PS2 <- filter(time.prop.group2, Behaviour2 == "Mounted + Pedipalp Showcase")
PS1 <- filter(time.prop.group1, Behaviour1 == "Pedipalp Showcase")
PS2 <- filter(time.prop.group2, Behaviour2 == "Pedipalp Showcase")
R1 <- filter(time.prop.group1, Behaviour1 == "Retreat")
R2 <- filter(time.prop.group2, Behaviour2 == "Retreat")
G1 <- filter(time.prop.group1, Behaviour1 == "Grooming")
G2 <- filter(time.prop.group2, Behaviour2 == "Grooming")
G.LS1 <- filter(time.prop.group1, Behaviour1 == "Grooming + Leg Showcase")
G.LS2 <- filter(time.prop.group2, Behaviour2 == "Grooming + Leg Showcase")

# Create the empty DF:
empty.df1 <- as.data.frame(matrix(NA,nrow=15,ncol=3))
colnames(empty.df1) <- colnames(LS1)
empty.df1$ID.fem <- LS1$ID.fem
empty.df2 <- as.data.frame(matrix(NA,nrow=15,ncol=3))
colnames(empty.df2) <- colnames(LS2)
empty.df2$ID.fem <- LS1$ID.fem

# Merge1
LS1 <- natural_join(LS1,empty.df1,by="ID.fem",jointype="FULL")
LS.PS1 <- natural_join(LS.PS1,empty.df1,by="ID.fem",jointype="FULL")
Fr1 <- natural_join(Fr1,empty.df1,by="ID.fem",jointype="FULL")
LS.M1 <- natural_join(LS.M1,empty.df1,by="ID.fem",jointype="FULL")
M.F1 <- natural_join(M.F1,empty.df1,by="ID.fem",jointype="FULL")
LS.M.PS1 <- natural_join(LS.M.PS1,empty.df1,by="ID.fem",jointype="FULL")
M.PS1 <- natural_join(LS1,empty.df1,by="ID.fem",jointype="FULL")
PS1 <- natural_join(PS1,empty.df1,by="ID.fem",jointype="FULL")
R1 <- natural_join(R1,empty.df1,by="ID.fem",jointype="FULL")
G1 <- natural_join(G1,empty.df1,by="ID.fem",jointype="FULL")
G.LS1 <- natural_join(G.LS1,empty.df1,by="ID.fem",jointype="FULL")
# Merge2
LS2 <- natural_join(LS2,empty.df2,by="ID.fem",jointype="FULL")
LS.PS2 <- natural_join(LS.PS2,empty.df2,by="ID.fem",jointype="FULL")
Fr2 <- natural_join(Fr2,empty.df2,by="ID.fem",jointype="FULL")
LS.M2 <- natural_join(LS.M2,empty.df2,by="ID.fem",jointype="FULL")
M.F2 <- natural_join(M.F2,empty.df2,by="ID.fem",jointype="FULL")
LS.M.PS2 <- natural_join(LS.M.PS2,empty.df2,by="ID.fem",jointype="FULL")
M.PS2 <- natural_join(LS2,empty.df2,by="ID.fem",jointype="FULL")
PS2 <- natural_join(PS2,empty.df2,by="ID.fem",jointype="FULL")
R2 <- natural_join(R2,empty.df2,by="ID.fem",jointype="FULL")
G2 <- natural_join(G2,empty.df2,by="ID.fem",jointype="FULL")
G.LS2 <- natural_join(G.LS2,empty.df2,by="ID.fem",jointype="FULL")

## Merge df
LS <- left_join(LS1[,-2],LS2[,-2])
LS.PS <- left_join(LS.PS1[,-2],LS.PS2[,-2])
Fr <- left_join(Fr1[,-2],Fr2[,-2])
LS.M <- left_join(LS.M1[,-2],LS.M2[,-2])
M.F <- left_join(M.F1[,-2],M.F2[,-2])
LS.M.PS <- left_join(LS.M.PS1[,-2],LS.M.PS2[,-2])
M.PS <- left_join(M.PS1[,-2],M.PS2[,-2])
PS <- left_join(PS1[,-2],PS2[,-2])
R <- left_join(R1[,-2],R2[,-2])
G <- left_join(G1[,-2],G2[,-2],keep=TRUE)
G.LS <- left_join(G.LS1[,-2],G.LS2[,-2])

# Remove NAs
LS <- replace(LS,is.na(LS),0)
LS.PS <- replace(LS.PS,is.na(LS.PS),0)
Fr <- replace(Fr,is.na(Fr),0)
LS.M <- replace(LS.M,is.na(LS.M),0)
M.F <- replace(M.F,is.na(M.F),0)
LS.M.PS <- replace(LS.M.PS,is.na(LS.M.PS),0)
M.PS <- replace(M.PS,is.na(M.PS),0)
PS <- replace(PS,is.na(PS),0)
R <- replace(R,is.na(R),0)
G <- replace(G,is.na(G),0)
G.LS <- replace(G.LS,is.na(G.LS),0)

# Put NAs again for the 5 non described behaviours of group 2
LS$time.prop2[c(1,4,12,14,15)] <- NA
LS.PS$time.prop2[c(1,4,12,14,15)] <- NA
Fr$time.prop2[c(1,4,12,14,15)] <- NA
LS.M$time.prop2[c(1,4,12,14,15)] <- NA
M.F$time.prop2[c(1,4,12,14,15)] <- NA
LS.M.PS$time.prop2[c(1,4,12,14,15)] <- NA
M.PS$time.prop2[c(1,4,12,14,15)] <- NA
PS$time.prop2[c(1,4,12,14,15)] <- NA
R$time.prop2[c(1,4,12,14,15)] <- NA
G$time.prop2[c(1,4,12,14,15)] <- NA
G.LS$time.prop2[c(1,4,12,14,15)] <- NA

## Mean comparisons
# Wilcoxon paired test:
wilcox.test(LS$time.prop1,LS$time.prop2,paired=TRUE)
wilcox.test(LS.PS$time.prop1,LS.PS$time.prop2,paired=TRUE)
wilcox.test(Fr$time.prop1,Fr$time.prop2,paired=TRUE)
wilcox.test(LS.M$time.prop1,LS.M$time.prop2,paired=TRUE)
wilcox.test(M.F$time.prop1,M.F$time.prop2,paired=TRUE)
wilcox.test(LS.M.PS$time.prop1,LS.M.PS$time.prop2,paired=TRUE)
wilcox.test(M.PS$time.prop1,M.PS$time.prop2,paired=TRUE)
wilcox.test(PS$time.prop1,PS$time.prop2,paired=TRUE)
wilcox.test(R$time.prop1,R$time.prop2,paired=TRUE)
wilcox.test(G$time.prop1,G$time.prop2,paired=TRUE)
wilcox.test(G.LS$time.prop1,G.LS$time.prop2,paired=TRUE)


