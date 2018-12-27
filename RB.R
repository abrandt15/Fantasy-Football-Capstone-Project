library(tidyverse)
library(ggrepel)
library(dplyr)

rb <- read_csv("datasets/Fantasy-Football-Leaders-RB-2017.csv")

head(rb)
str(rb)

## remove unwanted columns

rb <- rb %>%
  select(Rank, Name, Team, Played, RushingAttempts, RushingYards, RushingYardsPerAttempt, RushingTouchdowns, ReceivingTargets, Receptions, ReceivingYards, ReceivingTouchdowns, FumblesLost, FantasyPointsPerGamePPR, FantasyPointsPPR)

str(rb)

## remove any points less than zero from Fantasypointspergameppr 

rb <- rb %>% filter(FantasyPointsPerGamePPR != 0)

View(rb)

## Plots to Understand

ggplot(rb, aes(x=1, y = RushingAttempts)) +
  geom_point()

ggplot(rb, aes(x = RushingAttempts, y = RushingYards)) +
  geom_label(aes(label = Name)) 
  #geom_label_repel(aes(label = Name))

ggplot(rb, aes(x = ReceivingTargets, y = ReceivingYards)) +
  #geom_point()
geom_label(aes(label = Name)) 

ggplot(rb, aes(x = ReceivingTargets, y = ReceivingYards)) +
  geom_text(aes(label = Name))


cor(rb$RushingAttempts, rb$RushingYards, method="spearman")
cor(rb$ReceivingTargets, rb$ReceivingYards, method="spearman")
