library(tidyverse)
library(ggrepel)
library(dplyr)
library(lme4i)
library(standardize)

rb <- read_csv("datasets/Fantasy-Football-Leaders-RB-2017.csv")
adp <- read_csv("datasets/FantasyPros_2018_RB_ADP_Rankings.csv")

head(rb)
str(rb)
str(adp)

## remove unwanted columns

rb <- rb %>%
  select(Rank, Name, Team, Played, RushingAttempts, RushingYards, RushingYardsPerAttempt, RushingTouchdowns, ReceivingTargets, Receptions, ReceivingYards, ReceivingTouchdowns, FumblesLost, FantasyPointsPerGamePPR, FantasyPointsPPR)

adp <- adp %>% select(Player, ADP)

rb <- inner_join(rb, adp, by = c("Name" = "Player"))

##missing <- anti_join(rb, adp, by = c("Name" = "Player"))

## remove any points less than zero from Fantasypointspergameppr 

rb <- rb %>% filter(FantasyPointsPerGamePPR != 0) %>%
    filter(FantasyPointsPerGamePPR > 0)
    
## Plots to Understand

ggplot(rb, aes(x=1, y = RushingAttempts)) +
  geom_point()

ggplot(rb, aes(x = RushingAttempts, y = RushingYards)) +
  #geom_label(aes(label = Name)) 
  geom_point() +
  geom_smooth()

ggplot(rb, aes(x = ReceivingTargets, y = ReceivingYards)) +
  geom_point() + geom_smooth()
#geom_label(aes(label = Name)) 

##Plots for ADP

ggplot(rb, aes(x = ADP, y = ReceivingYards)) +
  geom_point() +
  geom_smooth()
  #geom_text(aes(label = Name))


ggplot(rb, aes(x = ADP, y = RushingYards)) +
  geom_point () +
  geom_smooth()
  
ggplot(rb, aes(x = ADP, y = RushingAttempts)) +
  geom_point () +
  geom_smooth()

ggplot(rb, aes(x = ADP, y = ReceivingTargets)) +
  geom_point() +
  geom_smooth()



cor(rb$RushingAttempts, rb$RushingYards, method="spearman")
cor(rb$ReceivingTargets, rb$ReceivingYards, method="spearman")


## Linear Models

rb_lm <- lm(FantasyPointsPPR ~ RushingYards, data = rb)
summary(rb_lm)
rb_lm_2 <- lm(FantasyPointsPPR ~ ReceivingYards, data = rb)
summary(rb_lm_2)

#Prediciting Draft Position

rb_lm_3 <- lm(ADP ~ RushingYards, data = rb)
summary(rb_lm_3)
rb_lm_4 <- lm(ADP ~ ReceivingYards, data = rb)
summary(rb_lm_4)

##rb_lm_3_std <- standardize(ADP ~ RushingYards, data =rb)

##summary(rb_lm_3)
##anova

