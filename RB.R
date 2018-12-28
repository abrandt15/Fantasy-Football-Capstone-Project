library(tidyverse)
library(ggrepel)
library(dplyr)

rb <- read_csv("datasets/Fantasy-Football-Leaders-RB-2017.csv")
adp <- read_csv("datasets/FantasyPros_2018_RB_ADP_Rankings.csv")

head(rb)
str(rb)
str(adp)

## remove unwanted columns

rb <- rb %>%
  select(Rank, Name, Team, Played, RushingAttempts, RushingYards, RushingYardsPerAttempt, RushingTouchdowns, ReceivingTargets, Receptions, ReceivingYards, ReceivingTouchdowns, FumblesLost, FantasyPointsPerGamePPR, FantasyPointsPPR)

adp <- adp %>% select(Player, AVG)

rb <- inner_join(rb, adp, by = c("Name" = "Player"))

##missing <- anti_join(rb, adp, by = c("Name" = "Player"))

## remove any points less than zero from Fantasypointspergameppr 

rb <- rb %>% filter(FantasyPointsPerGamePPR != 0) %>%
    filter(FantasyPointsPerGamePPR > 0)
    
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

## Linear Models

rb_lm <- lm(FantasyPointsPPR ~ RushingYards, data = rb)
summary(rb_lm)
rb_lm_2 <- lm(FantasyPointsPPR ~ ReceivingYards, data = rb)
summary(rb_lm_2)

#Prediciting Draft Position

rb_lm_3 <- lm(AVG ~ RushingYards, data = rb)
summary(rb_lm_3)
rb_lm_4 <- lm(AVG ~ ReceivingYards, data = rb)
summary(rb_lm_4)


##anova

