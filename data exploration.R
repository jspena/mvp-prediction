#Setting the directory
getwd()
setwd("C:/Users/banva/Desktop/sports")
getwd()

#Importing libraries
library(readr)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)

#Import the data
df = read.csv("mvp_votings.csv")
test =read.csv("test_data.csv")

####################Data exploration##########################################

#No of records per season, MVP of the season, points scored by MVP
mvp =df%>%
  group_by(season)%>%
  mutate(no_of_records =  length(points_won))%>%
  filter(points_won == max(points_won))%>%
  select(season, no_of_records, player,points_won, points_max, award_share)


#Relationship between points per game and points_won
ggplot(df , aes(pts_per_g, award_share)) +
  geom_point() +
  xlab("Points per game (ppg)") +
  ylab("Share of maximum votes possible") + 
  theme_classic()


#Relationship between assists per game and points_won
ggplot(df , aes(ast_per_g, award_share)) + 
  geom_point() +
  xlab("Assists per game (asg)") +
  ylab("Share of maximum votes possible") + 
  theme_classic()


#Relationship between rebounds per game and points_won
ggplot(df , aes(trb_per_g, award_share)) +
  geom_point() +
  xlab("Rebounds per game (rpg)") +
  ylab("Share of maximum votes possible") + 
  theme_classic()


#Relationship between steals per game and points_won
ggplot(df , aes(stl_per_g, award_share)) + 
  geom_point() +
  xlab("steals per game (spg)") +
  ylab("Share of maximum votes possible") + 
  theme_classic()


#Relationship between blocks per game and points_won
ggplot(df , aes(blk_per_g, award_share))+ 
  geom_point() +
  xlab("Blocks per game (bpg)") +
  ylab("Share of maximum votes possible") +
  theme_classic()


#Relationship between win percentage per game and points_won
ggplot(df , aes(win_pct * 100, award_share))+ 
  geom_point() +
  xlab("Win percentage (%)") +
  ylab("Total points scored in a season") +
  theme_classic()



#///////////////////////////////////////Accuracy, Attempts, and Advanced metrics/////////////////////////////////////////////////////////////////////////


#Relationship between field goal attempts and points_won
ggplot(df , aes(fga, award_share)) + 
  geom_point() +
  xlab("Field goal attempts (fga)") +
  ylab("Share of maximum votes possible") +
  theme_classic()


#Relationship between 3 pt field goal attempts and points_won
ggplot(df , aes(fg3a ,award_share )) + 
  geom_point() +
  xlab("3-pt field goal attempts (fg3a)") +
  ylab("Share of maximum votes possible") +
  theme_classic()


#Relationship between free throw and points_won
ggplot(df , aes(fta, award_share)) + 
  geom_point() +
  xlab("Free throw attempts (fta)") +
  ylab("Share of maximum votes possible") +
  theme_classic()


############################################################################################

#Relationship between field goal percentage and points_won
ggplot(df , aes(fg_pct * 100, award_share)) + 
  geom_point() +
  xlab("Field goal percentage (%)") +
  ylab("Share of maximum votes possible") +
  theme_classic()


#Relationship between 3 pt field goal percentage and points_won
ggplot(df , aes(fg3_pct * 100, award_share)) + 
  geom_point() +
  xlab("3-pt field goal percentage (%)") +
  ylab("Share of maximum votes possible") +
  theme_classic()


#Relationship between free throw percentage and points_won
ggplot(df , aes(ft_pct * 100, award_share)) + 
  geom_point() +
  xlab("Free throw percentage (%)") +
  ylab("Share of maximum votes possible") +
  theme_classic()



#////////////////////////////////////////////////////////////////////////////////////////////////////////////


#Relationship between player efficiency rating and points_won
ggplot(df , aes(per, award_share)) + 
  geom_point() +
  xlab("Player Efficiency Rating (PER)") +
  ylab("Share of maximum votes possible") +
  theme_classic()


#Relationship between win share and points_won
ggplot(df , aes(ws, award_share)) + 
  geom_point() +
  xlab("Win share") +
  ylab("Share of maximum votes possible") +
  theme_classic()


#Relationship between win share per 48 minutes and points_won
ggplot(df , aes(ws_per_48, award_share))+ 
  geom_point() +
  xlab("Win share per 48 minutes") +
  ylab("Share of maximum votes possible") +
  theme_classic()


#Relationship between usage percentage and points_won
ggplot(df , aes(usg_pct * 100, award_share)) + 
  geom_point() +
  xlab("Usage percentage (%)") +
  ylab("Total points scored in a season") +
  theme_classic()
