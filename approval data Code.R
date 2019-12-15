install.packages("RColorBrewer")

library(lubridate)
library(dplyr)
library(tidyverse)
library(tsbox)
library(ggplot2)
library(RColorBrewer)
Approval <- read.csv('School/Data Analysis/Final Project/Pres_Aprv.csv', header = TRUE)

class(Approval)
dim(Approval)
names(Approval)
summary(Approval)

ggplot(Approval) +
  geom_segment( aes(x=President, xend=President, y=LowApproval, yend=HighApproval), color="grey") +
    geom_point( aes(x=President, y=LowApproval), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=President, y=HighApproval), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip()+
  theme_classic() +
  theme(
    legend.position = "none",
  ) +
  xlab("President") +
  ylab("Approval Rating")


#This is the graph I Want
coul <- brewer.pal(5, "Set3") 
barplot(height=Approval$AvgAppr, 
        names.arg = Approval$President, 
        col=coul,
        las = 2)

par(mar=c(11,4,4,4))


ggplot(Approval) + geom_point(aes( colour = AvgAppr <53, size = 6)) +
  scale_colour_manual(name = 'President > 0', values = setNames(c('red','green'),c(T, F))) +  
  coord_flip()+
  theme(legend.position = "none",)+
  xlab('President') + ylab('Average Approval')

ggplot(Approval) + geom_bar(aes(x = President, y =AvgAppr, colour = AvgAppr <53, size = 6)) +
  scale_colour_manual(name = 'President > 0', values = setNames(c('red','green'),c(T, F))) +  
  theme(legend.position = "none",)+
  xlab('President') + ylab('Average Approval')