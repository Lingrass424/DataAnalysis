install.packages("ggthemes")

library(lubridate)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)

Caocodata <- read.csv("School/Data Analysis/ExplorDataAssign/flavors_of_cacao.csv", header = TRUE)

class(Caocodata)
dim(Caocodata)
names(Caocodata)
str(Caocodata)
glimpse(Caocodata)
summary(Caocodata)

head(Caocodata)
tail(Caocodata)

Caocodata$Cocoa.Percent <- gsub("\\%","",Caocodata$Cocoa.Percent)
head(Caocodata)

Caocodata$Cocoa.Percent <- as.numeric(Caocodata$Cocoa.Percent)

hist(Caocodata$Cocoa.Percent)
boxplot(Caocodata$Cocoa.Percent)

hist(CCdata$Rating)
boxplot(CCdata$Rating)

Caocodata$Bean.Type <- gsub("\\Â","",Caocodata$Bean.Type)
CCbean <- Caocodata$Bean.Type [Caocodata$Bean.Type==""]<-"NA"

Caocodata$Broad.Bean.Origin <- gsub("\\Â","",Caocodata$Broad.Bean.Origin)
CCorigin <- Caocodata$Broad.Bean.Origin [Caocodata$Broad.Bean.Origin==""]<-"NA"
sum(is.na(Caocodata))

CCdata <- Caocodata[,]
head(CCdata)

mean(CCdata$Rating)
median(CCdata$Rating)

maxrating <- max(CCdata$Rating)
print(maxrating)

minrating <- min(CCdata$Rating)
print(minrating)

glimpse(CCdata)
hist(CCdata$Rating)

unique(CCdata$Broad.Bean.Origin)
unique(CCdata$Bean.Type)

ggplot(CCdata,aes(x=Broad.Bean.Origin, y=Rating)) + geom_point()

summary(CCdata$Rating)

CCdata %>%
  summarise(meanRating = mean(Rating), 
            sdevRating = sd(Rating, na.rm=FALSE))

CCdata %>%
  summarise(meanCCpercent = mean(Cocoa.Percent), 
            sdevRating = sd(Cocoa.Percent, na.rm=FALSE))
  

ggplot(CCdata, aes(x= Cocoa.Percent, y = Rating)) + 
  geom_point() + 
  geom_jitter() 

ggplot(CCdata, aes(x= Cocoa.Percent, y = Rating, color = Company.Location)) + 
  geom_point() + 
  geom_jitter() 

ggplot(CCdata, aes(x= "meanCCpercent", y = "meanRating")) + 
  geom_point() + 
  geom_jitter() 

CoP_by_Rating <- CCdata %>%
  group_by(Cocoa.Percent) %>%
  summarise(meanRating = mean(Rating))

ggplot(CoP_by_Rating, aes(y= meanRating, x = Cocoa.Percent)) + 
  geom_point()+
  geom_line()

summary(CCdata)

