library(tidyverse)
library(lubridate)
library(broom)
library(GGally)
library(dplyr)
install.packages("viridis")
library(ggplot2)
library(ggthemes)
library(viridis)

Happy16 <- read.csv("School/Data Analysis/PredictDataAssign/Happy2016.csv", header = TRUE)
class(Happy16)
dim(Happy16)
names(Happy16)
str(Happy16)
glimpse(Happy16)
summary(Happy16)
head(Happy16)
tail(Happy16)

Opendata <- read.csv("School/Data Analysis/PredictDataAssign/ODB16.csv", header = TRUE)
class(Opendata)
dim(Opendata)
names(Opendata)
str(Opendata)
glimpse(Opendata)
summary(Opendata)
head(Opendata)
tail(Opendata)

#combining
open_data_happiness <- Opendata %>%
  left_join(Happy16, by = "Country") %>%
  select(Country,Happiness.Score,ODB.Score.Scaled)

head(open_data_happiness)  
glimpse(open_data_happiness)  
class(open_data_happiness)
dim(open_data_happiness)
names(open_data_happiness)
str(open_data_happiness)
summary(open_data_happiness)

ggplot(open_data_happiness,aes(x=open_data_happiness$ODB.Score.Scaled, y=open_data_happiness$Happiness.Score )) + geom_point()+ 
  stat_smooth(method = "lm",se = FALSE)+
  labs(x="Openness Score", y="Happiness Score",
        title = "Data openness and happiness by country")
       
lm_HappyData <- lm(Happiness.Score  ~ ODB.Score.Scaled, data = open_data_happiness)
lm_HappyData
       
summary(lm_HappyData)
coef(lm_HappyData)

#what the model predicted       
fitted_HappyData <- fitted.values(lm_HappyData)
fitted_HappyData
       
# difference between actual and predicted
residual_HappyData <- residuals(lm_HappyData)
residual_HappyData
       
# joins the origional data set with the linear model and the residual 
lm_matrix_HappyData <- broom::augment(lm_HappyData)
head(lm_matrix_HappyData)
       
lm_matrix_HappyData %>%
   arrange(desc(.resid)) %>%
   head()
       
#absolute value
lm_matrix_HappyData$.resid_abs <- abs(lm_matrix_HappyData$.resid)
       lm_matrix_HappyData %>%
         arrange(desc(.resid_abs)) %>%
         head()
       
ggplot(open_data_happiness,aes(x=open_data_happiness$ODB.Score.Scaled, 
                               y=open_data_happiness$Happiness.Score )) + 
  geom_smooth(method = "lm") +
         labs(x="Openness Score", y="Happiness Score",
              title = "Data openness and happiness by country")       

cov(open_data_happiness$ODB.Score.Scaled, open_data_happiness$Happiness.Score)
cor(open_data_happiness$ODB.Score.Scaled, open_data_happiness$Happiness.Score)

res <- cor.test (open_data_happiness$ODB.Score.Scaled, open_data_happiness$Happiness.Score, method = "pearson")
res
     