library(tidyverse)
library(lubridate)
library(broom) 
library(GGally) 
library(ggplot2)
library(dplyr)

sentiment <- read.csv("School/Data Analysis/Final Project/sotu_senitment_bing 6 .csv", header=TRUE)
sentiment

approval <- read.csv("School/Data Analysis/Final Project/Pres_AprvforSent.csv", header=TRUE)
approval

sotu_total <-read.csv("School/Data Analysis/Final Project/Pres_AprvforSent.csv", header=TRUE)
sotu_total

#regPlot
ggplot(sotu_total, aes(x=sotu_total$AvgSentpos, y=sotu_total$AvgAppr)) + geom_point()+
  stat_smooth(method="lm", se=FALSE)+
  labs(title="Presidential Approval by Sentiment")

lm_sotu <- lm(AvgSentpos ~ AvgAppr, data=sotu_total)
lm_sotu

summary(lm_sotu)
fitted_sotu <- fitted.values(lm_sotu)
fitted_sotu

residual_sotu <- residuals(lm_sotu)
residual_sotu
lm_matrix_sotu <- broom::augment(lm_sotu)
head(lm_matrix_sotu)

lm_matrix_sotu %>%
  arrange(desc(.resid)) %>%
  head()

lm_matrix_sotu$.resid_abs <- abs(lm_matrix_sotu$.resid)
lm_matrix_sotu %>%
  arrange(desc(.resid_abs)) %>%
  head()

ggplot(sotu_total, aes(x=sotu_total$AvgSentpos, y=sotu_total$AvgAppr)) + geom_point()+
  stat_smooth(method="lm", se=FALSE)+
  labs(x= "Sentiment",y="Approval",
       title="Presidential Approval by Sentiment")

#linearplot with confidence trend
lm_sotu_plot <- ggplot(lm_matrix_sotu, 
                       aes(x=lm_matrix_sotu$AvgSentpos, y=lm_matrix_sotu$AvgAppr)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="blue", se=TRUE) +
  labs(x= "Sentiment",y="Approval",
       title="Presidential Approval by Sentiment")+
  theme_classic()
lm_sotu_plot
res <- cor.test (lm_matrix_sotu$AvgSentpos, lm_matrix_sotu$AvgAppr, method = "pearson")
res