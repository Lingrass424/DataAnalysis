install.packages("gmodels")
library(tidyverse)
library(gmodels)

bodytemp <- rnorm(10000, mean=97.82, sd=.69)
glimpse(bodytemp)
hist(bodytemp)

set.seed(1234)

bodysample <- sample(bodytemp, 10)
mean(bodysample)

bodysample <- sample(bodytemp, 100)
mean(bodysample)

bodysample <- sample(bodytemp, 1000)
mean(bodysample)

our_sample <- numeric(10000)
for(i in 1:10000){
  a_sample <- sample(bodytemp, 50)
  our_sample[i] <- mean(a_sample)}


hist(our_sample, breaks = 50)

temp_mean <- mean(bodytemp)
temp_stdev <- sd(bodytemp, na.rm = TRUE)
sample_size = length(bodytemp)

temp_mean

error_n <- qnorm(0.975)*temp_stdev/sqrt(sample_size)
left_n <- temp_mean - error_n
right_n <- temp_mean + error_n

error_t <- qt(0.975, df=sample_size-1)
left_t <- temp_mean - error_t
right_t <- temp_mean + error_t

print(left_n)
print(right_n)
print(left_t)
print(right_t)

ci(bodytemp, confidence= .95)

t.test(bodytemp, mu=temp_mean, conf.level = .95)

realtemps <- read.csv("School/Data Analysis/Labs/Normtemp.csv", header = TRUE)
glimpse(realtemps)

realtemps$Gender <- as.factor(realtemps$Gender)
glimpse(realtemps)

realtemps$ï..Body.Temp <- as.integer(realtemps$ï..Body.Temp)

glimpse(bodytemp)

realtemps$ï..Body.Temp <- as.numeric(realtemps$ï..Body.Temp)
summary(realtemps) 

hist(realtemps$ï..Body.Temp)

body_mean = mean(realtemps$ï..Body.Temp)
t.test(realtemps$ï..Body.Temp, mu= body_mean, conf.level = .95)
