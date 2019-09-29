library(lubridate)
library(dplyr)
library(tidyverse)

meals <- read.csv("mealplan.csv" , header =TRUE)

class(meals)
dim(meals)
names(meals)
