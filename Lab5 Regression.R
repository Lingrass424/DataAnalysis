install.packages("broom")
install.packages("GGally")

library(tidyverse)
library(lubridate)
library(broom)
library(GGally)

?trees

data(trees)
glimpse(trees)

ggplot(trees,aes(x=trees$Girth, y=trees$Height)) + geom_point()+
  labs(title="Tree height by girth")

ggplot(trees,aes(x=trees$Girth, y=trees$Height)) + geom_point()+ 
  stat_smooth(method = "lm",se = FALSE)+
  labs(title = "Tree height by girth")

#linear model dependent then indipendent. what you want to predict first
lm_trees <- lm(Height ~ Girth, data = trees)
lm_trees

summary(lm_trees)
coef(lm_trees)

#what the model would have predicted 
fitted_trees <- fitted.values(lm_trees)
fitted_trees

# residuals noise how much was my model off from what was actually measured
residual_trees <- residuals(lm_trees)
residual_trees

# investigating strategially especially with a lot of data
# joins the origional data set with the linear model and the residual 
lm_matrix_trees <- broom::augment(lm_trees)
head(lm_matrix_trees)

lm_matrix_trees %>%
  arrange(desc(.resid)) %>%
  head()

#absolute value
lm_matrix_trees$.resid_abs <- abs(lm_matrix_trees$.resid)
lm_matrix_trees %>%
  arrange(desc(.resid_abs)) %>%
  head()

trees %>% filter(Girth ==13.8)

new_trees <- data.frame("Girth" = 19.0)
predict(lm_trees, newdata = new_trees)

mytree <- broom::augment(lm_trees, newdata = new_trees)
mytree

ggplot(data = trees, aes(x=Girth, y=Height)) + 
  stat_smooth(method="lm") + 
  geom_point(data = mytree, aes(y= .fitted),size=3, color="red") + 
  labs("Tree height by girth")

tree_null <- lm(Height ~ 1, data = trees)
tree_null

mean(trees$Height)

ggplot(data = trees, aes(x=Girth, y=Height)) + 
  geom_point() + 
  geom_hline(yintercept = 76) + 
  labs("Tree height with null model")

ggplot(data = trees, aes(x=Girth, y=Height)) + 
  geom_point() + 
  stat_smooth(method = lm) +
  geom_hline(yintercept = 76) + 
  labs("Tree height with null model")

#standard error how much outside of the line is accounted for by the model
ggplot(data = trees, aes(x=Girth, y=Height)) + 
  geom_point() + 
  stat_smooth(method = lm) +
  labs("Tree height with null model")

#how good is our model - Multple R squared
summary(lm_trees)

library(GGally)
ggpairs(data = trees, columns = 1:3)

# create linear models 
# for how 2 different variables predict employment rates
#dependant first
# which model is better able to predict the number of people who are employed 

data(longley)
?longley

glimpse(longley)

longley.x <- data.matrix(longley[,1:6])
longley.y <- longley[, "Employed"]

ggplot(longley,aes(x=longley$GNP, y=longley$Employed)) + geom_point()+
  labs(title="GNP by Employment")

ggplot(longley,aes(x=longley$GNP.deflator, y=longley$Employed)) + geom_point()+ 
  stat_smooth(method = "lm",se = FALSE)+
  labs(title = "GNP by EMployment Line")

#linear model dependent then indipendent. what you want to predict first
lm_longley <- lm(Employed ~ GNP, data = longley)
lm_longley

summary(lm_longley)
coef(lm_longley)

fitted_longley <- fitted.values(lm_longley)
fitted_longley

residual_longley <- residuals(lm_longley)
residual_longley

lm_matrix_longley <- broom::augment(lm_longley)
head(lm_matrix_longley)

lm_matrix_longley %>%
  arrange(desc(.resid)) %>%
  head()

#absolute value
lm_matrix_longley$.resid_abs <- abs(lm_matrix_longley$.resid)
lm_matrix_longley %>%
  arrange(desc(.resid_abs)) %>%
  head()

longley %>% filter(GNP.deflator ==83)

new_longley <- data.frame("GNP" = 88.0)
predict(lm_longley, newdata = new_longley)

mylongley <- broom::augment(lm_longley, newdata = new_longley)
mylongley

ggplot(data = longley, aes(x=GNP, y=Employed)) + 
  stat_smooth(method="lm") + 
  geom_point(data = mylongley, aes(y= .fitted),size=3, color="red") + 
  labs("GND by Employment")

longley_null <- lm(Employed ~ 1, data = longley)
longley_null

mean(longley$Employed)

ggplot(data = longley, aes(x=GNP, y=Employed)) + 
  geom_point() + 
  geom_hline(yintercept = 76) + 
  labs("longley with null model")

ggplot(data = longley, aes(x=GNP, y=Employed)) + 
  geom_point() + 
  stat_smooth(method = lm) +
  geom_hline(yintercept = 65.32) + 
  labs("ongley with null modell")

#standard error how much outside of the line is accounted for by the model
ggplot(data = longley, aes(x=GNP, y=Employed)) + 
  geom_point() + 
  stat_smooth(method = lm) +
  labs("longley with null model")

summary(lm_longley)
