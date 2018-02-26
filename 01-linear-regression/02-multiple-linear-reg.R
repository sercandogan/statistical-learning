# Multiple Linear Regression -----------------------------

# LOAD LIBRARY -------------------------------------------
library(MASS)
library(ISLR)
library(car) # for vif function
# DATASET ------------------------------------------------
View(Boston)

g <- lm(medv ~ lstat + age, data = Boston) # model
summary(g)

g <- g <- lm(medv ~ . , data = Boston) # all of the predictors
summary(g)

# VIF ----------------------------------------------------

vif(g)


g1 <- lm(medv ~. -age, data = Boston) # age has high p-value
summary(g1)
