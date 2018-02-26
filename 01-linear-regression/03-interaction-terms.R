# Interaction Terms --------------------------------------

# LOAD LIBRARY -------------------------------------------
library(MASS)
library(ISLR)

# DATASET ------------------------------------------------

View(Boston)

# MODEL --------------------------------------------------

g <- lm(medv ~ lstat * age, data = Boston)
summary(g)
