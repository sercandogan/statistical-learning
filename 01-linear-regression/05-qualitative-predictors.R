# Qualitative Predictions

# LOAD LIBRARY -------------------------------------------
library(MASS)
library(ISLR)

# DATASET ------------------------------------------------

View(Carseats)
str(Carseats)

# MODEL --------------------------------------------------

g <- lm(Sales ~. + Income:Advertising + Price:Age, data = Carseats)
summary(g)
