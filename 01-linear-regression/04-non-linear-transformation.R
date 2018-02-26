# Non-Linear Transformations of the Predictors

# LOAD LIBRARY -------------------------------------------
library(MASS)
library(ISLR)

# DATASET ------------------------------------------------

View(Boston)

# MODEL --------------------------------------------------

g1 <- lm(medv ~ lstat, data = Boston)
g2 <- lm(medv ~ lstat + I(lstat ^ 2), data = Boston)

anova(g1,g2)


# Log transformation
summary(lm(medv ~ log(rm), data = Boston))



