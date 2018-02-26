# Simple Linear Regression -----------------------------

# LOAD LIBRARY -----------------------------------------
library(MASS)
library(ISLR)

# DATASET ----------------------------------------------

# Boston data set from MASS library
names(Boston) # 13
#' >> [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"     "dis"    
#' >> [9] "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"

g <- lm(medv ~ lstat, data = Boston)
summary(g)

confint(g) # Confidence interval for coefficient estimates


# confidence interval
predict(g, data.frame(lstat = c(5,10,15)), interval = "confidence")

# prediction interval
predict(g, data.frame(lstat = c(5,10,15)), interval = "prediction") 

plot(Boston$lstat, Boston$medv)
abline(g)


par(mfrow = c(2,2))
plot(g) # plotting model


# Leverage Statistic
par(mfrow = c(1,1))
plot(hatvalues(g))
which.max(hatvalues(g)) # largest leverage

