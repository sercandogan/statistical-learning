# Quadratic Discriminat Analysis ----------------------------

# LOAD LIBRARY -------------------------------------------
library(ISLR)
library(MASS) # for qda function
library(tidyverse)

# DATASET ------------------------------------------------
data("Smarket")

train <- Smarket[Smarket$Year < 2005,]
test <- Smarket[!(Smarket$Year < 2005),]


# MODEL --------------------------------------------------

qda_fit <- qda(Direction ~ Lag1 + Lag2, data = train)

qda_fit

qda_class <- predict(qda_fit, test)$class

table(qda_class, test$Direction) # Confusion Matrix

1 - mean(qda_class == test$Direction) # 1 - 0.6 = 0.4 test error rate





