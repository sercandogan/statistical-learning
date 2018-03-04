# K-Nearest Neighbors ------------------------------------

# LOAD LIBRARY -------------------------------------------
library(ISLR)
library(class) # for knn function
library(tidyverse)

# DATASET ------------------------------------------------
data("Smarket")

train_x <- cbind(Smarket$Lag1, Smarket$Lag2)[Smarket$Year < 2005,]
test_x <- cbind(Smarket$Lag1, Smarket$Lag2)[!(Smarket$Year < 2005),]
train_res <- Smarket$Direction[Smarket$Year < 2005]
test_res <- Smarket$Direction[!(Smarket$Year < 2005)]

# MODEL --------------------------------------------------

set.seed(04032018)
knn_pred <- knn(train_x, test_x, train_res, k = 3)

table(knn_pred,test_res)

mean(knn_pred == test_res) # 0.53

# An Application to Caravan Insurance Data --------------

data("Caravan")

glimpse(Caravan)

summary(Caravan)

# STANDARDIZE the data -----------------------------------

standardized_x <- scale(Caravan[,-86])

var(Caravan[,1])
# [1] 165.0378
var(standardized_x[,1])
# [1] 1

# ---------------------------------------------------------
test_samp <- 1:1000

train_x <- standardized_x[-test_samp,]
test_x <- standardized_x[test_samp,]
train_y <- Caravan$Purchase[-test_samp]
test_y <- Caravan$Purchase[test_samp]

set.seed(1636)

knn_pred <- knn(train_x, test_x, train_y, k = 5) # k = 3, k = 5

mean(test_y != knn_pred) # 0.117 test error rate

table(knn_pred, test_y)
