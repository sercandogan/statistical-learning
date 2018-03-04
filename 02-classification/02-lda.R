# Linear Discriminat Analysis ----------------------------

# LOAD LIBRARY -------------------------------------------
library(ISLR)
library(MASS) # for lda function
library(tidyverse)

# DATASET ------------------------------------------------
data("Smarket")

train <- Smarket[Smarket$Year < 2005,]
test <- Smarket[!(Smarket$Year < 2005),]


# MODEL --------------------------------------------------

lda_fit <- lda(Direction ~ Lag1 + Lag2, data = train)

lda_fit

#' Coefficients of linear discriminat output provides the linear combination
#' of Lag1 and Lag2 that are used to form the LDA decision rule.
#' -0.64 * Lag1 + -0.51 * Lag2

plot(lda_fit)

# PREDICTION --------------------------------------------

lda_pred <- predict(lda_fit, test)
names(lda_pred)

#'> [1] "class"     "posterior" "x"  

table(lda_pred$class, test$Direction)

mean(lda_pred$class == test$Direction) # 0.55

