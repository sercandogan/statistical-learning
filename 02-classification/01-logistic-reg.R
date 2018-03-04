# Logistic Regression ------------------------------------

# LOAD LIBRARY -------------------------------------------
library(ISLR)
library(tidyverse)

# DATASET ------------------------------------------------
glimpse(Smarket) # Data 

summary(Smarket)


cor(Smarket[,-9]) # Correlation Matrix

#' As one expect, the correlations between the lag variables and today's returns
#' are close to zero.

# MODEL --------------------------------------------------

glm_fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
             data = Smarket,
             family = binomial)

summary(glm_fit)

#' The negative coefficient for this predictor suggests that if the market had a 
#' positive return yesterday, then it is less likely to go up today.


# PREDICTION ----------------------------------------------

glm_probs <- predict(glm_fit, type = "response")

contrasts(Smarket$Direction)

glm_pred <- rep("Down",1250)
glm_pred[glm_probs > 0.5] <- "Up"

# CONFUSION MATRIX ----------------------------------------

table(glm_pred,Smarket$Direction)

#' glm_pred Down  Up
#' Down  145 141
#' Up    457 507

(145 + 507) / 1250 # Correct predictions
mean(glm_pred == Smarket$Direction)

1 - (145 + 507) / 1250 # training error rate

# ---------------------------------------------------------

train <- Smarket[Smarket$Year < 2005,]
test <- Smarket[!(Smarket$Year < 2005),]

glm_fit2 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = train,
               family = binomial)

glm_probs2 <- predict(glm_fit2,test,type = "response") # probs

glm_pred2 <- rep("Down",252)
glm_pred2[glm_probs2 > 0.5] <- "Up"

table(glm_pred2, test$Direction)

#' glm_pred2 Down Up
#' Down   77 97
#' Up     34 44


mean(glm_pred2 == test$Direction) #'> 0.48
mean(glm_pred2 != test$Direction) #'> 0.52 test error rate

#' The result are rather disappointing: the test error rate is 52%, which is
#' worse than random guessing!

# NEW MODEL ---------------------------------------------

glm_fit3 <- glm_fit2 <- glm(Direction ~ Lag1 + Lag2, 
                            data = train,
                            family = binomial)

summary(glm_fit3)

glm_probs3 <- predict(glm_fit3,test,type = "response") # probs

glm_pred3 <- rep("Down",252)
glm_pred3[glm_probs3 > 0.5] <- "Up"

table(glm_pred2, test$Direction)

mean(glm_pred2 == test$Direction) #'> 0.48 --> Same
mean(glm_pred2 != test$Direction) #'> 0.52 test error rate --> Same

