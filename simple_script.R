#################################################
#    An XGB model with tuning grid of size 3840 and sample size of 1000
#    binary outcome variable and two randomly generated predictors.
#    ROC curve is printed as the result.
#    Plamena P. Powla
##################################################

library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(caret)
library(pROC)
library(ROCR)
library(xgboost)


df <- data.frame(id=1:1000)
df$Age <- rnorm(1000, 50, 10)

df$Mortality <- sample(LETTERS[1:2], 1000, replace=TRUE, prob=c(.5,.5))

df$Injury <- sample(LETTERS[1:4], 1000, replace=TRUE, prob=c(.15,.25, .4, .2))


ctrl <- trainControl(method = "LOOCV",
                     classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = T, savePredictions = T, returnResamp = "final")

xgbGrid <- expand.grid(nrounds = c(15:30), 
                       max_depth = 2:6,
                       eta = c(0,.1,.2,.3),
                       colsample_bytree = 1,
                       min_child_weight = c(0,.1),
                       subsample = c(.8,.9,1),
                       gamma = c(0, .1))

xgb_mod <- train(Mortality ~ Age + Injury, 
                 data = df, method = "xgbTree", metric = "ROC",
                 trControl = ctrl, tuneGrid = xgbGrid, 
                 preProcess = c("center","scale"))


xgb_mod_ev <- evalm(xgb_mod)
xgb_mod_ev$roc


dev.copy(jpeg, filename="ROC.jpg");
dev.off ();








