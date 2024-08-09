#################################################
#    A simple XGB model with two randomly generated predictors and a binary outcome.
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


df <- data.frame(id=1:100)
df$A <- rnorm(100, 50, 10)
df$B <- sample(LETTERS[1:4], 100, replace=TRUE, prob=c(.15,.25, .4, .2))
df$C <- sample(LETTERS[1:2], 100, replace=TRUE, prob=c(.5,.5))


ctrl <- trainControl(method = "LOOCV",
                     classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = T, savePredictions = T, returnResamp = "final")

xgbGrid <- expand.grid(nrounds = c(20:30), 
                       max_depth = 2:4,
                       eta = c(0,.1,.2),
                       colsample_bytree = 1,
                       min_child_weight = c(0,.1),
                       subsample = c(.9,1),
                       gamma = c(0, .1))

xgb_mod <- train(C ~ A + B, 
                 data = df, method = "xgbTree", metric = "ROC",
                 trControl = ctrl, tuneGrid = xgbGrid, 
                 preProcess = c("center","scale"))


xgb_mod_ev <- evalm(xgb_mod)
xgb_mod_ev$roc


dev.copy(jpeg, filename="ROC.jpg");
dev.off ();








