#################################################
#    A simple RF model with two randomly generated predictors and a binary outcome.
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


df <- data.frame(id=1:50)
df$A <- rnorm(50, 50, 10)
df$B <- sample(LETTERS[1:4], 50, replace=TRUE, prob=c(.15,.25, .4, .2))
df$C <- sample(LETTERS[1:2], 50, replace=TRUE, prob=c(.5,.5))


ctrl <- trainControl(method = "CV", number = 5,
                     classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = T, savePredictions = T, returnResamp = "final")

rf_grid <- expand.grid(mtry=c(1, 2))

rf_mod <- train(C ~ A + B, 
                 data = df, method = "rf", metric = "ROC",
                 trControl = ctrl, tuneGrid = rf_grid, 
                 preProcess = c("center","scale"))


rf_mod_ev <- evalm(rf_mod)
rf_mod_ev$roc


dev.copy(jpeg, filename="ROC.jpg");
dev.off ();








