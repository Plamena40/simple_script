#################################################
#    A simple logistic regression model with two randomly generated predictors and a binary outcome.
#    ROC curve is printed as the result.
#    Plamena P. Powla
##################################################

library(dplyr)
library(tidyr)
library(tidyverse)
library(caret)
library(pROC)
library(ROCR)


df <- data.frame(id=1:50)
df$A <- rnorm(50, 50, 10)
df$B <- sample(LETTERS[1:4], 50, replace=TRUE, prob=c(.15,.25, .4, .2))
df$C <- sample(LETTERS[1:2], 50, replace=TRUE, prob=c(.5,.5))


ctrl <- trainControl(method = "CV", number = 5,
                     classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = T, savePredictions = T, returnResamp = "final")

glm_mod <- train(C ~ A + B, 
                 data = df, method = "glm", metric = "ROC",
                 trControl = ctrl,
                 preProcess = c("center","scale"))


glm_mod_ev <- evalm(glm_mod)
glm_mod_ev$roc


dev.copy(jpeg, filename="ROC.jpg");
dev.off ();








