#################################################
#    An XGB model with tuning grid of size 129600
#
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
library(ResourceSelection)
library(PresenceAbsence)



TBI_data <- read.csv("/Users/plamena/Desktop/mimic-iv-2.2/TBI_data_all")




df <- TBI_data %>% filter(!GCS_arrival >= 13) %>% 
  filter(!is.na(pupil_reactivity_arrival)) %>% 
  filter(!is.na(anchor_age.x)) %>% 
  filter(!is.na(GCS_M_arrival)) %>% filter(!is.na(dead_at24hrs))

df$Mortality_outcome <- ifelse(df$discharge_location.y == "DIED", "yes", "no")
df$Mortality_outcome2 <- ifelse(df$discharge_location.y == "DIED", "Died", "Lived")
df$Mortality_outcome_bin <- ifelse(df$discharge_location.y == "DIED", 1, 0)

ctrl <- trainControl(method = "LOOCV",
                     classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = T, savePredictions = T, returnResamp = "final")

xgbGrid <- expand.grid(nrounds = c(1:50), 
                       max_depth = 1:6,
                       eta = c(0,.1,.2,.3,.4,.5),
                       colsample_bytree = 1,
                       min_child_weight = c(0,.1,.2,.3),
                       subsample = c(.75,.8,.85,.9,.95,1),
                       gamma = c(0, .1, .2))

xgb_mod <- train(Mortality_outcome ~ GCS_M_arrival + 
                   pupil_reactivity_arrival + anchor_age.x, 
                 data = df, method = "xgbTree", metric = "ROC",
                 trControl = ctrl, tuneGrid = xgbGrid, 
                 preProcess = c("center","scale"))


xgb_mod_ev <- evalm(xgb_mod)
xgb_mod_ev$stdres








