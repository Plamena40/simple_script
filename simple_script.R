#################################################
#    A simple elastic net model with generated data
#    Model summary is printed as the result.
#    Plamena P. Powla
##################################################

library(dplyr)
library(tidyr)
library(caret)


df <- data.frame(id=1:100000)

b0 <- -10
b1 <- 1.5
b2 <- 2
df$A <- rnorm(1000,0,3)
df$B <- df$A^2
df$e <- rnorm(1000,1,10)
df$p <- exp(b0+b1*df$A+b2*df$B+df$e)/(1+exp(b0+b1*df$A+b2*df$B+df$e))
df$C <- rbinom(1000,1, prob = df$p)
df$C <- ifelse(df$C == 1, "A", "B")


ctrl <- trainControl(method = "CV", number = 5,
                     classProbs = TRUE, summaryFunction = twoClassSummary,
                     verboseIter = T, savePredictions = T, returnResamp = "final")

glm_mod <- train(C ~ A + B, 
                 data = df, method = "glm", metric = "ROC",
                 trControl = ctrl,
                 preProcess = c("center","scale"))

capture.output(print(glm_mod), file = "output.txt")
