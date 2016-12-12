library(foreign)
library(caret)
library(devtools)
library(gsubfn)
library(ggplot2)
library(reshape2)
source('data_split.R')
source('threshold.R')

setwd('~/projects/r/final_project/')
seed <- 33
set.seed(33)

# Data cleanup, and division to train, dev-test, and test sets
temporal.df <- read.delim('sporozoite temporal dynamic.txt')
temporal.df$carriers <- as.factor(temporal.df$carriers)
temporal.df$infection <- as.factor(ifelse(temporal.df$infection == 1, 'yes', 'no'))
list[temporal.train, temporal.dev, temporal.test] <- split.train.dev.test(temporal.df)

# Using a random forest
set.seed(seed)
temporal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                             summaryFunction = twoClassSummary, 
                             classProbs = TRUE)
# rf.tune.grid <- expand.grid(mtry=c(12, 14, 16))
# nnet.tune.grid <- expand.grid(.size = seq(3, 9, 2),
#                              .decay = seq(0.25, 1.75, 0.5))
temporal.rf.model <- train(infection ~ treatment + time + carriers, 
                             data=temporal.train, method='rf', trControl = temporal.control, 
                             preProcess = c('center', 'scale'),  
                             # tuneGrid = rf.tune.grid,
                             metric = "ROC", 
                             verbose = TRUE)
getTrainPerf(temporal.rf.model)
importance(temporal.rf.model$finalModel)
ggplot(temporal.rf.model)
dev.temporal.rf.classes <- predict(temporal.rf.model, newdata = temporal.dev)
confusionMatrix(data=dev.temporal.rf.classes, temporal.dev$infection)

# Using a logit:
set.seed(seed)
temporal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                 summaryFunction = twoClassSummary, 
                                 classProbs = TRUE)
# rf.tune.grid <- expand.grid(mtry=c(12, 14, 16))
# nnet.tune.grid <- expand.grid(.size = seq(3, 9, 2),
#                              .decay = seq(0.25, 1.75, 0.5))
temporal.glm.model <- train(infection ~ treatment + time + carriers, 
                           data=temporal.train, method='glm', trControl = temporal.control, 
                           preProcess = c('center', 'scale'),  
                           # tuneGrid = rf.tune.grid,
                           metric = "ROC")
getTrainPerf(temporal.glm.model)
summary(temporal.glm.model)
dev.temporal.glm.classes <- predict(temporal.glm.model, newdata = temporal.dev)
confusionMatrix(data=dev.temporal.glm.classes, temporal.dev$infection)