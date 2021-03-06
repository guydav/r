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
eggs.df <- read.delim('oocyst and egg.txt')
eggs.df$carriers <- as.factor(eggs.df$carriers)
eggs.df$infection <- as.factor(ifelse(eggs.df$infection == 1, 'yes', 'no'))
eggs.df$eggs <- as.factor(ifelse(eggs.df$eggs == 1, 'yes', 'no'))
eggs.df$date <- as.Date(eggs.df$date, format = "%d/%m/%Y")
eggs.df$log.oocyst <- log(eggs.df$oocyst + 1)
eggs.df$has.oocyst <- as.factor(ifelse(eggs.df$oocyst == 0, 'no', 'yes'))
list[eggs.train, eggs.dev, eggs.test] <- split.train.dev.test(eggs.df)

# Using a random forest
set.seed(seed)
eggs.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                             # summaryFunction = twoClassSummary, 
                             classProbs = TRUE)
# rf.tune.grid <- expand.grid(mtry=c(12, 14, 16))
# nnet.tune.grid <- expand.grid(.size = seq(3, 9, 2),
#                              .decay = seq(0.25, 1.75, 0.5))
eggs.rf.model <- train(eggs ~ treatment + I(treatment : infection) + carriers + log.oocyst + infection + replicate + gametocyt + date, 
                             data=eggs.train, method='rf', trControl = eggs.control, 
                             preProcess = c('center', 'scale'),  
                             # tuneGrid = rf.tune.grid,
                             metric = "Kappa", 
                             verbose = TRUE)
getTrainPerf(eggs.rf.model)
importance(eggs.rf.model$finalModel)
ggplot(eggs.rf.model)
dev.eggs.rf.classes <- predict(eggs.rf.model, newdata = eggs.dev)
confusionMatrix(data=dev.eggs.rf.classes, eggs.dev$eggs)

# Using AdaBoost Classification Trees
set.seed(seed)
eggs.control <- trainControl(method="repeatedcv", number=10, repeats=1,
                             # summaryFunction = twoClassSummary, 
                             classProbs = TRUE)
# rf.tune.grid <- expand.grid(mtry=c(1:6))
# nnet.tune.grid <- expand.grid(.size = seq(3, 9, 2),
#                              .decay = seq(0.25, 1.75, 0.5))
eggs.adaboost.model <- train(eggs ~ treatment + I(treatment : infection) + carriers + log.oocyst + infection + replicate + gametocyt + date, 
                    data=eggs.train, method='adaboost', trControl = eggs.control, 
                    preProcess = c('center', 'scale'),  
                    # tuneGrid = rf.tune.grid,
                    metric = "Kappa", 
                    verbose = FALSE)
getTrainPerf(eggs.adaboost.model)
ggplot(eggs.adaboost.model)
dev.eggs.adaboost.classes <- predict(eggs.adaboost.model, newdata = eggs.dev)
confusionMatrix(data=dev.eggs.adaboost.classes, eggs.dev$eggs)

# Using GBM (boosting)
set.seed(seed)
eggs.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                             # summaryFunction = twoClassSummary, 
                             classProbs = TRUE)
gbm.tune.grid <- expand.grid(shrinkage=c(0.1, 0.01, 0.001), n.trees=c(150, 200, 250), 
                             interaction.depth=c(1, 2, 3), n.minobsinnode=c(10))
eggs.gbm.model <- train(eggs ~ treatment + I(treatment : infection) + carriers + oocyst + infection + replicate + gametocyt + date, 
                             data=eggs.train, method='gbm', trControl = eggs.control, 
                             preProcess = c('center', 'scale'),  
                             tuneGrid = gbm.tune.grid,
                             metric = "Kappa", 
                             verbose = FALSE)
getTrainPerf(eggs.gbm.model)
ggplot(eggs.gbm.model)
summary(eggs.gbm.model)
dev.eggs.gbm.classes <- predict(eggs.gbm.model, newdata = eggs.dev)
confusionMatrix(data=dev.eggs.gbm.classes, eggs.dev$eggs)

# Run test set on gbm model
test.eggs.gbm.classes <- predict(eggs.gbm.model, newdata = eggs.test)
confusionMatrix(data=test.eggs.gbm.classes, eggs.test$eggs)

set.seed(seed)
threshold.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = fourStats, classProbs = TRUE)
# eggs.tune.grid <- expand.grid(mtry=c(1:2), threshold=seq(0.01, 0.99, 0.02))
# nnet.tune.grid <- expand.grid(.size = seq(3, 9, 2),
#                              .decay = seq(0.25, 1.75, 0.5))
eggs.model <- train(eggs ~ treatment + I(treatment : infection) + carriers + log.oocyst + infection + replicate + gametocyt + date, 
                          data=eggs.train, method=thresh_code, trControl = threshold.control, 
                          preProcess = c('center', 'scale'),  
                          # tuneGrid = eggs.tune.grid,
                          metric = "Dist", maximize = FALSE, tuneLength = 20, ntree = 1000,
                          verbose = FALSE)
getTrainPerf(eggs.model)
ggplot(eggs.model)
importance(eggs.model$finalModel, type=2)
dev.classes <- predict(eggs.model, newdata = eggs.dev)
confusionMatrix(data=dev.classes, eggs.dev$eggs)

metrics <- eggs.model$results[, c(2, 4:6)]
metrics <- melt(metrics, id.vars = "threshold", 
                variable.name = "Resampled",
                value.name = "Data")

ggplot(metrics, aes(x = threshold, y = Data, color = Resampled)) + 
    geom_line() + 
    ylab("") + xlab("Probability Cutoff") +
    theme(legend.position = "top")

# Model everything on oocyst
set.seed(seed)
oocyst.control <- trainControl(method="repeatedcv", number=10, repeats=3)
oocyst.rf.model <- train(oocyst ~ treatment + infection + I(treatment : infection) + carriers + replicate + gametocyt + date, # I(treatment : infection) +
                    data=eggs.train, method='rf', trControl = oocyst.control, 
                    preProcess = c('center', 'scale'),  
                    # tuneGrid = eggs.tune.grid,
                    # metric = 'ROC',
                    verbose = FALSE)
getTrainPerf(oocyst.rf.model)
importance(oocyst.rf.model$finalModel, type = 2)
ggplot(oocyst.rf.model)
dev.oocyst.rf.pred <- predict(oocyst.rf.model, newdata = eggs.dev)
dev.oocyst.rf.rmse <-  sqrt(mean((dev.oocyst.rf.pred - eggs.dev$oocyst) ** 2))
dev.oocyst.rf.rmse

# Oocyst with gbm
set.seed(seed)
oocyst.control <- trainControl(method="repeatedcv", number=10, repeats=3)
gbm.tune.grid <- expand.grid(shrinkage=c(0.1, 0.01, 0.001), n.trees=c(150, 200, 250), 
                             interaction.depth=c(1, 2, 3), n.minobsinnode=c(10))
oocyst.gbm.model <- train(oocyst ~ treatment + infection + I(treatment : infection) +  carriers + replicate + gametocyt + date, # I(treatment : infection) +
                         data=eggs.train, method='gbm', trControl = oocyst.control, 
                         preProcess = c('center', 'scale'),  
                         tuneGrid = gbm.tune.grid,
                         # metric = 'ROC',
                         verbose = FALSE)
getTrainPerf(oocyst.gbm.model)
ggplot(oocyst.gbm.model)
summary(oocyst.gbm.model)
dev.oocyst.gbm.pred <- predict(oocyst.gbm.model, newdata = eggs.dev)
dev.oocyst.gbm.rmse <-  sqrt(mean((dev.oocyst.gbm.pred - eggs.dev$oocyst) ** 2))
dev.oocyst.gbm.rmse

# Oocyst with glmStepAIC
set.seed(seed)
oocyst.control <- trainControl(method="repeatedcv", number=10, repeats=3)
oocyst.glm.model <- train(oocyst ~ treatment + infection + I(treatment : infection) + carriers + replicate + gametocyt + date,
                          data=eggs.train, method='glmStepAIC', trControl = oocyst.control, 
                          preProcess = c('center', 'scale'),  
                          # metric = 'ROC',
                          verbose = FALSE)

getTrainPerf(oocyst.glm.model)
ggplot(oocyst.glm.model)
summary(oocyst.glm.model)
dev.oocyst.glm.pred <- predict(oocyst.glm.model, newdata = eggs.dev)
dev.oocyst.glm.rmse <-  sqrt(mean((dev.oocyst.glm.pred - eggs.dev$oocyst) ** 2))
dev.oocyst.glm.rmse

# predict test set with both glm and gbm models
test.oocyst.glm.pred <- predict(oocyst.glm.model, newdata = eggs.test)
test.oocyst.glm.rmse <-  sqrt(mean((test.oocyst.glm.pred - eggs.test$oocyst) ** 2))
test.oocyst.glm.rmse

test.oocyst.gbm.pred <- predict(oocyst.gbm.model, newdata = eggs.test)
test.oocyst.gbm.rmse <-  sqrt(mean((test.oocyst.gbm.pred - eggs.test$oocyst) ** 2))
test.oocyst.gbm.rmse
