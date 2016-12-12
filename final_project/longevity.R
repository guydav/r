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
longevity.df <- read.delim('longevity.txt')
longevity.df$carr <- as.factor(longevity.df$carr)
longevity.df$rep <- as.factor(longevity.df$rep)

list[longevity.train, longevity.dev, longevity.test] <- split.train.dev.test(longevity.df)

# Using a random forest
set.seed(seed)
longevity.control <- trainControl(method="repeatedcv", number=10, repeats=3)
gbm.tune.grid <- expand.grid(n.trees = c(100, 200, 300),
                            interaction.depth = c(1, 2, 3),
                            shrinkage = c(0.1, 0.01, 0.001),
                            n.minobsinnode = c(5, 10))
longevity.gbm.model <- train(death ~ treatment + carr + rep + group + infection + tm + logcopygene, 
                             data=longevity.train, method='gbm', trControl = longevity.control, 
                            #tuneGrid = rf.tune.grid, 
                            preProcess = c('center', 'scale'))
getTrainPerf(longevity.gbm.model)
ggplot(longevity.gbm.model)
summary(longevity.gbm.model)
dev.longevity.gbm.rmse <- sqrt(sum((predict(longevity.gbm.model, newdata = longevity.dev) - longevity.dev$death) ** 2) / nrow(longevity.dev))
dev.longevity.gbm.rmse

# Using a linear model:
set.seed(seed)
longevity.control <- trainControl(method="repeatedcv", number=10, repeats=3)
# rf.tune.grid <- expand.grid(mtry=c(12, 14, 16))
# nnet.tune.grid <- expand.grid(.size = seq(3, 9, 2),
#                              .decay = seq(0.25, 1.75, 0.5))
longevity.lm.model <- train(death ~ treatment + carr + rep + group + infection + tm + logcopygene, 
                           data=longevity.train, method='glmStepAIC', trControl = longevity.control, 
                           # tuneGrid = rf.tune.grid,
                           preProcess = c('center', 'scale'))
getTrainPerf(longevity.lm.model)
summary(longevity.lm.model)
dev.longevity.lm.rmse <- sqrt(sum((predict(longevity.lm.model, newdata = longevity.dev) - longevity.dev$death) ** 2) / nrow(longevity.dev))
