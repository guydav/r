library(foreign)
library(caret)
library(devtools)
library(gsubfn)
library(gbm)
source('data_split.R')

setwd('~/projects/r/final_project/')
seed <- 33
set.seed(33)


# Data cleanup, and division to train, dev-test, and test sets
survival.14.days.df <- read.delim('survival 14 dpi.txt')
survival.14.days.df$death.date <- as.Date(survival.14.days.df$death.date, format = "%d/%m/%Y")
survival.14.days.df$carriers <- as.factor(survival.14.days.df$carriers)
survival.14.days.df$gametocyt <- as.factor(survival.14.days.df$gametocyt)
survival.14.days.df$status <- as.factor(ifelse(survival.14.days.df$status == 1, 'yes', 'no'))
# survival.14.days.df$exposition <- as.factor(ifelse(is.na(survival.14.days.df$exposition), 'no', 'yes'))
# survival.14.days.df$infected.PCR[is.na(survival.14.days.df$infected.PCR)] <- 'no' 
# survival.14.days.df$exposed.not.infected <- (survival.14.days.df$infected.PCR == 'no') & (survival.14.days.df$exposition == 'yes')

exposed.only <- survival.14.days.df[-which(is.na(survival.14.days.df$exposition)),]
list[survival.train, survival.dev, survival.test] <- split.train.dev.test(exposed.only)

set.seed(seed)
minimal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = twoClassSummary, classProbs = TRUE)
# rf.tune.grid <- expand.grid(mtry=c(1:(ncol(peace.df.minimal) - 1)))
survival.model <- train(infected.PCR ~ treatment + carriers + planteffect, 
                          data=exposed.only, method="rf", trControl = minimal.control, 
                          preProcess = c('center', 'scale'),  
                          # tuneGrid = rf.tune.grid,
                          metric = "ROC", verbose = TRUE)
plot(survival.model)
importance(survival.model$finalModel, type=2)
dev.classes <- predict(survival.model, newdata = exposed.only)
confusionMatrix(data=rf.dev.classes, exposed.only$infected.PCR)


