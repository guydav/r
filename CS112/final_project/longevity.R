library(foreign)
library(caret)
library(devtools)
library(gsubfn)
library(ggplot2)
library(RColorBrewer)
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
longevity.gbm.model <- train(death ~ treatment + carr + rep + group + infection + logcopygene + tm,
                             data=longevity.train, method='gbm', trControl = longevity.control, 
                            #tuneGrid = rf.tune.grid, 
                            preProcess = c('center', 'scale'))
getTrainPerf(longevity.gbm.model)
ggplot(longevity.gbm.model)
summary(longevity.gbm.model)
dev.longevity.gbm.pred <- predict(longevity.gbm.model, newdata = longevity.dev)
dev.longevity.gbm.rmse <- sqrt(sum((dev.longevity.gbm.pred - longevity.dev$death) ** 2) / nrow(longevity.dev))
dev.longevity.gbm.rmse

# Using a linear model:
set.seed(seed)
longevity.control <- trainControl(method="repeatedcv", number=10, repeats=3)
# rf.tune.grid <- expand.grid(mtry=c(12, 14, 16))
# nnet.tune.grid <- expand.grid(.size = seq(3, 9, 2),
#                              .decay = seq(0.25, 1.75, 0.5))
longevity.glm.model <- train(death ~ treatment + carr + rep + group + infection + tm + logcopygene, 
                           data=longevity.train, method='glmStepAIC', trControl = longevity.control, 
                           # tuneGrid = rf.tune.grid,
                           preProcess = c('center', 'scale'))
getTrainPerf(longevity.glm.model)
summary(longevity.glm.model)
dev.longevity.glm.pred <- predict(longevity.glm.model, newdata = longevity.dev)
dev.longevity.glm.rmse <- sqrt(sum((dev.longevity.glm.pred - longevity.dev$death) ** 2) / nrow(longevity.dev))
dev.longevity.glm.rmse

# predict test set with both glm and gbm models
test.longevity.glm.pred <- predict(longevity.glm.model, newdata = longevity.test)
test.longevity.glm.rmse <-  sqrt(mean((test.longevity.glm.pred - longevity.test$death) ** 2))
test.longevity.glm.rmse

test.longevity.gbm.pred <- predict(longevity.gbm.model, newdata = longevity.test)
test.longevity.gbm.rmse <-  sqrt(mean((test.longevity.gbm.pred - longevity.test$death) ** 2))
test.longevity.gbm.rmse

# Plot
longevity.df$treat.and.group <- paste(longevity.df$treatment, longevity.df$group)
longevity.df$treat.and.group[longevity.df$treat.and.group == 'glucose5% control'] <- 'gluc5%cont'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'glucose5% expuninf'] <- 'gluc5%uninf'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'glucose5% infected'] <- 'gluc5%inf'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'thevetia infected'] <- 'thev-inf'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'thevetia expuninf'] <- 'thev-uninf'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'thevetia control'] <- 'thev-cont'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'mangifera infected'] <- 'mang-inf'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'mangifera expuninf'] <- 'mang-uninf'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'mangifera control'] <- 'mang-control'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'lupilina infected'] <- 'lup-inf'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'lupilina control'] <- 'lup-cont'
longevity.df$treat.and.group[longevity.df$treat.and.group == 'lupilina expuninf'] <- 'lup-uninf'

group.colors <- brewer.pal(3, "Set1")
names(group.colors) <- rev(levels(longevity.df$group))
group.color.scale <- scale_colour_manual(name = "group",values = group.colors)
qplot(x=longevity.df$treat.and.group, y=longevity.df$death,
      main='Longevity boxplots by treatment and group assignments',
      xlab='Treatment and Group Assignment', ylab='Time of death (days post beginning of experiment)',
      colour=longevity.df$group, geom = 'boxplot') + group.color.scale