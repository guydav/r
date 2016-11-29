library(foreign)
library(MASS)
library(caret)
library(WhatIf)
library(reshape)
setwd('~/projects/r/un_policy/')
seed <- 33

set.seed(seed)
peace.df <- read.dta('./replication_data/peace.dta')

minimal.cols <- c("decade", "wardur", "wartype", "un2int", "nouncint", "peaceop", 
                  "interven", "major", "gdpcap", "area",
                   "factnum", "factnum2", "logcost", "trnsfcap", 
                  "develop", "exp", "treaty", "pbs2s3")
peace.df.minimal.raw <- peace.df[,minimal.cols]
peace.df.minimal.raw <- na.omit(peace.df.minimal.raw)
peace.df.minimal <- peace.df.minimal.raw
peace.df.minimal$pbs2s3 <- factor(peace.df.minimal$pbs2s3, labels=make.names(c(0, 1)))

# Split the data into training and test sets
set.seed(seed)
train.indices <- createDataPartition(y=peace.df.minimal$pbs2s3,
                                     p=0.66, 
                                     list=FALSE)
minimal.train <- peace.df.minimal[train.indices,]
minimal.test <- peace.df.minimal[-train.indices,]
minimal.raw.train <- peace.df.minimal.raw[train.indices,]
minimal.raw.test <- peace.df.minimal.raw[-train.indices,]

# Now let's try automating this with caret
set.seed(seed)
minimal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = twoClassSummary, classProbs = TRUE)
# This uses binomial by itself, as the output variable is a two-valued factor
minimal.caret.logit <- train(pbs2s3 ~ un2int + . - pbs2s3,
                             data=minimal.train, method="glmStepAIC",
                             trControl = minimal.control, metric = "ROC", 
                             preProcess = c('center', 'scale'),
                             scope=list(upper="pbs2s3 ~ un2int + . - pbs2s3",  
                                 lower="pbs2s3 ~ un2int"))
summary(minimal.caret.logit)

logit.test.classes <- predict(minimal.caret.logit, newdata = minimal.test)
str(logit.test.classes)

testProbs <- predict(minimal.caret.logit, newdata = minimal.test, type='prob')
head(testProbs)

confusionMatrix(data=logit.test.classes, minimal.test$pbs2s3, positive = 'X1')

# Let's see if this can be done using a random forest as well
set.seed(seed)
minimal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = twoClassSummary, classProbs = TRUE)
# rf.tune.grid <- expand.grid(mtry=c(1:(ncol(peace.df.minimal) - 1)))
minimal.caret.rf <- train(pbs2s3 ~ un2int + . - pbs2s3, data=minimal.train, method="rf",
                          trControl = minimal.control, preProcess = c('center', 'scale'),  
                          # tuneGrid = rf.tune.grid,
                          metric = "ROC", verbose = TRUE)
plot(minimal.caret.rf)
importance(minimal.caret.rf$finalModel, type=2)
rf.test.classes <- predict(minimal.caret.rf, newdata = minimal.test)
confusionMatrix(data=rf.test.classes, minimal.test$pbs2s3, positive = 'X1')


# Once more, with gusto, using a neural net
set.seed(seed)
minimal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = twoClassSummary, classProbs = TRUE)
nnet.tune.grid <- expand.grid(.size = seq(1, 15, 2),
                              .decay = seq(0, 2, 0.125))
minimal.caret.nnet <- train(pbs2s3 ~ un2int + . - pbs2s3, data=minimal.train, method="nnet",
                            trControl = minimal.control, preProcess = c('center', 'scale'),
                            tuneGrid = nnet.tune.grid,
                            metric = "ROC")

plot(minimal.caret.nnet)
nnet.test.classes <- predict(minimal.caret.nnet, newdata = minimal.test)
confusionMatrix(data=nnet.test.classes, minimal.test$pbs2s3, positive = 'X1')


# And with a support vector machine
set.seed(seed)
minimal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = twoClassSummary, classProbs = TRUE)
minimal.caret.svm <- train(pbs2s3 ~ un2int + . - pbs2s3, data=minimal.train, method="svmLinear",
                            trControl = minimal.control, preProcess = c('center', 'scale'), metric = "ROC")

summary(minimal.caret.svm)
svm.test.classes <- predict(minimal.caret.svm, newdata = minimal.test)
confusionMatrix(data=svm.test.classes, minimal.test$pbs2s3, positive = 'X1')

# Attempting to use King's WhatIf package to find examples 
# whose un2int counterfactual still lies within the convex hull
peace.df.counterfactual <- peace.df.minimal.raw
peace.df.counterfactual$un2int <- (1 - peace.df.counterfactual$un2int)

hull.results <- whatif(data=peace.df.minimal.raw, cfact=peace.df.counterfactual)
# Tragically, none are. Therefore, we predict probabilities on the test set counterfacturl

# Set a multiple of -1 for rows where the counterfactual is no intervention
# and of 1 for rows where the counterfactual is intervention
peace.df.counterfactual$counterfact.multiple <- -1
peace.df.counterfactual$counterfact.multiple[peace.df.counterfactual$un2int == 1] <- 1

counterfactual.test <- peace.df.counterfactual[-train.indices,]

counterfactual.prediction <- function(model, counterfactual) {
    mean((predict(model, newdata = counterfactual, type='prob') - 
     predict(model, newdata = minimal.test, type='prob'))$X1 *
         counterfactual$counterfact.multiple)
}

# Compute Cohen's d for each counterfactual effect prediction
counterfactual.effect.size <- function(model, counterfactual) {
    cf.probs <- predict(model, newdata = counterfactual, type='prob')$X1
    test.probs <- predict(model, newdata = minimal.test, type='prob')$X1
    prediction <- mean((cf.probs - test.probs) * counterfactual$counterfact.multiple)
    abs(prediction / sqrt((var(cf.probs) + var(test.probs)) / 2))
}

# logit.intervention.prediction <- counterfactual.prediction(minimal.caret.logit, counterfactual.test)
# rf.intervention.prediction <- counterfactual.prediction(minimal.caret.rf, counterfactual.test)
# nnet.intervention.prediction <- counterfactual.prediction(minimal.caret.nnet, counterfactual.test)
# svm.intervention.prediction <- counterfactual.prediction(minimal.caret.svm, counterfactual.test)

all.counterfactual.predictions <- function(counterfactual) {
    return (list(logit=counterfactual.prediction(minimal.caret.logit, counterfactual), 
                 rf=counterfactual.prediction(minimal.caret.rf, counterfactual),
                 nnet=counterfactual.prediction(minimal.caret.nnet, counterfactual),
                 svm=counterfactual.prediction(minimal.caret.svm, counterfactual)))
}

all.counterfactual.effect.sizes <- function(counterfactual) {
    return (list(logit=counterfactual.effect.size(minimal.caret.logit, counterfactual), 
                 rf=counterfactual.effect.size(minimal.caret.rf, counterfactual),
                 nnet=counterfactual.effect.size(minimal.caret.nnet, counterfactual),
                 svm=counterfactual.effect.size(minimal.caret.svm, counterfactual)))
}

all.counterfactual.predictions(counterfactual.test)
all.counterfactual.effect.sizes(counterfactual.test)

# Count how many test entries the model predicts a negative effect for

negative.prediction.count <- function(model, counterfactual) {
    length(which((predict(model, newdata = counterfactual, type='prob') - 
                      predict(model, newdata = minimal.test, type='prob'))$X1 *
                     counterfactual$counterfact.multiple < 0))
}

all.negative.prediction.count <- function(counterfactual) {
    return (list(logit=negative.prediction.count(minimal.caret.logit, counterfactual), 
                 rf=negative.prediction.count(minimal.caret.rf, counterfactual),
                 nnet=negative.prediction.count(minimal.caret.nnet, counterfactual),
                 svm=negative.prediction.count(minimal.caret.svm, counterfactual)))
}

all.negative.prediction.count(counterfactual.test)
    
# Create a different counterfactual to observe its effect - the treaty variable
treaty.counterfactual <- peace.df.minimal.raw
treaty.counterfactual$treaty <- (1 - treaty.counterfactual$treaty)

treaty.hull.results <- whatif(data=peace.df.minimal.raw, cfact=treaty.counterfactual)

treaty.counterfactual$counterfact.multiple <- -1
treaty.counterfactual$counterfact.multiple[treaty.counterfactual$treaty == 1] <- 1
treaty.counterfactual.test <- treaty.counterfactual[-train.indices,]

all.counterfactual.predictions(treaty.counterfactual.test)
all.counterfactual.effect.sizes(treaty.counterfactual.test)
all.negative.prediction.count(treaty.counterfactual.test)

# Create a different counterfactual to observe its effect - the major variable
major.counterfactual <- peace.df.minimal.raw
major.counterfactual$major <- (1 - major.counterfactual$major)

major.hull.results <- whatif(data=peace.df.minimal.raw, cfact=major.counterfactual)

major.counterfactual$counterfact.multiple <- -1
major.counterfactual$counterfact.multiple[major.counterfactual$treaty == 1] <- 1
major.counterfactual.test <- major.counterfactual[-train.indices,]

all.counterfactual.predictions(major.counterfactual.test)
all.counterfactual.effect.sizes(major.counterfactual.test)
all.negative.prediction.count(major.counterfactual.test)

# And how about one with reducing the log cost by a standard deviation
logcost.sd <- sd(peace.df.minimal$logcost)
logcost.counterfactual <- peace.df.minimal.raw
logcost.counterfactual$logcost <- logcost.counterfactual$logcost - logcost.sd
logcost.hull.results <- whatif(data=peace.df.minimal.raw, cfact=logcost.counterfactual)
# plot(logcost.hull.results)

# The multiple here is +1 for all of them as we decrease the logcost for every entry
logcost.counterfactual$counterfact.multiple <- 1
logcost.counterfactual.test <- logcost.counterfactual[-train.indices,]

all.counterfactual.predictions(logcost.counterfactual.test)
all.counterfactual.effect.sizes(logcost.counterfactual.test)
all.negative.prediction.count(logcost.counterfactual.test)

# Attempt to pick the optimal cutoff based on the training set, 
# and then check the results on the test set
# ended up not using these results, but leaving the code here

cutoffs <- seq(0.1, 0.9, 0.05)
accuracy.df <- data.frame(cutoff = cutoffs)

cutoff.accuracy <- function(model, test.set, cuttofs) {
    accuracy <- NULL
    probs <- predict(model, newdata = test.set, type='prob')$X1
    for (cutoff in cutoffs){
        prediction <- ifelse(probs >= cutoff, 1, 0)
        accuracy <- c(accuracy, length(which(test.set$pbs2s3 == prediction)) / length(prediction) * 100)
    }
    accuracy
}

accuracy.df$logit <- cutoff.accuracy(minimal.caret.logit, minimal.raw.train, cutoffs)
accuracy.df$rf <- cutoff.accuracy(minimal.caret.rf, minimal.raw.train, cutoffs)
accuracy.df$nnet <- cutoff.accuracy(minimal.caret.nnet, minimal.raw.train, cutoffs)
accuracy.df$svm <- cutoff.accuracy(minimal.caret.svm, minimal.raw.train, cutoffs)

accuracy.df.melted <- melt(accuracy.df, id='cutoff')

ggplot(data=accuracy.df.melted) + 
    geom_line(aes(x = cutoff, y = value, colour = variable)) +
    geom_point(aes(x = cutoff, y = value, colour = variable)) 


# The logic of taking the mean assumes there's a continuous peak, 
# and not two far-away values performing equally well
# In fact, my hunch is that it's mathematically impossible for two far values to 
# perform equally well without pleateau or maxima between them
logit.max.cutoff <- mean(accuracy.df[accuracy.df$logit == max(accuracy.df$logit),'cutoff'])
rf.max.cutoff <- mean(accuracy.df[accuracy.df$rf == max(accuracy.df$rf),'cutoff'])
nnet.max.cutoff <- mean(accuracy.df[accuracy.df$nnet == max(accuracy.df$nnet),'cutoff'])
svm.max.cutoff <- mean(accuracy.df[accuracy.df$svm == max(accuracy.df$svm),'cutoff'])

cutoff.predict <- function(model, test.set, cutoff) {
    probs <- predict(model, newdata = test.set, type='prob')$X1
    as.factor(ifelse(probs >= cutoff, "X1", "X0"))
}

logit.max.cutoff.classes <- cutoff.predict(minimal.caret.logit, minimal.test, logit.max.cutoff)
confusionMatrix(data=logit.max.cutoff.classes, minimal.test$pbs2s3, positive = 'X1')

rf.max.cutoff.classes <- cutoff.predict(minimal.caret.rf, minimal.test, rf.max.cutoff)
confusionMatrix(data=rf.max.cutoff.classes, minimal.test$pbs2s3, positive = 'X1')

nnet.max.cutoff.classes <- cutoff.predict(minimal.caret.nnet, minimal.test, nnet.max.cutoff)
confusionMatrix(data=nnet.max.cutoff.classes, minimal.test$pbs2s3, positive = 'X1')

svm.max.cutoff.classes <- cutoff.predict(minimal.caret.svm, minimal.test, svm.max.cutoff)
confusionMatrix(data=svm.max.cutoff.classes, minimal.test$pbs2s3, positive = 'X1')

# Open question - how can it be that not all data points are within the convex hull??
# summary(whatif(data=peace.df.minimal.raw, cfact=peace.df.minimal.raw))


