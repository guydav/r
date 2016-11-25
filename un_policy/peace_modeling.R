library(foreign)
library(MASS)
library(caret)
setwd('~/projects/r/un_policy/')
seed <- 33

set.seed(seed)
peace.df <- read.dta('./replication_data/peace.dta')

minimal.cols <- c("decade", "wardur", "wartype", "un2int", "nouncint", "peaceop", 
                  "interven", "major","gdpcap", "area",
                   "factnum", "factnum2", "logcost", "trnsfcap", 
                  # "UNop1", "UNop2", "UNop3", "UNop4",
                  "develop", "exp", "treaty", "pbs2s3")
peace.df.minimal <- peace.df[,minimal.cols]
# peace.df.minimal$pbs2s3 <- as.factor(peace.df.minimal$pbs2s3)
peace.df.minimal$pbs2s3 <- factor(peace.df.minimal$pbs2s3, labels=make.names(c(0, 1)))
peace.df.minimal <- na.omit(peace.df.minimal)

# Split the data into training and test sets
train.indices <- createDataPartition(y=peace.df.minimal$pbs2s3,
                                     p=0.66, 
                                     list=FALSE)
minimal.train <- peace.df.minimal[train.indices,]
minimal.test <- peace.df.minimal[-train.indices,]

# Now let's try automating this with caret
set.seed(seed)
minimal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = twoClassSummary, classProbs = TRUE)
# This uses binomial by itself, as the output variable is a two-valued factor
minimal.caret.logit <- train(pbs2s3 ~ un2int + ., data=minimal.train, method="glmStepAIC",
                             trControl = minimal.control, metric = "ROC", 
                             preProcess = c('center', 'scale'),
                             scope=list(upper=as.formula(paste("pbs2s3 ~ ", paste(colnames(peace.df.minimal)[1:(ncol(peace.df.minimal) - 1)], collapse = ' + '))),
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
minimal.caret.rf <- train(pbs2s3 ~ un2int + . - pbs2s3, data=minimal.train, method="rf",
                             trControl = minimal.control, preProcess = c('center', 'scale'),  metric = "ROC")
importance(minimal.caret.rf$finalModel)

rf.test.classes <- predict(minimal.caret.rf, newdata = minimal.test)
confusionMatrix(data=rf.test.classes, minimal.test$pbs2s3, positive = 'X1')


# Once more, with gusto, using a neural net
set.seed(seed)
minimal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = twoClassSummary, classProbs = TRUE)
minimal.caret.nnet <- train(pbs2s3 ~ un2int + . - pbs2s3, data=minimal.train, method="nnet",
                            trControl = minimal.control, preProcess = c('center', 'scale'), metric = "ROC")

nnet.test.classes <- predict(minimal.caret.nnet, newdata = minimal.test)
str(nnet.test.classes)
confusionMatrix(data=nnet.test.classes, minimal.test$pbs2s3, positive = 'X1')


# And with a support vector machine
set.seed(seed)
minimal.control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = twoClassSummary, classProbs = TRUE)
minimal.caret.svm <- train(pbs2s3 ~ un2int + . - pbs2s3, data=minimal.train, method="svmLinear",
                            trControl = minimal.control, preProcess = c('center', 'scale'), metric = "ROC")

svm.test.classes <- predict(minimal.caret.svm, newdata = minimal.test)
str(nnet.test.classes)
confusionMatrix(data=svm.test.classes, minimal.test$pbs2s3, positive = 'X1')