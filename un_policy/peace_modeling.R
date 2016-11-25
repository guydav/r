library(foreign)
library(MASS)
library(caret)
setwd('~/projects/r/un_policy/')
set.seed(33)
peace.df <- read.dta('./replication_data/peace.dta')

minimal.cols <- c("decade", "wardur", "wartype", "un2int", "nouncint", "peaceop", 
                  "interven", "major","gdpcap", "area",
                   "factnum", "factnum2", "logcost", "trnsfcap", 
                  "UNop1", "UNop2", "UNop3", "UNop4",
                  "develop", "exp", "treaty", "pbs2s3")
peace.df.minimal <- peace.df[,minimal.cols]
# peace.df.minimal$pbs2s3 <- as.factor(peace.df.minimal$pbs2s3)
peace.df.minimal$pbs2s3 <- factor(peace.df.minimal$pbs2s3, labels=make.names(c(0, 1)))
peace.df.minimal <- na.omit(peace.df.minimal)

# Now let's try automating this with caret
set.seed(33)
train.indices <- createDataPartition(y=peace.df.minimal$pbs2s3,
                                     p=0.75, 
                                     list=FALSE)
minimal.train <- peace.df.minimal[train.indices,]
minimal.test <- peace.df.minimal[-train.indices,]
minimal.control <- trainControl(summaryFunction = twoClassSummary, classProbs = TRUE)
minimal.caret.model <- train(pbs2s3 ~ un2int + ., data=minimal.train, method="glmStepAIC",
                             trControl = minimal.control, metric = "ROC",
                             scope=list(upper=as.formula(paste("pbs2s3 ~ ", paste(colnames(peace.df.minimal)[1:(ncol(peace.df.minimal) - 1)], collapse = ' + '))),
                                        lower="pbs2s3 ~ un2int"))
summary(minimal.caret.model)

testClasses <- predict(minimal.caret.model, newdata = minimal.test)
str(testClasses)

testProbs <- predict(minimal.caret.model, newdata = minimal.test, type='prob')
head(testProbs)

confusionMatrix(data=testClasses, minimal.test$pbs2s3, positive = 'X1')