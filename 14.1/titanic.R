library(foreign)
library(caret)
setwd('~/projects/r/14.1/')

titanic.train <- read.csv("train.csv")
str(titanic.train)
titanic.test <- read.csv("test.csv")
titanic.test$Survived <- 0
str(titanic.test)

titanic.train$Child <- 0
titanic.train$Child[titanic.train$Age < 18] <- 1
titanic.test$Child <- 0
titanic.test$Child[titanic.test$Age < 18] <- 1

# Feature engineering
titanic.combined <- rbind(titanic.train, titanic.test)
titanic.combined$Name <- as.character(titanic.combined$Name)
titanic.combined$Title <- sapply(titanic.combined$Name, 
                                 FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
titanic.combined$Title <- sub(' ', '', titanic.combined$Title)
titanic.combined$Title[titanic.combined$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
titanic.combined$Title[titanic.combined$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
titanic.combined$Title[titanic.combined$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
titanic.combined$Title <- as.factor(titanic.combined$Title)
titanic.combined$FamilySize <- titanic.combined$SibSp + titanic.combined$Parch + 1

titanic.combined$Survived <- as.factor(ifelse(titanic.combined$Survived == 1, 'Yes', 'No'))
combined.train <- titanic.combined[1:nrow(titanic.train),]
combined.test <- titanic.combined[(nrow(titanic.train) + 1):nrow(titanic.combined),]

# TODO: Attempt to engineer features out of, say Cabin as well.

# Train a random forest on this data
set.seed(33)
control <- trainControl(method="repeatedcv", number=10, repeats=3,
                                summaryFunction = twoClassSummary, classProbs = TRUE)
caret.rf <- train(Survived ~ . - Survived - Name - Ticket - Cabin, data=na.omit(combined.train), method="rf",
                          trControl = control, preProcess = c('center', 'scale'),  
                          metric = "ROC", verbose = TRUE)
plot(caret.rf)
importance(caret.rf$finalModel, type=2)
test.classes <- predict(caret.rf, newdata = combined.test)

