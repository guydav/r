library(tree)
library(foreign)
library(ISLR)
attach(Carseats)

High <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

# Construct a tree over the entire data set
tree.carseats <- tree(High ~ . - Sales, Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, cex = 0.5)

# Construct one over a training set, test over a test set
set.seed(2)
train.indices <- sample(1:nrow(Carseats), 200)
Carseats.train <- Carseats[train.indices,]
Carseats.test <- Carseats[-train.indices,]
High.test <- High[-train.indices]
tree.carseats <- tree(High ~ . - Sales, Carseats, subset = train.indices)
tree.pred <- predict(tree.carseats, Carseats.test, type = 'class')
table(tree.pred, High.test)

# Construct a tree with pruning
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats
# Plot error rates
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")
# Created pruned tree
prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, cex = 0.5)
# Test pruned tree
tree.pred <- predict(prune.carseats, Carseats.test, type = 'class')
table(tree.pred, High.test)

# Regression trees
library(MASS)
data("Boston")
par(mfrow = c(1, 1))

set.seed(1)
boston.train <- sample(1:nrow(Boston), nrow(Boston) / 2)
tree.boston <- tree(medv ~ ., Boston, subset = boston.train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, cex = 0.5)

# Cross-validate the tree and prune it
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = "b")

# Perform bagging / random forest
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv ~ ., Boston, subset = boston.train, 
                           mtry = 13, importance = TRUE)
yhat.bag <- predict(bag.boston, newdata = Boston[-boston.train,])
plot(yhat.bag, Boston[-boston.train, "medv"])
abline(0, 1)
mean((yhat.bag - Boston[-boston.train, "medv"])^2)
# change ntree to add trees, change mtry to reduce predictors tried

# Use gbm to do some boosting
library(gbm)
set.seed(1)
# Use Gaussian for regression problems, Bernoulli for classification
boost.boston <- gbm(medv ~ ., data = Boston[boston.train,], 
                    distribution = "gaussian", n.trees = 5000,
                    interaction.depth = 4, shrinkage = 0.1)
summary(boost.boston)
yhat.boost <- predict(boost.boston, newdata = Boston[-boston.train,],
                      n.trees = 5000)
mean((yhat.boost - Boston[-boston.train, "medv"])^2)

# Discrimination effects study

discrimination.df <- read.dta('./13.2/discrimination/mazedata1.dta')
discrimination.df$caste <- as.factor(discrimination.df$caste)
discrimination.df$brahmin <- ifelse(discrimination.df$caste == 'brahmin', 1, 0)
discrimination.df$brahmin.factor <- as.factor(ifelse(discrimination.df$caste == 'brahmin', 1, 0))
discrimination.df$mothedu.factor <- as.factor(discrimination.df$mothedu)
discrimination.df$fathedu.factor <- as.factor(discrimination.df$fathedu)
discrimination.df$mothedu.numeric <- as.numeric(discrimination.df$mothedu)
discrimination.df$fathedu.numeric <- as.numeric(discrimination.df$fathedu)
attach(discrimination.df)

disc.train.indices <- sample(1:nrow(discrimination.df), nrow(discrimination.df) / 2)
tree.disc <- tree(brahmin.factor ~ seenmaze + knowkids + read + land + 
                  mothedu.numeric + fathedu.numeric,
                  discrimination.df[disc.train.indices,])
plot(tree.disc)
text(tree.disc, cex = 0.5)
disc.pred <- predict(tree.disc, discrimination.df[-disc.train.indices,], type = 'class')
table(disc.pred, discrimination.df$brahmin.factor[-disc.train.indices])

library(gbm)
set.seed(1)
# Use Gaussian for regression problems, Bernoulli for classification
boost.disc <- gbm(brahmin ~ seenmaze + knowkids +  read + land + mothedu.numeric + fathedu.numeric, 
                  data = discrimination.df[disc.train.indices,], 
                  distribution = "bernoulli", 
                  n.trees = 5000,
                  interaction.depth = 4,
                  shrinkage = 0.01)
summary(boost.disc)
boost.pred <- ifelse(predict(boost.disc, newdata = discrimination.df[-disc.train.indices,], n.trees = 5000, type='response') > 0.5, 1, 0)
table(boost.pred, discrimination.df$brahmin[-disc.train.indices])

kinderegg <-       c(1,1,1,1,1,1,0,0,0,0,0,0)
german <-          c(1,1,1,0,0,0,1,1,1,0,0,0)
married <-         c(1,1,1,0,1,0,0,0,0,1,0,1)
kids <-            c(1,0,1,1,1,1,0,1,0,0,0,0)
political.party <- c(1,1,1,1,0,0,0,0,0,0,1,1)

kinder.df <- data.frame(kinder = as.factor(kinderegg), german = german, married = married, kids = kids, party = political.party)
attach(kinder.df)
kinder.tree <- tree(kinder ~ german + married + kids + party, kinder.df, minsize = 1)
plot(kinder.tree)
text(kinder.tree, cex = 0.66)

library(titanic)
titanic_train$Survived <- as.factor(titanic_train$Survived)
titanic.tree <- tree(titanic_train$Survived ~ ., titanic_train)
summary(titanic.tree)
plot(titanic.tree)
text(titanic.tree, cex = 0.66)

View(titanic_test)
titanic_test$Survived <- as.factor(titanic_test$Survived)
disc.pred <- predict(tree.disc, discrimination.df[-disc.train.indices,], type = 'class')
table(disc.pred, discrimination.df$brahmin.factor[-disc.train.indices])

## Brahmin
library(foreign)

mazedata1 <- read.dta("./13.2/discrimination/mazedata1.dta") # available on platform

# sneak a peek at the data
head(mazedata1)

# create an indicator variable for "brahmin" caste 
# and append it to mazedata1 dataframe

yes <- which(mazedata1$caste == "brahmin")
no <- which(mazedata1$caste != "brahmin")
brahminYES <- rep(0, nrow(mazedata1))
brahminYES[yes] <- "Yes"
brahminYES[no] <- "No"
mazedata1 <- data.frame(brahminYES, mazedata1)

# sneak another peek at the data (see the new first column)
head(mazedata1)

# count the number of brahmins
sum(mazedata1[,1])

# how big is the dataset?
dim(mazedata1)

# run a tree
mazetree <- tree(brahminYES ~ 
                     fathedu + mothedu + mothocc + fathocc + land + read, 
                 data = mazedata1)

# create a figure
plot(mazetree)
text(mazetree, pretty = 0)

# perform training set/test set analysis
set.seed(2)
train <- sample(1: nrow(mazedata1 ), 0.8*nrow(mazedata1))
mazedata1.train <- mazedata1[train,]
mazedata1.test <- mazedata1[-train,]

tree.mazedata1.train = tree(brahminYES ~ fathedu + mothedu + land + read, 
                            data = mazedata1, subset = train)

maze.pred=predict(tree.mazedata1.train, mazedata1.test, type = "class")

set.seed(3)
cv.mazedata1 =cv.tree(tree.mazedata1.train ,FUN=prune.misclass )
cv.mazedata1

par(mfrow=c(1,2))
plot(cv.mazedata1$size ,cv.mazedata1$dev ,type="b")
plot(cv.mazedata1$k ,cv.mazedata1$dev ,type="b")

prune.mazedata1 <- prune.misclass(tree.mazedata1.train, best=5)
plot(prune.mazedata1 )
text(prune.mazedata1, pretty =0)

tree.pred <- predict(prune.mazedata1, mazedata1.test , type="class")

table(tree.pred, mazedata1.test$brahminYES)
