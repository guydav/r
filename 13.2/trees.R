library(tree)
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