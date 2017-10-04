library(ISLR)
library(boot)
set.seed(1)
data("Auto")
attach(Auto)
data("Portfolio")

train <- sample(392, 162)
lm.fit <- lm(mpg~horsepower, data=Auto, subset = train)
mean( (Auto$mpg - predict(lm.fit, Auto))[-train]^2 )

lm.fit2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset = train)
mean( (Auto$mpg - predict(lm.fit2, Auto))[-train]^2 )

lm.fit3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset = train)
mean( (Auto$mpg - predict(lm.fit3, Auto))[-train]^2 )

# LOOCV
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

cv.error <- rep(0, 5)
for (i in 1:5) {
    glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
    cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

# k-fold CV
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10) {
    glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
    cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

# Bootstrap
alpha.func <- function(data, index) {
    x <- data$X[index]
    y <- data$Y[index]
    var_x <- var(x)
    var_y <- var(y)
    cov_x_y <- cov(x, y)
    return ( (var_y - cov_x_y) / (var_x + var_y - 2 * cov_x_y) )
} 
alpha.func(Portfolio, 1:100)
set.seed(1)
alpha.func(Portfolio, sample(100, 100, replace = TRUE))
boot(Portfolio, alpha.func, R = 1000)

# Bootstrap linear regression accuracy
coef.func <- function(data, index) {
    return( coef( lm(mpg~horsepower + I(horsepower^2), data = data, subset = index) ) )
}
coef.func(Auto, 1:392)
set.seed(1)
boot(Auto, coef.func, 1000)


# Pre-class work
population <- rnorm(10000, 0, 5)
n <- 15

boot.mean <- function(data, index) {
    return (mean(data[index]))
}

test.confidence.intervals <- function(population, n, rounds, R = 1000) {
    boot.success <- 0
    t.success <- 0
    pop.mean <- mean(population)

    pb <- txtProgressBar(max = rounds)
    for (i in 1:rounds) {
        setTxtProgressBar(pb, i)
        s <- sample(population, n)  
        
        boot.results <- boot(s, boot.mean, R)
        boot.summary <- summary(boot.results)
        if ((boot.summary$original - 1.96 * boot.summary$bootSE < pop.mean) && 
            (pop.mean < boot.summary$original + 1.96 * boot.summary$bootSE)) {
            boot.success = boot.success + 1
        }
        
        s.mean <- mean(s)
        s.se <- sd(s) / sqrt(n)
        if ((s.mean + qt(0.25, n - 1) * s.se < pop.mean) && 
            (pop.mean < s.mean + qt(0.975, n - 1) * s.se)) {
            t.success = t.success + 1
        }  
    }
    
    return(c(boot.success / rounds, t.success / rounds))
}

test.confidence.intervals(population, n, 1000)

uniform.population <- runif(10000, -5, 5) 
test.confidence.intervals(uniform.population, n, 1000)


library(Matching)
data("lalonde")
attach(lalonde)
boot.results <- NULL

for (i in 1:10000) {
    s <- sample(1:(nrow(lalonde)), size = 445, replace = TRUE)
    d <- lalonde[s,]
    reg <- lm(re78 ~ treat, data = d)
    boot.results <- c(boot.results, summary(reg)$coef[4])
}

sd(boot.results) 

library(WhatIf)

chessboard <- data.frame(x=numeric(64), y=numeric(64))
counter = 1
for (x in 1:8) {
    for (y in 1:8) {
        chessboard[counter,'x'] <- x
        chessboard[counter,'y'] <- y
        counter = counter + 1
    }
}

chessboard.whatif <- whatif(data = chessboard, cfact = chessboard)
summary(chessboard.whatif)

chessboard.cfact <- chessboard
chessboard.cfact$x <- chessboard.cfact$x - 1
cfact.whatif <- whatif(data = chessboard, cfact = chessboard.cfact)
summary(cfact.whatif)


chess.3d <- data.frame(x=numeric(1000), y=numeric(1000), z=numeric(1000))
counter = 1
for (x in 1:10) {
    for (y in 1:10) {
        for (z in 1:10) {
            chess.3d[counter,'x'] <- x
            chess.3d[counter,'y'] <- y
            chess.3d[counter,'z'] <- z
            counter = counter + 1   
        }
    }
}

chess.3d.whatif <- whatif(data = chess.3d, cfact = chess.3d)
chess.3d.cfact <- chess.3d
chess.3d.cfact$x <- chess.3d.cfact$x - 1
cfact.3d.whatif <- whatif(data = chess.3d, cfact = chess.3d.cfact)
