train.df <- data.frame(x=traindata[,1], y=traindata[,2])
test.df <- data.frame(x=testdata[,1], y=testdata[,2])

sorted.train.df <- train.df[order(train.df$x),]
sorted.test.df <- test.df[order(test.df$x),]

x <- sorted.train.df$x
y <- sorted.train.df$y

test.x <- sorted.test.df$x
test.y <- sorted.test.df$x

fit <- loess(y~x, span = 0.1, degree = 1) # fit the regression
test.rmse <- sqrt( mean( (test.y - predict(test.fit, test.x))^2 ) ) 
test.rmse

for (span in seq(0.1, 2, by=0.1)) {
    fit <- loess(y~x, span = span, degree = 1) # fit the regression
    rmse <- sqrt( mean( (test.y - predict(fit, test.x))^2 ) ) 
    print(paste(span, " ", rmse))
}

plot(test.x, test.y, col='blue')

plot(x, y)

fit1.0 <- loess(y~x, span = 1, degree = 1) # fit the regression
fit.5 <- loess(y~x, span = 0.2, degree = 1) # fit the regression
fit.2 <- loess(y~x, span = 0.3, degree = 1) # fit the regression


lines(x, predict(fit1.0, x), lwd = 3, col = "red") # add lines to plot
lines(x, predict(fit.5, x), lwd = 3, col = "blue") # add lines to plot
lines(x, predict(fit.2, x), lwd = 3, col = "green")# add lines to plot


rmse1.0 <- sqrt( mean( (y - predict(fit1.0))^2 ) ) #calculate RMSE
rmse.5 <- sqrt( mean( (y - predict
                       (fit.5))^2 ) ) # calculate RMSE
rmse.2 <- sqrt( mean( (y - predict(fit.2))^2 ) ) # calculate RMSE

for (span in seq(0.02, 0.2, by=0.01)) {
    fit <- loess(y~x, span = span, degree = 1) # fit the regression
    lines(x, predict(fit, x), lwd = 3, col = sample(colours())) # add lines to plot
    rmse <- sqrt( mean( (y - predict(fit, x))^2 ) ) 
    print(paste(span, " ", rmse))
}