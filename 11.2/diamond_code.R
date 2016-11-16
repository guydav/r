### RESULTS FOR CS112 LESSON PLAN 11.1
rm(list=ls())
set.seed(333)

num_data <- 200

### TO CREATE THE DATA (LATER SEPARATED INTO TEST AND TRAINING SETS)
x <- round(runif(num_data, -4, 3), 2)
y = x^3 + 2*x^2 - 5*x - 3
dev.off()

par(mfrow = c(2,2))
plot(x,y, main = "Data With No Random Component")

y <- jitter(y, 3000) # to add some random noise (i.e., the epsilon in the regression equation)

### VISUALIZE THE DATA
plot(x,y, main = "Data: Systematic AND Random Components")

### TO IDENTIFY THE ROWS IN THE DATA THAT WILL BECOME THE TRAINING SET
training <- sample(x = c(1:num_data), size = 0.5*num_data, replace = FALSE)

### TO CREATE THE TRAINING SET
x_train <- x[training]
y_train <- y[training]

### TO CREATE THE TEST SET
x_test <- x[-training]
y_test <- y[-training]

### TO PUT THE TRAINING DATA IN ORDER (SMALLEST TO BIGGEST, BY THE X-VARIABLE)
y_train <- y_train[order(x_train)]
x_train <- x_train[order(x_train)]

### TO PREPARE THE TEST SET DATA, WE HAVE TO:
### (1) RETAIN ONLY THE TEST SET OBSERVATIONS WITH 
###     X < THE MAXIMUM VALUE OF X IN THE TRAINING SET AND
###     X > THE MINIMUM VALUE OF X IN THE TRAINING SET,
###     BECAUSE LOCAL REGRESSION WILL ONLY PREDICT WITHIN THE HULL OF THE TEST SET
y_test <- y_test[which(x_test <= max(x_train))]
x_test <- x_test[which(x_test <= max(x_train))]
y_test <- y_test[which(x_test > min(x_train))]
x_test <- x_test[which(x_test > min(x_train))]

### (2) PUT TEST SET DATA OBSERVATIONS IN ORDER (SMALLEST TO BIGGEST, BY X-VARIABLE)
y_test <- y_test[order(x_test)]
x_test <- x_test[order(x_test)]

########## DATA IS READY. TIME FOR ANALYSIS...
RMSE_in_sample <- c()     # storage vector for training set results
RMSE_out_of_sample <- c() # storage vector for test set results

### LOOPING THROUGH ALL THE SPANS FROM SPAN = 1/20 TO SPAN = 20/20
for(i in 1:20) {
    fit.out <- loess(y_train ~ x_train, span = i/20, degree = 1) # fit the regression
    RMSE_in_sample[i] <-  sqrt( mean( (y_train - predict(fit.out, x_train))^2 ) )
    RMSE_out_of_sample[i] <- sqrt( mean( (y_test - predict(fit.out, x_test))^2 ) )
}

### CREATE A TABLE SUMMARIZING RESULTS
results_table <- data.frame(c(1:20/20), RMSE_in_sample, RMSE_out_of_sample)

### ESTIMATING AND PLOTTING THE INTERESTING RESULTS
smallest_RMSE <- which(RMSE_out_of_sample == min(RMSE_out_of_sample))

cat("\nThe smallest RMSE (which is approximately ", 
    round(RMSE_out_of_sample[smallest_RMSE]), 
    ") is associated with a span of ", 
    smallest_RMSE*(1/20), ".\n\n", sep = "")

plot(results_table[,1], RMSE_in_sample, ylim = c(0, max(RMSE_out_of_sample, RMSE_in_sample)), 
     ylab = "RMSE", xlab = "span", main = "Span vs.\ RMSE", xaxp = c(0, 1, 10))

points(results_table[,1], RMSE_out_of_sample, pch = 19)
text(x = 0.25, y = RMSE_out_of_sample[4] +1, "OUT-OF-SAMPLE RMSE", cex = 0.7)
text(x = 0.25, y = RMSE_in_sample[5] - 0.75, "IN-SAMPLE RMSE", cex = 0.7)

points(results_table[,1][smallest_RMSE], RMSE_out_of_sample[smallest_RMSE], 
       cex = 3, col = "red")
lines(x = c(results_table[,1][smallest_RMSE], results_table[,1][smallest_RMSE]),
      y = c(0, RMSE_out_of_sample[smallest_RMSE]), col = "red", lty = 2)

### BEST OUT OF SAMPLE FIT
fit.out <- loess(y_train ~ x_train, span = smallest_RMSE*(1/20), degree = 1)  # WE USE TRAINING DATA TO GET THE "FIT.OUT" MODEL

plot(x_test, y_test, main = "Predicting Test-Set Data",       # AND WE PLOT THE TEST DATA
     xlab = c("Test-Set X Values"), ylab = "Test-Set Y Values", col = "red")

points(x_train, y_train, # AND WE PLOT THE TRAIN DATA
       col = "blue", cex = 0.3, pch = 19)

lines(x_test, predict(fit.out, x_test), lwd = 5, col = "blue") # VIZ THE TRAINING MODEL APPLIED TO TEST DATA

### TO SATISFY CURIOUSITY, I VISUALIZE THE IN-SAMPLE TEST-SET FIT WITH SPAN = 0.35, SHOWN IN ORANGE,
### TO DEMONSTRATE THAT WE OVERFIT THE TEST-SET WHEN WE BOTH FIT THE MODEL AND APPLY IT IN THE TEST SET
fit.out2 <- loess(y_test ~ x_test, span = smallest_RMSE*(1/20), degree = 1)
lines(x_test, predict(fit.out2, x_test), lwd = 5, col = "red") # add lines to plot

text(x = -0.5, y = predict(fit.out, 1) + 17 , "OUT-OF-SAMPLE PREDICTION", col = "red", cex = 0.7)
text(x = -2.00, y = predict(fit.out2, -2.00) - 20, "OVERFIT IN-SAMPLE", col = "blue", cex = 0.7)