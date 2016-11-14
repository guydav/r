size <- 200
cutoff <- 0.666

cholesterol <- rnorm(size, 175, 50)
age <- rnorm(size, 65, 7.5)
weight <- rnorm(size, 80, 10)
height <- rnorm(size, 175, 15)
married <- rbinom(size, 1, 0.5)

data <- data.frame(cholesterol=cholesterol, age=age, weight=weight, height=height, married=married)

max_c <- max(data$cholesterol)
max_weight <- max(data$weight)

data$y.before <- data$cholesterol/max_c * 0.4 + data$weight / max_weight * 0.4 + runif(size, 0, 0.2) - 0.05 * data$married
data$y.after <- data$y.before + runif(size, -0.1, 0.1) 
data[data$y.before > cutoff,'y.after'] = data[data$y.before > cutoff,'y.after'] - 0.1

qplot(data$y.before, data$y.after)
write.csv(data, file="9.1/data_upgraded.csv")

library(rdrobust)
rdrobust(data$y.after, data$y.before, c = cutoff, cov=cbind(cholesterol,age,weight,height,married), all = TRUE)
rdrobust(data$y.after, data$y.before, c = cutoff)
rdplot(data$y.after, data$y.before, c = cutoff, title = "Without Covariates")
rdplot(data$y.after, data$y.before, c = cutoff , title = "With Covariates")



library("ggplot2")
ggplot(data=data, aes(x=y.before, y=y.after)) + geom_point() +
    geom_vline(xintercept = cutoff, linetype = "longdash") +
    geom_vline(xintercept = cutoff - 0.0427, linetype = 8, colour = 'blue') +
    geom_vline(xintercept = cutoff + 0.0427, linetype = 8, colour = 'blue') +
    xlab("Predicted Pre-Treatment Mortality Rate") + 
    ylab("Predicted Post-Treatment Mortality Rate") +
    ggtitle("Regression Discontinuity Design Graph") +
    geom_smooth(data=data[data$y.before <= cutoff,], method = 'lm') + 
    geom_smooth(data=data[data$y.before > cutoff,], method='lm')