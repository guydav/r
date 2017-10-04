library(ggplot2)

# 2.3

n <- 1000
p <- 1/6
mu <- n * p
sigma <- sqrt(n * p * (1-p))
df <- data.frame(x=round(mu - 4 * sigma):round(mu + 4 * sigma))

ggplot(df, aes(x=x,)) + 
    stat_function(fun=dnorm,args=list(mean=mu, sd=sigma)) +
    # stat_function(fun=dbinom,args=list(size=n, prob=p)) +
    theme(aspect.ratio=3/4) +
    xlab("Number of 6's") + 
    ylab("Normal approximation probability density")


percentiles <- c(0.05, 0.25, 0.50, 0.75, 0.95)
qnorm(percentiles, mu, sigma)

# 2.8

