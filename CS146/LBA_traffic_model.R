library(gtools)
library(ramify)

PRIOR_ALPHA = 46
PRIOR_BETA = 8

DATA_MINUTES = 5

traffic.data.df = data.frame(count = c(27, 42, 30, 37, 35, 19, 42, 19),
                             post.alpha = rep.int(PRIOR_ALPHA, 8), 
                             post.beta = rep.int(PRIOR_BETA, 8))
row.names(traffic.data.df) <-  c('N_in', 'N_out', 'S_in', 'S_out', 
                                 'E_in', 'E_out', 'W_in', 'W_out')

traffic.data.df$post.alpha = traffic.data.df$post.alpha + traffic.data.df$count
traffic.data.df$post.beta = traffic.data.df$post.beta + DATA_MINUTES
# probability of success
traffic.data.df$post.pred.p = 1 / (1 + traffic.data.df$post.beta)  

for (r in 1:nrow(traffic.data.df)) {
    print(c(rownames(traffic.data.df)[r], 
            traffic.data.df$post.alpha[r], 
            traffic.data.df$post.beta[r]))
    # Probabilities subtracted from 1 since R expects the probability of failure
    print(qnbinom(c(0.25, 0.75), 
                  traffic.data.df$post.alpha[r], 
                  1 - traffic.data.df$post.pred.p[r]))
    print(qnbinom(c(0.025, 0.975), 
                  traffic.data.df$post.alpha[r], 
                  1 - traffic.data.df$post.pred.p[r]))
}


