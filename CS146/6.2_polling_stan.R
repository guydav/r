library(stringr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# From the 2012 election results:
# https://en.wikipedia.org/wiki/United_States_presidential_election,_2012
priors.df = data.frame(alabama = c(.3836, .6055, .0059, .0016),
                       alaska = c(.4081, .5480, .0246, .0097), 
                       arizona = c(.4459, .5365, .0140, .0034),
                       arkansas = c(.3688, .6057, .0152, .0087))
row.names(priors.df) <-  c('Obama', 'Romney', 'Johnson', 'Stein')



# Provided data
polling.df = data.frame(alabama = c(0, 0, 0, 0),
                        alaska = c(743, 780, 232, 77), 
                        arizona = c(6809, 7010, 942, 244),
                        arkansas = c(610, 1058, 74, 35))

stan_model <- "
data {
    int<lower=1> N;                         // total number of states
    int<lower=2> C;                         // total number of candidates
    int<lower=0> polling_counts[N, C];      // NxC table of polling votes per candidate/state
    vector<lower=0>[N] unscaled_alpha[C];   // NxC table of prior data per candidate/state
    real<lower=0> nu;                       // hyper-parameter for alpha dirichlet scaling
}

parameters {
    simplex[N] theta[C];                    // probability distribution for the multinomial of each state
    // simplex[N] unscaled_alpha[N];        // hyperparameters for dirichlet prior for theta
    // vector<lower=0>[N] gamma;            // Scale hyperparameter for scaled dirichlet hyperprior
}

transformed parameters {
    vector<lower=0>[N] alpha[C];    // hyperparameters for dirichlet prior for theta    
    for(i in 1:N) {
        alpha[i] = unscaled_alpha[i] * nu;
    }
}

model {
    for(i in 1:N) {
        // unscaled_alpha[i] ~ dirichlet(beta);    // hyperprior for state i
        theta[i] ~ dirichlet(alpha[i]);            // prior for state i
        polling_counts[i] ~ multinomial(theta[i]); // likelihood for state i
    }
}
"

polling_data <- list(
    N = 4,
    C = 4,
    polling_counts = as.vector(as.data.frame(t(polling.df))),
    unscaled_alpha = as.vector(as.data.frame(t(priors.df))),
    # Could (should?) generalize to a per-column weight
    nu = 200
)

polling_fit <- stan(
    model_code = stan_model,
    data = polling_data,
    # cores = 1,
    save_warmup = FALSE #,
    # chains = 1,
    # iter = 10
)

# Print out mean, standard deviation and quantiles
print(polling_fit, pars=c('lp__'))
print(polling_fit, pars=c("theta"), probs=c(.025, 0.975))
# print(polling_fit, pars=c("alpha"), probs=c(.025, 0.975))
# print(polling_fit, pars=c("gamma"), probs=c(.025, 0.975))

# Get the samples for the model parameters.
samples = extract(hyperprior_fit)
theta_samples = as.data.frame(samples$theta)


for (c in 1:ncol(polling.df)) {
    print(colnames(polling.df)[c])
    ts = theta_samples[str_detect(names(theta_samples), paste(c, "\\..*", sep = ""))]
    print(ts[1:10,])
    winners = argmax(ts)
    results = as.data.frame(table(winners))
    results$Freq = results$Freq / 1000 
    print(results)
}

