library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

father_farm = c(703, 1478, 1430, 1109)
# father_farm = father_farm / sum(father_farm)
father_unskilled = c(58, 1756, 1630, 1568)
# father_unskilled = father_unskilled / sum(father_unskilled)
father_skilled = c(63, 1453, 2068, 2483)
# father_skilled = father_skilled / sum(father_skilled)
father_professional = c(61, 749, 1183, 3315)
# father_professional = father_professional / sum(father_professional)

mobility.df = data.frame(farm = c(703, 1478, 1430, 1109), 
                         unskilled = c(58, 1756, 1630, 1568),
                         skilled = c(63, 1453, 2068, 2483),
                         professional = c(61, 749, 1183, 3315))
row.names(mobility.df) <- c('farm', 'unskilled', 'skilled', 'professional')

beta = rowSums(mobility.df) / sum(mobility.df)

stan_model <- "
data {
    int<lower=1> N;                         // total number of classes
    int<lower=0> mobility_counts[N,N];      // NxN table of mobility counts
    simplex[N] beta;                        // Hyperparameters for scaled dirichlet over alpha
    real<lower=0> gamma;                    // Scale hyperparameter for scaled dirichlet hyperprior
}

parameters {
    simplex[N] theta[N];                    // probability distribution for the multinomial of each class
    simplex[N] unscaled_alpha[N];  // hyperparameters for dirichlet prior for theta
}

transformed parameters {
    vector<lower=0>[N] alpha[N];        // hyperparameters for dirichlet prior for theta
    for(i in 1:N) {
        alpha[i] = gamma * unscaled_alpha[i];
    }
}

model {
    for(i in 1:N) {
        unscaled_alpha[i] ~ dirichlet(beta);    // hyperprior for class i
        theta[i] ~ dirichlet(alpha[i]);         // prior for class i
        mobility_counts[i] ~ multinomial(theta[i]);     // likelihood for class i
    }
}
"

hyperprior_data <- list(
    N = 4,
    beta = as.vector(beta),
    gamma = sum(mobility.df) / 100,
    mobility_counts = as.vector(as.data.frame(t(mobility.df)))
)

hyperprior_fit <- stan(
    model_code = stan_model,
    data = hyperprior_data,
    # cores = 1,
    save_warmup = FALSE #,
    # chains = 1,
    # iter = 10
)

# Print out mean, standard deviation and quantiles
print(hyperprior_fit, pars=c("theta"), probs=c(.025, 0.975))
print(hyperprior_fit, pars=c("alpha"), probs=c(.025, 0.975))


# Get the samples for the model parameters.
samples <- extract(hyperprior_fit)
