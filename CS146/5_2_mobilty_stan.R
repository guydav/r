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

dirichlet_prior = rowSums(mobility.df) / 10

stan_model <- "
data {
int<lower=1> N;                         // total number of classes
int<lower=0> mobility_counts[N,N];      // NxN table of mobility counts
vector<lower=0>[N] alpha;                 // dirichlet prior over alpha
}

parameters {
simplex[N] theta[N];  // probability distribution for the multinomial of each class
}

model {
for(i in 1:N) {
theta[i] ~ dirichlet(alpha);                    // prior for study i
mobility_counts[i] ~ multinomial(theta[i]);     // likelihood for study i
}
}
"

data <- list(
    N = 4,
    alpha = as.vector(dirichlet_prior), #  as.vector(c(1, 1, 1, 1)), # 
    mobility_counts = as.vector(as.data.frame(t(mobility.df)))
)

fit <- stan(
    model_code = stan_model,
    data = data,
    # cores = 1,
    save_warmup = FALSE #,
    # chains = 1,
    # iter = 10
)

# Print out mean, standard deviation and quantiles
print(fit, pars=c("theta"), probs=c(.025, 0.975))

# Get the samples for the model parameters.
samples <- extract(fit)
