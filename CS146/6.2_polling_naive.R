library(gtools)
library(ramify)

SAMPLE_SIZE = 10 ** 5

# naive prior - uninformative
# dirichlet_prior = c(1, 1, 1, 1)

# From the 2012 election results:
# https://en.wikipedia.org/wiki/United_States_presidential_election,_2012
priors.df = data.frame(alabama = c(.3836, .6055, .0059, .0016),
                       alaska = c(.4081, .5480, .0246, .0097), 
                       arizona = c(.4459, .5365, .0140, .0034),
                       arkansas = c(.3688, .6057, .0152, .0087))
row.names(priors.df) <-  c('Obama', 'Romney', 'Johnson', 'Stein')

# Could (should?) generalize to a per-column weight
PRIOR_WEIGHT = 200
priors.df = priors.df * PRIOR_WEIGHT

# Provided data
polling.df = data.frame(alabama = c(0, 0, 0, 0),
                        alaska = c(743, 780, 232, 77), 
                        arizona = c(6809, 7010, 942, 244),
                        arkansas = c(610, 1058, 74, 35))
row.names(polling.df) <-  c('Clinton', 'Trump', 'Johnson', 'Stein')

for (c in 1:ncol(polling.df)) {
    print(colnames(polling.df)[c])
    posterior = priors.df[,c] + polling.df[,c]
    mode = posterior - 1
    mode = mode / sum(mode)
    print(mode)
    
    samples = rdirichlet(SAMPLE_SIZE, posterior)
    winners = max.row(samples)
    results = as.data.frame(table(winners))
    results$Freq = results$Freq / SAMPLE_SIZE 
    print(results)
}

# quantile(unskilled_father_skilled_son_samples, probs = c(0.025, 0.975))

