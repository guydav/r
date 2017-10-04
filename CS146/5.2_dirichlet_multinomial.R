library(gtools)

SAMPLE_SIZE = 10 ** 5

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

dirichlet_prior = c(100, 100, 100, 100)

unskilled_father_posterior = dirichlet_prior + mobility.df[,'unskilled']
unskilled_father_samples = rdirichlet(SAMPLE_SIZE, unskilled_father_posterior)
unskilled_father_skilled_son_samples = unskilled_father_samples[,3]
quantile(unskilled_father_skilled_son_samples, probs = c(0.025, 0.975))

son_professional_posterior = as.numeric(dirichlet_prior + mobility.df['professional',])
son_professional_samples = rdirichlet(SAMPLE_SIZE, son_professional_posterior)
son_professional_father_farm_samples = son_professional_samples[,1]
quantile(son_professional_father_farm_samples, probs = c(0.025, 0.975))


