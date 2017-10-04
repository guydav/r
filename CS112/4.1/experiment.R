library(ggplot2)

storage.vector <- NA

# Function that assigns treatment/control depending on 
# propensity scores (assignment probabilities)
experiment <- function(vector.of.probabilities = NULL) {
  k = 0
  for (i in 1:length(vector.of.probabilities)) {
    if(
      sample(x = c(1,0), size = 1, 
             prob = c(vector.of.probabilities[i], 1 - vector.of.probabilities[i])) == 1) {
      
      storage.vector[k] <- i
      k = k + 1
    }
  }
  
  return(list(treated.units = storage.vector, 
              control.units = (1:(length(vector.of.probabilities)))[-storage.vector]))
}

set.seed(12345)

men.income <- round(abs(exp(rnorm(1000, 5, 1))))
income_df = data.frame(men=men.income)
qplot(men, data=income_df, binwidth=100, geom="histogram", col=I("grey"))

women.children <- round(sqrt(abs((rnorm(1000, 12, 100)))))
children_df = data.frame(women=women.children)
qplot(women, data=children_df, binwidth=1, geom="histogram", col=I("grey"))

example_df = data.frame(sex=c(replicate(4, 'F'), replicate(5, 'M')), 
                        age=c(28, 45, 26, 24, 22, 35, 37, 32, 34), 
                        prop=c(0.68, 0.42, 0.73, 0.79, 0.63, 0.40, 0.38, 0.44, 0.41), 
                        w=c(1, 1, 0, 1, 0, 0, 1, 0, 0))

ggplot(example_df) + geom_point(mapping=aes(x=prop, y=w)) + xlim(0.2, 0.8) + ylim(-0.5, 1.5)

exp = experiment(example_df[['prop']])

probs.men <- 0.5*((((max(men.income) + 100) - men.income)/(max(men.income) + 100))^4)
probs.women <- women.children/(max(women.children) + 1)

men = experiment(probs.men)
women = experiment(probs.women)
