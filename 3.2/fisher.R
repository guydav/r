city.names <- c("A", "B", "C", "D", "E", "F", "G", "H")
observed.turnout <- c(17, 30, 13, 55, 26, 29, 48, 43)

observed.diffmeans <- mean(observed.turnout[c(2,4,6,8)]) - 
  mean(observed.turnout[c(1,3,5,7)])

print(observed.diffmeans)

foo <- data.frame(cities=city.names, turnouts=observed.turnout)
print(foo)

# Assignment function
assignment <- function() {
  # Four coin flips, establishing random assignment
  assig        <- foo[sample(1:2),]
  assig[3:4,]  <- foo[sample(3:4),]
  assig[5:6,]  <- foo[sample(5:6),]
  assig[7:8,]  <- foo[sample(7:8),]
  
  treatment.group   <- assig[c(1,3,5,7),]
  control.group     <- assig[c(2,4,6,8),]
  
  return(mean(treatment.group[,2]) - mean(control.group[,2]))
}

# Iterating the Assignment function
iter.RI <- function(iterations = 10000) {
  for (i in 1:iterations) {
    if (i %% 1000 == 0) {
      print(i)
    }
    storage.vector[i] <- assignment()
  }
  
  return(storage.vector)
}

storage.vector <- NULL
results <- iter.RI()

# Exploring the results

quantile(results, prob = c(0.95, 0.975))

length(unique(results))

# hist(results)
results_df = data.frame(results=results)
qplot(results, data=results_df, binwidth=0.5, geom="histogram")

# plot(density(results))
# abline(v = 5, lwd = 2, col = "red")
ggplot(results_df, aes(x=results)) + geom_density() + geom_vline(xintercept = observed.diffmeans, color="red")

print("P-value:")
print(sum(results > observed.diffmeans) / length(results))

