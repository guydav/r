survival.nrow <- nrow(survival.14.days.df)
# set aside 40% of the sample as dev and test
survival.dev.test.indices <- sample(1:survival.nrow, 0.4 * survival.nrow) 
dev.test.length <- length(survival.dev.test.indices)
# Take half of those 50% (as test indices within that sample)
survival.test.sub.indices <- sample(1:dev.test.length, 0.5 * dev.test.length)
survival.test.indices <- survival.dev.test.indices[survival.test.sub.indices]
survival.dev.indices <- survival.dev.test.indices[-survival.test.sub.indices]

survival.train <- survival.14.days.df[-survival.dev.test.indices,]
survival.dev <- survival.14.days.df[survival.dev.indices,]
survival.test <- survival.14.days.df[survival.test.indices,]