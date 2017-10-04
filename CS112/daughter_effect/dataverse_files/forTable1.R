# Generates Table 1

library(cem)
data(LL)
mod <- glm(treated ~ . - re78, data=LL)
w <- predict(mod)
idx <- which(LL$treated==1)
w[idx] <- 1-w[idx] # pscore weights


set.seed(123)

imb0 <- L1.profile(LL$treated,LL, max.cut=20, drop=c("treated","re78"),M=250,plot=FALSE)


#on the original data
raw <- imbalance(LL$treated, LL, drop=c("re78","treated"), br=imb0$medianCP)

# after pscore weighing
pw <- imbalance(LL$treated, LL, drop=c("re78","treated"), br=imb0$medianCP, weights = w)

nm <- names(LL)
nm <- nm[-c(1,9)]

br <- list()
for(i in nm)
 br[i] <- 9
names(br) <- nm

mat <- cem("treated", LL, drop="re78",cut=br)

# after cem
cm <- imbalance(LL$treated, LL, drop=c("re78","treated"), br=imb0$medianCP, weights = mat$w)


m1 <- cbind( raw$tab["statistic"], pw$tab["statistic"], cm$tab["statistic"])
m2 <- rbind( m1, c(raw$L1$L1,pw$L1$L1,cm$L1$L1) )
colnames(m2) <- c("RAW", "PSW", "CEM")
rownames(m2)[NROW(m2)] <- "L1"


