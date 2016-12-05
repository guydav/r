library(foreign)
library(WhatIf)

setwd('./un_policy/')
peace.df <- read.dta('./replication_data/peace.dta')

# Take only a selected subset of the original data set
minimal.cols <- c("decade", "wardur", "wartype", "un2int", "nouncint", "peaceop", 
                  "interven", "major", "gdpcap", "area",
                  "factnum", "factnum2", "logcost", "trnsfcap", 
                  "develop", "exp", "treaty", "pbs2s3")
peace.df.minimal.raw <- peace.df[,minimal.cols]
peace.df.minimal.raw <- na.omit(peace.df.minimal.raw)
rows <- nrow(peace.df.minimal.raw)

# This function runs a whatif convex hull test on a subset of the data with itself
# and returns the indices of rows that are not in the convex hull
whatif.with.itself <- function(data) {
    which(summary(whatif(data=data, cfact=data))$sum.df$in.hull == FALSE)    
}

results <- data.frame(i=numeric(1), outside.hull=numeric(1), stringsAsFactors = FALSE)

for (i in c(seq(10, rows - rows %% 10, 10), rows)) { 
    res <- whatif.with.itself(peace.df.minimal.raw[1:i,])
    print(paste('Outside of hull in first', i, 'rows:', paste(res, collapse = ', ')))
    results <- rbind(results, c(i, length(res)))
}

results
