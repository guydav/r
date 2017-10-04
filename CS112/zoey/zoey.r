library(ebal)
library(foreign)
library(Matching)
library(ggplot2)
setwd('~/projects/r/zoey/')

indiv.data <- read.dta('./BW JOP individual replication data.dta')
indiv.data$unemployed <- as.logical(indiv.data$unemployed)
indiv.noNA <- na.omit(cbind.data.frame(indiv.data$age, indiv.data$educ, indiv.data$income, 
                                       indiv.data$fips_state, indiv.data$black, indiv.data$other,
                                       indiv.data$hisp, indiv.data$female, indiv.data$married,
                                       indiv.data$republican, indiv.data$democrat, indiv.data$unemployed,
                                       indiv.data$incomedk, indiv.data$partyvote, indiv.data$uerate, 
                                       indiv.data$totalspend_voter_inf))
colnames(indiv.noNA) <- gsub("indiv.data\\$", "", colnames(indiv.noNA))
### Resulting dataframe contains 7620 observations, 1040 fewer than original individual data

###dataframe of all the covariates, excluding treatment and outcomes variables
chosenones <- cbind(indiv.noNA$age, indiv.noNA$educ, indiv.noNA$income, 
                    indiv.noNA$fips_state, indiv.noNA$black, indiv.noNA$other,
                    indiv.noNA$hisp, indiv.noNA$female, indiv.noNA$married,
                    indiv.noNA$republican, indiv.noNA$democrat,
                    indiv.noNA$incomedk, indiv.noNA$uerate, 
                    indiv.noNA$totalspend_voter_inf)

indiv.entropy.balance <- ebalance(Treatment = indiv.noNA$unemployed, chosenones)
trimmed.entropy.balance <- ebalance.trim(indiv.entropy.balance)
all.weights <- numeric(nrow(indiv.noNA))
all.weights[indiv.noNA$unemployed == 0] <- trimmed.entropy.balance$w
all.weights[indiv.noNA$unemployed == 1] <- 1
balance <- MatchBalance(unemployed ~ . - partyvote, indiv.noNA, weights = all.weights)

ggplot() + 
    geom_bar(data=indiv.noNA[indiv.noNA$unemployed == 1,], aes(x=age, colour='red', fill='red', y=..count../sum(..count..)), position='dodge', alpha=0.66, binwidth = 5) + 
    geom_bar(data=indiv.noNA[indiv.noNA$unemployed == 0,], aes(x=age, colour='blue', fill='blue', y=..count../sum(..count..)), position='dodge', alpha=0.66, binwidth = 5) 

age.threshold <- 60
indiv.young.noNA <- indiv.noNA[indiv.noNA$age < age.threshold,]    
ggplot() + 
    geom_bar(data=indiv.young.noNA[indiv.young.noNA$unemployed == 1,], aes(x=age, colour='red', fill='red', y=..count../sum(..count..)), position='dodge', alpha=0.66, binwidth = 5) + 
    geom_bar(data=indiv.young.noNA[indiv.young.noNA$unemployed == 0,], aes(x=age, colour='blue', fill='blue', y=..count../sum(..count..)), position='dodge', alpha=0.66, binwidth = 5) 

young.chosenones <- cbind(indiv.young.noNA$age, indiv.young.noNA$educ, indiv.young.noNA$income, 
                          indiv.young.noNA$fips_state, indiv.young.noNA$black, indiv.young.noNA$other,
                          indiv.young.noNA$hisp, indiv.young.noNA$female, indiv.young.noNA$married,
                          indiv.young.noNA$republican, indiv.young.noNA$democrat,
                          indiv.young.noNA$incomedk, 
                          indiv.young.noNA$totalspend_voter_inf)

young.balance <- ebalance(Treatment = indiv.young.noNA$unemployed, young.chosenones)
trimmed.young.balance <- ebalance.trim(young.balance)
young.weights <- numeric(nrow(indiv.young.noNA))
young.weights[indiv.young.noNA$unemployed == 0] <- trimmed.young.balance$w
young.weights[indiv.young.noNA$unemployed == 1] <- 1
balance <- MatchBalance(unemployed ~ . - uerate -  partyvote, indiv.young.noNA, weights = all.weights)
