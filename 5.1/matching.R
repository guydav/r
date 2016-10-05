library("Matching")
library("rgenoud")
library("rbounds")

setwd("~/projects/r/5.1/")

lalonde.columns <- c("treatment", "age", "education", "black", "hispanic", "married", "no.degree", "re75", "re78")
lalonde.control <- data.frame(read.table("lalonde_nsw_control.txt"))
colnames(lalonde.control) <- lalonde.columns
lalonde.treatment <- data.frame(read.table("lalonde_nsw_treated.txt"))
colnames(lalonde.treatment) <- lalonde.columns

dw.columns <- c("treatment", "age", "education", "black", "hispanic", "married", "no.degree", "re74", "re75", "re78")
dw.control <- data.frame(read.table("dw_nswre74_control.txt"))
colnames(dw.control) <- dw.columns
dw.treatment <- data.frame(read.table("dw_nswre74_treated.txt"))
colnames(dw.treatment) <- dw.columns

print(paste("Lalonde differnce in means:", mean(lalonde.treatment$re78) - mean(lalonde.control$re78)))
print(paste("DW differnce in means:", mean(dw.treatment$re78) - mean(dw.control$re78)))

print(t.test(lalonde.control$age, lalonde.treatment$age))
print(t.test(lalonde.control$education, lalonde.treatment$education))
print(t.test(lalonde.control$black, lalonde.treatment$black))

print(t.test(dw.control$age, dw.treatment$age))
print(t.test(dw.control$education, dw.treatment$education))
print(t.test(dw.control$black, dw.treatment$black))

cps.control <- data.frame(read.table("cps_controls.txt"))
colnames(cps.control) <- dw.columns

cps.dw <- rbind(cps.control, dw.treatment)
cps.match <- Match(Y=cps.dw$re78, Tr=cps.dw$treatment, X=subset(cps.dw, select=c("age", "education", "black", "hispanic", "married", "no.degree")))
summary(cps.match)
cps.match.balance  <- MatchBalance(treatment~age + I(age^2) + education + I(education^2) + black +
                      hispanic + married + no.degree + re74  + I(re74^2) + re75 + I(re75^2), 
                      data=cps.dw, match.out=cps.match, nboots=10)
summary(cps.match.balance)
psens(cps.match, Gamma=3, GammaInc=.1)
#hlsens(cps.match, Gamma=2, GammaInc=.1, .1)

cps.gen.weights <- GenMatch(Tr=cps.dw$treatment, X=subset(cps.dw, select=c("age", "education", "black", "hispanic", "married", "no.degree")))
summary(cps.gen.weights)
cps.gen.match <- Match(Y=cps.dw$re78, Tr=cps.dw$treatment, X=subset(cps.dw, select=c("age", "education", "black", "hispanic", "married", "no.degree")), Weight.matrix=cps.gen.weights)
cps.gen.match.balance  <- MatchBalance(treatment~age + I(age^2) + education + I(education^2) + black +
                                     hispanic + married + no.degree + re74  + I(re74^2) + re75 + I(re75^2), 
                                   data=cps.dw, match.out=cps.gen.match, nboots=10)
summary(cps.gen.match.balance)
psens(cps.gen.match, Gamma=3, GammaInc=.1)
#hlsens(cps.gen.match, Gamma=2, GammaInc=.1, .1)