library(readstata13)
library(foreign)
library(Matching)
library(rgenoud)
library(rbounds)

jtpa.df <- read.dta("./8.2/jtpa.dta")
View(jtpa.df)
attach(jtpa.df)

# 1 - assess balance
MatchBalance(assignmt ~ site + sex + age + prevearn + married + hsorged + black + hispanic + I(age ^ 2) + I(prevearn ^ 2),
             data=jtpa.df, nboots=1000)

# 2 - compliance
sum(assignmt != training) / length(assignmt)

# 3 - treatment effect estimate - difference in differences
jtpa.treat <- jtpa.df[assignmt == 1,]
jtpa.control <- jtpa.df[assignmt == 0,]
d.in.d <- mean(jtpa.treat$earnings) - mean(jtpa.treat$prevearn) - (mean(jtpa.control$earnings) - mean(jtpa.control$prevearn))

# 4 - compliance levels
treatment.p.compliers <- sum(jtpa.treat$training == 1) / nrow(jtpa.treat)
treatment.p.compliers
control.p.compliers <- sum(jtpa.control$training == 0) / nrow(jtpa.control)
control.p.compliers

# 5 - wald estimator
itt.outcomes <- mean(jtpa.treat$earnings) - mean(jtpa.control$earnings)
print("LATE for treatment compliers probability:")
itt.outcomes / treatment.p.compliers
print("LATE using D-in-D for treatment compliers probability:")
d.in.d / treatment.p.compliers

weighted.p.compliers = (treatment.p.compliers * nrow(jtpa.treat) + control.p.compliers * nrow(jtpa.control)) / nrow(jtpa.df)
print("LATE for weighted compliers probability:")
itt.outcomes / weighted.p.compliers
print("LATE using D-in-D for weighted compliers probability:")
d.in.d / weighted.p.compliers