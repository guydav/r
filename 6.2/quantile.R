library("Matching")
library("rgenoud")
library("rbounds")
library("quantreg")
library("ggplot2")

data(lalonde)
attach(lalonde)

ll.X <- cbind(age, educ, black, hisp, married, nodegr, u74, u75, re75, re74)
ll.balance.matrix <- cbind(age, educ, black, hisp, married, nodegr, u74, u75, re75, re74, I(re74*re75))
ll.weights <- GenMatch(Tr=treat, X=ll.X, BalanceMatrix=ll.balance.matrix)
summary(quantile.weights)
ll.Y <- re78 / 1000
ll.gen.match <- Match(Y=ll.Y, Tr=treat, X=ll.X, Weight.matrix=ll.weights)
summary(ll.gen.match)
ll.gen.balance <- MatchBalance(treat~age + educ + black + hisp + married + nodegr + u74 + u75 + re75 + re74 + I(re74*re75), match.out=ll.gen.match, nboots=500)
summary(ll.gen.balance)

ll.treated = lalonde[ll.gen.match$index.treated,'re78'] / 1000
ll.control = lalonde[ll.gen.match$index.control,'re78'] / 1000
ll.treated.quantile = quantile(ll.treated, seq(0, 1, 0.05))
ll.control.quantile = quantile(ll.control, seq(0, 1, 0.05))
ll.quantile.diffs = ll.treated.quantile - ll.control.quantile

ll.treated.df = data.frame(Y=ll.treated)
ll.control.df = data.frame(Y=ll.control)
ggplot() + geom_density(data=ll.treated.df, aes(Y, color='#FF0000')) + geom_density(data=ll.control.df, aes(Y, color='#00FF00')) +
    + xlab("Outcome - re78 / 1000") + ylab("Density") + ggtitle("Outcome variable distribution density plots")

ll.quantile.df = data.frame(treated=ll.treated.quantile, control=ll.control.quantile, quantile=seq(0, 100, 5))
ggplot(data=ll.quantile.df) + geom_point(aes(x=quantile, y=treated, color='#FF0000', label='Treatment')) + geom_point(aes(x=quantile, y=control, color='#00FF00')) +
    + legend("Hello") +
    xlab("Quantile") + ylab("Outcome - re78 / 1000") + ggtitle("Quantile value plots for control vs. treatment")

ll.qr <- rq(Y~age + educ + black + hisp + married + nodegr + u74 + u75 + re75 + re74 + I(re74*re75), data = lalonde, tau=seq(0,1,0.05))
plot.rqs(ll.qr)

ll.qr.nomin <- rq(Y~age + educ + black + hisp + married + nodegr + u74 + u75 + re75 + re74, data = lalonde, tau=seq(0.1,0.9,0.1))
plot.summary.rqs(summary(ll.qr.nomin))
