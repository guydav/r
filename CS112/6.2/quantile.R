library("Matching")
library("rgenoud")
library("rbounds")
library("quantreg")
library("ggplot2")
library("plyr")

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
ggplot() + geom_density(data=ll.treated.df, aes(Y), color='#FF0000') + geom_density(data=ll.control.df, aes(Y), color='#0000FF') +
    geom_vline(xintercept = median(ll.treated.df$Y), color='#FF8888') + 
    geom_vline(xintercept = median(ll.control.df$Y), color='#8888FF') +
    geom_text(aes(x=median(ll.treated.df$Y) + 2, label=median(ll.treated.df$Y), y=0.12), color="red") + 
    geom_text(aes(x=median(ll.control.df$Y) - 3, label=median(ll.control.df$Y), y=0.102), color="blue") +
    geom_text(aes(x=median(ll.treated.df$Y)-median(ll.control.df$Y) + 1, label=paste('Median', '\n', median(ll.treated.df$Y)-median(ll.control.df$Y)), y=0.01), color="black") +
    geom_vline(xintercept = quantile(ll.treated.df$Y, 0.8), color='#FF8888') +
    geom_vline(xintercept = quantile(ll.control.df$Y, 0.8), color='#8888FF') +   
    geom_text(aes(x=quantile(ll.control.df$Y, 0.8) + 1, label='80%', y=0.01), color="black") +
    geom_vline(xintercept = quantile(ll.treated.df$Y, 0.9), color='#FF8888') +
    geom_vline(xintercept = quantile(ll.control.df$Y, 0.9), color='#8888FF') +
    geom_text(aes(x=quantile(ll.control.df$Y, 0.95) - 1, label='95%', y=0.005), color="black") +
    xlab("Outcome - re78 / 1000") + ylab("Density") + 
    ggtitle("Outcome variable distribution density plots") + theme(axis.line=element_line(color='#FFFFFF')) 

ggplot() + stat_ecdf(data=ll.treated.df, aes(Y), color='#FF0000') + stat_ecdf(data=ll.control.df, aes(Y), color='#0000FF') +
    xlab("Outcome - re78 / 1000") + ylab("Cumulative Density") + ggtitle("Outcome variable CDF Plots")

ll.quantile.df = data.frame(treated=ll.treated.quantile, control=ll.control.quantile, quantile=seq(0, 100, 5))
ggplot(data=ll.quantile.df) + geom_point(aes(x=quantile, y=treated, color='#FF0000', label='Treatment')) + geom_point(aes(x=quantile, y=control, color='#00FF00')) 
    + xlab("Quantile") + ylab("Outcome - re78 / 1000") + ggtitle("Quantile value plots for control vs. treatment")

ll.qr <- rq(Y~age + educ + black + hisp + married + nodegr + u74 + u75 + re75 + re74 + I(re74*re75), data = lalonde, tau=seq(0,1,0.05))
plot.rqs(ll.qr)

ll.qr.nomin <- rq(Y~age + educ + black + hisp + married + nodegr + u74 + u75 + re75 + re74, data = lalonde, tau=seq(0.1,0.9,0.1))
plot.summary.rqs(summary(ll.qr.nomin))
