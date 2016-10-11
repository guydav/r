library("Matching")
library("rgenoud")
library("rbounds")
library("plyr")
data("GerberGreenImai")

D<-GerberGreenImai$PHN.C1 #treatment phone calls
Y<-GerberGreenImai$VOTED98 #outcome, turnout
X  <- fitted(pscore.glm)

# pscore.glm<-glm(PHN.C1 ~ PERSONS + VOTE96.1 + NEW + MAJORPTY + AGE +
# WARD + PERSONS:VOTE96.1 + PERSONS:NEW + AGE2, 
# family=binomial(logit), data=GerberGreenImai)

GerberGreenImai$NUMERIC_WARD = as.numeric(levels(WARD))[WARD]
attach(GerberGreenImai)

ggi.balance = cbind(AGE, AGE2, PERSONS, VOTE96.1, NEW, MAJORPTY, WARD, I(PERSONS*VOTE96.1), I(PERSONS*NEW))
ggi.balance.subset = cbind(AGE, AGE2, APPEAL, PERSONS, I(PERSONS * VOTE96.1), NEW, I(PERSONS * NEW), NUMERIC_WARD)
gen.weights <- GenMatch(Tr=D, X=ggi.balance.subset, BalanceMatrix=ggi.balance)
summary(gen.weights)
gen.match <- Match(Y=Y, Tr=D, X=ggi.balance.subset, Weight.matrix=gen.weights)
summary(gen.match)
psens(gen.match, Gamma=10, GammaInc=.5)
cps.match.balance  <- MatchBalance(D ~ AGE + AGE2 + PERSONS + VOTE96.1 + NEW + MAJORPTY +
                                       + WARD + I(PERSONS*VOTE96.1) + I(PERSONS*NEW), data=GerberGreenImai, match.out=gen.match, nboots=10)


ggi.pca.subset = data.frame(cbind(PERSONS, QUESTION, PHONEGRP, APPEAL, log(AGE), MAJORPTY, VOTE96.0, VOTE96.1, VOTED98, 
                       PHNSCRPT, DIS.PHN, PHN.C, PHNTRT1, PHNTRT2, PHN.C1, PHN.C2, NEW, log(AGE2)))
ggi.pca <- prcomp(ggi.pca.subset, center=TRUE, scale=TRUE)
summary(ggi.pca)
ggi.props <- data.frame(props=ggi.pca$sdev^2 / sum(ggi.pca$sdev ^ 2))
ggi.props <- mutate(ggi.props, cumulative=cumsum(props))

ggplot(ggi.props) + geom_line(aes(x=1:length(ggi.props$cumulative), y=cumulative)) + 
    geom_point(aes(x=1:length(ggi.props$cumulative), y=props)) + 
    ggtitle("Cumulative and Individual Variances") + xlab("Variable Number") + ylab("Variance")

ggi.pca.balance = cbind(AGE, AGE2, PERSONS, VOTE96.1, NEW, MAJORPTY, WARD, I(PERSONS*VOTE96.1), I(PERSONS*NEW))
ggi.pca.df <- data.frame(ggi.pca$x)
ggi.pca.weights <- GenMatch(Tr=D, X=ggi.pca.df[,1:8], BalanceMatrix=ggi.pca.balance)
summary(ggi.pca.weights)
ggi.pca.match <- Match(Y=Y, Tr=D, X=ggi.pca.df[,1:8], Weight.matrix=ggi.pca.weights)
summary(ggi.pca.match)
ggi.pca.match.balance  <- MatchBalance(D ~ AGE + AGE2 + PERSONS + VOTE96.1 + NEW + MAJORPTY + WARD + I(PERSONS*VOTE96.1) + I(PERSONS*NEW), 
                                       data=GerberGreenImai, match.out=ggi.pca.match, nboots=10)
summary(ggi.pca.match.balance)

