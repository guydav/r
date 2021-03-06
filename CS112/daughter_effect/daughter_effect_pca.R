library("Matching")
library("rgenoud")
library("rbounds")
library("plyr")
library("ggplot2")
library("foreign")
library("GGally")
library("reshape2")
library("gridExtra")

set.seed(33)

ESTIMAND = "ATC"

basic.df <- read.dta("./daughter_effect/dataverse_files/basic.dta")
sample.df <- basic.df[!is.na(basic.df$anygirls),] # remove any rows in which the treatment variable is NA
sample.df <- sample.df[!is.na(sample.df$nowtot),] # remove rows in which the outcome variable is NA
sample.df <- sample.df[sample.df$year=='1997-1998',] # keep only the 105th congress, as King did
sample.df$rgroup.factor = as.factor(sample.df$rgroup)
sample.df <- cbind(sample.df, model.matrix(~ rgroup.factor - 1, data=sample.df)) # bind the individual factors as columns as well
# sample.df <- sample.df[,c(7:12, 16:38,41)] # removing unnecesary columns for this analysis
table(sample.df$anygirls)
sample.df <- na.omit(sample.df) 
# omitting rows with any NAs loses 6 control and 27 treatment elements 
# unfortunate, but not tragic, as we already have way more treatment than control elements.
table(sample.df$anygirls)
attach(sample.df)

# Covariate elaboration: 
# rgroup: religious group, repub: is republican, srvlng: # of years of veterancy in the house of reps,
# demvote: democratic vote share in most recent presidential election (1996), medinc: median income in district
# perf/w/hs/col/ur: proportion female (voting age) / white (voting age) / high school graduate (25+) / college graduate (25+) / urban in the district 
# drXper: percent christian / catholic / mormon or jehovahs / other / no religion in the state
# alabort/moreserv/moredef/morecrimesp/protgay: stances on different issues in the state
# statabb: state code, statalph: state number code, region: state district #
# aauw / rtl: scores assigned by different organization

sample.pca.subset = cbind(#rgroup.factor0, rgroup.factor1, rgroup.factor2, rgroup.factor3, rgroup.factor4,
    repub, srvlng, female, white, age, demvote, medinc, rgroup.factor,
    perf, perw, perhs, percol, perur, alabort, moreserv, moredef, morecrimesp, protgay,
    dr1per, dr2per, dr3per, dr4per, dr5per)

sample.balance.subset = cbind(#rgroup.factor0, rgroup.factor1, rgroup.factor2, rgroup.factor3, rgroup.factor4,
                              repub, srvlng, female, white, age, rgroup.factor, 
                              demvote, medinc,
                              perf, perw, perhs, percol, perur, alabort, moreserv, moredef, morecrimesp, protgay,
                              dr1per, dr2per, dr3per, dr4per, dr5per,
                              I(age ^ 2), I(srvlng ^ 2), I(age * srvlng))

sample.pca <- prcomp(sample.balance.subset, center = TRUE, scale. = TRUE)
summary(sample.pca)

sample.pca.props <- data.frame(props=sample.pca$sdev^2 / sum(sample.pca$sdev ^ 2))
sample.pca.props <- mutate(sample.pca.props, cumulative=cumsum(props))
sample.pca.props
ggplot(sample.pca.props) + geom_line(aes(x=1:length(sample.pca.props$cumulative), y=cumulative)) + 
    geom_point(aes(x=1:length(sample.pca.props$cumulative), y=props)) + 
    ggtitle("Cumulative and Individual Variances") + xlab("Variable Number") + ylab("Variance")

sample.pca.df <- data.frame(sample.pca$x)
sample.pca.weights <- GenMatch(Tr=anygirls, X=sample.pca.df, BalanceMatrix=sample.balance.subset, 
                               max.generations=500, pop.size=5000, wait.generations = 10, estimand = ESTIMAND,
                               int.seed=33, unif.seed=33)
summary(sample.pca.weights)
sample.pca.match <- Match(Y=nowtot, Tr=anygirls, X=sample.pca.df, Weight.matrix=sample.pca.weights, 
                      estimand = ESTIMAND, BiasAdjust = TRUE)
summary(sample.pca.match)
sample.pca.match.balance  <- MatchBalance(anygirls ~ # rgroup.factor0 + rgroup.factor1 + rgroup.factor2 + rgroup.factor3 + rgroup.factor4 +
                                          repub + srvlng + female + white + age + rgroup.factor + 
                                          demvote + medinc + perf + perw + perhs + percol + perur + alabort + 
                                          moreserv + moredef + morecrimesp + protgay + dr1per + dr2per + 
                                          dr3per + dr4per + dr5per + 
                                          I(age ^ 2) + I(srvlng ^ 2) + I(age * srvlng),
                                          data=sample.df, match.out=sample.pca.match, nboots=1000)
summary(sample.pca.match.balance)

# Plot akin to King's figure 4
parallel.plot.data <- data.frame(religion=rescale01(rgroup), party=repub, seniority=rescale01(srvlng), 
                                 gender=female, race=white, age=rescale01(age), w=anygirls, name=name)
parallel.plot.data$matched = 0
parallel.plot.data[sample.pca.match$index.treated,'matched']=1
parallel.plot.data[sample.pca.match$index.control,'matched']=1
parallel.plot.melted <- melt(parallel.plot.data, id.vars = c("name", "matched"))
parallel.plot.melted$matched = as.factor(parallel.plot.melted$matched)

ggplot(parallel.plot.melted, aes(x = variable, y = value)) +
    geom_path(aes(group = name, color = matched), size = 0.2) +
    theme(strip.text.x = element_text(size = rel(0.8)),
          axis.text.x = element_text(size = rel(0.8)),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    xlab("") + ylab("") + ggtitle("Parallel Plot for PCA Matched Data, Many Covariates") +
    scale_colour_discrete(drop=TRUE, limits = levels(parallel.plot.melted$w), name="",
                          breaks=c(0, 1),
                          labels=c("Unmatched", "Matched")) + 
    guides(color = guide_legend(ncol=1)) 


# Key Covariate Histograms
create.covariate.histograms <- function(data.set, title) {
    hist.rgroup <- ggplot(data.set, aes(rgroup, fill=anygirls.factor)) + 
        geom_histogram(aes(y=..density.. * 0.33), position = "dodge", binwidth = 0.33) +
        ggtitle("Religious Group") + xlab("Religious Group") + ylab("Proportion") +
        theme(legend.position="none")
    hist.party <- ggplot(data.set, aes(repub, y=..density.. * 0.1, fill=anygirls.factor)) + 
        geom_histogram(position = "dodge", binwidth = 0.1) +
        ggtitle("Party Affiliation") + xlab("1 = Republican") + ylab("Proportion") + 
        theme(legend.position="none")
    hist.gender <- ggplot(data.set, aes(female, y=..density.. * 0.1, fill=anygirls.factor)) + 
        geom_histogram(position = "dodge", binwidth = 0.1) +
        ggtitle("Gender") + xlab("1 = Female") + ylab("Proportion") +
        scale_fill_discrete(name="Group",
                            breaks=c(0, 1),
                            labels=c("Control", "Treatment"))
    hist.race <- ggplot(data.set, aes(white, y=..density.. * 0.1, fill=anygirls.factor)) + 
        geom_histogram(position = "dodge", binwidth = 0.1) +
        ggtitle("Race") + xlab("1 = White") + ylab("Proportion") +
        theme(legend.position="none")
    hist.age <- ggplot(data.set, aes(age, y=..density.. * 5, fill=anygirls.factor)) + 
        geom_histogram(position = "dodge", binwidth = 5) + 
        ggtitle("Age") + xlab("Age") + ylab("Proportion") +
        theme(legend.position="none")
    hist.seniority <- ggplot(data.set, aes(srvlng, y=..density.. * 5, fill=anygirls.factor)) + 
        geom_histogram(position = "dodge", binwidth = 5) +
        ggtitle("Seniority") + xlab("Seniority") + ylab("Proportion") + 
        scale_fill_discrete(name="Group",
                            breaks=c(0, 1),
                            labels=c("Control", "Treatment"))
    grid.arrange(hist.rgroup, hist.party, hist.gender, hist.race, hist.age, hist.seniority, ncol=3, nrow=2, top=title, widths=c(1,1,1.4))
}

sample.df$anygirls.factor <- as.factor(sample.df$anygirls)
sample.df$matched = 0
sample.df[sample.pca.match$index.treated,'matched']=1
sample.df[sample.pca.match$index.control,'matched']=1
create.covariate.histograms(sample.df[sample.df$matched ==1,], "PCA Matched Data, Many Covariates")

# Different regression models treatment effect estimate
params <- c("repub", "srvlng", "female", "white", "age")
interact.two.params <- function(v) {
    return(paste("I(", v[1], "+", v[2], ")"))
}
two.param.combinations <- combn(params, 2, FUN = interact.two.params)
params.all <- c(params, "I(age ^ 2)", "I(srvlng ^ 2)", "rgroup.factor", two.param.combinations)
three.param.formulae <- function(v) {
    return(paste("nowtot", "~", "anygirls", "+", v[1], "+", v[2], "+", v[3]))
}
regression.combinations <- combn(params.all, 3, FUN=three.param.formulae)
get.coefficient.sample <- function(string.formula) {
    return(glm(as.formula(string.formula), data=sample.df)$coefficients["anygirls"])
}

get.coefficient.matched <- function(string.formula) {
    return(glm(as.formula(string.formula), data=sample.df[sample.df$matched == 1,])$coefficients["anygirls"])
}

dec.places <- function(x, d) {
    return(format(round(x, d), nsmall=d))
} 

coefs.df <- data.frame(coefs=sapply(regression.combinations, get.coefficient.sample))
matched.coefs.df <- data.frame(coefs=sapply(regression.combinations, get.coefficient.matched))
ggplot() + 
    geom_density(data=matched.coefs.df, aes(coefs, color='b')) + 
    geom_segment(data=matched.coefs.df, x=mean(matched.coefs.df$coefs), xend=mean(matched.coefs.df$coefs), y=0, yend=0.72, linetype=2, aes(color="b")) +
    geom_text(aes(x = 0.75, y=0.45, color='b'), label=paste("mean =", dec.places(mean(matched.coefs.df$coefs), 4), "\nsdev = ", dec.places(sd(matched.coefs.df$coefs), 4))) +
    geom_density(data=coefs.df, aes(coefs, color='a')) + 
    geom_segment(data=coefs.df, x=mean(coefs.df$coefs), xend=mean(coefs.df$coefs), y=0, yend=0.13, linetype=2, aes(color='a')) +
    geom_text(aes(x = -4.5, y=0.25, color='a'), label=paste("mean =", dec.places(mean(coefs.df$coefs), 4), "\nsdev = ", dec.places(sd(coefs.df$coefs), 4))) + 
    xlab("Predicted Regression anygirls Coefficient") + ylab("Density") + 
    ggtitle("PCA Matched Reg. Model TE Estimates, Many Covariates") +
    scale_colour_discrete(name="Group",
                          breaks=c("a", "b"),
                          labels=c("Unmatched", "Matched"))

# Outputs for building some tables in latex
sample.pca.match.balance$BMsmallestVarName
sample.pca.match.balance$BMsmallest.p.value
sample.pca.match.balance$AMsmallestVarName
sample.pca.match.balance$AMsmallest.p.value
length(unique(sample.pca.match$index.treated))
length(unique(sample.pca.match$index.control))
sample.pca.match$est
sample.pca.match$se

source("./daughter_effect/dataverse_files/modelDep.R")
pruned.sample = sample.df[c(sample.pca.match$index.treated, sample.pca.match$index.control), ]
weights.vec = as.vector(cbind(sample.pca.match$weights, sample.pca.match$weights))
md <- modelDep(formula=nowtot ~ ngirls + totchi + gender + race + repub + srvlng + region + age + rgroup + demvote,
               data=pruned.sample,
               tevar="ngirls",
               weights = weights.vec,
               alpha = 0.2)
te.no.na = na.omit(md$TE)
qplot(te.no.na, geom = "density", main="PCA-ATC matched TE estimate, alpha = 0.2")
length(te.no.na)
mean(te.no.na)
sd(te.no.na)
