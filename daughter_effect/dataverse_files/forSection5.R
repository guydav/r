# Replication file of Section 5 in
# Iacus, King, Porro (2011), Multivariate Matching Methods 
# That Are Monotonic Imbalance Bounding, JASA, V 106, N. 493,
# p. 345-361


library(foreign)

x <- read.dta("basic.dta")

# Table 1 in Ebonya
# Indep. vars
# anygirls: Any female children
# ngirls: Number of female children
# totchi: Total number of children

# white: White=1, 0 otherwise
# female: Female=1, 0 otherwise
# age: age
# srvlng: Service length (years)
# rgroup: 0=None, 1=Protestant, 2=Catholic, 3=Other Christian, 4=Other religion

# demvote : Democratic vote share

# Dependent variables
# nowtot: NOW score
# aaw: AAUW score
# rtl: supposed to be "NRLC score", but summary is totally different

# Other vars
# party: Democrats=1, Republicans=2



x$party <- factor(x$party)
levels(x$party) <- c("Democrats", "Republicans", "Other")
x$race <- factor(x$white)
levels(x$race) <- c("Other","White")
x$white <- NULL

x$gender<- factor(x$female)
levels(x$gender) <- c("Male","Female")
x$female <- NULL

idx0 <- which(x$rgroup==0)
idx1 <- which(x$rgroup==1)
x$rgroup[idx1] <- 0
x$rgroup[idx0] <- 1
x$rgroup <- factor(x$rgroup)
levels(x$rgroup) <- c("Protestant", "None", "Catholic", "Other Christian", "Other religion")

require(memisc)


x$hasGirls <- x$ngirls>0

tot105 <- subset(x, congress==105)

require(cem)

mvars <- c("gender", "race", "srvlng", "age","demvote", "repub") 
tvar <- "hasGirls"


basemod <- lm(nowtot ~ ngirls + factor(totchi)+gender + race + repub + srvlng + region+I(srvlng^2) + age + I(age^2) + rgroup + demvote, data=tot105)

l1vars <- c(mvars, "region", "rgroup")
gr <- list(rgroup=list("None", c("Catholic", "Other Christian", "Protestant"), "Other religion"))
gr <- NULL
set.seed(123)
imb0 <- L1.profile(tot105$hasGirls,tot105[,c("hasGirls", l1vars)], drop=c("hasGirls"),M=500, grouping=gr)

tmp <- L1.meas(tot105$hasGirls, tot105[,c("hasGirls", l1vars)], drop=c("hasGirls"), breaks=imb0$medianCP, grouping=gr)
L1 <- tmp$L1
LCS <- tmp$LCS
mt <- sum(tot105$hasGirls==TRUE,na.rm=TRUE)
mc <- sum(tot105$hasGirls==FALSE,na.rm=TRUE)



m <- 1
qoi <- summary(basemod)$coefficients["ngirls",]
cuts <- vector(mode="list")
cuts[[m]] <- "NONE"
for(i in 3:11){
 for(j in 3:11){
  for(k in 3:11){
	m <- m +1
 	cuts[[m]] <- list(srvlng=i, demvote=j, age=k)
	mymat <- cem(tvar, data=tot105[c(tvar,mvars)], cutpoints=cuts[[m]], eval.imb=FALSE)
    mt <- c(mt, mymat$tab["Matched", "GTRUE"])
    mc <- c(mc, mymat$tab["Matched", "GFALSE"])
	mymod <- lm(nowtot ~ ngirls + factor(totchi)+gender + race + repub + srvlng + region+I(srvlng^2) + age + I(age^2) + rgroup + demvote, data=tot105, weights= mymat$w)	
    qoi <- cbind(qoi, summary(mymod)$coefficients["ngirls",])
	tmp <- L1.meas(tot105$hasGirls, tot105[,c("hasGirls", l1vars)], drop=c("hasGirls"), breaks=imb0$medianCP, weights=mymat$w, grouping=imb0$medianGR)
	L1 <- c(L1, tmp$L1)
	LCS <- c(LCS, tmp$LCS)
	cat(".")
}}}
ssize <- mt+mc

res <- cbind(ssize, qoi["Estimate",], qoi["Estimate",]-qoi["Std. Error",], qoi["Estimate",]+qoi["Std. Error",], L1)
colnames(res) <- c("n", "att", "low", "upp", "L1")
rownames(res) <- NULL

ans <- res[order(res[,"n"], decreasing=TRUE),]
newcuts <- cuts[ order(res[,"n"], decreasing=TRUE) ]

require(arules)

duplicated(ans) -> idx2
ans <- ans[which(idx2==FALSE),]

newcuts <- newcuts[ which(idx2==FALSE) ]




imb <- imbalance(tot105$hasGirls, tot105[,c("hasGirls", l1vars)], drop=c("hasGirls"), breaks=imb0$medianCP, grouping=imb0$medianGR)


mod <- lm(nowtot ~ ngirls + factor(totchi)+gender + race + repub + srvlng + region+I(srvlng^2) + age + I(age^2) + rgroup + demvote, data=tot105)	

idx.min <- which.min(ans[,"L1"])
mymat1 <- cem(tvar, data=tot105[c(tvar,mvars)], cutpoints=newcuts[[idx.min]], eval.imb=FALSE)
imb1 <- imbalance(tot105$hasGirls, tot105[,c("hasGirls", l1vars)], drop=c("hasGirls"), breaks=imb0$medianCP, weights=mymat1$w, grouping=imb0$medianGR)
mod1 <- lm(nowtot ~ ngirls + factor(totchi)+gender + race + repub + srvlng + region+I(srvlng^2) + age + I(age^2) + rgroup + demvote, data=tot105, weights= mymat1$w)	

idx.min <- which.min(ans[,"n"])
mymat2 <- cem(tvar, data=tot105[c(tvar,mvars)], cutpoints=newcuts[[idx.min]], eval.imb=FALSE)
imb2 <- imbalance(tot105$hasGirls, tot105[,c("hasGirls", l1vars)], drop=c("hasGirls"), breaks=imb0$medianCP, weights=mymat2$w, grouping=imb0$medianGR)
mod2 <- lm(nowtot ~ ngirls + factor(totchi)+gender + race + repub + srvlng + region+I(srvlng^2) + age + I(age^2) + rgroup + demvote, data=tot105, weights= mymat2$w)	

bigmod <- mtable(mod, mod1, mod2, summary.stats=c("R-squared","adj. R-squared","Log-likelihood", "AIC", "N"))

bigmod

bigmod$summaries -> tab
tab <- rbind(tab, round(c(imb$L1$L1, imb1$L1$L1, imb2$L1$L1),3))
nr <- NROW(tab)
rownames(tab)[nr] <- "L1"
tab <- tab[c(1:(nr-2), nr, nr-1),]
bigmod$summaries <- tab

### totchi no factor

mod <- lm(nowtot ~ ngirls + totchi+gender + race + repub + srvlng + region+I(srvlng^2) + age + I(age^2) + rgroup + demvote, data=tot105)	

idx.min <- which.min(ans[,"L1"])
mymat1 <- cem(tvar, data=tot105[c(tvar,mvars)], cutpoints=newcuts[[idx.min]], eval.imb=FALSE)
imb1 <- imbalance(tot105$hasGirls, tot105[,c("hasGirls", l1vars)], drop=c("hasGirls"), breaks=imb0$medianCP, weights=mymat1$w, grouping=imb0$medianGR)
mod1 <- lm(nowtot ~ ngirls + totchi+gender + race + repub + srvlng + region+I(srvlng^2) + age + I(age^2) + rgroup + demvote, data=tot105, weights= mymat1$w)	

idx.min <- which.min(ans[,"n"])
mymat2 <- cem(tvar, data=tot105[c(tvar,mvars)], cutpoints=newcuts[[idx.min]], eval.imb=FALSE)
imb2 <- imbalance(tot105$hasGirls, tot105[,c("hasGirls", l1vars)], drop=c("hasGirls"), breaks=imb0$medianCP, weights=mymat2$w, grouping=imb0$medianGR)
mod2 <- lm(nowtot ~ ngirls + totchi+gender + race + repub + srvlng + region+I(srvlng^2) + age + I(age^2) + rgroup + demvote, data=tot105, weights= mymat2$w)	

bigmod0 <- mtable(mod, mod2, summary.stats=c("R-squared","adj. R-squared","Log-likelihood", "AIC", "N"))

bigmod0

bigmod0$summaries -> tab
tab <- rbind(tab, round(c(imb$L1$L1, imb2$L1$L1),3))
nr <- NROW(tab)
rownames(tab)[nr] <- "L1"
tab <- tab[c(1:(nr-2), nr, nr-1),]
bigmod0$summaries <- tab

aa <- relabel(bigmod0, "I(age^2)"="age squared", "I(srvlng^2)"="srvlng squared", 
"ngirls" = "number of girls", "totchi" = "total number of children",
"repub" = "republican party", "demvote"="percentage of democrat votes")
bb <- relabel(aa, "srvlng"="service length", "rgroup"="religion group", "region"="region:", gsub=TRUE)


toLatex(bb)

source("modelDep.R")


mdep0 <- modelDep( nowtot ~ ngirls + totchi+gender + race + repub + srvlng + region+ + age + rgroup + demvote, data=tot105, tevar="ngirls")
mdep1 <- modelDep( nowtot ~ ngirls + totchi+gender + race + repub + srvlng + region+ + age + rgroup + demvote, data=tot105, tevar="ngirls",weights=mymat1$w)
mdep2 <- modelDep( nowtot ~ ngirls + totchi+gender + race + repub + srvlng + region+ + age + rgroup + demvote, data=tot105, tevar="ngirls",weights=mymat2$w)

summary(mdep0$TE)
summary(mdep1$TE)
summary(mdep2$TE)

round(range(mdep0$TE,na.rm=TRUE),3)
round(range(mdep2$TE,na.rm=TRUE),3)

par(mfrow=c(3,1))

plot(density(mdep0$TE, na.rm=TRUE), xlim=c(-4, 10),  main=sprintf("raw data, L1=%.3f, n=%d",imb$L1$L1,sum(summary(mod)$df[1:2])),xlab="TE estimate")
abline(v=summary(mod)$coef["ngirls","Estimate"],lty=3)
plot(density(mdep1$TE, na.rm=TRUE), xlim=c(-4, 10), main=sprintf("min L1, L1=%.3f, n=%d",imb1$L1$L1,sum(summary(mod1)$df[1:2])),xlab="TE estimate")
abline(v=summary(mod1)$coef["ngirls","Estimate"],lty=3)
plot(density(mdep2$TE, na.rm=TRUE), xlim=c(-4, 10), main=sprintf("min n, L1=%.3f, n=%d",imb2$L1$L1,sum(summary(mod2)$df[1:2])),xlab="TE estimate")
abline(v=summary(mod2)$coef["ngirls","Estimate"],lty=3)



par(mar=c(2.5,2.5,.5,.5))
plot(density(mdep0$TE, na.rm=TRUE), xlim=c(min(mdep0$TE,na.rm=TRUE), max(mdep0$TE,na.rm=TRUE)),  
main="",xlab="TE estimate",ylim=c(0,1.8))
abline(v=summary(mod)$coef["ngirls","Estimate"],lty=3)
lines(density(mdep2$TE, na.rm=TRUE),  lty=2, col="red")
abline(v=summary(mod2)$coef["ngirls","Estimate"],lty=3, col="red")

text(1.5,0.5, "raw data")
text(4.8,0.5, "matched data")





require(gdata)
tot105BIS <- tot105
tot105BIS$gender <- as.numeric(tot105$gender)
tot105BIS$repub <- 2-as.numeric(tot105$repub)
tot105BIS$race <- 2-as.numeric(tot105$race)
tot105BIS$rgroup <- reorder(tot105$rgroup, new.order=c(1,5,4,2,3))
tot105BIS$rgroup <- as.numeric(tot105BIS$rgroup)


idx <- which(tot105$hasGirls)
tot105BIS$gender[idx] <- tot105BIS$gender[idx]+rnorm(length(idx),0,.01)
tot105BIS$repub[idx] <- tot105BIS$repub[idx]+rnorm(length(idx),0,.01)
tot105BIS$race[idx] <- tot105BIS$race[idx]+rnorm(length(idx),0,.01)
tot105BIS$rgroup[idx] <- tot105BIS$rgroup[idx]+rnorm(length(idx),0,.01)
tot105BIS$age[idx] <- tot105BIS$age[idx]+rnorm(length(idx),0,.01)
tot105BIS$demvote[idx] <- tot105BIS$demvote[idx]+rnorm(length(idx),0,.01)
 

nm <- colnames(tot105BIS)
idx <- which(nm=="repub")
nm[idx] <- "PartyID"
idx <- which(nm=="demvote")
nm[idx] <- "DemVote"
idx <- which(nm=="rgroup")
nm[idx] <- "Religion"
idx <- which(nm=="age")
nm[idx] <- "Age"
#idx <- which(nm=="seniority")
idx <- which(nm=="srvlng")
nm[idx] <- "Seniority"
idx <- which(nm=="gender")
nm[idx] <- "Gender"
idx <- which(nm=="race")
nm[idx] <- "Race"

pvars1 <- c("Gender", "Race", "Seniority", "Age", "DemVote", "PartyID", "Religion")
colnames(tot105BIS) <- nm

pdf(file="par_plot.pdf", width=10, height=5, pointsize=10)

parallel( ~tot105BIS[which( tot105BIS$hasGirls), pvars1], group= mymat2$w[which( tot105BIS$hasGirls)]>0,
 alpha=c(0.05,0.2),  col=c("red","blue"),horizontal.axis=FALSE,scales = list(x = list(rot = 90)))

dev.off()


#vvars <- c("gender",  "race",    "seniority",  "age" ,    "demvote" ,"repub"  , "region" , "rgroup" )
vvars <- c("gender",  "race",    "srvlng",  "age" ,    "demvote" ,"repub"  , "region" , "rgroup" )


d0 <- tot105[which( tot105$hasGirls), vvars]

d2 <- tot105[which( tot105$hasGirls& mymat2$w>0), vvars]

d3 <- tot105[which( tot105$hasGirls& mymat2$w==0), vvars]

r1 <- c(prop.table(table(d0$gender))["Male"], prop.table(table(d2$gender))["Male"],  prop.table(table(d3$gender))["Male"])
#r2 <- c(mean(d0$seniority,na.rm=TRUE), mean(d2$seniority,na.rm=TRUE),  mean(d3$seniority,na.rm=TRUE))
r2 <- c(mean(d0$srvlng,na.rm=TRUE), mean(d2$srvlng,na.rm=TRUE),  mean(d3$srvlng,na.rm=TRUE))

r3 <- c(mean(d0$age,na.rm=TRUE), mean(d2$age,na.rm=TRUE),  mean(d3$age,na.rm=TRUE))
r4 <- c(mean(d0$demvote,na.rm=TRUE), mean(d2$demvote,na.rm=TRUE), mean(d3$demvote,na.rm=TRUE))
r5 <- c(mean(d0$repub,na.rm=TRUE), mean(d2$repub,na.rm=TRUE),  mean(d3$repub,na.rm=TRUE))
r6 <- c(prop.table(table(d0$race))["White"], prop.table(table(d2$race))["White"], prop.table(table(d3$race))["White"])
r7 <- cbind(prop.table(table(d0$rgroup)), prop.table(table(d2$rgroup)), prop.table(table(d3$rgroup)))

comptab <- rbind(r1, r2,r3,r4,r5,r6,r7)
rownames(comptab) <- c("gender (% males)", "service length (years)", 
"age (years)", "dem. vote (%)", 
"republican (%)", 
"race (% white)", 
"religion: protestant (%)", "religion: none (%)", "religion: catholic (%)", 
"religion: other christian (%)", "religion: other (%)")
colnames(comptab) <- c("raw data", "matched data", "unmatched data")

tmptab <- comptab
tmptab[8:7,] <- comptab[7:8,]
rownames(tmptab)[8:7] <- rownames(comptab)[7:8]




require(xtable)

xtable(tmptab)


save.image("all.rda")


