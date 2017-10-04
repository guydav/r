### R code to generate Figure 2


require(cem)
require(MatchIt)
require(optmatch)

data(LL)
data(LLvsPSID)




rerunIMB <- TRUE

# set rerunIMB=FALSE if file imb0.rba is already available

if(rerunIMB){
 m1 <- cem("treated", LL, drop="re78", eval=FALSE)

 m2 <- matchit(treated ~ black + hispanic + married + nodegree + u74 + u75 + education +
  age + re74 + re75, data=LL, distance="logit")

   m3 <- fullmatch(mahal.dist(treated ~ black + hispanic + married + nodegree + u74 + u75 + education +
            age + re74 + re75 , data=LL), omit.fraction=(425-297)/425)
   m3.w <- as.numeric(matched(m3))			

			
			
			
 set.seed(123)

 M <- 250

 imb0 <- L1.profile(LL$treated,LL, drop=c("treated","re78"), M = M, plot=FALSE)
 
 imb1 <- L1.profile(LL$treated,LL, drop=c("treated","re78"), 
  weights=m2$w, useCP=imb0$CP, plot=FALSE)

 imb2 <- L1.profile(LL$treated,LL, drop=c("treated","re78"), 
  weights=m3.w, useCP=imb0$CP, plot=FALSE)
  
 imb3 <- L1.profile(LL$treated,LL, drop=c("treated","re78"), 
  weights=m1$w>0, useCP=imb0$CP, plot=FALSE)
 save(imb0, imb1, imb2, imb3, m1, m2, m3, file="imb0.rda")
} else {
 load(file="imb0.rda")
}



myplot <- function (x, add = FALSE, cex.axis = 0.3, ...) 
{
plot(unclass(x$L1), type = "n", axes = F, xlab = "cut points", 
ylab = expression(L[1]), ...)
axis(2)
lines(x$L1, ...)
}


par(mfrow=c(1,2))

idM <- which(imb0$L1>= imb0$medianL1)[1]

myplot(imb0, col="red", main="raw vs other profiles", lwd=3,ylim=c(0,0.7))
polygon(c(0,imb2$L1,0), col=rgb(.1,.1,.1, alpha=0.3))
polygon(c(0,imb1$L1,0), col=rgb(.1,.1,.1, alpha=0.3))
polygon(c(0,imb3$L1,0), col=rgb(.1,.1,.1, alpha=0.3))
lines(c(idM,idM, 0), c(0, imb0$medianL1, imb0$medianL1), lty=3, col="red")
legend(0.2, 0.7, legend=c("RAW","MAH","PSC", "CEM"), col=c(
"red", rgb(.1,.1,.1, alpha=0.3), rgb(.1,.1,.1, alpha=0.6), rgb(.1,.1,.1, alpha=0.9)),lwd=5)


plot(ecdf(imb0$L1),col="red", cex=0.3,main="ECDF of L1 measures",
xlab=expression(L[1]), ylab="ecdf")
plot(ecdf(imb1$L1),add=TRUE, col=rgb(.1,.1,.1, alpha=0.6), cex=0.3)
plot(ecdf(imb2$L1),add=TRUE, col=rgb(.1,.1,.1, alpha=0.3), cex=0.3)
plot(ecdf(imb3$L1),add=TRUE, col=rgb(.1,.1,.1, alpha=0.9), cex=0.3)
legend(0.5, 0.6, legend=c("CEM", "MAH", "PSC", "RAW"), 
col=c(rgb(.1,.1,.1, alpha=0.9),  rgb(.1,.1,.1, alpha=0.3), rgb(.1,.1,.1, alpha=0.6), "red"),lwd=5)



