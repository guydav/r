###################################################
### chunk number 1: 
###################################################
rm(list=ls())
require(cem)
require(MatchIt)
require(rpart)

NT <- 5000
NC <- NT
N <- NC+NT

nt <- 200
nc <- 2*nt

n <- nc+nt

set.seed(123)

tr.pop <- rnorm(NT, mean=1)
ct.pop <- rnorm(NC, mean=5)

g1 <- function(x,t) 100*t + x*exp(abs(x))
g2 <- function(x,t) 100*exp(1-(2+0.2*t)*x)
g3 <- function(x,t) 100+10*t*exp(-abs(x+3))  +40*(1-t)*exp(-abs(x-5))
g4 <- function(x,t) 100+10*t*x + x^2
g5 <- function(x,t) 100+100*t+x     
g6 <- function(x,t) 100+100*t*x + 250*(t-1)*abs(x)     

G <- list()
G[[1]] <- g1
G[[2]] <- g2
G[[3]] <- g3
G[[4]] <- g4
G[[5]] <- g5
G[[6]] <- g6

br <- 3:12
K <- length(br)


###################################################
### chunk number 2: fig1plot
###################################################
myc <- 0.5
par(mfrow=c(3,2))
par(mar=c(0,0,0,0))
g <- G[[1]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10, ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-10,10,add=TRUE, type="o",cex=myc)
text(0,100000, expression(g[1](x,t)== 100*t + x*e^abs(x)),cex=1.2)
#axis(1)
box()


g <- G[[2]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-8,-5, ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-8,-5,add=TRUE,  type="o",cex= myc)
text(-6,4e+9, expression(g[2](x,t)== 100*e^{1-(2+0.2*t)*x}  ),cex=1.2)
#axis(1)
box()


g <- G[[3]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10, ylim=c(100,150),ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-10,10,add=TRUE, type="o",cex= myc)
text(-1,145, expression(g[3](x,t)== 100+10*t*e^{-abs(x+3)}+40*(1-t)*e^{-abs(x-5)}  ),cex=1.2)
#axis(1)
box()

g <- G[[4]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10,ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-10,10,add=TRUE,  type="o",cex= myc)
text(0,200, expression(g[4](x,t)== 100+10*t*x + x^2  ),cex=1.2)
#axis(1)
box()


g <- G[[5]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10,ylab=expression(g(x,t)),axes=FALSE,ylim=c(90,210))
curve(f0,-10,10,add=TRUE,  type="o",cex= myc)
text(-5,150, expression(g[5](x,t)== 100+100*t+x  ),cex=1.2)
#axis(1)
box()



g <- G[[6]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10,ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-10,10,add=TRUE,  type="o",cex= myc)
text(-3,630, expression(g[6](x,t)== 100+100*t*x + 250*(t-1)*abs(x)  ),cex=1.2)
#axis(1)
box()

#par(mfrow=c(3,2))
#
#g <- G[[1]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g1",ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)
#
#g <- G[[2]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-8,-5,main="g2",ylab=expression(g(x,t)))
#curve(f0,-8,-5,add=TRUE, lty=3)
#
#g <- G[[3]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g3",ylim=c(100,140),ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)
#
#g <- G[[4]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g4",ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)
#
#g <- G[[5]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g5",ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)
#
#g <- G[[6]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g6",ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)


###################################################
### chunk number 3: fig1
###################################################
myc <- 0.5
par(mfrow=c(3,2))
par(mar=c(0,0,0,0))
g <- G[[1]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10, ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-10,10,add=TRUE, type="o",cex=myc)
text(0,100000, expression(g[1](x,t)== 100*t + x*e^abs(x)),cex=1.2)
#axis(1)
box()


g <- G[[2]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-8,-5, ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-8,-5,add=TRUE,  type="o",cex= myc)
text(-6,4e+9, expression(g[2](x,t)== 100*e^{1-(2+0.2*t)*x}  ),cex=1.2)
#axis(1)
box()


g <- G[[3]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10, ylim=c(100,150),ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-10,10,add=TRUE, type="o",cex= myc)
text(-1,145, expression(g[3](x,t)== 100+10*t*e^{-abs(x+3)}+40*(1-t)*e^{-abs(x-5)}  ),cex=1.2)
#axis(1)
box()

g <- G[[4]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10,ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-10,10,add=TRUE,  type="o",cex= myc)
text(0,200, expression(g[4](x,t)== 100+10*t*x + x^2  ),cex=1.2)
#axis(1)
box()


g <- G[[5]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10,ylab=expression(g(x,t)),axes=FALSE,ylim=c(90,210))
curve(f0,-10,10,add=TRUE,  type="o",cex= myc)
text(-5,150, expression(g[5](x,t)== 100+100*t+x  ),cex=1.2)
#axis(1)
box()



g <- G[[6]]
f1 <- function(x) g(x,1)
f0 <- function(x) g(x,0)
curve(f1,-10,10,ylab=expression(g(x,t)),axes=FALSE)
curve(f0,-10,10,add=TRUE,  type="o",cex= myc)
text(-3,630, expression(g[6](x,t)== 100+100*t*x + 250*(t-1)*abs(x)  ),cex=1.2)
#axis(1)
box()

#par(mfrow=c(3,2))
#
#g <- G[[1]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g1",ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)
#
#g <- G[[2]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-8,-5,main="g2",ylab=expression(g(x,t)))
#curve(f0,-8,-5,add=TRUE, lty=3)
#
#g <- G[[3]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g3",ylim=c(100,140),ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)
#
#g <- G[[4]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g4",ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)
#
#g <- G[[5]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g5",ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)
#
#g <- G[[6]]
#f1 <- function(x) g(x,1)
#f0 <- function(x) g(x,0)
#curve(f1,-10,10,main="g6",ylab=expression(g(x,t)))
#curve(f0,-10,10,add=TRUE, lty=3)


###################################################
### chunk number 4: 
###################################################
# This function assumes that
# N, G, tr.pop, ct.pop , etc
# are defined on the workspace

tauEST <- function(nsim=100){

 pb <- txtProgressBar(min = 1, max = nsim, initial = 1, style = 3)

 NG <- length(G)


 data <- data.frame(treat=c(rep(1, NT), rep(0,NC)), x = c(tr.pop, ct.pop))

 Z <- rnorm(N, sd=0.3)
 est <- list()
 qoi <- list()

 for(i in 1:NG){
  g <- G[[i]]
  ytr <- g(tr.pop, 1)
  yct <- g(ct.pop, 0) 
  ypop <- c(ytr, yct)
  y <- ypop + Z
  data[sprintf("y%d",i)] <- y

  est[[i]] <- matrix(,nsim, 1+2+2*K)
  colnames(est[[i]]) <- c("raw", "PS0", "PS1", sprintf("cem%.2d",1:K), sprintf("cem%.2dA",1:K))

  qoi[[i]] <-  matrix(,nsim, 1+K)
  colnames(qoi[[i]]) <- c("QOI", sprintf("QOI%.2d",1:K))
 }
 
 n.m <- matrix(,nsim, 1+2+K)
 
 for(sim in 1:nsim){
  setTxtProgressBar(pb, sim)

  tr.idx <- sample(1:NT, nt)
  ct.idx <- sample((NT+1):N, nc)
  all.idx <- c(tr.idx, ct.idx)
  dt <- data[all.idx,]
  
  idxT <- list()
  idxC <- list()
  wT <- list()
  wC <- list()
  mod <- list()
  
  mvars <- c("treat", "x")

  for(k in 1:K){
   m <- cem("treat", data=dt[mvars], cut=list(x=br[k]), eval=F)
   idxT[[k]] <- which(m$matched == TRUE & m$groups == "1")
   idxC[[k]] <- which(m$matched == TRUE & m$groups == "0")
   wT[[k]] <- m$w[idxT[[k]]]
   wC[[k]] <- m$w[idxC[[k]]] 
   n.m[sim, 3+k] <- m$tab["Matched", "G1"]
  }
  
  ps0 <- matchit(treat ~ x , data = dt, distance = "logit")
  idxTP <- which(ps0$weights==1 & dt$treat==1)
  idxCP <- which(ps0$weights==1 & dt$treat==0)

  for(i in 1:NG){
   g <- G[[i]]
   form <- as.formula(sprintf("y%d ~ x",i))
   mod <- rpart( form , data=data[ct.idx,] )
   ovar <- sprintf("y%d",i)


#   qoi[[i]][sim, 1] <-  mean(g(data$x[tr.idx], 1) - g(data$x[tr.idx], 0))   
   qoi[[i]][sim, 1] <-  mean(g(data$x, 1) - g(data$x, 0))   
   est[[i]][sim, 1] <- mean(data[tr.idx,ovar]) - mean(data[ct.idx,ovar])
   n.m[sim, 1] <- nt
   n.m[sim, 2] <- nt
   
   for(k in 1:K){
    id1 <- idxT[[k]]
    id2 <- idxC[[k]]

    est[[i]][sim, 3+k] <- weighted.mean(dt[id1, ovar],wT[[k]])-weighted.mean(dt[id2, ovar],wC[[k]])
    est[[i]][sim, 3+K+k] <- est[[i]][sim, 3+k] - ( weighted.mean( predict(mod, newdata=dt[ id1, ]), wT[[k]]) -
        weighted.mean( predict(mod, newdata=dt[ id2, ]), wC[[k]]) ) 
    qoi[[i]][sim, 1+k] <- mean(g(dt$x[id1], 1) - g(dt$x[id1], 0)) 
   }
  id3 <- idxTP  
  id4 <- idxCP  
  est[[i]][sim, 2] <- weighted.mean(dt[ id3, ovar ],ps0$weights[ idxTP ]) -
                              weighted.mean(dt[ id4, ovar ],ps0$weights[ idxCP ])
  est[[i]][sim, 3] <- est[[i]][sim, 2] - 
    ( weighted.mean(predict(mod, newdata=dt[ id3 ,])) -
      weighted.mean(predict(mod, newdata=dt[ id4 ,])) ) 
   }
 }
 close(pb)
   
 MSE <- list()
 MSESUB <- list()  
 mMSE  <- list()
 mMSESUB  <- list()

 for(i in 1:NG){
   MSE[[i]] <- sqrt(colMeans((est[[i]]-qoi[[i]][,rep(1, 1+2+2*K)])^2, na.rm=TRUE))
   MSESUB[[i]] <- sqrt(colMeans((est[[i]][,c(1+2+(1:(2*K)))]-qoi[[i]][,c(1+1:K, 1+1:K)])^2, na.rm=TRUE))
   mMSE[[i]] <- max(MSE[[i]][-1],na.rm=TRUE)
   mMSESUB[[i]] <- max(MSESUB[[i]],na.rm=TRUE)
 }   

ans <- list(K=K, MSE=MSE, MSESUB=MSESUB, n.m=n.m, mMSE=mMSE, mMSESUB= mMSESUB,qoi=qoi)
return(ans)
}


###################################################
### chunk number 5: OneDim
###################################################
nsim <- 1000
set.seed(123)
#res1 <- tauEST(nsim)
#save(res1, file="res1-PATT.rda")


###################################################
### chunk number 6: figG1plot
###################################################
load("res1-PATT.rda")
res <- res1
MSE <- res$MSE[[1]]
MSESUB <- res$MSESUB[[1]]
mMSE <- res$mMSE[[1]]
mMSESUB <- res$mMSESUB[[1]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 7: figG1
###################################################
load("res1-PATT.rda")
res <- res1
MSE <- res$MSE[[1]]
MSESUB <- res$MSESUB[[1]]
mMSE <- res$mMSE[[1]]
mMSESUB <- res$mMSESUB[[1]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 8: figG2plot
###################################################
res <- res1
MSE <- res$MSE[[2]]
MSESUB <- res$MSESUB[[2]]
mMSE <- res$mMSE[[2]]
mMSESUB <- res$mMSESUB[[2]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 9: figG2
###################################################
res <- res1
MSE <- res$MSE[[2]]
MSESUB <- res$MSESUB[[2]]
mMSE <- res$mMSE[[2]]
mMSESUB <- res$mMSESUB[[2]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 10: figG3plot
###################################################
res <- res1
MSE <- res$MSE[[3]]
MSESUB <- res$MSESUB[[3]]
mMSE <- res$mMSE[[3]]
mMSESUB <- res$mMSESUB[[3]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 11: figG3
###################################################
res <- res1
MSE <- res$MSE[[3]]
MSESUB <- res$MSESUB[[3]]
mMSE <- res$mMSE[[3]]
mMSESUB <- res$mMSESUB[[3]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 12: figG4plot
###################################################
res <- res1
MSE <- res$MSE[[4]]
MSESUB <- res$MSESUB[[4]]
mMSE <- res$mMSE[[4]]
mMSESUB <- res$mMSESUB[[4]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 13: figG4
###################################################
res <- res1
MSE <- res$MSE[[4]]
MSESUB <- res$MSESUB[[4]]
mMSE <- res$mMSE[[4]]
mMSESUB <- res$mMSESUB[[4]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 14: figG5plot
###################################################
res <- res1
MSE <- res$MSE[[5]]
MSESUB <- res$MSESUB[[5]]
mMSE <- res$mMSE[[5]]
mMSESUB <- res$mMSESUB[[5]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 15: figG5
###################################################
res <- res1
MSE <- res$MSE[[5]]
MSESUB <- res$MSESUB[[5]]
mMSE <- res$mMSE[[5]]
mMSESUB <- res$mMSESUB[[5]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 16: figG6plot
###################################################
res <- res1
MSE <- res$MSE[[6]]
MSESUB <- res$MSESUB[[6]]
mMSE <- res$mMSE[[6]]
mMSESUB <- res$mMSESUB[[6]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 17: figG6
###################################################
res <- res1
MSE <- res$MSE[[6]]
MSESUB <- res$MSESUB[[6]]
mMSE <- res$mMSE[[6]]
mMSESUB <- res$mMSESUB[[6]]
K <- res$K
n.m <- res$n.m

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


plot(1:K, MSESUB[1:K],type="b", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="CEM changed QOI")
points(1:K, MSESUB[K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",br[1:K], round(colMeans(n.m)[3+(1:K)])))
axis(2, c(0,MSE[c("raw","PS0","PS1")],mMSE), 
c("0","raw",sprintf("PS0 = %.1f",MSE["PS0"]), 
sprintf("PS0A = %.1f",MSE["PS1"]),
sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS1"],lty=3)


###################################################
### chunk number 18: 
###################################################
rm(list=ls())
require(cem)
require(MatchIt)
require(rpart)

NT <- 5000
NC <- NT
N <- NC+NT

nt <- 500
nc <- 2*nt

n <- nc+nt

set.seed(123)


muT <- 0
muC <- 2

set.seed(123)
Sigma <- diag(1,5)


t.data <- data.frame(mvrnorm(n=NT, rep(muT,5), Sigma))
colnames(t.data) <- c("X1","X2","X3","X4","X5")
c.data <- data.frame(mvrnorm(n=NC, rep(muC,5), Sigma))
colnames(t.data) <- c("X1","X2","X3","X4","X5")
GData <- data.frame(treat=c(rep(1,NT), rep(0,NC)), rbind(t.data, c.data))


###################################################
### chunk number 19: 
###################################################
G <-list()

G[[1]] <- function(x,t) 100*t + t*x$X1*exp(abs(x$X2-2))+log(10+x$X3)+100*(1-t)*x$X2*exp(abs(x$X4+2))+x$X5^2+x$X3*x$X4*x$X5
G[[2]] <- function(x,t) 100*t + x$X1 + x$X2 + x$X3 + x$X4 + x$X5
G[[3]] <- function(x,t) 100*t + x$X1 + x$X2 + x$X3 + x$X4 + x$X5 + x$X1^2 + x$X2^2 + x$X3^2 + x$X4^2 + x$X5^2
G[[4]] <- function(x,t) 100+100*t*(x$X1 + x$X2 + x$X3 + x$X4 + x$X5)+250*(t-1)*(x$X1^2 + x$X2^2 + x$X3^2 + x$X4^2 + x$X5^2)     
G[[5]] <- function(x,t) 100+100*t*(x$X1 + x$X2 + x$X3 + x$X4 + x$X5) + 250*(t-1)*(x$X1 + x$X2 + x$X3 + x$X4 + x$X5)     ## se c'e' x senza t pscore fallisce. mah!

breaks <- list()

K <- 50

for(i in 1:K){
 br <- list()
 br$X1 <- sample(3:7, 1)	
 br$X2 <- sample(3:7, 1)	
 br$X3 <- sample(3:7, 1)	
 br$X4 <- sample(3:7, 1)	
 br$X5 <- sample(3:7, 1)
 breaks[[i]] <- br	
}


###################################################
### chunk number 20: 
###################################################
tauEST5 <- function(nsim=100){
 pb <- txtProgressBar(min = 1, max = nsim, initial = 1, style = 3)

 NG <- length(G)

 data <- GData
 
 Z <- rnorm(N, sd=0.3)
 est <- list()
 qoi <- list()

 for(i in 1:NG){
  g <- G[[i]]
  ytr <- g(t.data, 1)
  yct <- g(c.data, 0) 
  ypop <- c(ytr, yct)
  y <- ypop + Z
  data[sprintf("y%d",i)] <- y

  est[[i]] <- matrix(,nsim, 1+2*3+2*K)
  colnames(est[[i]]) <- c("raw", "PS0", "PS0A", "PS1", "PS1A", "PS2", "PS2A", sprintf("cem%.2d",1:K), sprintf("cem%.2dA",1:K))

  qoi[[i]] <-  matrix(,nsim, 1+2+K)
  colnames(qoi[[i]]) <- c("QOI", "PS1", "PS2", sprintf("cem%.2d",1:K))
  
 }
 
 n.m <- matrix(,nsim, 1+3+K)
 colnames(n.m) <- c("raw", "PS0", "PS1", "PS2", sprintf("cem%.2d",1:K))

 MYQOI <- numeric(NG)
 
  for(i in 1:NG){
   g <- G[[i]]
   MYQOI[i] <- mean( g(t.data, 1)  - g(t.data, 0) )
  }
   
 for(sim in 1:nsim){
  setTxtProgressBar(pb, sim)

  tr.idx <- sample(1:NT, nt)
  ct.idx <- sample((NT+1):N, nc)
  all.idx <- c(tr.idx, ct.idx)
  dt <- data[all.idx,]

  idxT <- list()
  idxC <- list()
  wT <- list()
  wC <- list()
  mod <- list()
  
  mvars <- c("treat", "X1", "X2", "X3", "X4", "X5")

  for(k in 1:K){
   m <- cem("treat", data=dt[mvars], cut=breaks[[k]], eval=F)
   idxT[[k]] <- which(m$matched == TRUE & m$groups == "1")
   idxC[[k]] <- which(m$matched == TRUE & m$groups == "0")
   wT[[k]] <- m$w[idxT[[k]]]
   wC[[k]] <- m$w[idxC[[k]]] 
   n.m[sim, sprintf("cem%.2d",k)] <- m$tab["Matched", "G1"]
  }

# naive pscore
   idxTP <- numeric(0)
   idxCP <- numeric(0)

  ps0 <-  try( matchit(treat ~ X1+X2+X3+X4+X5, data = dt, distance = "logit") )
  if(class(ps0) !="try-error"){ 

   idxTP <- which(ps0$weights==1 & dt$treat==1)
   idxCP <- which(ps0$weights==1 & dt$treat==0)


# I&R pscore
   mod1 <- try(pscoreSelect( treat ~ X1+X2+X3+X4+X5,  data=dt))

   ps1 <- NULL
   ps2 <- NULL
   idxTP1 <- NULL
   idxTP2 <- NULL
   idxCP1 <- NULL
   idxCP2 <- NULL

   if(class(mod1)!="try-error"){ 
    ps1 <- try(matchit(mod1, data = dt, distance = "logit"))
     if(class(ps1)!="try-error"){ 	
     idxTP1 <- which(ps1$weights==1 & dt$treat==1)
     idxCP1 <- which(ps1$weights==1 & dt$treat==0)
     n.m[sim, "PS1"] <- length(idxTP1)
    }
# I&R pscore + caliper
    ps2 <- try( matchit(mod1, data = dt, distance = "logit", caliper=0.05) )
     if(class(ps2)!="try-error"){ 	
      idxTP2 <- which(ps2$weights==1 & dt$treat==1)
      idxCP2 <- which(ps2$weights==1 & dt$treat==0)
      n.m[sim, "PS2"] <- length(idxTP2)
     }
   }  
  }

#
 for(i in 1:NG){
   g <- G[[i]]
   form <- as.formula(sprintf("y%d ~ X1+X2+X3+X4+X5",i))
   mod <- rpart( form , data=data[ct.idx,] )
   ovar <- sprintf("y%d",i)

#   qoi[[i]][sim, 1] <-  mean( g(data[tr.idx,], 1)  - g(data[tr.idx,], 0) )
   qoi[[i]][sim, 1] <-  MYQOI[i] 
   est[[i]][sim, "raw"] <- mean(data[tr.idx,ovar]) - mean(data[ct.idx,ovar])
   n.m[sim, "raw"] <- nt
   n.m[sim, "PS0"] <- nt

  
   for(k in 1:K){
    id1 <- idxT[[k]]
    id2 <- idxC[[k]]

    est[[i]][sim, sprintf("cem%.2d",k)] <- weighted.mean(dt[id1, ovar],wT[[k]])-weighted.mean(dt[id2, ovar],wC[[k]])

    est[[i]][sim, sprintf("cem%.2dA",k)] <- est[[i]][sim, sprintf("cem%.2d",k)] - 
      ( weighted.mean(predict(mod, newdata=dt[id1,]), wT[[k]]) -
        weighted.mean(predict(mod, newdata=dt[id2,]), wC[[k]]) ) 

    qoi[[i]][sim, sprintf("cem%.2d",k)] <- mean(g(dt[id1,], 1) - g(dt[id1,], 0)) 
   }

 if(class(ps0) !="try-error"){ 
  est[[i]][sim, "PS0"] <- weighted.mean(dt[ idxTP, ovar ],ps0$weights[ idxTP ]) -
                              weighted.mean(dt[ idxCP, ovar ],ps0$weights[ idxCP ])
  est[[i]][sim, "PS0A"] <- est[[i]][sim, "PS0"] - 
    ( weighted.mean(predict(mod, newdata=dt[ idxTP ,])) -
      weighted.mean(predict(mod, newdata=dt[ idxCP ,])) ) 
  }
   
  if(class(mod1)!="try-error"){ 
    if(class(ps1)!="try-error"){ 	
 
   est[[i]][sim, "PS1"] <- weighted.mean(dt[idxTP1, ovar],ps1$weights[idxTP1])-weighted.mean(dt[idxCP1,ovar],ps1$weights[idxCP1])
   est[[i]][sim, "PS1A"] <- est[[i]][sim, "PS1"] - 
    ( weighted.mean(predict(mod, newdata=dt[idxTP1,])) -
	 weighted.mean(predict(mod, newdata=dt[idxCP1,])) ) 	

   qoi[[i]][sim, "PS1"] <- mean( g(dt[idxTP1,], 1) - g(dt[idxTP1,], 0) )
}
 
    if(class(ps2)!="try-error"){ 	
 
   est[[i]][sim, "PS2"] <- weighted.mean(dt[idxTP2, ovar],ps2$weights[idxTP2])-weighted.mean(dt[idxCP2,ovar],ps2$weights[idxCP2])
   est[[i]][sim, "PS2A"] <- est[[i]][sim, "PS2"] - 
    ( weighted.mean(predict(mod, newdata=dt[idxTP2,])) -
	 weighted.mean(predict(mod, newdata=dt[idxCP2,])) ) 	
   qoi[[i]][sim, "PS2"] <- mean( g(dt[idxTP2,], 1) - g(dt[idxTP2,], 0) )
   }
  }

  }
 }
 close(pb)
	

 MSE <- list()
 MSESUB <- list()  
 mMSE  <- list()
 mMSESUB  <- list()

 means <- colMeans(n.m,na.rm=TRUE)
 tt <- order(means[sprintf("cem%.2d",1:K)],decreasing=TRUE)

 for(i in 1:NG){
   MSE[[i]] <- sqrt(colMeans((est[[i]]-qoi[[i]][,rep(1, NCOL(est))])^2, na.rm=TRUE))
   MSE[[i]] <- MSE[[i]][ c("raw", "PS0", "PS0A", "PS1", "PS1A", "PS2", "PS2A", sprintf("cem%.2d",1:K)[tt], sprintf("cem%.2dA",1:K)[tt]) ]
   MSESUB[[i]] <- sqrt(colMeans((est[[i]][,c("PS1","PS1A","PS2","PS2A",sprintf("cem%.2d",1:K), sprintf("cem%.2dA",1:K) )]-qoi[[i]][,c("PS1","PS1","PS2","PS2",sprintf("cem%.2d",1:K), sprintf("cem%.2d",1:K) )])^2, na.rm=TRUE))
   mMSE[[i]] <- max(MSE[[i]][-1],na.rm=TRUE)
   mMSESUB[[i]] <- max(MSESUB[[i]],na.rm=TRUE)
 }   

 ans <- list(MSE=MSE, MSESUB=MSESUB, mMSE=mMSE, mMSESUB=mMSESUB, n.m=n.m, tt=tt, means=means,K=K,qoi=qoi)
 return(ans) 
}


###################################################
### chunk number 21: FiveDim
###################################################
nsim <- 1000

set.seed(123)

#res5 <- tauEST5(nsim)
#save(res5, file="res5-PATT.rda")


###################################################
### chunk number 22: figMSE51plot
###################################################
load(file="res5.rda")
#load(file="res5-PATT.rda")
res <- res5
MSE <- res$MSE[[1]]
MSESUB <- res$MSESUB[[1]]
mMSE <- res$mMSE[[1]]
mMSESUB <- res$mMSESUB[[1]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)



plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 23: figMSE51
###################################################
load(file="res5.rda")
#load(file="res5-PATT.rda")
res <- res5
MSE <- res$MSE[[1]]
MSESUB <- res$MSESUB[[1]]
mMSE <- res$mMSE[[1]]
mMSESUB <- res$mMSESUB[[1]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)



plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 24: 
###################################################
means[-1]


###################################################
### chunk number 25: figMSE52plot
###################################################
res <- res5

MSE <- res$MSE[[2]]
MSESUB <- res$MSESUB[[2]]
mMSE <- res$mMSE[[2]]
mMSESUB <- res$mMSESUB[[2]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)


plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 26: figMSE52
###################################################
res <- res5

MSE <- res$MSE[[2]]
MSESUB <- res$MSESUB[[2]]
mMSE <- res$mMSE[[2]]
mMSESUB <- res$mMSESUB[[2]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)


plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 27: 
###################################################
means[-1]


###################################################
### chunk number 28: figMSE53plot
###################################################
res <- res5

MSE <- res$MSE[[3]]
MSESUB <- res$MSESUB[[3]]
mMSE <- res$mMSE[[3]]
mMSESUB <- res$mMSESUB[[3]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)



plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 29: figMSE53
###################################################
res <- res5

MSE <- res$MSE[[3]]
MSESUB <- res$MSESUB[[3]]
mMSE <- res$mMSE[[3]]
mMSESUB <- res$mMSESUB[[3]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)



plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 30: 
###################################################
means[-1]


###################################################
### chunk number 31: figMSE54plot
###################################################
res <- res5

MSE <- res$MSE[[4]]
MSESUB <- res$MSESUB[[4]]
mMSE <- res$mMSE[[4]]
mMSESUB <- res$mMSESUB[[4]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)



plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 32: figMSE54
###################################################
res <- res5

MSE <- res$MSE[[4]]
MSESUB <- res$MSESUB[[4]]
mMSE <- res$mMSE[[4]]
mMSESUB <- res$mMSESUB[[4]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)



plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 33: 
###################################################
means[-1]


###################################################
### chunk number 34: figMSE55plot
###################################################
res <- res5

MSE <- res$MSE[[5]]
MSESUB <- res$MSESUB[[5]]
mMSE <- res$mMSE[[5]]
mMSESUB <- res$mMSESUB[[5]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)



plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 35: figMSE55
###################################################
res <- res5

MSE <- res$MSE[[5]]
MSESUB <- res$MSESUB[[5]]
mMSE <- res$mMSE[[5]]
mMSESUB <- res$mMSESUB[[5]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)



plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 36: 
###################################################
means[-1]


###################################################
### chunk number 37: 
###################################################
rm(list=ls())
require(cem)
require(MatchIt)
require(rpart)

data(LL)

LL$re78 <- NULL

idT <- which(LL$treated==1)
idC <- which(LL$treated==0)

NT <- length(idT)
NC <- length(idC)

N <- NC+NT

nt <- 150
nc <- 2*nt

n <- nc+nt

set.seed(123)

tr.pop <- LL[idT,]
ct.pop <- LL[idC,]



set.seed(123)
t.data <- LL[idT, ]
c.data <- LL[idC, ]
GData <- data.frame(rbind(t.data, c.data))
GData$treat <- GData$treated
GData$treated <- NULL


G <- list()

G[[1]] <- function(x,t)    1000+2000*t*x$age+x$re74+log(1+x$re75)+x$black*x$re75^2     
G[[2]] <- function(x,t)    1000+2000*t+x$age+x$re74+x$re75^2   
G[[3]] <- function(x,t)    1000+2000*t+x$age+x$re74+x$re75+x$black+x$education    
G[[4]] <- function(x,t)    1000+2000*t+x$age+x$re74+x$re75+x$black+x$education+x$hispanic+x$nodegree+x$married   

breaks <- list()

K <- 50

for(i in 1:K){
 br <- list()
 br$age <- sample(3:7, 1)	
 br$education <- sample(3:7, 1)	
 br$re74 <- sample(3:7, 1)	
 br$re75 <- sample(3:7, 1)	
 breaks[[i]] <- br	
}



###################################################
### chunk number 38: 
###################################################
tauESTLL <- function(nsim=100){
 pb <- txtProgressBar(min = 1, max = nsim, initial = 1, style = 3)

 NG <- length(G)

 data <- GData

 myvars <- c("age", "education", "black", "married", "nodegree", "re74", "re75", "hispanic", "u74", "u75")
 
 Z <- rnorm(N, sd=0.3)
 est <- list()
 qoi <- list()

 for(i in 1:NG){
  g <- G[[i]]
  ytr <- g(t.data, 1)
  yct <- g(c.data, 0) 
  ypop <- c(ytr, yct)
  y <- ypop + Z
  data[sprintf("y%d",i)] <- y

  est[[i]] <- matrix(,nsim, 1+2*3+2*K)
  colnames(est[[i]]) <- c("raw", "PS0", "PS0A", "PS1", "PS1A", "PS2", "PS2A", sprintf("cem%.2d",1:K), sprintf("cem%.2dA",1:K))

  qoi[[i]] <-  matrix(,nsim, 1+2+K)
  colnames(qoi[[i]]) <- c("QOI", "PS1", "PS2", sprintf("cem%.2d",1:K))
  
 }
 
 n.m <- matrix(,nsim, 1+3+K)
 colnames(n.m) <- c("raw", "PS0", "PS1", "PS2", sprintf("cem%.2d",1:K))

 MYQOI <- numeric(NG)
 
  for(i in 1:NG){
   g <- G[[i]]
   MYQOI[i] <- mean( g(t.data, 1)  - g(t.data, 0) )
  }

   
 for(sim in 1:nsim){
  setTxtProgressBar(pb, sim)

  tr.idx <- sample(1:NT, nt)
  ct.idx <- sample((NT+1):N, nc)
  all.idx <- c(tr.idx, ct.idx)
  dt <- data[all.idx,]

  idxT <- list()
  idxC <- list()
  wT <- list()
  wC <- list()
 
   
  mvars <- c("treat", myvars)

  for(k in 1:K){
   m <- cem("treat", data=dt[mvars], cut=breaks[[k]], eval=F)
   idxT[[k]] <- which(m$matched == TRUE & m$groups == "1")
   idxC[[k]] <- which(m$matched == TRUE & m$groups == "0")
   wT[[k]] <- m$w[idxT[[k]]]
   wC[[k]] <- m$w[idxC[[k]]] 
   n.m[sim, sprintf("cem%.2d",k)] <- m$tab["Matched", "G1"]
  }

# naive pscore
  ps0 <- matchit(treat ~ age + education + black + married + nodegree + re74 + re75 +
						 hispanic + u74 + u75, data = dt, distance = "logit")
  idxTP <- which(ps0$weights==1 & dt$treat==1)
  idxCP <- which(ps0$weights==1 & dt$treat==0)

# I&R pscore
  mod1 <- try(pscoreSelect( treat ~ age + education + black + married + nodegree + re74 + re75 +
						 hispanic + u74 + u75,  data=dt))
  ps1 <- NULL
  ps2 <- NULL
  idxTP1 <- NULL
  idxTP2 <- NULL
  idxCP1 <- NULL
  idxCP2 <- NULL
  
  if(class(mod1)!="try-error"){ 
   						  
   ps1 <- matchit(mod1, data = dt, distance = "logit")	
   idxTP1 <- which(ps1$weights==1 & dt$treat==1)
   idxCP1 <- which(ps1$weights==1 & dt$treat==0)
   n.m[sim, "PS1"] <- length(idxTP1)

# I&R pscore + caliper
   ps2 <- matchit(mod1, data = dt, distance = "logit", caliper=0.05)	
   idxTP2 <- which(ps2$weights==1 & dt$treat==1)
   idxCP2 <- which(ps2$weights==1 & dt$treat==0)
   n.m[sim, "PS2"] <- length(idxTP2)
  }


#
 for(i in 1:NG){
   g <- G[[i]]
   form <- as.formula(sprintf("y%d ~ %s",i, paste(myvars,sep="",collapse=" + ")))
   mod <- rpart( form , data=data[ct.idx,] )
   ovar <- sprintf("y%d",i)

 #  qoi[[i]][sim, 1] <-  mean( g(data[tr.idx,], 1)  - g(data[tr.idx,], 0) )
   qoi[[i]][sim, 1] <-  MYQOI[i] 
   est[[i]][sim, "raw"] <- mean(data[tr.idx,ovar]) - mean(data[ct.idx,ovar])
   n.m[sim, "raw"] <- nt
   n.m[sim, "PS0"] <- nt

  
   for(k in 1:K){
    id1 <- idxT[[k]]
    id2 <- idxC[[k]]

    est[[i]][sim, sprintf("cem%.2d",k)] <- weighted.mean(dt[id1, ovar],wT[[k]])-weighted.mean(dt[id2, ovar],wC[[k]])

    est[[i]][sim, sprintf("cem%.2dA",k)] <- est[[i]][sim, sprintf("cem%.2d",k)] - 
      ( weighted.mean(predict(mod, newdata=dt[id1,]), wT[[k]]) -
        weighted.mean(predict(mod, newdata=dt[id2,]), wC[[k]]) ) 

    qoi[[i]][sim, sprintf("cem%.2d",k)] <- mean(g(dt[id1,], 1) - g(dt[id1,], 0)) 
   }


  est[[i]][sim, "PS0"] <- weighted.mean(dt[ idxTP, ovar ],ps0$weights[ idxTP ]) -
                              weighted.mean(dt[ idxCP, ovar ],ps0$weights[ idxCP ])
  est[[i]][sim, "PS0A"] <- est[[i]][sim, "PS0"] - 
    ( weighted.mean(predict(mod, newdata=dt[ idxTP ,])) -
      weighted.mean(predict(mod, newdata=dt[ idxCP ,])) ) 
	  
  if(class(mod1)!="try-error"){
   est[[i]][sim, "PS1"] <- weighted.mean(dt[idxTP1, ovar],ps1$weights[idxTP1])-weighted.mean(dt[idxCP1,ovar],ps1$weights[idxCP1])
   est[[i]][sim, "PS1A"] <- est[[i]][sim, "PS1"] - 
    ( weighted.mean(predict(mod, newdata=dt[idxTP1,])) -
	 weighted.mean(predict(mod, newdata=dt[idxCP1,])) ) 	

   qoi[[i]][sim, "PS1"] <- mean( g(dt[idxTP1,], 1) - g(dt[idxTP1,], 0) )

   est[[i]][sim, "PS2"] <- weighted.mean(dt[idxTP2, ovar],ps2$weights[idxTP2])-weighted.mean(dt[idxCP2,ovar],ps2$weights[idxCP2])
   est[[i]][sim, "PS2A"] <- est[[i]][sim, "PS2"] - 
    ( weighted.mean(predict(mod, newdata=dt[idxTP2,])) -
	 weighted.mean(predict(mod, newdata=dt[idxCP2,])) ) 	
   qoi[[i]][sim, "PS2"] <- mean( g(dt[idxTP2,], 1) - g(dt[idxTP2,], 0) )
  }

  }
 }
 close(pb)
	

 MSE <- list()
 MSESUB <- list()  
 mMSE  <- list()
 mMSESUB  <- list()

 means <- colMeans(n.m,na.rm=TRUE)
 tt <- order(means[sprintf("cem%.2d",1:K)],decreasing=TRUE)

 for(i in 1:NG){
   MSE[[i]] <- sqrt(colMeans((est[[i]]-qoi[[i]][,rep(1, NCOL(est))])^2, na.rm=TRUE))
   MSE[[i]] <- MSE[[i]][ c("raw", "PS0", "PS0A", "PS1", "PS1A", "PS2", "PS2A", sprintf("cem%.2d",1:K)[tt], sprintf("cem%.2dA",1:K)[tt]) ]
   MSESUB[[i]] <- sqrt(colMeans((est[[i]][,c("PS1","PS1A","PS2","PS2A",sprintf("cem%.2d",1:K), sprintf("cem%.2dA",1:K) )]-qoi[[i]][,c("PS1","PS1","PS2","PS2",sprintf("cem%.2d",1:K), sprintf("cem%.2d",1:K) )])^2, na.rm=TRUE))
   mMSE[[i]] <- max(MSE[[i]][-1],na.rm=TRUE)
   mMSESUB[[i]] <- max(MSESUB[[i]],na.rm=TRUE)
 }   

 ans <- list(MSE=MSE, MSESUB=MSESUB, mMSE=mMSE, mMSESUB=mMSESUB, n.m=n.m, tt=tt, means=means,K=K,qoi=qoi)
 return(ans) 
}


###################################################
### chunk number 39: LL
###################################################
nsim <- 1000

set.seed(123)

resLL <- tauESTLL(nsim)
save(resLL, file="resLL-PATT.rda")


###################################################
### chunk number 40: figLL1plot
###################################################
#load(file="resLL.rda")
load(file="resLL-PATT.rda")
res <- resLL

MSE <- res$MSE[[1]]
MSESUB <- res$MSESUB[[1]]
mMSE <- res$mMSE[[1]]
mMSESUB <- res$mMSESUB[[1]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)

plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 41: figLL1
###################################################
#load(file="resLL.rda")
load(file="resLL-PATT.rda")
res <- resLL

MSE <- res$MSE[[1]]
MSESUB <- res$MSESUB[[1]]
mMSE <- res$mMSE[[1]]
mMSESUB <- res$mMSESUB[[1]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)

plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 42: 
###################################################
means[-1]


###################################################
### chunk number 43: figLL2plot
###################################################
res <- resLL

MSE <- res$MSE[[2]]
MSESUB <- res$MSESUB[[2]]
mMSE <- res$mMSE[[2]]
mMSESUB <- res$mMSESUB[[2]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)

plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 44: figLL2
###################################################
res <- resLL

MSE <- res$MSE[[2]]
MSESUB <- res$MSESUB[[2]]
mMSE <- res$mMSE[[2]]
mMSESUB <- res$mMSESUB[[2]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)

plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 45: 
###################################################
means[-1]


###################################################
### chunk number 46: figLL3plot
###################################################
res <- resLL

MSE <- res$MSE[[3]]
MSESUB <- res$MSESUB[[3]]
mMSE <- res$mMSE[[3]]
mMSESUB <- res$mMSESUB[[3]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)


plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 47: figLL3
###################################################
res <- resLL

MSE <- res$MSE[[3]]
MSESUB <- res$MSESUB[[3]]
mMSE <- res$mMSE[[3]]
mMSESUB <- res$mMSESUB[[3]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)


plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 48: 
###################################################
means[-1]


###################################################
### chunk number 49: figLL4plot
###################################################
res <- resLL

MSE <- res$MSE[[4]]
MSESUB <- res$MSESUB[[4]]
mMSE <- res$mMSE[[4]]
mMSESUB <- res$mMSESUB[[4]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)


plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 50: figLL4
###################################################
res <- resLL

MSE <- res$MSE[[4]]
MSESUB <- res$MSESUB[[4]]
mMSE <- res$mMSE[[4]]
mMSESUB <- res$mMSESUB[[4]]
K <- res$K
n.m <- res$n.m
means <- res$means
tt <- res$tt

par(mfrow=c(2,1))

plot(1:K, MSE[3+(1:K)],type="b", axes=F, xlab="cutpoints(mT)", 
ylab="RMSE",ylim=c(0, mMSE),cex=0.5, main="original QOI")
points(1:K, MSE[3+K+1:K],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:K, sprintf("%d\n(%d)",1:K, round(means[sprintf("cem%.2d",1:K)[tt]])))
axis(2, c(0,MSE[c("PS0","PS0A")],mMSE), c("0","PS0", "PS0A", sprintf("%.1f",mMSE)))
abline(h=MSE["PS0"],lty=2)
abline(h=MSE["PS0A"],lty=3)


plot( MSESUB[c("PS1","PS2", sprintf("cem%.2d",1:K)) ],type="n", axes=F, xlab="cutpoints(mT)", ylab="RMSE",
ylim=c(0, mMSE),cex=0.5 , main="changed QOI")
points(1,MSESUB["PS1"],type="p",cex=0.5,col="blue",pch="1")
points(2,MSESUB["PS2"],type="p",cex=0.5,col="blue",pch="2")
points(1,MSESUB["PS1A"],type="p",cex=0.5,col="red",pch="3")
points(2,MSESUB["PS2A"],type="p",cex=0.5,col="red",pch="4")

points(2+1:K,MSESUB[sprintf("cem%.2d",1:K)[tt]],type="b",cex=0.5,col="black",pch="o")
points(2+1:K,MSESUB[sprintf("cem%.2dA",1:K)[tt]],type="b",cex=0.5,col="red",pch="x")
axis(1, 1:(2+K), sprintf("%s\n(%d)",c("PS1","PS2", 1:K), round(means[c("PS1","PS2", sprintf("cem%.2d",1:K)[tt])] )))
axis(2, c(0,mMSE), c("0", sprintf("%.1f",mMSE)))


###################################################
### chunk number 51: 
###################################################
means[-1]



### R code from vignette source 'MSE-tabs-PATT'

###################################################
### code chunk number 1: 1dim
###################################################
require(xtable)
load("res1-PATT.rda")

MSE <- res1$MSE
MSESUB <- res1$MSESUB

mat <- round(cbind(MSE[[1]], MSE[[2]], MSE[[3]], MSE[[4]], MSE[[5]], MSE[[6]]),1)
mat <- mat[1:13, ]
mat <- cbind(mat, colMeans(res1$n.m,na.rm=TRUE))
mat <- mat[-3,]
colnames(mat) <- c(sprintf("$g_%d$",1:6),"$m_T$")
rownames(mat) <- c("RAW","PS0", sprintf("CEM%.2d",1:10))
print(file="MSEtab1-PATT.tex", xtable(mat, display=c("s","f","f","f","f","f","f","d")), type="latex", floating=FALSE, sanitize.text.function = function(x){x})

mat1 <- mat
for(i in 1:NROW(mat)) mat1[i,-7] <- mat[i,-7]/mat["PS0",-7]
mat1 <- mat1[-(1:2),]

pdf("MSE1D-PATT.pdf", width=9,  height=3.5)
par(mar=c(3,3,1,1))
matplot(mat1[,-7],type="b",ylim=c(0,1.5), col="black", pch=c("1", "2", "3", "4", "5","6"), ylab="",axes=F,cex=0.7)
axis(2)
axis(1, 1:10, sprintf("C%d\n(%d)", 2:11, as.integer(mat1[,7])) , cex.axis=0.5)
abline(h=1,lty=3)
dev.off()



mat <- round(cbind(MSESUB[[1]], MSESUB[[2]], MSESUB[[3]], MSESUB[[4]], MSESUB[[5]], MSESUB[[6]]),1)
mat <- mat[1:10, ]
mat <- cbind(mat, colMeans(res1$n.m,na.rm=TRUE)[4:13])
colnames(mat) <- c(sprintf("$g_%d$",1:6),"$m_T$")
rownames(mat) <- sprintf("CEM%.2d",1:10)
print(file="MSESUBtab1-PATT.tex",  xtable(mat, display=c("s","f","f","f","f","f","f","d")), type="latex", floating=FALSE, sanitize.text.function = function(x){x})

pdf("MSE1DCH-PATT.pdf", width=9,  height=3.5)

mat1 <- mat
par(mar=c(3,3,1,1))
matplot(mat1[,-7],type="b", col="black", pch=c("1", "2", "3", "4", "5", "6"), ylab="",axes=F,cex=0.7)
axis(2)
axis(1, 1:10, sprintf("C%d\n(%d)", 2:11, as.integer(mat1[,7])) , cex.axis=0.5)


dev.off()






load("res5-PATT.rda")

MSE <- res5$MSE
MSESUB <- res5$MSESUB

mat <- round(cbind(MSE[[1]], MSE[[2]], MSE[[3]], MSE[[4]], MSE[[5]]),1)
mat <- mat[c(1,2,4,6,8:57), ]
mn <- colMeans(res5$n.m, na.rm=TRUE)
idx <- match(rownames(mat), names(mn))
mat <- cbind(mat, mn[idx])
colnames(mat) <- c(sprintf("$g_%d$",1:5),"$m_T$")
rownames(mat) <- toupper(rownames(mat))
print(file="MSEtab2-PATT.tex", xtable(mat, display=c("s","f","f","f","f","f","d")), type="latex", floating=FALSE, sanitize.text.function = function(x){x})


mat1 <- mat
for(i in 1:NROW(mat)) mat1[i,-6] <- mat[i,-6]/mat["PS0",-6]
mat1 <- mat1[-c(1,2,4),]

ps1 <- mat1[1,]
mat1 <- mat1[-1,]

pdf("MSE5D-PATT.pdf", width=9,  height=3.5)

par(mar=c(3,3,1,1))
matplot(mat1[,-6],type="b", ylim=c(0,1.1), col="black", pch=c("1", "2", "3", "4", "5"), ylab="",xlim=c(0,NROW(mat1)),axes=F,cex=0.7)
axis(2)
axis(1, 0:NROW(mat1), c("PS1\n(500)", sprintf("%s\n(%d)", rownames(mat1), as.integer(mat1[,6]))) , cex.axis=0.5)

points( rep(0,5), ps1[-6], pch="+",cex=0.5)
text( rep(0,5), ps1[-6], sprintf("PS1(%d)",1:5), cex=0.3,adj=-.5)
abline(v=0, lty=3)
dev.off()






mat <- round(cbind(MSESUB[[1]], MSESUB[[2]], MSESUB[[3]], MSESUB[[4]], MSESUB[[5]]),1)
mat <- mat[c(1,3,5:54), ]
idx <- match(rownames(mat), names(mn))
mat <- cbind(mat, mn[idx])
mat1 <- mat[-(1:2),]
id2 <- order(mat1[,6], decreasing=TRUE)
mat <- rbind(mat[1:2,], mat1[id2,])
colnames(mat) <- c(sprintf("$g_%d$",1:5),"$m_T$")
rownames(mat) <- toupper(rownames(mat))
print(file="MSESUBtab2-PATT.tex", xtable(mat,display=c("s","f","f","f","f","f","d")), type="latex", floating=FALSE, sanitize.text.function = function(x){x})



mat1 <- mat
mat1 <- mat1[-2,]

ps1 <- mat1[1,]
mat1 <- mat1[-1,]

pdf("MSE5DCH-PATT.pdf", width=9,  height=3.5)

ymax <- max(c(ps1, mat1[,-6]))


par(mar=c(3,3,1,1))
matplot(mat1[,-6],type="b",  ylim=c(0, ymax), col="black", pch=c("1", "2", "3", "4", "5"), ylab="",xlim=c(0,NROW(mat1)),axes=F,cex=0.7)
axis(2)
axis(1, 0:NROW(mat1), c("PS1\n(500)", sprintf("%s\n(%d)", rownames(mat1), as.integer(mat1[,6]))) , cex.axis=0.5)

points( rep(0,5), ps1[-6], pch="+",cex=0.5)
text( rep(0,5), ps1[-6], sprintf("PS1(%d)",1:5), cex=0.5,adj=-.5)
abline(v=0, lty=3)
dev.off()







require(xtable)
load("resLL-PATT.rda")

MSE <- resLL$MSE
MSESUB <- resLL$MSESUB

mat <- round(cbind(MSE[[1]], MSE[[2]], MSE[[3]], MSE[[4]]),1)
mat <- mat[c(1,2,4,6,8:57), ]
mn <- colMeans(resLL$n.m, na.rm=TRUE)
idx <- match(rownames(mat), names(mn))
mat <- cbind(mat, mn[idx])

colnames(mat) <- c(sprintf("$g_%d$",1:4),"$m_T$")
rownames(mat) <- toupper(rownames(mat))
print(file="MSEtab3-PATT.tex", xtable(mat,display=c("s","f","f","f","f","d")), type="latex", floating=FALSE, sanitize.text.function = function(x){x})



mat1 <- mat
for(i in 1:NROW(mat)) mat1[i,-5] <- mat[i,-5]/mat["PS0",-5]
mat1 <- mat1[-c(1,2,4),]

ps1 <- mat1[1,]
mat1 <- mat1[-1,]

pdf("MSELL-PATT.pdf", width=9,  height=3.5)

par(mar=c(3,3,1,1))
matplot(mat1[,-5],type="b", ylim=c(0,1.6), col="black", pch=c("1", "2", "3", "4"), ylab="",xlim=c(0,NROW(mat1)),axes=F,cex=0.7)
axis(2)
axis(1, 0:NROW(mat1), c("PS1\n(150)", sprintf("%s\n(%d)", rownames(mat1), as.integer(mat1[,5]))) , cex.axis=0.5)

points( rep(0,4), ps1[-5], pch="+",cex=0.5)
text( rep(0,4), ps1[-5], sprintf("PS1(%d)",1:4), cex=0.3,adj=-.5)
abline(v=0, lty=3)
dev.off()




mat <- round(cbind(MSESUB[[1]], MSESUB[[2]], MSESUB[[3]], MSESUB[[4]]),1)
mat <- mat[c(1,3,5:54), ]
mn <- colMeans(resLL$n.m, na.rm=TRUE)
idx <- match(rownames(mat), names(mn))
mat <- cbind(mat, mn[idx])
mat1 <- mat[-(1:2),]
id2 <- order(mat1[,5], decreasing=TRUE)
mat <- rbind(mat[1:2,], mat1[id2,])
colnames(mat) <- c(sprintf("$g_%d$",1:4),"$m_T$")
rownames(mat) <- toupper(rownames(mat))
print(file="MSESUBtab3-PATT.tex", xtable(mat,display=c("s","f","f","f","f","d")), type="latex", floating=FALSE, sanitize.text.function = function(x){x})




mat1 <- mat
mat1 <- mat1[-2,]

ps1 <- mat1[1,]
mat1 <- mat1[-1,]

pdf("MSELLCH-PATT.pdf", width=9,  height=3.5)

ymax <- max(c(ps1, mat1[,-5]))


par(mar=c(3,3,1,1))
matplot(mat1[,-5],type="b",  ylim=c(0, ymax), col="black", pch=c("1", "2", "3", "4"), ylab="",xlim=c(0,NROW(mat1)),axes=F,cex=0.7)
axis(2)
axis(1, 0:NROW(mat1), c("PS1\n(150)", sprintf("%s\n(%d)", rownames(mat1), as.integer(mat1[,5]))) , cex.axis=0.5)

points( rep(0,4), ps1[-5], pch="+",cex=0.5)
text( rep(0,4), ps1[-5], sprintf("PS1(%d)",1:5), cex=0.5,adj=-.5)
abline(v=0, lty=3)
dev.off()





