# to generate Table 2. It takes very long time to run

rm(list=ls())

require(MASS)
require(cem)
require(MatchIt)
require(Matching)

set.seed(123)


r <- 3	
p <- 5
CEMbr <- 9

M <- 100 # imbspace 
n.sim <- 1000


rdaname <- sprintf("epbrL1_r%d_p%d.rda",r,p)

nt <- 1000  # number of treated
nc.ref <- nt  # number of controls


s12 <-  10
s13 <-   5
s14 <-  -5
s15 <-  -2
s23 <-   2
s24 <-  -1
s25 <-   1
s34 <-   5
s35 <-   2
s45 <-   5
s11 <-  30
s22 <-  10
s33 <-   5
s44 <-  10
s55 <-   5

S <- matrix( c(s11, s12, s13, s14, s15,
			   s12, s22, s23, s24, s25,
			   s13, s23, s33, s34, s35,
			   s14, s24, s34, s44, s45,
			   s15, s25, s35, s45, s55), 5, 5, byrow=TRUE)
S <- S/5
eigen(S)

bias <- rep(c(1,1,1,1,1), 4)

imb.raw <- matrix(NA, n.sim, p)
imb.cem <- matrix(NA, n.sim, p)
imb.mah <- matrix(NA, n.sim, p)
imb.psc <- matrix(NA, n.sim, p)

imb.mah2 <- matrix(NA, n.sim, p)
imb.psc2 <- matrix(NA, n.sim, p)

L1 <- matrix(NA, n.sim, 6)

colnames(L1) <- c("RAW", "CEM", "MAH", "PSC",  "MAH2", "PSC2")

loc.cem <- matrix(NA, n.sim, p)
loc.mah <- matrix(NA, n.sim, p)
loc.psc <- matrix(NA, n.sim, p)
loc.mah2 <- matrix(NA, n.sim, p)
loc.psc2 <- matrix(NA, n.sim, p)

sizes <- matrix(NA, n.sim, 12)
times <- matrix(NA, n.sim, 6)


CEMcuts <- list(V1=CEMbr, V2=CEMbr, V3=CEMbr, V4=CEMbr, V5=CEMbr)



Sigma <- S[1:p, 1:p]


for(i in 1:n.sim){
		t.data <- mvrnorm(n=nt, bias[1:p], Sigma)
			nc <- r*nc.ref
			n <- nt+nc
			tsubjects <- 1:nt
			csubjects <- (nt+1):n
			treated <- 1*logical(n)
			treated[tsubjects] <- 1*TRUE
			tmp <- NULL
				c.data <- mvrnorm(n=nc, numeric(p), Sigma)
				data <- rbind(t.data, c.data)
				dati <- as.data.frame(data)
			
	
                dati1 <- data.frame(treated=treated, dati)
				cat("\n running profile for raw data\n")
				imb0 <- L1.profile(treated,dati1, drop=c("treated"), M = M, plot=FALSE)
				L1breaks <- imb0$medianCP
	
				L1.raw <- imb0$medianL1 

# CEM			
				system.time(cem.mat <- cem("treated", dati1, cutpoints=CEMcuts))[3] -> t.cem
				cem.mat$tab
				cem.mat$tab
        		cem.tr <- which(cem.mat$groups=="1" & cem.mat$matched==TRUE)
	            cem.ct <- which(cem.mat$groups=="0" & cem.mat$matched==TRUE)				
				cem.m <- c(cem.tr,cem.ct)

				L1.cem <- L1.meas(treated[cem.m], dati[cem.m,], weights=cem.mat$w[cem.m], breaks=L1breaks)$L1 

# MAHALANOBIS WITH OPTIMAL MATCHING	
				system.time(mah.mat <- matchit(treated ~ V1+V2+V3+V4+V5, data=dati1, distance="mahalanobis", method = "optimal"))[3] -> t.mah                
				mah.tr <- which(mah.mat$weights>0 & dati1$treated==1)
			    mah.ct <- which(mah.mat$weights>0 & dati1$treated==0)
 	            mah.m <- c(mah.tr,mah.ct)
				L1.mah <- L1.meas(treated[mah.m], dati[mah.m,], breaks=L1breaks)$L1 	
				g1.mah <- as.integer(row.names(mah.mat$match.matrix))
				g0.mah <- as.integer(mah.mat$match.matrix[,1])

	
				system.time(mah3.mat <- Match(Tr=treated, X=dati1,Weight=2,replace=FALSE))[3] -> t.mah3
				mah3.tr <- mah3.mat$index.treated
				mah3.ct <- mah3.mat$index.control
	
				mah3.m <- c(mah3.tr,mah3.ct)
				L1.mah3 <- L1.meas(treated[mah.m], dati[mah.m,], breaks=L1breaks)$L1
				g1.mah3 <- mah3.tr
				g0.mah3 <- mah3.ct
	
				mah.tr <- mah3.tr
				mah.ct <- mah3.ct
				L1.mah <- L1.mah3
				g0.mah <- g0.mah3
				g1.mah <- g1.mah3
				mah.m <- mah3.m
	
# PSCORE WITH NEAREST NEIGHBOR
				system.time(psc.mat <- matchit(treated ~ V1+V2+V3+V4+V5, data=dati1, distance="logit", method = "nearest"))[3] -> t.psc
				psc.tr <- which(psc.mat$weights>0 & dati1$treated==1)
				psc.ct <- which(psc.mat$weights>0 & dati1$treated==0)
				psc.m <- c(psc.tr, psc.ct)
				L1.psc <- L1.meas(treated[psc.m], dati[psc.m,], breaks=L1breaks)$L1 	
	
				g1.psc <- as.integer(row.names(psc.mat$match.matrix))
				g0.psc <- as.integer(psc.mat$match.matrix[,1])

	
# MAHALANOBIS WITH OPTIMAL MATCHING	+ CEM SELECTED
				dati2 <- dati1[cem.m, ]
				system.time(mah2.mat <- matchit(treated ~ V1+V2+V3+V4+V5, data=dati2, distance="mahalanobis", method = "optimal"))[3] -> t.mah2                
				mah2.tr <- which(mah2.mat$weights>0 & dati2$treated==1)
				mah2.ct <- which(mah2.mat$weights>0 & dati2$treated==0)
				mah2.m <- c(mah2.tr,mah2.ct)
				L1.mah2 <- L1.meas(dati2$treated[mah2.m], dati2[mah2.m,], drop="treated", breaks=L1breaks)$L1 	
				g1.mah2 <- as.integer(row.names(mah2.mat$match.matrix))
				g0.mah2 <- as.integer(mah2.mat$match.matrix[,1])
	
	
# PSCORE WITH NEAREST NEIGHBOR + CEM SELECTED
				system.time(psc2.mat <- matchit(treated ~ V1+V2+V3+V4+V5, data=dati2, distance="logit", method = "nearest"))[3] -> t.psc2
				psc2.tr <- which(psc2.mat$weights>0 & dati2$treated==1)
				psc2.ct <- which(psc2.mat$weights>0 & dati2$treated==0)
				psc2.m <- c(psc2.tr, psc2.ct)
				L1.psc2 <- L1.meas(dati2$treated[psc2.m], dati2[psc2.m,], drop="treated", breaks=L1breaks)$L1 	
	
				g1.psc2 <- as.integer(row.names(psc2.mat$match.matrix))
				g0.psc2 <- as.integer(psc2.mat$match.matrix[,1])
	
	
				nt.cem <- length(unique(cem.tr))
				nc.cem <- length(unique(cem.ct))
				nt.mah <- length(unique(mah.tr))
				nc.mah <- length(unique(mah.ct))
				nt.psc <- length(unique(psc.tr))
				nc.psc <- length(unique(psc.ct))
				nt.mah2 <- length(unique(mah2.tr))
				nc.mah2 <- length(unique(mah2.ct))
				nt.psc2 <- length(unique(psc2.tr))
				nc.psc2 <- length(unique(psc2.ct))
	
                RAW <- abs(sapply(1:p, function(x) mean(dati[tsubjects,x])) 
				  - sapply(1:p, function(x) mean(dati[csubjects,x])) )

				ms <- unique(cem.mat$mstrata[which(!is.na(cem.mat$mstrata))])
				loc.CEM <- 0
                for (s in ms) {
				 idx <- which(cem.mat$mstrata == s)
				 idx.t <- which(cem.mat$groups[idx]=="1")
				 idx.c <- which(cem.mat$groups[idx]=="0")
				 loc.CEM <- loc.CEM + abs(colMeans(dati[idx[idx.t],]) - colMeans(dati[idx[idx.c],]))
                }
				loc.CEM <- loc.CEM/length(ms)


                CEM <- abs(sapply(1:p, function(x) weighted.mean(dati[cem.tr,x], cem.mat$w[cem.tr])) 
				  - sapply(1:p, function(x) weighted.mean(dati[cem.ct,x], cem.mat$w[cem.ct])) )

				MAH <- abs(sapply(1:p, function(x) mean(dati[mah.tr,x])) 
				      - sapply(1:p, function(x) mean(dati[mah.ct,x])) )

				loc.MAH <- colMeans(abs(dati1[g1.mah,] - dati1[g0.mah,]))[-1]
	
                PSC <- abs(sapply(1:p, function(x) weighted.mean(dati[psc.tr,x], w=psc.mat$weights[psc.tr])) 
				      - sapply(1:p, function(x) weighted.mean(dati[psc.ct,x], w=psc.mat$weights[psc.ct])) )

				loc.PSC <- colMeans(abs(dati1[g1.psc,] - dati1[g0.psc,]))[-1]

	
				MAH2 <- abs(sapply(1:p, function(x) weighted.mean(dati2[mah2.tr,x+1], w=mah2.mat$weights[mah2.tr])) 
			           - sapply(1:p, function(x) weighted.mean(dati2[mah2.ct,x+1], w=mah2.mat$weights[mah2.ct])) )
	
				loc.MAH2 <- colMeans(abs(dati1[g1.mah2,] - dati1[g0.mah2,]))[-1]  # LEAVE dati1 HERE
	
				PSC2 <- abs(sapply(1:p, function(x) weighted.mean(dati2[psc2.tr,x+1], w=psc2.mat$weights[psc2.tr])) 
								- sapply(1:p, function(x) weighted.mean(dati2[psc2.ct,x+1], w=psc2.mat$weights[psc2.ct])) )
	
				loc.PSC2 <- colMeans(abs(dati1[g1.psc2,] - dati1[g0.psc2,]))[-1]   # LEAVE dati1 HERE
	
 
m <- rbind(RAW, CEM, MAH, PSC, MAH2, PSC2)
rownames(m) <- c("RAW", "CEM",  "MAH", "PSC",  "MAH2", "PSC2")

loc <- rbind(loc.CEM,  loc.MAH, loc.PSC, loc.MAH2, loc.PSC2)
rownames(loc) <- c("CEM", "MAH", "PSC", "MAH2", "PSC2")

m <- cbind(m, matrix(c(nt,nc,nt.cem,nc.cem,  nt.mah, nc.mah, nt.psc, nc.psc, nt.mah2, nc.mah2, nt.psc2, nc.psc2), 6,2, byrow=TRUE))
m <- cbind(m, c(0, t.cem,  t.mah, t.psc, t.mah2, t.psc2))
colnames(m) <- c(colnames(dati1[,-1]), "mT","mC","Seconds")
colnames(loc) <- colnames(dati1[,-1])
cat(sprintf("\nsim=%.5d\n",i))
print(m)
print(loc)
			imb.raw[i,] <- RAW
			imb.cem[i,] <- CEM
			imb.mah[i,] <- MAH
			imb.psc[i,] <- PSC
			imb.mah2[i,] <- MAH2
			imb.psc2[i,] <- PSC2
			loc.cem[i,] <- loc.CEM
			loc.mah[i,] <- loc.MAH
			loc.psc[i,] <- loc.PSC
			loc.mah2[i,] <- loc.MAH2
			loc.psc2[i,] <- loc.PSC2

			sizes[i,] <- c(nt,nc,nt.cem,nc.cem, nt.mah, nc.mah, nt.psc, nc.psc, nt.mah2, nc.mah2, nt.psc2, nc.psc2)
            times[i, ] <-  c(0, t.cem, t.mah, t.psc, t.mah2, t.psc2)

			L1[i,] <- c(L1.raw, L1.cem, L1.mah, L1.psc, L1.mah2, L1.psc2)
print(L1[i,])

		}


tab <- rbind(colMeans(imb.raw,na.rm=TRUE), colMeans(imb.cem,na.rm=TRUE),  colMeans(imb.mah,na.rm=TRUE), colMeans(imb.psc,na.rm=TRUE),
colMeans(imb.mah2,na.rm=TRUE), colMeans(imb.psc2,na.rm=TRUE)) 
tab <- cbind(tab, matrix( colMeans(sizes,na.rm=TRUE), 6, 2, byrow=TRUE), colMeans(times,na.rm=TRUE))

rownames(tab) <- c("RAW", "CEM", "MAH", "PSC", "MAH2", "PSC2")
colnames(tab) <- c(colnames(dati1[,-1]), "mT","mC","Seconds")

tab

loc.tab <-  rbind(colMeans(loc.cem,na.rm=TRUE), colMeans(loc.mah,na.rm=TRUE), colMeans(loc.psc,na.rm=TRUE),
colMeans(loc.mah2,na.rm=TRUE), colMeans(loc.psc2,na.rm=TRUE)) 
rownames(loc.tab) <- c("CEM", "MAH", "PSC", "MAH2", "PSC2")
colnames(loc.tab) <- colnames(dati1[,-1])
loc.tab

L1.tab <- colMeans(L1,na.rm=TRUE)

L1.tab

save.image(rdaname)

library(xtable)
load("epbrL1_r3_p5.rda")
tab <- tab[,-8]
colnames(tab) <- c("$X_1$", "$X_2$", "$X_3$", "$X_4$", "$X_5$", "$m_T$","$m_C$") 
rownames(tab)[1] <- "initial imb."
print(xtable(tab, display=c("s","f","f","f","f","f","d","d")), floating=FALSE, sanitize.text.function = function(x) {x})


###################################################
### code chunk number 2: tab1a
###################################################
loc.tab <- cbind(rbind(rep(NA,5), loc.tab), L1.tab)
L1r1 <- sprintf("%.2f",L1.tab[1])
colnames(loc.tab) <- c("$X_1$", "$X_2$", "$X_3$", "$X_4$", "$X_5$", "$\\mathcal L_1$") 
rownames(loc.tab)[1] <- "initial"
print(xtable(loc.tab), floating=FALSE, sanitize.text.function = function(x) {x})



