library(Synth)
library(ggplot2)
library(xtable)

data("basque")

basque.prepped <- dataprep(
    foo = basque,
    predictors = c("school.illit", "school.prim", "school.med", 
                   "school.high", "school.post.high", "invest"),
    predictors.op = "mean",
    time.predictors.prior = 1961:1969,
    special.predictors = list(
        list("gdpcap", 1961:1969, "mean"),
        list("sec.agriculture", seq(1961, 1969, 2), "mean"),
        list("sec.industry", seq(1961, 1969, 2), "mean"),
        list("sec.construction", seq(1961, 1969, 2), "mean"),
        list("sec.services.venta", seq(1961, 1969, 2), "mean"),
        list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
        list("popdens", 1969, "mean")
    ),
    dependent = "gdpcap",
    unit.variable = "regionno",
    unit.names.variable = "regionname",
    time.variable = "year",
    treatment.identifier = 17,
    controls.identifier = c(2:16, 18),
    time.optimize.ssr = 1961:1969,
    time.plot = 1955:1997)

basque.synth.out <- synth(data.prep.obj = basque.prepped, method = "BFGS")
                          #optimxmethod = "All", genoud = TRUE)

basque.synth.tables <- synth.tab(dataprep.res = basque.prepped, synth.res = basque.synth.out)
basque.synth.tables

#### Figure 1: Trends in per-capita GDP: Basque Country vs. Rest of Spain
Text.height <- 10
Cex.set <- .8
plot(1955:1997,basque.prepped$Y1plot,
     type="l",ylim=c(0,15),col="black",lty="solid",
     ylab ="per-capita GDP",
     xlab ="year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
### Does this really remove West Germany from the sample?
lines(1955:1997,aggregate(basque[,c("gdpcap")], by=list(basque$year), mean, na.rm=TRUE)[,2]
      ,col="black",lty="dashed",lwd=2) # mean 2
abline(v=1975,lty="dotted")
legend(x="bottomright",
       legend=c("Basque Country","Spain per-county Average")
       ,lty=c("solid","dashed"),col=c("black","black")
       ,cex=Cex.set,bg="white",lwd=c(2,2))
arrows(1972,Text.height,1974,Text.height,col="black",length=.1)
text(1965,Text.height,"ETA terrorism begins",cex=Cex.set)

#### Figure 2: Trends in Per-Capita GDP: Basque Country vs. Synthetic Basque Country
Text.height <- 10
Cex.set <- .8
synthY0 <- (basque.prepped$Y0plot %*% basque.synth.out$solution.w)
plot(1955:1997,basque.prepped$Y1plot,
     type="l",ylim=c(0,15),col="black",lty="solid",
     ylab ="per-capita GDP",
     xlab ="year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
lines(1955:1997,synthY0,col="black",lty="dashed",lwd=2)
abline(v=1975,lty="dotted")
legend(x="bottomright",
       legend=c("Basque Country","Synthetic Basque")
       ,lty=c("solid","dashed"),col=c("black","black")
       ,cex=Cex.set,bg="white",lwd=c(2,2))
arrows(1972,Text.height,1974,Text.height,col="black",length=.1)
text(1965,Text.height,"ETA terrorism begins",cex=Cex.set)

### Figure 3: Per-Capita GDP Gap
#pdf(file = "ger_vs_synthger_gaps2.pdf", width = 5.5, height = 5.5, family = "Times",pointsize = 12)
gaps <- basque.prepped$Y1plot - (basque.prepped$Y0plot %*% basque.synth.out$solution.w)
Text.height = 1
plot(1955:1997,gaps,
     type="l",ylim=c(-2,2),col="black",lty="solid",
     ylab =c("gap in per-capita GDP (PPP, 2002 USD)"),
     xlab ="year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
abline(h=0,lty="dotted", lwd=2)
abline(v=1975,lty="dotted")
legend(x="bottomright",
       legend=c("Basque Country","Synthetic Basque")
       ,lty=c("solid","dashed"),col=c("black","black")
       ,cex=Cex.set,bg="white",lwd=c(2,2))
arrows(1972,Text.height,1974,Text.height,col="black",length=.1)
text(1965,Text.height,"ETA terrorism begins",cex=Cex.set)
#dev.off()

### Figure 4: Placebo Treatment 1965
basque.time.placebo.prepped <- dataprep(
    foo = basque,
    predictors = c("school.illit", "school.prim", "school.med", 
                   "school.high", "school.post.high", "invest"),
    predictors.op = "mean",
    time.predictors.prior = 1961:1965,
    special.predictors = list(
        list("gdpcap", 1961:1965, "mean"),
        list("sec.agriculture", seq(1961, 1965, 2), "mean"),
        list("sec.industry", seq(1961, 1965, 2), "mean"),
        list("sec.construction", seq(1961, 1965, 2), "mean"),
        list("sec.services.venta", seq(1961, 1965, 2), "mean"),
        list("sec.services.nonventa", seq(1961, 1965, 2), "mean") #,
        #list("popdens", 1969, "mean")
    ),
    dependent = "gdpcap",
    unit.variable = "regionno",
    unit.names.variable = "regionname",
    time.variable = "year",
    treatment.identifier = 17,
    controls.identifier = c(2:16, 18),
    time.optimize.ssr = 1961:1965,
    time.plot = 1955:1975)

basque.time.placebo.synth.out <- synth(data.prep.obj = basque.time.placebo.prepped, method = "BFGS")
#optimxmethod = "All", genoud = TRUE)

basque.time.placebo.synth.tables <- synth.tab(dataprep.res = basque.time.placebo.prepped, synth.res = basque.time.placebo.synth.out)

Text.height = 10
time.placebo.synthY0 <- (basque.time.placebo.prepped$Y0plot %*% basque.time.placebo.synth.out$solution.w)
plot(1955:1975,basque.time.placebo.prepped$Y1plot,
     type="l",ylim=c(0,15),col="black",lty="solid",
     ylab ="per-capita GDP",
     xlab ="year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
lines(1955:1975,time.placebo.synthY0,col="black",lty="dashed",lwd=2)
abline(v=1965,lty="dotted")
legend(x="bottomright",
       legend=c("Basque Country","Synthetic Basque")
       ,lty=c("solid","dashed"),col=c("black","black")
       ,cex=Cex.set,bg="white",lwd=c(2,2))
arrows(1962.5,Text.height,1964.5,Text.height,col="black",length=.1)
text(1959.5,Text.height,"ETA terrorism begins",cex=Cex.set)

### Figure 5: Ratio of post-reunification RMSPE to pre-reunification RMSPE: West Germany and control countries.

# loop across control units
storegaps <- 
    matrix(NA,
           length(1955:1997),
           length(unique(d$index))-1
    )
rownames(storegaps) <- 1955:1997
i <- 1
co <- 2:18
co.no.basque <- c(2:16, 18)
for(k in co.no.basque){
    # data prep for main model
    dataprep.out <-
        dataprep(
            foo = basque,
            predictors = c("school.illit", "school.prim", "school.med", 
                           "school.high", "school.post.high", "invest"),
            predictors.op = "mean",
            time.predictors.prior = 1961:1969,
            special.predictors = list(
                list("gdpcap", 1961:1969, "mean"),
                list("sec.agriculture", seq(1961, 1969, 2), "mean"),
                list("sec.industry", seq(1961, 1969, 2), "mean"),
                list("sec.construction", seq(1961, 1969, 2), "mean"),
                list("sec.services.venta", seq(1961, 1969, 2), "mean"),
                list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
                list("popdens", 1969, "mean")
            ),
            dependent = "gdpcap",
            unit.variable = "regionno",
            unit.names.variable = "regionname",
            time.variable = "year",
            treatment.identifier = k,
            controls.identifier = co.no.basque[-which(co.no.basque==k)],
            time.optimize.ssr = 1961:1969,
            time.plot = 1955:1997)
    
    # fit main model
    synth.out <- synth(data.prep.obj=dataprep.out, method = "BFGS")
    
    storegaps[,i] <-  
        dataprep.out$Y1-
        (dataprep.out$Y0%*%synth.out$solution.w)
    i <- i + 1
} # close loop over control units
basque <- basque[order(basque$regionno,basque$year),]
cn = unique(basque$regionname)[-1]
colnames(storegaps) <- cn[-16]
storegaps <- cbind(gaps,storegaps)
colnames(storegaps)[1] <- c("Basque Country")

# compute ratio of post-reunification RMSPE 
# to pre-reunification RMSPE                                                  
rmse <- function(x){sqrt(mean(x^2))}
preloss <- apply(storegaps[1:20,],2,rmse)
postloss <- apply(storegaps[21:43,],2,rmse)
spp <- sort(postloss/preloss)
dotchart(spp,
         xlab="Post-Period RMSE / Pre-Period RMSE",
         pch=19)
sum(spp >= 7.376512) / length(spp)

#### Figure XXX - Andalucia?
andalucia.prepped <- dataprep(
    foo = basque,
    predictors = c("school.illit", "school.prim", "school.med", 
                   "school.high", "school.post.high", "invest"),
    predictors.op = "mean",
    time.predictors.prior = 1961:1969,
    special.predictors = list(
        list("gdpcap", 1961:1969, "mean"),
        list("sec.agriculture", seq(1961, 1969, 2), "mean"),
        list("sec.industry", seq(1961, 1969, 2), "mean"),
        list("sec.construction", seq(1961, 1969, 2), "mean"),
        list("sec.services.venta", seq(1961, 1969, 2), "mean"),
        list("sec.services.nonventa", seq(1961, 1969, 2), "mean"),
        list("popdens", 1969, "mean")
    ),
    dependent = "gdpcap",
    unit.variable = "regionno",
    unit.names.variable = "regionname",
    time.variable = "year",
    treatment.identifier = 4,
    controls.identifier = c(2:3, 5:16, 18),
    time.optimize.ssr = 1961:1969,
    time.plot = 1955:1997)

andalucia.synth.out <- synth(data.prep.obj = andalucia.prepped, method = "BFGS")
#optimxmethod = "All", genoud = TRUE)

andalucia.synth.tables <- synth.tab(dataprep.res = andalucia.prepped, synth.res = andalucia.synth.out)
Text.height <- 10
Cex.set <- .8
synthY0 <- (andalucia.prepped$Y0plot %*% andalucia.synth.out$solution.w)
plot(1955:1997,andalucia.prepped$Y1plot,
     type="l",ylim=c(0,15),col="black",lty="solid",
     ylab ="per-capita GDP",
     xlab ="year",
     xaxs = "i", yaxs = "i",
     lwd=2
)
lines(1955:1997,synthY0,col="black",lty="dashed",lwd=2)
abline(v=1975,lty="dotted")
legend(x="bottomright",
       legend=c("Principado","Synthetic Principado")
       ,lty=c("solid","dashed"),col=c("black","black")
       ,cex=Cex.set,bg="white",lwd=c(2,2))
arrows(1972,Text.height,1974,Text.height,col="black",length=.1)
text(1965,Text.height,"ETA terrorism begins",cex=Cex.set)
