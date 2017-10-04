####################################################################################
############ This is file analysis.R
############ I load the data and run the analyses for the article:
############ Fiscal Governance in the Eurozone: How effectively does the 
############ Stability and Growth Pact limit governmental debt in the Eurozone?
############ by Sebastian Koehler and Thomas König
############ forthcoming in: Political Science Research and Methods
####################################################################################


#### load required packages
library(Synth)
library(lattice)
library(xtable)
lattice.options(default.theme = modifyList(standard.theme(color = FALSE), list(strip.background = list(col = "transparent"))))


#### clear workspace
rm(list=ls())

#### set your working directory here (adjust the path to the files on your computer)
dir <- "/Users/guydavidson/projects/r/7.2/Kohler_replication"
setwd("/Users/guydavidson/projects/r/7.2/Kohler_replication")

#### load dataset (adjust the path to the files on your computer)
load("scdata.Rdata")



#### Define aggregates we need later
Euro12 <- c('Germany','Netherlands','Greece','Spain','Portugal','Italy',#
            'Finland','France','Luxembourg','Belgium','Austria','Ireland')

Euro17 <- c('Germany','Netherlands','Greece','Spain','Portugal','Italy','Finland','France','Luxembourg','Belgium','Austria','Ireland','Slovakia','Cyprus','Malta','Slovenia','Estonia')

donor <- c("Germany","Netherlands","Belgium","Austria","Finland","France")

recipient <- c("Portugal","Spain","Greece","Italy","Ireland")

#### Define list element to store estimation results
scresults <- vector(mode="list",length=11)
names(scresults) <- c("Euro11","donor","recipient","Greece","Portugal","Spain","Ireland","Italy","robustness","deficit","general")

#### source scripts with individual analyses for the following aggregates/countries

# loop across control units

pred <- names(scdata)[c(#
    7, # pop65+
    8, # pop15-	
    #9,# unemployment (1)
    #10,# system
    11,# yrcurnt
    12,# allhouse
    13,# legelec
    14, # pluralty
    15, # pr
    #16, # checks 
    #17, # fri
    #18,# GDP growth	
    #19,# growth in multi factor productivity
    20,# labor productivity annual growth	
    21, # health expenditure/GDP
    22,# GDP expenditure approach
    #23,# tax revenue %GDP (general)	
    24,# taxrev %GDP, no Social Security
    25, # CO2 emissions
    #26, # FDI	
    #27, # GDP growth	
    #28,# Gini index	
    #29,#,# Inflation (Consumer Prices)
    #30, # Poverty	
    31,#, # unemployment (World Bank)	
    ##32, #Population
    #33,#, #openness (PWT)
    34,#,# openness (expenditure)
    #35, # Expenditure on Families %GDP
    36, # PolconIII
    #37, # PolconV
    38,  # Potrafke ideology
    39, # Majority margin
    #40, # Herfindahl Index Government
    41, #lag debt/gdp (RR)	
    42#,# Rae Fractionalisation index (government)
    #43 # Rae Fractionalisation Index (total)
)]

contr <- sort(unique(scdata$ccode[is.element(scdata$country,setdiff(scdata$country,c(Euro12,"Euro 11","Slovenia")))]))

#### The following countries have to be excluded due to data constraints (missing values)
contr <- setdiff(contr, c(1111,2222,70,155,225,269,290,310,316,317,349,355,360,366,666,732,sort(unique(scdata$ccode[scdata$eu==0 & scdata$oecd==0]))))

#### Show countries that are available for Synthetic Greece
country <- sort(unique(scdata$country[scdata$ccode %in% contr]))

scdata.relevant.countries <- scdata[scdata$ccode %in% contr,]

storegaps <- 
    matrix(NA,
           length(1983:2010),
           length(contr)
    )
rownames(storegaps) <- 1983:2010
i <- 1


for(k in contr){
    # data prep for main model
    dataprep.out <-
        dataprep(foo = scdata.relevant.countries,
                 predictors = pred,
                 dependent = names(scdata[6]),
                 unit.variable = "ccode",
                 time.variable = "Year", 
                 treatment.identifier = k,
                 controls.identifier = contr[-which(contr==k)], 
                 time.predictors.prior = c(1983:1998),
                 time.optimize.ssr = c(1983:1999),
                 unit.names.variable = "country",
                 time.plot = 1983:2010
        )
    
    # fit main model
    synth.out <- synth(data.prep.obj=dataprep.out, method = "BFGS")
    
    storegaps[,i] <-  
        dataprep.out$Y1-
        (dataprep.out$Y0%*%synth.out$solution.w)
    i <- i + 1
} # close loop over control units

#### Generate Synth object (debt/gdp ratio) to be used to run the analysis
sdata <- dataprep(foo = scdata[scdata$ccode %in% contr | scdata$country == "Euro 11",],
                  predictors = pred,
                  dependent = names(scdata[6]),
                  unit.variable = "ccode",
                  time.variable = "Year", 
                  treatment.identifier = 0,
                  controls.identifier = contr, 
                  time.predictors.prior = c(1983:1998),
                  time.optimize.ssr = c(1983:1999),
                  unit.names.variable = "country",
                  time.plot = 1983:2010
)


#### Run the synthetic control analysis:
synth.out <- synth(data.prep.obj = sdata, method = "BFGS")

#### calculate output gaps from the results
gaps <- sdata$Y1plot - (sdata$Y0plot %*% synth.out$solution.w)

colnames(storegaps) <- country
storegaps <- cbind(gaps,storegaps)
colnames(storegaps)[1] <- c("Euro 11")

# compute ratio of post-reunification RMSPE 
# to pre-reunification RMSPE                                                  
rmse <- function(x){sqrt(mean(x^2))}
preloss <- apply(storegaps[1:17,],2,rmse)
postloss <- apply(storegaps[18:28,],2,rmse)
spp <- sort(postloss/preloss)
dotchart(spp,
         xlab="Post-Period RMSE / Pre-Period RMSE",
         pch=19)



#### Euro 11
source(paste(dir,"euro11.R",sep="/"))

#### Donor
source(paste(dir,"donor.R",sep="/"))

#### Recipient
source(paste(dir,"recipient.R",sep="/"))

#### Greece
source(paste(dir,"Greece.R",sep="/"))

#### Portugal
source(paste(dir,"Portugal.R",sep="/"))

#### Spain
source(paste(dir,"Spain.R",sep="/"))

#### Ireland
source(paste(dir,"Ireland.R",sep="/"))

#### Italy
source(paste(dir,"Italy.R",sep="/"))

#### Robustness checks
source(paste(dir,"robustness.R",sep="/"))

#### General Government Debt
source(paste(dir,"general.R",sep="/"))

#### Deficit
source(paste(dir,"deficit.R",sep="/"))

#### Save Results as Rdata file
save(scresults,file='../data/scresults.Rdata')

