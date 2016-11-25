# Attempts to replicate King & Zeng (2006)'s Table 2
wp.dat <- read.table('./replication_data/wp.dat')
colnames(wp.dat) <- unlist(strsplit('pbs2s3 wartype logcost wardur factnum factnum2 trnsfcap develop exp decade treaty un2 untype4', split = '\\s'))
wp.dat.orig.model <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum
                         + factnum2 + trnsfcap + develop + exp + decade 
                         + treaty + untype4, 
                         data=wp.dat, family = 'binomial')
summary(wp.dat.orig.model)
wp.dat.kz.model <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum
                       + factnum2 + trnsfcap + develop + exp + decade 
                       + treaty + untype4 + wardur * untype4, 
                       data=wp.dat, family = 'binomial')
summary(wp.dat.kz.model)


# Code dump
# Mask out factor columns with multiple levels - most are broken down to dummy variables either way
# peace.df.masked <- peace.df[, !(colnames(peace.df) %in% c('outcome', 'outcome2', 'uncint', 'un2cint', 'untype', 'UNops'))]

relevant.cols <- c("decade", "geo", "royal", "euro", "lac", "mideast", "asia", "africa",
                   "bollen80", "wardur", "wartype", "un2int", "nouncint", "peaceop", "interven",
                   "major", "pw10", "elf", "factnum", "factnum2", "battle", "dead",
                   "ridp", "lifes", "lifee", "garm", "gdp60", "gdp85", "gdpcap",
                   "rgdpcaps", "rgdpcape", "rgdpch", "rgdpch2", "aid91", "en1",
                   "en2st", "en2end", "area", "popst", "popen5", "popen10", "popgro",
                   "infst", "border", "illit", "illitend", "water", "gini1", "transf85",
                   "sxp", "nettrans", "electric", "en9903s", "eh", "coldwar", "logcost", "trnsfcap", 
                   "develop", "exp", "treaty", "UNop1", "UNop2", "UNop3", "UNop4", "pbs2s3")
peace.df.masked <- peace.df[,relevant.cols]
peace.df.masked <- na.omit(peace.df.masked)

minimal.model <- glm(pbs2s3 ~ un2int, data=peace.df.minimal, family='binomial')
# full.model <- glm(pbs2s3 ~ ., data=peace.df.masked, family='binomial')
step.result <- stepAIC(minimal.model, 
                       scope=list(upper=as.formula(paste("pbs2s3 ~ ", paste(minimal.cols[1:length(minimal.cols) - 1], collapse = ' + '))),
                                  lower="pbs2s3 ~ un2int"))
summary(step.result)
