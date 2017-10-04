
modelDep <- function (formula, data, tevar, weights, depth=3, min.vars =1,alpha=0.05) 
{
	outcomevar <- unlist(strsplit(as.character(formula)[2], 
									  split = "+", fixed = T))
	covariates <- unlist(strsplit(as.character(formula)[3], split = "+", 
									  fixed = T))
	covariates <- gsub(" ", "", covariates, fixed = T)
	if (length(which(covariates == "")) > 0) {
			covariates <- covariates[-c(which(covariates == ""))]
	}
	
	idx <- which(covariates==tevar)
	if(length(idx)>0)
 	 covariates <- covariates[-idx]
    if(missing(weights))
		weights= rep(1, NROW(data))

	xvars <- unique(covariates)
		
	start <- sprintf("%s ~ 1 + %s",outcomevar,tevar) #conditional effects
	
	total <- 1
	for (i in min.vars:(length(xvars)-1))
	total <- total + ncol(combn(xvars, i)) 
	
	for (i in 2:depth)
	 total <- total + ncol(combn(xvars, i)) 
	
	
	c.idx <- NULL	
	for(i in xvars){
		if(is.numeric(data[[i]]) | is.integer(data[[i]]))
		c.idx <- c(c.idx, i)
	}
	
	n.cvars <- length(c.idx)
	cvars <- NULL
	if(n.cvars>0){	
		cvars <- c.idx
		for (i in 1:min(depth, n.cvars))
		total <- total + ncol(combn(cvars, i)) 
	}
	
	cat("\n I'm going to run", total, "different models!\n\n")
	cat("\n\n")
	pb <- txtProgressBar(min = 1, max = total, initial = 1, style = 3)
	TE <- rep(as.numeric(NA), total)
	frml <- character(total)
	counter <- 0
	for (i in min.vars:(length(xvars)-1)) {
		allsubset <- combn(xvars, i)
		for (j in 1:ncol(allsubset)) {
			ftmp <- start
			for (k in 1:i)
			ftmp <- paste(ftmp, "+", allsubset[k,j])
			frml[counter] <- ftmp
			ftmp <- as.formula(ftmp)
			counter <- counter + 1
			setTxtProgressBar(pb, counter)
			model <- try(lm(ftmp, data = data, weights=weights), silent = TRUE)
			if (class(model) != "try-error") {
				if(summary(model)$coeff[tevar,"Pr(>|t|)"]<alpha)
				 TE[counter] <- summary(model)$coeff[tevar,"Estimate"]
				
			}
		}
	}
	
	mfull <- sprintf("%s ~ 1 + %s + %s",outcomevar, tevar,paste(xvars,collapse=" + "))
	
	for (i in 2:3) {
		allsubset <- combn(xvars, i)
		for (j in 1:ncol(allsubset)) {
			ftmp <- paste( mfull , "+", paste(allsubset[,j],collapse="*"))
			frml[counter] <- ftmp
			ftmp <- as.formula(ftmp)
			counter <- counter + 1
			setTxtProgressBar(pb, counter)
			model <- try(lm(ftmp, data = data, weights=weights), silent = TRUE)
			if (class(model) != "try-error") {
				if(summary(model)$coeff[tevar,"Pr(>|t|)"]<alpha)
				 TE[counter] <- summary(model)$coeff[tevar,"Estimate"]
			}
		}
	}
	
	if(n.cvars>1){
		for (i in 2:min(3, n.cvars)) {
			allsubset <- combn(cvars, i)
			for (j in 1:ncol(allsubset)) {
				ftmp <- paste( mfull , "+", paste(sprintf("+ I(%s^%d)",allsubset[,j],2), collapse=""))
				frml[counter] <- ftmp
				ftmp <- as.formula(ftmp)
				counter <- counter + 1
				setTxtProgressBar(pb, counter)
				model <- try(lm(ftmp, data = data, weights=weights), silent = TRUE)
				if (class(model) != "try-error") {
					if(summary(model)$coeff[tevar,"Pr(>|t|)"]<alpha)
					 TE[counter] <- summary(model)$coeff[tevar,"Estimate"]
				}
			}
		}
	}
	
	
	close(pb)
#	print(counter)
#	print(total)
	return( list(TE=TE, frml=frml) ) 
}





