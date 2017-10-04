
fisher.exact <- function(treatment, control, sharp.null, num.experiments) {
    results <- vector("double")
    len.treatment = length(treatment)
    len.control = length(control)
      
    for(exp in 1:num.experiments) {
        num.t.from.t = sample(0:len.treatment, 1)
        t.from.t.indices = sample(1:len.treatment, num.t.from.t)
        t.from.c.indices = sample(1:len.control, len.treatment - num.t.from.t)
        
        t.from.t = treatment[t.from.t.indices]
        t.from.c = control[t.from.c.indices]
        exp.treatment = c(t.from.t, t.from.c + sharp.null)
        
        if (length(t.from.t.indices) != 0) {
            c.from.t = treatment[-t.from.t.indices]    
        } else {
            c.from.t = treatment
        }
        if (length(t.from.c.indices) != 0) {
            c.from.c = control[-t.from.c.indices]    
        } else {
            c.from.c = control
        }
        
        exp.control = c(c.from.c, c.from.t - sharp.null)    
        
        test.statistic = mean(exp.treatment) - mean(exp.control)
        # cat(exp, num.t.from.t, exp.treatment, exp.control, test.statistic)
        # print("")
        results = c(results, test.statistic) 
    }
    
    observed.test.statistic = mean(treatment) - mean(control)
    return(1 - sum(observed.test.statistic > results) / num.experiments)
}

fisher.df <- function(treatment, control, sharp.null, num.experiments) {
    results <- vector("double")
    len.treatment = length(treatment)
    len.control = length(control)
    
    df.treatment = data.frame(y=children.treatment, w=rep(TRUE, len.treatment))
    df.control = data.frame(y=children.control, w=rep(FALSE, len.control))
    df = rbind(df.treatment, df.control)
    rows = nrow(df)
            
    for(exp in 1:num.experiments) {
        t.indices = sample(1:rows, len.treatment)
        t.values = df[t.indices,'y'] + (!df[t.indices,'w']) * sharp.null 
        c.values = df[-t.indices,'y'] - df[t.indices,'w'] * sharp.null 
        
        test.statistic = mean(t.values) - mean(c.values)
        # cat(exp, t.indices, t.values, c.values, test.statistic)
        # print("")
        results = c(results, test.statistic) 
    }
    
    observed.test.statistic = mean(treatment) - mean(control)
    return(1 - sum(observed.test.statistic > results) / num.experiments)
}


children.treatment = c(70.0, 66.0, 78.9)
children.control = c(55.0, 72.0, 72.7)
# fisher.exact(children.treatment, children.control, 0, 1000)
fisher.df(children.treatment, children.control, 0, 100)

