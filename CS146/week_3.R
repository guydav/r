dquad <- function(x, log.p=FALSE) {
    out <- 12 * (x ** 2) - 12 * x + 3
    if (log.p) {
        out <- log(out)
    }
    return(out)
}

pquad <- function(q, log.p=FALSE, lower.tail=TRUE) {
    out <- 4 * (q ** 3) - 6 * (q ** 2) + 3 * q
    
    if (!lower.tail) {
        out <- 1 - out
    }
    
    if (log.p) {
        out <- log(out)
    }
    return(out)
}

delta <- 0.0000001

qquad <- function(p, log.p=FALSE, lower.tail=TRUE) {
    if (log.p) {
        p <- exp(p)
    } 
    
    if (!lower.tail) {
        p <- 1 - p
    }
    out <- list()
    for (target in p) {
        x <- target
        while (abs(target - pquad(x)) > delta) {
            step <- (target - pquad(x)) / dquad(x)
            # print(paste(target, pquad(x), x, step))
            x <- x + step
        }
        out <- append(out, x)
    }
    
    return(unlist(out))
}

pquad(qquad(0.75))
