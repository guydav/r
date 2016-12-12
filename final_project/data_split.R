split.train.dev.test <- function(data, train.prop = 0.6, dev.prop = 0.2, test.prop = 0.2) {
    if (1 != train.prop + dev.prop + test.prop) {
        stop('Proportions must add up to one')
    }
    
    rows <- nrow(data)
    # set aside dev and test sets
    dev.test.indices <- sample(1:rows, (dev.prop + test.prop) * rows)
    dev.test.length <- length(dev.test.indices)
    # create test and dev indices
    test.sub.indices <- sample(1:dev.test.length, 0.5 * dev.test.length)
    test.indices <- dev.test.indices[test.sub.indices]
    dev.indices <- dev.test.indices[-test.sub.indices]
    # split the data set in three
    train <- data[-dev.test.indices,]
    dev <- data[test.indices,]
    test <- data[dev.indices,]
    list(train, dev, test)
}