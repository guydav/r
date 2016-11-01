library(rdrobust)

pre <- read.csv("./9.2/CS112_9_1_x_i.csv")$x
post.control <- read.csv("./9.2/CS112_9_1_y_i_0.csv")$x
post.treat <- read.csv("./9.2/CS112_9_1_y_i_1.csv")$x

rd.data <- data.frame(x=sort(pre), y=sort(c(post.control, post.treat)))
rdplot(y=rd.data$y, x=rd.data$x, c=7)
rdrobust(y=rd.data$y, x=rd.data$x, c=7, all=TRUE)
rdbwselect(y=rd.data$y, x=rd.data$x, c=7, all=TRUE)