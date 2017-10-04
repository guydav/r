library(rdrobust)

pre <- read.csv("./9.2/CS112_9_1_x_i.csv")$x
# post.control <- read.csv("./9.2/CS112_9_1_y_i_0.csv")$x
# post.treat <- read.csv("./9.2/CS112_9_1_y_i_1.csv")$x
post <- read.csv('./9.2/CS112_9_1_y_i.csv')$x

rd.data <- data.frame(x=pre, y=post)
rdplot(y=rd.data$y, x=rd.data$x, c=7)
rdrobust(y=rd.data$y, x=rd.data$x, c=7, all=TRUE)
bw <- rdbwselect(y=rd.data$y, x=rd.data$x, c=7, all=TRUE)