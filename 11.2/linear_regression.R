library(ggplot2)
library(foreign)

load("~/projects/r/11.2/haha.RData")

haha.df <- data.frame(pre=pre.test, post=post.test, treat=treatment)
qplot(x=haha.df$pre, y=haha.df$post, colour=haha.df$treat)

colette.df <- read.csv("./11.2/problem_data.csv")
colette.df$GenderName <- 'M'
colette.df[colette.df$Gender ==1,'GenderName'] = 'F'
ggplot(data=colette.df) + geom_text(aes(x=Age, y=Edu, colour=Treat, label=GenderName))

fat.cats.df <- read.csv("./11.1/fat_cats_test_set.csv")

true.outcome <- fat.cats.df$height / 10 + fat.cats.df$length / 10 + 2 * fat.cats.df$genetics - 1.5 * (fat.cats.df$treatment * fat.cats.df$gender + 2 * fat.cats.df$treatment * (1 - fat.cats.df$gender))
    

errors <- fat.cats.df$outcome - true.outcome
rss <- sum(errors ^ 2)
rse <- (rss / nrow(fat.cats.df)) ^ 0.5
tss <- sum((fat.cats.df$outcome - mean(fat.cats.df$outcome))^2)
r.squared <- 1 - rss/tss