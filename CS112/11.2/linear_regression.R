library(ggplot2)
library(foreign)

load("~/projects/r/11.2/haha.RData")

haha.df <- data.frame(pre=pre.test, post=post.test, treat=treatment)
qplot(x=haha.df$pre, y=haha.df$post, colour=haha.df$treat)

colette.df <- read.csv("./11.2/problem_data.csv")
colette.df$GenderName <- 'M'
colette.df[colette.df$Gender ==1,'GenderName'] = 'F'
ggplot(data=colette.df) + geom_text(aes(x=Age, y=Edu, colour=Treat, label=GenderName))
colette.model <- lm(Edu~Age + Gender + Treat, data=colette.df)
summary(colette.model)

fat.cats.df <- read.csv("./11.1/fat_cats_test_set.csv")
model = lm(outcome~height + length + genetics + gender + treatment +
           height * length + gender * genetics + gender * treatment, data=fat.cats.df)
summary(model)
true.outcome <- fat.cats.df$height / 10 + fat.cats.df$length / 10 + 2 * fat.cats.df$genetics - 1.5 * (fat.cats.df$treatment * fat.cats.df$gender + 2 * fat.cats.df$treatment * (1 - fat.cats.df$gender))

fat.cats.df$black = fat.cats.df$color == 'Black'
fat.cats.df$gray = fat.cats.df$color == 'Gray'
fat.cats.df$ginger = fat.cats.df$color == 'Ginger'
fat.cats.df$albino = fat.cats.df$color == 'Albino'
model = lm(outcome~height + length + genetics + gender + treatment +
               height * length + gender * genetics + gender * treatment +
               black + gray + ginger + albino, data=fat.cats.df)
summary(model)

errors <- fat.cats.df$outcome - true.outcome
rss <- sum(errors ^ 2)
rse <- (rss / nrow(fat.cats.df)) ^ 0.5
tss <- sum((fat.cats.df$outcome - mean(fat.cats.df$outcome))^2)
r.squared <- 1 - rss/tss