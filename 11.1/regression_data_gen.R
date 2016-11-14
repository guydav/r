library(randomNames)
size <- 100
size.test <- 50

colors <- c('Ginger', 'Black', 'Gray', 'Albino')
cat.color = sample(colors, size, replace=TRUE)
treatment <- rbinom(size, 1, 0.5)
gender <- rbinom(size, 1, 0.5)
height <- rnorm(size, 24, 2)
length <- rnorm(size, 46, 4)
names <- randomNames(size, which.names = 'first')
genetic.factor <- runif(size, 0, 1)
error <- rnorm(size, 0, 0.5)

min.height <- min(height)
min.length <- min(length)

outcome <- 2 + (height - min.height) / 10  + (length - min.length) / 10 + genetic.factor - treatment * gender - 0.5 * treatment * (1 - gender) + error

fat.cats <- data.frame(name=names, color=cat.color, gender=gender,
                          height=height, lenght=length, genetics=genetic.factor,
                          treatment=treatment, outcome=outcome)

fat.cats.test <- fat.cats[sample(nrow(fat.cats), size.test),]
write.csv(fat.cats.test, file='11.1/fat_cats_test_set.csv')


