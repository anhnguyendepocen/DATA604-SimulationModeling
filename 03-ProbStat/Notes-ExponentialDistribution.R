library(ggplot2)

# Random sample of 100,000 values from Exp Distribution with rate 1000
d <- data.frame(d=rexp(100000, rate=1/1000))
# Visualize it
g1 <- ggplot(d) + geom_histogram(aes(x=d))
g1
# Summary statistics
summary(d)

sd(d$d)