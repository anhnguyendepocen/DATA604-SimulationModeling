library(ggplot2)



r <- runif(2000000)

g1 <- ggplot(data=data.frame(r=r)) + geom_histogram(aes(x=r))
g1

acf(r)