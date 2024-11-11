library(dplyr)
library(ggplot2)

pbinom(6, size=20, prob=0.3) 

dbinom(3, size = 13, prob = 1/6)

probabilities = dbinom(x = c(0:20), size = 20, prob = 1/6)
p <- data.frame(p = probabilities)
#Densidade sobreviventes por idade e classe

p %>%
  ggplot(aes(x = p)) +
  geom_density(alpha=0.2)


d <- dbinom(success, size=50, prob=1/6)
mean(d)

success <- 0:50
format(dbinom(success, size=50, prob=1/6), scientific = F)
plot(success,dbinom(success,size=50,prob=1/6),
     type='h',
     main='Binomial Distribution (n=20, p=0.3)',
     ylab='Probability',
     xlab ='# Successes',
     lwd=3)



