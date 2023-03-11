#### Q1 ####
pbinom(2,200,0.004)

#### Q2 ####
M1<-c(82.5, 83.7, 80.9, 95.2, 80.8)
median(M1)
quantile(M1, probs = c(.75), type = 2)

#### Q3 ####
# P(X = 0) = lambda^(0)/0!*exp(-lambda) = 0.3
# lambda = 1.2
lambda <- 1.2
1 - ppois(7,lambda*4)

#### Q4 ####
n <- 2
a <- 3
N <- 9
# Mean
n*a/N
# Variance
(n*a*(N-a)*(N-n))/(N**2*(N-1))

#### Q5 ####
# All of them contain reports
4/9*3/8*2/7
# One of each type
(choose(2,1)*choose(3,1)*choose(4,1))/(choose(9,3))

#### Q6 ####
lambda <- 3
1 - ppois(5,lambda)

