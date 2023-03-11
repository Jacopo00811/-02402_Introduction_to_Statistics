#Q1
mu1 <- 122.4
mu2 <- 145.9
n1 <- 15
n2 <- 10
s1 <- 30.5
s2 <- 22.3
# tobs
(mu1-mu2)/sqrt(s1^2/n1+s2^2/n2)
#v
v <-(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
#CIs
mu1-mu2+qt(p=0.975, v)*sqrt(s1^2/n1+s2^2/n2)
mu1-mu2-qt(p=0.975, v)*sqrt(s1^2/n1+s2^2/n2)


#Q2
b <- c(155,164,177,167,156,198,155,187,185,177,150,145,189,176)
a <- c(165,154,172,156,150,185,145,182,165,166,145,150,169,172)
mean(a)
mean(b)
t.test(b, a, paired = TRUE)


#Q3
mu <- 309
var <- 132
n <-10
mu+qt(0.995,n-1)*sqrt(var/n)
mu-qt(0.995,n-1)*sqrt(var/n)


#Q4
#n
stdbike <- sqrt(132)
ME <- 4
qnorm(0.975)
# To find z__(1-alpha/2)
qnorm(0.975)
((1.96*stdbike)/ME)^2


#Q6
# Rate = 12 for 3 hrs
# Probability of having <= 9 in 3 hrs
ppois(9,12)


#Q7
sigma <- 0.43
ME <- 0.1
qnorm(0.975)
# To find z__(1-alpha/2)
((1.96*sigma)/ME)^2


#Quiz question
# alpha = 0.01
qt(0.995,9)
