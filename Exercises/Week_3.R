#####################################
### Empirical cdf for height data ###
#####################################
# Empirical cdf for sample of height data from Chapter 1
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
plot(ecdf(x), verticals = TRUE, main = "ecdf and cdf")

# 'True cdf' for normal distribution (with sample mean and variance)
xp <- seq(0.9*min(x), 1.1*max(x), length = 100) 
lines(xp, pnorm(xp, mean(x), sd(x)), col = 2, lw = 2) 


#####################################
###        Try an example         ###
#####################################
plot(-4:4, dnorm(-4:4), type="l")


#####################################
###        Try an example         ###
#####################################
punif(q=30, min=0, max=30)-punif(q=20, min=0, max=30)
1- punif(q=30, min=0, max=30)


#####################################
### Normal distribution:qunatiles ###
#####################################
qnorm(p=0.025, mean=500, sd=10)
qnorm(p=0.975, mean=500, sd=10)


#####################################
###    Exponential distribution   ###
#####################################
1-pexp(q=2, rate=1/2)
# Or with the Poisson distribution with the re-scaled lambda
lambda_2 = 1 
dpois(0, lambda_2)


#####################################
### Empirical pdf for height data ###
#####################################
# Histogram for sample of height data from Chapter 1
hist(x, nclass = 6, probability = TRUE, main = "histogram and pdf")

# 'True pdf' for normal distribution (with sample mean and variance)
xp <- seq(160,200,1)
lines(xp, dnorm(xp, mean(x), sd(x)), col = 'red', lw = 2)


####################################
### Standard Normal Distribution ###
####################################
# Draw a sample of 10000 observations from a standard normal distribution 
x <- rnorm(10000, mean = 0, sd = 1)
hist(x)

####################################
### Log-normal distribution      ###
####################################
# Draw a sample of 10000 observations from a log-normal distribution with alpha = 1 and beta = 1
y <- rlnorm(10000, meanlog = 1, sdlog = 1)

par(mfrow = c(1,2))

# Plot histogram of log-normal distributed data
hist(y, nclass = 20, probability = TRUE, 
     main = 'X ~ LN(1,1)', col = 'black', xlab = 'Y')
yp <- seq(0,100,0.1)
curve(expr = dlnorm(yp,1,1), xname = 'yp', 
      col = 'red', add = TRUE, lw = 2)

# Plot histogram of ln(log-normal) distributed data
alpha <- mean(log(y))
beta <- sd(log(y))
hist(log(y), nclass = 20, probability = TRUE, 
     main = 'ln(X) ~ N(1,1)', col = 'black', xlab = 'X')
xp <- seq(-2,4,0.1)
curve(expr = dnorm(xp,alpha,beta), xname = 'xp', 
      col = 'red', add = TRUE, lw = 2)



#################
### Exercises ###
#################


#2.9
#A
pnorm(2)
# Standard normal distribution X<=2
pnorm(2,1,1)
# Normal distribution X<=2
pnorm(2,1,2)
# Normal distribution X<=2 but with different mean and variance
curve(dnorm, xlim=c(-4,4))
xseq <- seq(-4, 2, len=1000)
polygon(x=c(xseq,2,xseq[1]),
        y=c(dnorm(xseq),0,dnorm(xseq[1])),
        col="pink")

#B
qnorm(pnorm(2))
# The command gives the 97% quantile of the standard normal distribution
# with X<=2. Note that are each other inverse so the result is 2

#C
qnorm(0.975)
# Is the 97.5% percentile of the standard normal distribution
qnorm(0.975,1,1)
# Is the 97.5% percentile of the normal distribution
qnorm(0.975,1,2)
# Is the 97.5% percentile of the normal distribution with different mean and variance
## Plot the standard normal distribution
curve(dnorm, xlim=c(-4,4))
## Add a vertical line at the 0.975 quantile
abline(v=qnorm(0.975))


#2.10
#A
#3

#B
mean = 24
variance = 4
pnorm(20, mean, variance)
1 - pnorm(29.5, mean, variance)
# A continuous random variable cannot have an outcome equal to a single
# value, so it is 0


#2.11
#A
mold = 65
mnew = 54
sdold = 16
sdnew = 9
pnorm(10, mold-mnew, sqrt(sdold+sdnew))
# Or transform to standard normal distribution
z <- (10-11)/5
z
pnorm(z)

#B
# Using the theorems
mean = 54*100
sd = sqrt(9*100)


#2.12
#A
pnorm(2993, mean=3000, sd=3)*2
# So around 2%

#B
# This is the probability we are looking for P(90<3000-2900<110)=0.99
# E(L-L_beam)=100 and V(L-L_beam)=9+lambda_beam^2
# 0.99=P((90-100)/sqrt(9+lambda_beam^2)<Z<(110-100)/sqrt(9+lambda_beam^2))
# 0.99=P(-z_0.005<Z<z_0.005)
qnorm(0.995)
# = 2.57
# 2.57=(110-100)/sqrt(9+lambda_beam^2) <--> lambda_beam=2.46 mm


#2.13
#A
lambda_365=110000
lambda_30=lambda_365/(365*24*2)

#B
lambda_15=lambda_365/(365*24*4)
dpois(0, lambda_15)
# Or with the exponential distribution
beta <- 365*24*60/lambda_365
1-pexp(15, rate=1/beta)


#Quiz 1
# First we find the relevant percentiles of the standard normal (Z) distribution:
# P(Z<z0.99)=0.01 and P(Z>z0.05)=0.05
qnorm(0.01)
qnorm(0.95)
# qnorm(0.95)=(650-mu)/sd and qnorm(0.01)=(600-mu)/sd
# to get mu=629.3 sd=12.6


#Quiz 2
mean = 43.5
sd= 2.6726
(1-pnorm(48, mean, sd))*100000
