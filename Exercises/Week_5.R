#######################################
### 'Manual' one-sample t-test in R ###
#######################################

# Enter data
x <- c(1.2, 2.4, 1.3, 1.3, 0.9, 1.0, 1.8, 0.8, 4.6, 1.4) 
n <- length(x) # sample size

# Compute 'tobs' - the observed test statistic
tobs <- (mean(x) - 0) / (sd(x) / sqrt(n))

# Compute the p-value as a tail-probability 
# in the relevant t-distribution:
2 * (1 - pt(abs(tobs), df = n-1))


####################################
### QQ Plot of student heights   ###
####################################
require(MESS)
# Normal q-q plot of student heights
qqnorm(x)
qqline(x)

# Compare with normal distributed data to get a feeling
make_qq_norm <- function(n)
{
  x <- rnorm(n)
  qqnorm(x)
  qqline(x)
}

make_qq_norm(10)

# Use WallyPlot function for similar purpose
library(MESS)
qqwrap <- function(x, y, ...)
{
  stdy <- (y-mean(y)) / sd(y)
  qqnorm(stdy, main = "")
  qqline(stdy)
}

x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
wallyplot(x-mean(x), FUN = qqwrap, ylim=c(-3,3))


###########################
### Example: Radon data ###
###########################

## Reading in the data
radon <- c(2.4, 4.2, 1.8, 2.5, 5.4, 2.2, 4.0, 1.1, 1.5, 5.4, 6.3, 
           1.9, 1.7, 1.1, 6.6, 3.1, 2.3, 1.4, 2.9, 2.9)

## Histogram and q-q plot of data
par(mfrow = c(1,2))
hist(radon)
qqnorm(radon)
qqline(radon)

# Transform data using the natural logarithm
logRadon<-log(radon)

## Histogram and q-q plot of transformed data
par(mfrow = c(1,2))
hist(logRadon)
qqnorm(logRadon)
qqline(logRadon)








###############################################
################## EXERCISES ##################
###############################################


# 3.3
#A
# alpha = 5%, evidence against the null hypothesis
x <- c(3003,3005,2997,3006,2999,2998,3007,3005,3001)
t.test(x, mu=3000)
# t(obs) = 1.885 ~ 1.9 
# p = 0.1

#B
# alpha = 0.01 critical values
qt(p=0.995, df=8)

#C
# alpha = 0.05 critical values
qt(p=0.975, df=8)

#D
# Investigation with plots
x=c(3003,3005,2997,3006,2999,2998,3007,3005,3001)
hist(x, freq=F, col = 4)
xp <- seq(2996, 3008, 0.1)
lines(xp, dnorm(xp, mean(x), sd(x)), lwd = 2)

plot(ecdf(x), verticals = TRUE)
xp <- seq(0.9*min(x), 1.1*max(x), length.out = 100)
lines(xp, pnorm(xp, mean(x), sd(x)))

qqnorm(x)
qqline(x)

# The nine data points do not differ from the line than what truly normally
# distributed samples of size n = 9 do, so we cannot falsify the normality 
# assumption

#E
# The y-coordinates of the nine points from left to right in the plot are 
# the ordered observations

# The x-coordinates are quantiles from the standard normal distribution


# 3.4
#A 
# evidence against the null hypothesis
x <- c(180.02, 180.00, 180.01, 179.97, 179.92, 180.05, 179.94, 180.10,
       180.24, 180.12, 180.13, 180.22, 179.96, 180.10, 179.96, 180.06)
t.test(x, mu=180)

# There is a weak evidence against the hypothesis

#B
# alpha = 0.01 critical values
qt(p=0.995, df=15)

#C
t.test(x, c=0.995)

#D
# alpha = 0.05 critical values
# We already found the p-value = 0.055 above, and as this is larger than alpha
# we cannot reject the null hypothesis of µ = 180


