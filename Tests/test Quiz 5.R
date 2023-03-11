#### Q1 ####

# Enter data
x <- c(44.9, 44.2, 44.6, 44.8, 44.0, 45.1)
n <- length(x) # sample size

# Compute 'tobs' - the observed test statistic
tobs <- (mean(x) - 45) / (sd(x) / sqrt(n))

# Compute the p-value as a tail-probability 
# in the relevant t-distribution:
2 * (1 - pt(abs(tobs), df = n-1))

# Or using the t-test
t.test(x, mu=45)
# To change the confidence interval do
# t.test(x, mu= , c=)


#### Q2 ####
(0.75**14)*(0.25**6)*(choose(20, 6))
# Or with the binomial function
dbinom(6,20,0.25)


#### Q3 ####
x<-c(rep(2, times = 10), rep(4, times = 25), rep(7, times = 30), rep(10, times = 25), rep(12, times = 10))
var(x)
# Or 
d<-c(rep(c(2,4,7,10,12), c(10,25,30,25,10)))
var(d)
# Or 
e<-c(rep(2:12, c(10,0,25,0,0,30,0,0,25,0,10)))
var(e)


#### Q4 ####
# Enter data
m<-1.363
sd<-1.521
n <- 48 # sample size

# Compute 'tobs' - the observed test statistic
(m - 0) / (sd / sqrt(n))
tobs<-(m - 0) / (sd / sqrt(n))
# Compute t(1-alpha/2)
qt(0.975, df = n-1) # 95%

# Compute the p-value as a tail-probability 
# in the relevant t-distribution:
2 * (1 - pt(abs(tobs), df = n-1))


#### Q5 ####
# 95% confidence interval for sdt deviation
sqrt(c((n-1)*sd**2/qchisq(0.975, n-1), (n-1)*sd**2/qchisq(0.025, n-1))) # Std deviation
qchisq(c(0.025,0.975), n-1) # For the denominators
