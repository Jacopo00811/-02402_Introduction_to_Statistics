#3.1
mu <- 3000
n <- 9
x <- c(3003, 3005, 2997, 3006, 2999, 2998, 3007, 3005, 3001)

#A
mean(x)
sd(x)
sigma <- sd(x) 
sigma/sqrt(n) #Std error on the mean 

#B
mu<-15000
sigma<-5*3**2
sigma
2*(1-pnorm(15010,mu,sqrt(sigma)))

#C
qt(0.975, df = n-1) # 95%
t.test(x)

#D
qt(0.995, df = n-1) # 99%
t.test(x, conf.level=0.99)

#E
qchisq(c(0.975,0.025), n-1) # Denominators
# 95% confidence interval for variance
c((n-1)*sd(x)**2/qchisq(0.975, n-1), (n-1)*sd(x)**2/qchisq(0.025, n-1)) # Variance
# 95% confidence interval for sdt deviation
sqrt(c((n-1)*sd(x)**2/qchisq(0.975, n-1), (n-1)*sd(x)**2/qchisq(0.025, n-1))) # Std deviation

#F
c((n-1)*sd(x)**2/qchisq(0.995, n-1), (n-1)*sd(x)**2/qchisq(0.005, n-1)) # Variance
sqrt(c((n-1)*sd(x)**2/qchisq(0.995, n-1), (n-1)*sd(x)**2/qchisq(0.005, n-1))) # Std deviation


#3.2
# Same formulas just with different vector x