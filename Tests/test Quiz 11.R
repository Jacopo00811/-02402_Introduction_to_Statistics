#Q1
n <- 20
k <- 5
l <- 4
x <- k-1 
y <- n-k
z <- n-1
SStot <- 2487.20
SSerr <- 143.50
w <- SStot -SSerr
w


#Q2
tobs <- 61.2465
1-pf(tobs,x,y)


#Q3
muA <- 38.75
muB <- 36.25
nij <- 4
muA-muB+c(-1, 1)*qt(0.995,n-k)*sqrt(SSerr/(n-k)*(1/nij+1/nij))
qt(0.995,n-k)*sqrt(SSerr/(n-k)*(1/nij+1/nij))


#Q4
n <- 5
B1 <- 34.544
stderr <- 3.419
B1+c(-1, 1)*qt(0.975,n-2)*stderr


#Q5
# Look at the formula and find the only one with the right values
# and sqrt(1+....)


#Q6
# Considering the matched test (two products same test group)
# we need to use the value of n=10. So with alpha=0.01 we have
n <- 10
qt(0.995,n-1)


