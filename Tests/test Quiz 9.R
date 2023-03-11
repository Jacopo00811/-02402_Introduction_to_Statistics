#Q1
# CIs
n <- 14
qt(0.975, n-1)
139.21 - qt(0.975, n-1)*4.58/sqrt(n)
139.21 + qt(0.975, n-1)*4.58/sqrt(n)


#Q2
x1 <- c(314, 340, 331, 333, 329, 322, 332, 330, 338, 325)
x2 <- c(294, 317, 317, 310, 327, 300, 293, 321, 307, 304)
var(x1)
var(x2)
t.test(x1,x2)


#Q3 
# Read stupid


#Q4
# Both


#Q5 
# Read the value of residual std error


#Q6
B1 <- 2.6886
sigma1 <- 0.3736
n <- 15
qt(0.975, n-2-1)
B1+c(-1, 1)*qt(0.975, n-2-1)*sigma1
# Note that in the case of MLR do degrees of freedom -2 -1


#Q7
x <- c(9, 18, 19, 21, 25, 25, 21, 19, 16, 7)
median(x)


#Q8
var(x)

#Q9
# To solve this problem we need only look at the definition of significance level,
# which is the probability of committing a type I error, cf. definitions in Chapter 3.



