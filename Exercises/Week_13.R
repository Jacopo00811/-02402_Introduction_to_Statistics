##
## Q1
##
x=c(45,47,48,49,49,50,52,52,53,54)
y=c(48,48,49,49,52,54,54,55,55)

k = 10000 
xsamples = replicate(k, sample (x, replace = TRUE))
ysamples = replicate(k, sample (y, replace = TRUE))
mymeandifs = apply(xsamples, 2, mean)-apply(ysamples, 2, mean) 
myquantiles=quantile(mymeandifs, c(0.005,0.01,0.025,0.05,0.25,
                                   0.5,0.75,0.95,0.975,0.99,0.995))
round(myquantiles,2)
# 1% needs to be divededleft and right so the margins for the CI are 0.5% 
# and 99.5%

##
## Q2
##
# Read the table and find the multiple R-squared adnr the residual standard
# error

##
## Q3
##
n <- 6
slope <- 0.128031
t_percentile <- qt(0.975, n-2)
Rss <- 0.0005709
# And you have to devide by the Sxx 
Sxx <- 0.005767
slope+c(-1,1)*t_percentile*sqrt(Rss^2/Sxx)
# So the expected B1=0.155 doesn't fit in the CI

##
## Q4
##
# The CI for the line in the point x=0.2 are found as follows:
mean = 0.2288
Sxx <- 0.005767
n <- 6
x <- 0.2
B0 <- 0.025036
B1 <- 0.128031
Rss <- 0.0005709
B0+B1*x+c(-1,1)*qt(0.975,n-2)*Rss*sqrt(1/n+(x-mean)^2/Sxx)

##
## Q5
##
# These two are the same but apprently the second one is considered right to indicate
# the prop. that 18>= people have stopped smoking
dbinom(18,20,0.80)
dbinom(2,20,0.20)

##
## Q6
##
# The prop. is 0.8. So if you pick two or more people it decrases following: 
# 0.8^n now you need to find the value of n that make this go under 50% aka 0.5
0.8^4
# Thus we need to ask 4 people.

##
## Q7
##
# Since they are independet we can use the two sample test and find the t__obs for
# it. 
x1 <- 132.86
x2 <- 127.44
sd1 <- 15.34
sd2 <- 18.23
n1 <- 8
n2 <- 21
(x1-x2)/sqrt(sd1^2/n1+sd2^2/n2)

##
## Q8
##
# We want to find out if the cph edition is way different 
# from the statistic of some marathon. The p value is given by
# 1-(the propability of the chisq distribution, with df (c-1)*(r-1))
# df = (5-1)*(3-1) = 4*2 = 8
1-pchisq(79.25, 8)

##
## Q9
##
# 1
##

##
## Q12
##
# This time some of the columns are collapsed so it is
# X^2 0.95(4)

##
## Q10
##
# The columns are 3, so k=3 (df is (k-1)). The cells are N=15 so 
# the df for the error is (N-k)=12

##
## Q11
##
rho <- sqrt(0.9864)
rho

##
## Q12
##
# Since both the P-values given in the output are very
# small the correct answer is 5.

##
## Q13
##
# From the graph we notice that roughly 0 is in the middle so
# the difference is not significant