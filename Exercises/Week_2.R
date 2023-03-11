#####################
### A random draw ###
#####################
sample(1:6, size=1)


##################################
### Empirical density function ###
### 'Fair dice' example        ###
##################################
# Set random seed ensuring that the random outcome is reproducible
set.seed(2)

# Number of simulated realizations (sample size)
n <- 30

# n independent random draws from the set (1,2,3,4,5,6) 
# with equal probability of each outcome
xFair <- sample(1:6, size = n, replace = TRUE)
xFair

# Count number of each outcome using the 'table' function
table(xFair)

# Plot the empirical pdf
plot(table(xFair)/n, lwd = 10, ylim = c(0,1), xlab = "x", 
     ylab = "Density f(x)")
# Add the true pdf to the plot
lines(rep(1/6,6), lwd = 4, type = "h", col = 2)
# Add a legend to the plot
legend("topright", c("Empirical pdf","True pdf"), lty = 1, col = c(1,2), 
       lwd = c(5, 2), cex = 0.8)


##################################
### Empirical density function ###
### 'Unfair dice' example      ###
##################################
# Set random seed ensuring that the random outcome is reproducible
set.seed(3)

# Number of simulated realizations (sample size)
n <- 30

# n independent random draws from the set (1,2,3,4,5,6) 
# with higher probability of getting a six
xUnfair <- sample(1:6, size = n, replace = TRUE, prob = c(rep(1/7,5),2/7))
xUnfair

table(xUnfair)

# Plot the empirical pdf
plot(table(xUnfair)/n, lwd = 10, ylim = c(0,1), xlab = "x", 
     ylab = "Density f(x)")
# Add the true pdf to the plot
lines(c(rep(1/7,5),2/7), lwd = 4, type = "h", col = 2)
# Add a legend to the plot
legend("topright", c("Empirical pdf","True pdf"), lty = 1, col = c(1,2), 
       lwd = c(5, 2), cex = 0.8)


###############################################
### Simulating from a binomial distribution ###
###############################################
# Set random seed ensuring that the random outcome is reproducible
set.seed(4)

## Probability of success
p <- 0.1

## Number of repetitions
nRepeat <- 30

## Simulate Bernoulli experiment 'nRepeat' times
tmp <- sample(c(0,1), size = nRepeat, prob = c(1-p,p), replace = TRUE)

# Compute 'x'
sum(tmp)

## Or: Use the binomial distribution simulation function 
rbinom(1, size = 30, prob = p)


#####################################################
### Simulating from a hypergeometric distribution ###
#####################################################
# The probability that at least one of them has scratches
n <- 3
N <- 10
a <- 2
x <- 0
1 - choose(a,x)*choose(N-a,n-x)/choose(N,n)
1 - dhyper(x = x, m = a, n = N-a, k = n)


############################################
###Simulating from a Poisson distribution###
############################################
# Probability of getting at most two patients per day
ppois(2, 0.3)

# Probability of getting at most 1 patient in three days
# You can simply scale lambda to 0.9 patients / 3 days
ppois(1, 0.9)


###########################
### Sample mean         ###
### 'Fair dice' example ###
###########################
# Set random seed ensuring that random outcome is reproducible
set.seed(6)

# Number of simulated realizations (sample size)
n <- 30

# Sample independently from the set (1,2,3,4,5,6)
# with equal probability of outcomes
xFair <- sample(1:6, size = n, replace = TRUE)

# Compute the sample mean
mean(xFair)









#################
### Exercises ###
#################


#2.1
#A
# Binomial

#B
# P(X <= 5)
pbinom(5,10,0.6)
# P(X < 5)
pbinom(4,10,0.6)
# P(X > 4)
1-pbinom(4,10,0.6)
# P(X = 5)
dbinom(5,10,0.6) #or
pbinom(5,10,0.6) - pbinom(4,10,0.6)

#C
# Poisson distribution and the values is the probability of getting 4
#events per interval when the average is 3

#D
# P(X <= 5)
ppois(5,3)
# P(X < 5)
ppois(4,3)
# P(X > 4)
1-ppois(4,3)
# P(X = 5)
dpois(5,3) #or
ppois(5,3) - ppois(4,3)


#2.4
dhyper(0,3,20-3,6) #or
choose(3,0)*choose(20-3,6-0)/choose(20,6)


#2.5
#A
# Mean
n <- 2
a <- 3
N <- 20
n*(a/N)
# Variance
(n*a*(N-a)*(N-n))/(N**2*(N-1))
# P(X >= 1)
1 - dhyper(0, a,N-a,n) #or
1 - choose(a,x)*choose(N-a,n-x)/choose(N,n)


#2.7
#A
1 - ppois(5,1.6)

#B
ppois(8,1.6*5)


#2.8
#A
1 - ppois(19,180/60*5)

#B
ppois(20:30,15)

#Quiz 2
n <- 6
a <- 3
N <- 20
# Mean
n*a/N
# Variance
(n*a*(N-a)*(N-n))/(N**2*(N-1))



