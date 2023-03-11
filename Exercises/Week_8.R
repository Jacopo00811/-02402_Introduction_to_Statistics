#################################
### Linear regression example ###
### with simulated data 1     ###
#################################

# Set seed (so that simulations may be redone)  
set.seed(100)

# Number of data points
n <- 20

# Intercept, slope, and std. deviation for simulations 
beta0 <- 50
beta1 <- 200
sigma <- 90

# Simulated data points
x <- runif(n, -2, 4)
y <- beta0 + beta1 * x + rnorm(n, mean = 0, sd = sigma)

# Scatter plot of x and y
plot(x, y)

# Add 'true' line to the plot
lines(x, beta0 + beta1*x, col = 2)

#################################
### Linear regression example ###
### with simulated data 2     ###
#################################

# Set seed (so that simulations may be redone)  
set.seed(100)

# Generate x
x <- runif(n = 20, min = -2, max = 4)

# Simulate y
beta0 <- 50; beta1 <- 200; sigma <- 90
y <- beta0 + beta1 * x + rnorm(n = length(x), mean = 0, sd = sigma)

# From here: like for the analysis of 'real data', we have data in x and y:

# Scatter plot of y against x
plot(x, y)

# Find the least squares estimates, use Theorem 5.4
(beta1hat <- sum( (y - mean(y))*(x-mean(x)) ) / sum( (x-mean(x))^2 ))
(bet0hat <- mean(y) - beta1hat*mean(x))

# Use lm() to find the estimates
lm(y ~ x)

# Plot the fitted  line
abline(lm(y ~ x), col="red")

################################################
### Distribution of estimators of regression ###
### coefficients by simulation               ###
################################################

# Number of repetitions
nRepeat <- 1000

# Two vectors to save the estimates in
Beta0Hat <- numeric(nRepeat)
Beta1Hat <- numeric(nRepeat)

# Repeat the  simulation and estimation nRepeat times
for(i in 1:nRepeat){
  # Generate x
  x <- runif(n = 20, min = -2, max = 4)
  # Simulate from the linear regression model
  beta0 = 50; beta1 = 200; sigma = 90
  y <- beta0 + beta1 * x + rnorm(n = length(x), mean = 0, sd = sigma)
  # Use lm() to find the estimates
  fit <- lm(y ~ x)
  # Save the estimates
  Beta0Hat[i] <- fit$coefficients[1]
  Beta1Hat[i] <- fit$coefficients[2]
}

# See empirical distributions of the estimates
hist(Beta0Hat, probability = TRUE)
hist(Beta1Hat, probability = TRUE)

###########################################
### Linear regression: Hypothesis tests ###
### Example: Height-Weight data         ###
###########################################

# Read data into R

x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Fit model to data
fit <- lm(y ~ x)

# Look at model summary to find Tobs-values and p-values
summary(fit)

##################################################
### Example: Illustration of CIs by simulation ###
##################################################

# Number of repetitions (here: CIs)
nRepeat <- 1000

# Empty logical vector of length nRepeat
TrueValInCI <- logical(nRepeat)

# Repeat the simulation and estimation nRepeat times:
for(i in 1:nRepeat){
  # Generate x
  x <- runif(n = 20, min = -2, max = 4)
  # Simulate y
  beta0 = 50; beta1 = 200; sigma = 90
  y <- beta0 + beta1 * x + rnorm(n = length(x), mean = 0, sd = sigma)
  # Use lm() to fit model
  fit <- lm(y ~ x)
  # Use confint() to compute 95% CI for intercept
  ci <- confint(fit, "(Intercept)", level=0.95)
  # Was the 'true' intercept included in the interval? (covered)
  (TrueValInCI[i] <-  ci[1] < beta0  &  beta0 < ci[2])
}

# How often was the true intercept included in the CI?
sum(TrueValInCI) / nRepeat

##################################################
### Example: Confidence intervals for the line ###
##################################################

# Generate x
x <- runif(n = 20, min = -2, max = 4)

# Simulate y
beta0 = 50; beta1 = 200; sigma = 90
y <- beta0 + beta1 * x + rnorm(n = length(x), sd = sigma)

# Use lm() to fit model
fit <- lm(y ~ x)

# Make a sequence of 100 x-values
xval <- seq(from = -2, to = 6, length.out = 100)

# Use the  predict function
CI <- predict(fit, newdata = data.frame(x = xval),
              interval = "confidence",
              level = 0.95)

# Check what we got
head(CI)

# Plot the data, model fit and intervals
plot(x, y, pch = 20)
abline(fit)
lines(xval, CI[, "lwr"], lty=2, col = "red", lwd = 2)
lines(xval, CI[, "upr"], lty=2, col = "red", lwd = 2)

##################################################
### Example: Prediction intervals for the line ###
##################################################

# Generate x
x <- runif(n = 20, min = -2, max = 4)

# Simulate y
beta0 = 50; beta1 = 200; sigma = 90
y <- beta0 + beta1 * x + rnorm(n = length(x), sd = sigma)

# Use lm() to fit model
fit <- lm(y ~ x)

# Make a sequence of 100 x-values
xval <- seq(from = -2, to = 6, length.out = 100)

# Use the  predict function
PI <- predict(fit, newdata = data.frame(x = xval),
              interval = "prediction",
              level = 0.95)

# Check what we got
head(CI)

# Plot the data, model fit and intervals
plot(x, y, pch = 20)
abline(fit)
lines(xval, PI[, "lwr"], lty = 2, col = "blue", lwd = 2)
lines(xval, PI[, "upr"], lty = 2, col = "blue", lwd = 2)

##############################################
### Linear regression: Correlation and R^2 ###
### Example: Height-Weight data            ###
##############################################

# Read data into R

x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Fit model to data
fit <- lm(y ~ x)

# Scatter plot of data with fitted line
plot(x,y, xlab = "Height", ylab = "Weight")
abline(fit, col="red")  

# See summary
summary(fit)

# Correlation between  x and y
cor(x,y)

# Squared correlation is the "Multiple R-squared" from summary(fit)
cor(x,y)^2

###########################################
### Linear regression: Model validation ###
### Example: Height-Weight data         ###
###########################################

# Read data into R
x <- c(168, 161, 167, 179, 184, 166, 198, 187, 191, 179)
y <- c(65.5, 58.3, 68.1, 85.7, 80.5, 63.4, 102.6, 91.4, 86.7, 78.9)

# Fit model to data
fit <- lm(y ~ x)

# QQ-plot of residuals
qqnorm(fit$residuals, main = "") # or "Wally plot" of residuals

# Plots of residuals against fitted values
plot(fit$fitted, fit$residuals)





#5.1
#A
#Answer 3 (B0=252, B1=-0.9, sigma=3.6)

#B
#Answer 5, the proportion of explained variation is 96% and the correlation is -0.98


#5.2
#A
t <- c(seq(10, 90, by=10))
y <- c(420, 365, 285, 220, 176, 117, 69, 34, 5)
fit <- lm(y ~ t)
summary(fit)
n <- 9
# The std. error (sigma) for the slope can be read form here and it is 0.2558
# The t__(1-alpha/2) for the 95% is
qt(0.975, n-2)
# Evaluate the final result for the CI of the slope
-5.31+c(-1, 1)*qt(0.975,n-2)*0.2558

#B (Relation on level 5%)
# Since the confidence interval does not include 0, it can be documented that there
# is a relationship between life time and temperature, also the p-value is way smaller than alpha=0.05.
# Strong evidence against the null hypothesis.


#5.3
#A
# Test the null hypothesis H0: B1=0)
x <- seq(0, 100, by=25)
y <- c(14, 38, 54, 76, 95)
n <- 5
fit <- lm (y ~ x)
summary(fit)
meanx <- 50
sx <- 39.52847
meany <- 55.4
sy <- 31.66702
# Yes, there is a significant relationship since the p values is << alpha and the tobs is out of the
# optimal range 32.7

#B
B0 <- 15.4
B1 <- 0.8
xnew <- 80
sigma <- 1.932 # Residual standard error
# Since sx^2 = 1/(n-1)*sum((xi-meanx)^2,n=1..n) = 1/(n-1)*Sxx 
# Isolating Sxx we get
Sxx <- (n-1)*sx**2
(B0+B1*xnew)+c(-1, 1)*qt(0.975, n-2)*sigma*sqrt(1/n+(xnew-meanx)^2/Sxx)
# Or use the predict function
predict(fit, newdata=data.frame(x=80), interval="confidence", level=0.95)

#C
residuals <- c(-1.4,  2.6, -1.4,  0.6, -0.4)
# The upper quantile
p <- 0.75
n*p
# Take the 4th residual 
# The answer is 0.6


#5.4
#A
x <- c(15, 25, 35, 40)
y <- c(42.1, 36.0, 31.8, 28.7)
n <- 4
meanx <- 28.75 
meany <- 34.65
Sxx <- 368.75
fit <- lm (y ~ x)
summary(fit)
# From here we read
slope <- -0.52136
sigmaB1 <- 0.02898
t <- qt(0.975, n-2)
slope+c(-1, 1)*t*sigmaB1

#B
# Yes, we can say that there is a relation between the impact strength and the cooling 
# time at significance level alpha=5% because the p-value is smaller than alpha=0.05

#C
qt(0.975,n-2)


#5.5
#A
x <- seq(2, 10, by=2)
y <- c(11.5, 10.2, 10.3, 9.68, 9.32)
n <- 5
fit <- lm (y ~ x)
summary(fit)
# From here we read
B0 <- 11.664
B1 <- -0.244
sigma <- 0.348 

#B
# The amount of variation in the model output (Y) explained by the variable input
# (x) can be found from the squared correlation, that can be read off directly from the
# output as "Multiple R-squared"
R2 <- 0.868

#C
# Do like in 5.3 part B or use the function
predict(fit, newdata=data.frame(x=7), interval="confidence", level=0.95)


#5.6
#A
D <- data.frame(
pressure=c(1.02,2.08,2.89,4.01,5.32,5.83,7.26,7.96,9.11,9.99),
flux=c(1.15,0.85,1.56,1.72,4.32,5.07,5.00,5.31,6.17,7.04))
n <- 10
fit <- lm(flux ~ pressure, data=D)
summary(fit)
# From here we read
R2 <- 0.9289
# The sign of the correlation is the same as the sign of the slope, which can be read off to be positive so
R <- sqrt(R2)
R

#B
B1 <- 0.72248
sigmaB1 <- 0.07064
B1+c(-1, 1)*qt(0.95, n-2)*sigmaB1

#C
# If we know the explained variation (R^2) the unexplained express by the model is found
1-R2

#D
# The hypothesis is H0: B0=0
# From the summary we can read 
pvalB0 <- 0.681
pvalB0 
# Which is grater than alpha=0.05; so we can't reject the hypothesis that the line passes through (0, 0)

#E
# (5.0 − 5.547)^2 < (3.5 − 5.547)^2 < (9.5 − 5.547)^2



