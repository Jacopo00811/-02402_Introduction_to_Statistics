setwd("C:/Users/jacop/Desktop/DTU/IntroductionStatistics/bmi2")

# Read the dataset 'bmi2_data.csv' into R
D <- read.table("bmi2_data.csv", header = TRUE, sep = ";")
D

# Add log-BMI to the data-set
D$logbmi <- log(D$bmi)
D


########################## Descriptive analysis and summary ##########################
# Scatter plots
plot(D$age, D$logbmi, main = "Scatterplot of Log-BMI vs. Age", xlab = "Age", ylab = "Log-BMI")

plot(D$fastfood, D$logbmi, main = "Scatterplot Log-BMI vs. Fast food c.", xlab = "Fast food cons.", ylab = "Log-BMI")

# Box plots
boxplot(D$logbmi, col = "dark green", main = "Boxplot of Log-BMI", ylab = "Log-BMI")
text(1.35, quantile(c(D$logbmi)), c("Minimum", "Q1", "Median", "Q3", "Maximum"), col = "black")

boxplot(D$age, col = "cyan", main = "Boxplot of Age", ylab = "Age")
text(1.35, quantile(c(D$age)), c("Minimum", "Q1", "Median", "Q3", "Maximum"), col = "black")

boxplot(D$fastfood, col = "yellow", main = "Boxplot of fast food consumption", ylab = "Fast food consumption")
text(1.35, quantile(c(D$age)), c("Minimum", "Q1", "Median", "Q3", "Maximum"), col = "black")

# Histograms
hist(D$logbmi, breaks = 25, xlab="Log-BMI", xlim = range(2.8:3.8), prob=TRUE, col = "dark green", main = "Density histogram of Log-BMI")

hist(D$age, breaks = 25, xlab="Age", xlim = range(10:80), prob=TRUE, col = "cyan", main = "Density histogram of Age")

hist(D$fastfood, breaks = 25, xlab="Fast food consumption", xlim = range(0:400), prob=TRUE, col = "yellow", main = "Density histogram Fast food cons.")

# Table construction
## Selected summary statistics
summary(D)
## Another type of summary of the data-set
str(D)

# Standard deviations
sd(D$bmi)
sd(D$logbmi)
sd(D$age)
sd(D$fastfood)
 
# Means
mean(D$bmi)
mean(D$age)
mean(D$logbmi)
mean(D$fastfood)

# Quantile
quantile(D$bmi)
quantile(D$age)
quantile(D$logbmi)
quantile(D$fastfood)


############################################################################################################################################################


# Subset containing the first 840 observations (for model estimation)
D_model <- subset(D, id <= 840)

# Subset containing the last 7 observations (for validation)
D_test <- subset(D, id >= 841)

############################################################################################################################################################

# Estimate multiple linear regression model
fit <- lm(logbmi ~ age + fastfood, data = D_model)

# Show parameter estimates etc.
summary(fit)



# Plots for model validation
par(mfrow=c(2,3))

plot(fit$fitted.values, D_model$logbmi, xlab = "Fitted values", ylab = "Log-BMI", main = "Observations vs. fitted values")
plot(D_model$logbmi, fit$residuals, xlab = "Log-BMI", ylab = "Residuals", main = "Residuals vs. Log-BMI")
plot(D_model$age, fit$residuals, xlab = "Age", ylab = "Residuals", main = "Residuals vs. age")
plot(D_model$fastfood, fit$residuals, xlab = "Fastfood consumption", ylab = "Residuals", main = "Residuals vs. fastfood cons.")
plot(fit$fitted.values, fit$residuals, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs. fitted values")

qqnorm(fit$residuals, ylab = "Residuales", xlab = "Z-scores", main = "Normal QQ-plot of residuals")
qqline(fit$residuals)

# Wally plot
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}

wallyplot(fit$residuals-mean(fit$residuals), FUN=qqwrap, ylim=c(-3,3))



#Calculation of t_(1-α/2) with 837 degrees of freedom (task c)
B1 <- 0.0024
SigmaB1 <- 0.0004
qt(0.975, 837)
B1+c(-1, 1)*qt(0.975, 837)*SigmaB1
# Confidence intervals for the model coefficients
confint(fit, level = 0.95)

B0 <- 3.1124298
B2 <- 0.0005404
SigmaB0 <- 0.0193517
SigmaB2 <- 0.0001732
B0+c(-1, 1)*qt(0.975, 837)*SigmaB0
B2+c(-1, 1)*qt(0.975, 837)*SigmaB2

# P-value
2*(1-pt(3.5, df=837))

fit <- lm(logbmi ~ age + fastfood, data = D_model)
summary(fit)

fit2 <- lm(logbmi ~ age, data = D_model)
summary(fit2)

# Predictions and 95% prediction intervals
pred <- predict(fit, newdata = D_test, 
                interval = "prediction", level = 0.95)
 
# Observed values and predictions
cbind(id = D_test$id, logbmi = D_test$logbmi, pred)

