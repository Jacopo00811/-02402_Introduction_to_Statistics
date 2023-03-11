################################
## Se info about  data
?airquality

require(graphics)
pairs(airquality, panel = panel.smooth, main = "airquality data")

## Copy the data
Air <- airquality
## Remove rows with at least one NA value
Air <- na.omit(Air)

## Remove one outlier
Air <- Air[-which(Air$Ozone == 1), ]

## Check  the empirical density
hist(Air$Ozone, probability=TRUE, xlab="Ozon", main="")

## Concentrations are positive and very skewed, let's  
## log-transform right away: 
## (although really one could wait and check residuals from models)
Air$logOzone <- log(Air$Ozone)
## Bedre epdf?
hist(Air$logOzone, probability=TRUE, xlab="log Ozone", main="")

## Make a time variable (R timeclass, se ?POSIXct)
Air$t <- ISOdate(1973, Air$Month, Air$Day)
## Keep only some of the columns
Air <- Air[ ,c(7,4,3,2,8)]
## New names of the columns
names(Air) <- c("logOzone","temperature","wind","radiation","t")

## What's in Air?
str(Air)
Air
head(Air)
tail(Air)

## Typically one would  begin with a pairs plot
pairs(Air, panel = panel.smooth, main = "airquality data")


################################

## See the relation between ozone and temperature
plot(Air$temperature, Air$logOzone, xlab="Temperature", ylab="logOzone")

## Correlation
cor(Air$temperature, Air$logOzone)

## Fit a simple linear regression model
summary(lm(logOzone ~ temperature, data=Air))

## Add a vector with random values, is there a significant linear relation?
## ONLY for ILLUSTRATION purposes
Air$noise <- rnorm(nrow(Air))
plot(Air$noise, Air$temperature, xlab="Temp", ylab="Noise")
cor(Air$temperature, Air$noise)
summary(lm(noise ~ temperature, data=Air))


################################
## With each of the other two independent variables

## Simple linear regression model with the wind speed
plot(Air$wind, Air$logOzone, ylab="logOzone", xlab="Wind speed")
cor(Air$wind, Air$logOzone)
summary(lm(logOzone ~ wind, data=Air))

## Simple linear regression model with the radiation
plot(Air$radiation, Air$logOzone, ylab="logOzone", xlab="Radiation")
cor(Air$radiation, Air$logOzone) 
summary(lm(logOzone ~ radiation, data=Air))





################################
## Extend the model

## Forward selection:
## Add wind to the model
summary(lm(logOzone ~ temperature + wind, data=Air))
## Add radiation to the model
summary(lm(logOzone ~ temperature + wind + radiation, data=Air))

################################
## Backward selection

## Fit the full model
summary(lm(logOzone ~ temperature + wind + radiation + noise, data=Air))
## Remove the most non-significant input, are all now significant?
summary(lm(logOzone ~ temperature + wind + radiation, data=Air))


################################
## Assumption of normal distributed residuals

## Save the selected fit
fitSel <- lm(logOzone ~ temperature + wind + radiation, data=Air)

## qq-normalplot
qqnorm(fitSel$residuals)
qqline(fitSel$residuals)


################################
## Plot the residuals vs. predicted values

plot(fitSel$fitted.values, fitSel$residuals, xlab="Predicted values", 
     ylab="Residuals")


################################
## Plot the residuals vs. the independent variables

par(mfrow=c(1,3))
plot(Air$temperature, fitSel$residuals, xlab="Temperature")
lines(lowess(Air$temperature, fitSel$residuals))
plot(Air$wind, fitSel$residuals, xlab="Wind speed")
lines(lowess(Air$wind, fitSel$residuals))
plot(Air$radiation, fitSel$residuals, xlab="Radiation")
lines(lowess(Air$radiation, fitSel$residuals))

################################
## Extend the ozone model with appropriate curvilinear regression

## Make the squared wind speed
Air$windSq <- Air$wind^2
## Add it to the model
fitWindSq <- lm(logOzone ~ temperature + wind + windSq + radiation, data=Air)
summary(fitWindSq)

## Equivalently for the temperature
Air$temperature2 <- Air$temperature^2
## Add it
fitTemperatureSq <- lm(logOzone ~ temperature + temperature2 + wind + radiation, data=Air)
summary(fitTemperatureSq)

## Equivalently for the radiation
Air$radiation2 <- Air$radiation^2
## Add it
fitRadiationSq <- lm(logOzone ~ temperature + wind + radiation + radiation2, data=Air)
summary(fitRadiationSq)

## Which one was best?
## One could try to extend the model further
fitWindSqTemperaturSq <- lm(logOzone ~ temperature + temperature2 + wind + windSq + radiation, data=Air)
summary(fitWindSqTemperaturSq)

## Model validation
par(mfrow=c(1, 2))
qqnorm(fitWindSq$residuals)
qqline(fitWindSq$residuals)
plot(fitWindSq$fitted.values, fitWindSq$residuals, pch=19)


################################
## Confidence and prediction intervals for the curvilinear model

## Generate a new data.frame with constant temperature and radiation, but with varying wind speed
par(mfrow=c(1, 1))
wind <- seq(1, 20.3, by=0.1)
AirForPred <- data.frame(temperature=mean(Air$temperature), wind=wind, 
                         windSq=wind^2, radiation=mean(Air$radiation))

## Calculate confidence and prediction intervals (actually bands)
CI <- predict(fitWindSq, newdata=AirForPred, interval="confidence", level=0.95)
PI <- predict(fitWindSq, newdata=AirForPred, interval="prediction", level=0.95)

## Plot them
plot(wind, CI[,"fit"], ylim=range(CI,PI), type="l", ylab="logOzone",
     main=paste("At temperature =",format(mean(Air$temperature),digits=3), 
                "and radiation =", format(mean(Air$radiation),digits=3)))
lines(wind, CI[,"lwr"], lty=2, col=2)
lines(wind, CI[,"upr"], lty=2, col=2)
lines(wind, PI[,"lwr"], lty=2, col=3)
lines(wind, PI[,"upr"], lty=2, col=3)
## legend
legend("topright", c("Prediction","95% confidence band","95% prediction band"), lty=c(1,2,2), col=1:3)


################################
## See problems with highly correlated inputs
## Generate values for MLR
n <- 100
## First variable
x1 <- sin(0:(n-1)/(n-1)*2*2*pi) + rnorm(n, 0, 0.1)
plot(x1, type="b")
## The second variable is the first plus a little noise
x2 <- x1 + rnorm(n, 0, 0.1)
## x1 and x2 are highly correlated
plot(x1,x2)
cor(x1,x2)
## Simulate an MLR
beta0=20; beta1=1; beta2=1; sigma=1
y <- beta0 + beta1 * x1 + beta2 * x2 + rnorm(n,0,sigma)
## See scatter plots for y vs. x1, and y vs. x2
par(mfrow=c(1,2))
plot(x1,y)
plot(x2,y)
cor(x1, y)
cor(x2, y)

## Fit an MLR
summary(lm(y ~ x1 + x2))
summary(lm(y ~ x1))
summary(lm(y ~ x2))

## If it was an experiment and the effects could be separated in the design
x1[1:(n/2)] <- 0
x2[(n/2):n] <- 0
## Plot them
plot(x1, type="b")
lines(x2, type="b", col="red")
## Now very low correlation
cor(x1,x2)
## Simulate MLR again
y <- beta0 + beta1 * x1 + beta2 * x2 + rnorm(n,0,sigma)
## and fit MLR
summary(lm(y ~ x1 + x2))




#6.1
#a
# 4 is correct

#b
B0 <- -2.365
B1 <- 0.476
B2 <- 0.083
Variance <- 0.3064^2

#c
SigmaB0 <- 0.222
SigmaB1 <- 0.062
SigmaB2 <- 0.070
B0+c(-1, 1)*qt(0.975, 237)*SigmaB0
B1+c(-1, 1)*qt(0.975, 237)*SigmaB1
B2+c(-1, 1)*qt(0.975, 237)*SigmaB2

#d
# B0 and B1 are significantly different from 0 while for B2 we can't be sure.
# The p-values are read form the ourput.


#6.2
#a
D <- data.frame(
  x1=c(0.58, 0.86, 0.29, 0.20, 0.56, 0.28, 0.08, 0.41, 0.22,
       0.35, 0.59, 0.22, 0.26, 0.12, 0.65, 0.70, 0.30, 0.70,
       0.39, 0.72, 0.45, 0.81, 0.04, 0.20, 0.95),
  x2=c(0.71, 0.13, 0.79, 0.20, 0.56, 0.92, 0.01, 0.60, 0.70,
       0.73, 0.13, 0.96, 0.27, 0.21, 0.88, 0.30, 0.15, 0.09,
       0.17, 0.25, 0.30, 0.32, 0.82, 0.98, 0.00),
  y=c(1.45, 1.93, 0.81, 0.61, 1.55, 0.95, 0.45, 1.14, 0.74,
      0.98, 1.41, 0.81, 0.89, 0.68, 1.39, 1.53, 0.91, 1.49,
      1.38, 1.73, 1.11, 1.68, 0.66, 0.69, 1.98)
)
fit <- lm(y ~ x1 + x2, data=D)
summary(fit)
confint(fit)

#b
# Since the confidence interval for β2 cover zero (and the p-value is much larger than
# 0.05, the parameter should be removed from the model to get the simpler model
fit <- lm(y ~ x1, data=D)
summary(fit)

#c
par(mfrow=c(1,2))
qqnorm(fit$residuals, pch=19)
qqline(fit$residuals)
plot(fit$fitted.values, fit$residuals, pch=19, xlab="Fitted.values", ylab="Residuals")
# Model assumptions are fulfilled

#d
# Make a plot of the fitted line and 95% confidence and prediction intervals
# of the line for x1 ∈ [0, 1] (it is assumed that the model was reduced above).
x1new <- seq(0,1,by=0.01)
pred <- predict(fit, newdata=data.frame(x1=x1new),
                interval="prediction")
conf <- predict(fit, newdata=data.frame(x1=x1new),
                interval="confidence")
plot(x1new, pred[ ,"fit"], type="l", ylim=c(0.1,2.4),
     xlab="x1", ylab="Prediction")
lines(x1new, conf[ ,"lwr"], col="green", lty=2)
lines(x1new, conf[ ,"upr"], col="green", lty=2)
lines(x1new, pred[ ,"lwr"], col="red", lty=2)
lines(x1new, pred[ ,"upr"], col="red", lty=2)
legend("topleft", c("Prediction","Confidence band","Prediction band"),
       lty=c(1,2,2), col=c(1,3,2), cex=0.7)






