#Q1
# The proportion is directly read off the R-output as the “Multiple R-square’’ (= the squared correlation r2), and the residual standard deviation, 
# se as the “Residual standard error”, so the answer is 4


#Q2
# Yes, since the 95% confidence interval for the slope contains 1.1832
n <- 10
qt(0.975, n-2)
1.17758+c(-1, 1)*qt(0.975, n-2)*0.01136


#Q3
Sxx <- 7.5655
meanx <- 2.635
xnew <- 4.45
xnew14 <- 0.01207+1.17758*xnew
xnew14+c(-1, 1)*qt(0.995, n-2)*se*sqrt(1+1/n+(xnew-meanx)^2/Sxx)


#Q4
# For 99% CI, we should look at the 0.5% and 99.5% quantile
c(136.2143, 142.2143)