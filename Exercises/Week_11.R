########################################################
### Example: One-way and two-way ANOVA with B&O data ###
########################################################

# Get the B&O data from the lmerTest-package
library(lmerTest)
data(TVbo)
head(TVbo) # First rows of the data

# Define factor identifying the 12 TV set and picture combinations 
TVbo$TVPic <- factor(TVbo$TVset:TVbo$Picture)

# Each of 8 assessors scored each of the 12 combinations twice.
# Average the two replicates for each assessor and combination of 
# TV set and picture
library(doBy)
TVbonoise <- summaryBy(Noise ~ Assessor + TVPic, data = TVbo, 
                       keep.names = T)

# One-way ANOVA of the noise (not the correct analysis!)
anova(lm(Noise ~ TVPic, data = TVbonoise))

# Two-way ANOVA of the noise (better analysis, week 12)
anova(lm(Noise ~ Assessor + TVPic, data = TVbonoise))

##############################################
### Simple example: Plots of data by group ###
##############################################

# Input data
y <- c(2.8, 3.6, 3.4, 2.3,
       5.5, 6.3, 6.1, 5.7,
       5.8, 8.3, 6.9, 6.1)

## Define treatment groups
treatm <- factor(c(1, 1, 1, 1,
                   2, 2, 2, 2,
                   3, 3, 3, 3))

## Plot data by treatment groups
par(mfrow = c(1,2))
plot(y ~ as.numeric(treatm), xlab = "Treatment", ylab = "y")
boxplot(y ~ treatm, xlab = "Treatment", ylab = "y")

###############################################
###  Plot F-distribution and critical value ###
###############################################

# Remember, this is "under H0" (i.e. we compute as if H0 is true)

# Number of groups
k <- 3

# Total number of observations
n <- 12

# Sequence for plot
xseq <- seq(0, 10, by = 0.1)

# Plot density of the F-distribution
plot(xseq, df(xseq, df1 = k-1, df2 = n-k), type = "l")

# Plot critical value for significance level 5%
cr <- qf(0.95, df1 = k-1, df2 = n-k)
abline(v = cr, col = "red") 

############################################
### One-way ANOVA using anova() and lm() ###
############################################

anova(lm(y ~ treatm))

###############################
### One-way ANOVA 'by hand' ###
###############################

k <- 3; n <- 12  # Number of groups k, total number of observations n

# Total variation, SST
(SST <- sum( (y - mean(y))^2 ))

# Residual variance after model fit, SSE
y1 <- y[1:4]; y2 <- y[5:8]; y3 <- y[9:12]

(SSE <- sum( (y1 - mean(y1))^2 ) + 
    sum( (y2 - mean(y2))^2 ) + 
    sum( (y3 - mean(y3))^2 ))

# Variance explained by the model, SS(Tr)
(SSTr <- SST - SSE)

# Test statistic
(Fobs <- (SSTr/(k-1)) / (SSE/(n-k)))

# P-value
(1 - pf(Fobs, df1 = k-1, df2 = n-k))

########################
### Model validation ###
########################

# Check assumption of homogeneous variance using, e.g., 
# a box plot.
plot(treatm, y)

# Check normality of residuals using a normal QQ-plot
fit1 <- lm(y ~ treatm)
qqnorm(fit1$residuals)
qqline(fit1$residuals)





#8.1
#a
k <- 3
l <- 6
SSt <- 11.4944
SSres <- 4.1060
n <- 6*3
A <- k-1
D <- n-k
B <- SSt -SSres
C <- B/A

#b
#the F-distribution, here with degrees of freedom k − 1 = 2 and n − k = 15
qf(p=0.95, df1=k-1, df2=n-k)

#c
U <- ((SSt-SSres)/2)/(SSres/D)
U
1-pf(U, df1=k-1, df2=n-k)
# Yes there is significant difference for the mean values

#d
#CI at 90%
(4.0483-5.5517)+c(-1,1)*qt(0.95,n-k)*sqrt(SSres/(n-k)*(1/l+1/l))


#8.2
#a
# See pic

#b
D <- data.frame(
  nitrogen=c(5.01, 5.59, 3.02,
             6.23, 5.13, 4.76,
             5.98, 5.33, 3.46,
             5.31, 4.65, 4.12,
             5.13, 5.52, 4.51,
             5.65, 4.92, 4.42),
  year=factor(rep(c("1998", "2003", "2011"), 6)))
tapply(D$nitrogen, D$year, mean)
tapply(D$nitrogen, D$year, var)
mean(D$nitrogen)
17 *var(D$nitrogen)

#c
fit <- lm(nitrogen ~ year, data=D)
anova(fit)

#d
# Do M different confidence intervals
n <- 18
k <- 3
M <- (k*(k-1))/2
M
# As all n__is equal 6 in this case, all 3 confidence intervals will have the
# same width
alpha_bon <- 0.05/M
LSD_0.01667 <- qt(1-alpha_bon/2, n-k)*sqrt(2*0.2737/6)
LSD_0.01667

plot(D$year, D$nitrogen, xlab="Nitrogen", ylab="Year")
text(1:3, c(5.7, 5.4, 4), c("a", "a", "b"), cex=2)

#e
# Normal distribution
qqnorm(fit$residuals)
qqline(fit$residuals)


#8.3
#a
SStr <- 62
SSe <- 362.71
k <- 5
n <- 100
Fobs <- (SStr/(k-1))/(SSe/(n-k))
# For a single way ANOVA
1-pf(Fobs, k-1, n-k)
# it is small, we have strong evidence against the null hypothesis

#b
#CI
0.1+ c(-1,1)* qt(0.975, 95)*sqrt(362.71/(95*10))


#8.4
#a
# The formatting is fucked up 

#b
# Answer 1 to use one-way analysis of variance 
# The data must be normally and independently distributed within
# each group and the variances within each group should not differ
# significantly from each other

#c
#CI
3.48+ c(-1,1)* qt(0.975, 24)*sqrt(23.983*2/5)



