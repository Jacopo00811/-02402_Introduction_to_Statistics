###########################################################################
## Set the working directory

## In RStudio the working directory is easily set via the menu
## "Session -> Set Working Directory -> To Source File Location" 
## Note: In R only "/" is used for separating in paths 
## (i.e. no backslash).
setwd("C:/Users/jacop/Desktop/DTU/IntroductionStatistics/bmi1")


###########################################################################
## Read data into R

## Read data from bmi1_data.csv
D <- read.table("bmi1_data.csv", header=TRUE, sep=";", as.is=TRUE)
D

###########################################################################
## Simple overview of the data

## Dimensions of D (number of rows and columns)
dim(D)
##  Column/variable names
names(D)
## The first rows/observations
head(D)
## The last rows/observations
tail(D)
## Selected summary statistics
summary(D)
## Another type of summary of the dataset
str(D)


###########################################################################
## Calculate BMI scores

## Calculate BMI scores and add new variable to D
D$bmi <- D$weight/(D$height/100)^2

###########################################################################
## Histogram (empirical density)

## Histogram describing the empirical density of the BMI scores
## (histogram of the BMI scores normalized to have an area of 1)
hist(D$bmi, breaks = 20, xlab="BMI", xlim = range(15:40), prob=TRUE, col = "dark green", main = "Density histogram of BMI")


###########################################################################
## Taking subsets of the data using 'subset'

## Divide data into two subsets according to gender
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)


###########################################################################
## Density histograms by gender
## Density histograms describing the empirical density
## of the BMI scores of women and men, respectively.

# Hist Male
hist(Dmale$bmi, breaks = 20, xlab="BMI", xlim = range(15:40), prob=TRUE, col = "cyan", main = "Density histogram of BMI Males")
# Hist Female
hist(Dfemale$bmi, breaks = 20, xlab="BMI", xlim = range(15:40), prob=TRUE, col = "violet", main = "Density histogram of BMI Females")



###########################################################################
## Box plot

## Box plot of BMI scores by gender
boxplot(Dfemale$bmi, Dmale$bmi, boxlwd = 2, outwex = 0.5, boxwex = 0.35, names=c("Female", "Male"), xlab="Gender", col = "dark green", ylab="BMI")
text(1.35, quantile(c(Dfemale$bmi)), c("Minimum", "Q1", "Median", "Q3", "Maximum"), col = "black")
text(2.3, quantile(c(Dmale$bmi)), c("Minimum", "Q1", "Median", "Q3", "Maximum"), col = "black")

###########################################################################
## Summary statistics for BMI

## Total number of observations
## (doesn't include missing values if there are any)
sum(!is.na(D$bmi))
## Sample mean (both genders combined)
mean(D$bmi, na.rm=TRUE)
## Sample variance (both genders combined)
var(D$bmi, na.rm=TRUE)
## etc.
##
## The argument 'na.rm=TRUE' ensures that the statistic is
## computed even in cases where there are missing values.

## Use a 'for'-loop to calculate the summary statistics
## and assign the result to a new data.frame
Tbl <- data.frame(stringsAsFactors = FALSE)

Tbl[1,"n"] <- sum(!is.na(D$bmi))
Tbl[1, "mean"] <- mean(D$bmi, na.rm=TRUE)
Tbl[1, "var"] <- var(D$bmi, na.rm=TRUE)
Tbl[1, "std"] <- sd(D$bmi, na.rm=TRUE)
Tbl[1, "Q1"] <-  quantile(D$bmi, 0.25, na.rm=TRUE)
Tbl[1, "Median"] <-  quantile(D$bmi, 0.50, na.rm=TRUE)
Tbl[1, "Q3"] <-  quantile(D$bmi, 0.75, na.rm=TRUE)

for(i in 0:1){
  Tbl[i+2,"n"] <- sum(!is.na(D$bmi[D$gender == i]))
  Tbl[i+2, "mean"] <- mean(D$bmi[D$gender == i], na.rm=TRUE)
  Tbl[i+2, "var"] <- var(D$bmi[D$gender == i], na.rm=TRUE)
  Tbl[i+2, "std"] <- sd(D$bmi[D$gender == i], na.rm=TRUE)
  Tbl[i+2, "Q1"] <-  quantile(D$bmi[D$gender == i], 0.25, na.rm=TRUE)
  Tbl[i+2, "Median"] <-  quantile(D$bmi[D$gender == i], 0.50, na.rm=TRUE)
  Tbl[i+2, "Q3"] <-  quantile(D$bmi[D$gender == i], 0.75, na.rm=TRUE)

  
}
row.names(Tbl) <- c("Everyone","Women","Men")
## View the contents of Tbl
Tbl
xtable(Tbl)
#print.xtable(Final, type="latex", file="table.tex")

## In R there are also more condensed ways to do such calculations.
## For example,

# aggregate(D$bmi, by=list(D$gender), function(x){
#  c(mean=mean(x), var=var(x))
# })

## See more useful functions with: ?apply, ?aggregate and ?lapply
## For extremely efficient data handling see, e.g., the packages:
## dplyr, tidyr, reshape2 and ggplot2


###########################################################################
## qq-plot for model validation
# qq-plot of non logged data
qqnorm(D$bmi)
qqline(D$bmi)

## New variable 'logbmi' with log-transformed BMI
D$logbmi <- log(D$bmi)
## qq-plot of log-transformed BMI
qqnorm(D$logbmi, main = "Q-Q plot of log(BMI)")
qqline(D$logbmi)
 
###########################################################################
# The mean and sd. for everyone can be read from
summary(D)
sd(log(D$bmi))
var(log(D$bmi))

############################################################################
# Wally plot

# Define the plotting function
qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}
# Do the Wally plot
wallyplot(D$logbmi-mean(D$logbmi), FUN=qqwrap, ylim=c(-3,3))
#Confidence intervals for the mean
# t_obs 
qt(0.975,144)
# CIs
mean(D$logbmi)+qt(0.975,144)*3.218/sqrt(145)
mean(D$logbmi)-qt(0.975,144)*3.218/sqrt(145)

#Confidence intervals for the median using t.test
t.test(D$logbmi, conf.level = 0.95)
# The median is
exp(3.217641)
# The confidence interval for the median is
exp(c(3.193203, 3.242078))

###########################################################################
## One-sample t-test

## Testing hypothesis mu=log(25) for log-transformed BMI
t.test(D$logbmi, mu=log(25))


###########################################################################
# Statistical models men and women
Dfemale <- subset(D, gender == 0)
Dmale <- subset(D, gender == 1)

#Men
qqnorm(Dmale$logbmi,main = "Q-Q plot of Men log(BMI)")
qqline(Dmale$logbmi)
mean(Dmale$logbmi)
sd(Dmale$logbmi)

qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}
# Do the Wally plot
wallyplot(Dmale$logbmi-mean(Dmale$logbmi), FUN=qqwrap, ylim=c(-3,3))

# Women
qqnorm(Dfemale$logbmi, main="Q-Q plot of Women log(BMI)")
qqline(Dfemale$logbmi)
mean(Dfemale$logbmi)
sd(Dfemale$logbmi)

qqwrap <- function(x, y, ...){
  stdy <- (y-mean(y))/sd(y)
  qqnorm(stdy, main="", ...)
  qqline(stdy)}
# Do the Wally plot
wallyplot(Dfemale$logbmi-mean(Dfemale$logbmi), FUN=qqwrap, ylim=c(-3,3))

###########################################################################
## CI's for the mean and median
# Men
mean(Dmale$logbmi)+qt(0.975,72-1)*3.260588/sqrt(72)
mean(Dmale$logbmi)-qt(0.975,72-1)*3.260588/sqrt(72)
t.test(Dmale$logbmi, conf.level = 0.95)
# The median is
exp(3.260588)
# The confidence interval for the median is
exp(c(3.231677, 3.289498))

# Women
mean(Dfemale$logbmi)+qt(0.975,73-1)*3.174097/sqrt(73)
mean(Dfemale$logbmi)-qt(0.975,73-1)*3.174097/sqrt(73)
t.test(Dfemale$logbmi, conf.level = 0.95)
# The median is
exp(3.174097)
# The confidence interval for the median is
exp(c(3.136525, 3.211669))

## Consider data for women only
Dfemale <- subset(D, gender == 0)
## Compute CI for mean log-BMI score of a woman
KI <- t.test(Dfemale$logbmi, conf.level=0.95)$conf.int
KI
## "Back-transform" to get a CI for median BMI score of a woman
exp(KI)


###########################################################################
## Welch t-test for comparing two (independent) samples
mu1 <- mean(Dmale$logbmi)
mu2 <- mean(Dfemale$logbmi)
n1 <- 72
n2 <- 73
s1 <- sd(Dmale$logbmi)
s2 <- sd(Dfemale$logbmi)
# tobs
tobs<-(mu1-mu2)/sqrt(s1^2/n1+s2^2/n2)
tobs
#v
v <-(s1^2/n1+s2^2/n2)^2/((s1^2/n1)^2/(n1-1)+(s2^2/n2)^2/(n2-1))
v
#CIs
mu1-mu2+qt(p=0.975, v)*sqrt(s1^2/n1+s2^2/n2)
mu1-mu2-qt(p=0.975, v)*sqrt(s1^2/n1+s2^2/n2)
# p-value
(1 - pt(tobs,v))*2

## Comparison of mean log(BMI) for women and men
t.test(D$logbmi[D$gender == 0], D$logbmi[D$gender == 1])


###########################################################################
## Computing correlations

# Find the standard deviations
Sw <- sd(D$weight)
Sw
Sb <- sd(D$bmi)
Sb
Sf <- sd(D$fastfood)
Sf
# Find the covariances
Swb <- cov(D$weight, D$bmi)
Swb
Sbf <- cov(D$bmi, D$fastfood)
Sbf
Sfw <-cov(D$fastfood, D$weight)
Sfw
# Compute the correlations
rwb <- Swb/(Sw * Sb) 
rwb
rbf <- Sbf/(Sf * Sb)
rbf
rfw <- Sfw/(Sf * Sw)
rfw
# Plots
plot(D$bmi, D$weight, main = expression(r%~~%0.83), ylab= "Weight (kg)", xlab="BMI scores")
plot(D$bmi, D$fastfood, main = expression(r%~~%0.15), ylab= "N. days per year", xlab="BMI scores")
plot(D$fastfood, D$weight, main = expression(r%~~%0.28), ylab= "Weight (kg)", xlab="N. days per year")

## Computing correlations between selected variables
cor(D[,c("weight","fastfood","bmi")], use="pairwise.complete.obs")


###########################################################################
## Subsets in R
  
## Optional extra remark about taking subsets in R
##
## A logical vector with a TRUE or FALSE for each value 
## of a column in D, e.g.: Find all women in the data
D$gender == 0
## Can be used to find all the data for women
D[D$gender == 0, ]
## Alternatively, use the 'subset' function
subset(D, gender == 0)
## More complex logical expressions can be made, e.g.:
## Find all women who weigh less than 55 kg
subset(D, gender == 0 & weight < 55)






## LaTeX tips:
##
## The R package "xtable" can generate LaTeX tables written to a file 
## and thereby they can automatically be included in a .tex document.
## 
## The R package "knitr" can be used very elegantly to generate .tex 
## documents with R code written directly in the document. This 
## document and the book were generated using knitr.
