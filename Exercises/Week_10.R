#################################################################
### Testing hypotheses about one proportion using prop.test() ###
#################################################################

prop.test(10, 100, p = 0.5, correct = FALSE)

##################################################################
### Testing hypotheses about two proportions using prop.test() ###
##################################################################

# Read data table into R
pill.study <- matrix(c(23, 34, 35, 132), 
                     ncol = 2, byrow = TRUE)
colnames(pill.study) <- c("Blood Clot", "No Clot")
rownames(pill.study) <- c("Pill", "No pill")

# Show data table
pill.study

# Test whether probabilities are equal for the two groups
prop.test(pill.study, correct = FALSE)

###################################################################
### Testing hypotheses about two proportions using chisq.test() ###
###################################################################

# Test whether probabilities are equal for the two groups
chisq.test(pill.study, correct = FALSE)

# Expected values
chisq.test(pill.study, correct = FALSE)$expected

###################################################################
### Testing hypotheses in contingency tables using chisq.test() ###
###################################################################

# Read data table into R
poll <-matrix(c(79, 91, 93, 84, 66, 60, 37, 43, 47), 
              ncol = 3, byrow = TRUE)
colnames(poll) <- c("4 weeks", "2 weeks", "1 week")
rownames(poll) <- c("Cand1", "Cand2", "Undecided")

# Show data table
poll

# Show column percentages
prop.table(poll, 2)

# Plot probabilities
barplot(t(prop.table(poll, 2)), beside = TRUE, col = 2:4, las = 1, ylim = c(0, 0.5),
        ylab = "Percent", xlab = "Candidate", 
        main = "Distribution of votes")
legend(legend = colnames(poll), fill = 2:4, "topright")

# Testing for same distribution in the three populations
chisq.test(poll, correct = FALSE)

# Expected values
chisq.test(poll, correct = FALSE)$expected






#7.1
#a
n1 <- 108
n2 <- 143
x1 <- 82
x2 <- 104
n <- 251
p1 <- x1/n1
p2 <- x2/n2 
(x1/n1-x2/n2)+c(-1, 1)*qt(0.975, n-1)*sqrt((x1/n1*(1-x1/n1))/n1+(x2/n2*(1-x2/n2))/n2)

#b
# degrees of freedom(2 − 1)(2 − 1) = 1
qchisq(0.99, 1)

#c
n <- 250
p <- 0.2
mu <- n*p
mu
var <- n*p*(1-p)
std <- sqrt(var)
std


#7.2
#a
n <- 250
x <- 140
Zobs <- x-n*1/2/sqrt(n*1/2*1/2)
Zobs
# No

#b
# χ2 distribution with (r − 1)(c − 1) = 2 degrees of freedom
qchisq(0.95, 2)


#7.3
#a
p <- 0.295
x <- 168
n <- 740
Zobs <- (x-n*p)/sqrt(n*p*(1-p))
Zobs
Zobs<qnorm(0.9995)
# If the Zobs is true we reject the null hypothesis

#b
# CIs
x <- 168
n <- 740
p <- x/n
p+c(-1, 1)*qt(0.975, n-1)*sqrt(p*(1-p)/n)

#c
# Solve this 0.3 · 0.7 · (Z__0.995/ME)^2
# to find the numbers of people
0.3*0.7*(qnorm(0.995)/(0.01/2))^2


#7.4
#a
xa <- 6
na <- 50
xb <- 12
nb <- 50
pa <- xa/na
pb <- xb/nb
p <- (xa+xb)/(na+nb)
Zobs <- abs((pa-pb)/sqrt(p*(1-p)*(1/na+1/nb)))
Zobs
pval <- 2*(1 - pnorm(Zobs))
pval
# the null hypothesis cannot be rejected, since the p-value is 
# above the significance level 
# Or 
prop.test(x=c(6,12), n=c(50,50), correct = FALSE)
# Or 
chisq.test(matrix(c(6,12,44,38), ncol = 2), correct = FALSE)

#b
x <- 36
n <- 200
p <- x/n
qnorm(0.995)
p+c(-1, 1)*qnorm(0.995)*sqrt(p*(1-p)/n)
# Or
prop.test(x=36, n=200, correct=FALSE, conf.level=0.995)

#c
n <- 1537
# Solve this for ME n = 0.2 · 0.8 (Z0.975/ME)^2
# and multiply by two since the width of the confidence interval 
# is twice the margin of error


#7.5
# Look at the solutions

#Test quiz, look at solutions