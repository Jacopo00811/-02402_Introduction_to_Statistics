#Q1
x <- 55 
n <- 200
x/n+c(-1, 1)*qt(0.975, n-1)*sqrt(x/n*(1-x/n)/n)

prop.test(x, n, correct = FALSE)


#Q2
n1 <- 50
n2 <- 150
x1 <- 8
x2 <- 47
x <- x1 + x2
n <- n1 + n2
p <- x/n
p1 <- x1/n1
p2 <- x2/n2 
Zobs <- (p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2))
Zobs
pval <- 2*pnorm(Zobs)
pval

#Q3
p <-(51+68)/200
p1 <- 0.51
p2 <- 0.68
n1 <- 100
n2 <- 100
n <- n1 + n2
Zobs <- (p1-p2)/sqrt(p*(1-p)*(1/n1+1/n2))
Zobs
pval <- pnorm(Zobs)
pval


#Q4
# Use the t-test instead of z test


#Q5
# Look at the correlation between MW vs SURF and square it since that is the 
# correlation rho and it is known that R^2=rho


#Q6
# None of the above


#Q7
# Idk, for me should have been the 4th one
