#Q1
D <- 45
d <- 32
S <- 0.3
s <- 0.4
DD <- D-d
DD
Std <- sqrt(s^2+S^2)
Std
pnorm((12.5-DD)/Std)

#Q2
v <- c(294, 317, 317, 310, 327, 300, 293, 321, 307, 304)
quantile(v, type=2) # Important to use type 2 

#Q3
A <- 3*2
SST <- 2434.25
# Subtract from SST the SS(Tr)-SS(Bl)
B<- SST-1449.50-524.25
B
C <- B/A
C

#Q4
k <- 4
l <- 3
n <- l*k
# For a two way ANOVA
1-pf(2.2769, k-1, (k-1)*(l-1))

#Q5
# We need to use the critical value. For the 5% sign. level
# it is 0.95. The df are the corresponding ones and the
# (k-1)*(l-1)
qf(0.95, l-1, (k-1)*(l-1))


#Q6
D <- 626.9
d <- 364.7
S <- 4.2^2
s <- 3.6^2
DD <- D-d
DD
Std <- sqrt(s+S)
Std
# Here it is 1-pnorm because it's the probability of getting 
# a value grater than 265
1-pnorm((265-DD)/Std)

#Q7
626.9*24+1500 #total weight
sqrt(24*4.2^2) #total std
pnorm((16500-(626.9*24+1500))/sqrt(24*4.2^2))
 
#Q8
# Do +-2*sigma
sigma <- 0.3
2*sigma

#Q9
# If it follows a uniform distribution
1.5/6
