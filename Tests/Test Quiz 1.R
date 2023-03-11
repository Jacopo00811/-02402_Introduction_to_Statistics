#### Q1 ####
# Upper quartile
m1<-c(57, 52, 62, 49, 43)
sort(m1)
quantile(m1, probs = c(.75), type = 2)

#### Q2 ####
# Median
p<-c(8, 7, 10, 14, 11, 7,10 ,11, 16, 12)
median(p)

#### Q3 ####
# Look at the graphs

#### Q4 ####
M1<-c(82.5, 83.7, 80.9, 95.2, 80.8)
median(M1)
quantile(M1, probs = c(.75), type = 2)

#### Q5 ####
G<-c(48, 48, 49, 49, 52, 54, 54, 55, 55)
median(G)
quantile(G, probs = c(0, 0.25, 0.5, 0.75, 1), type = 2)

