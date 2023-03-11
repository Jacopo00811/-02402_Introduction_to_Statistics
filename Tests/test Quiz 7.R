#Q1
#Pi/32(D^4-d^4)
#d/dD(Pi/32*D^4)=Pi/8*D^3
#d/dd(-Pi/32*d^4)=-Pi/8*d^3
#Var(I)=(Pi/8*D^3)^2*sdD^2+(Pi/8*d^3)^2*sdd^2


#Q2
s2 <-2.1212 
n <-20
qchisq(0.005,n-1)
qchisq(0.995,n-1)


#Q3
# They are independent from the discrete distributin so do the summation of all
# of them for mean and variance


#Q4
ms <- c(39.0, 43.5) 
vs <- c(2.6458, 2.6726)^2 
ns <- c(7, 8) 
nu <- ((vs[1]/ns[1]+vs[2]/ns[2])^2)/ ((vs[1]/ns[1])^2/(ns[1]-1)+(vs[2]/ns[2])^2/(ns[2]-1)) 
nu
qt(0.975, 12.77)


#Q5
#x_2/x_1
#d/dX_2(x_2/x_1)=-1/x_1
#d/dX_1(x_1/x_2)=-x_2/x_1^2
#Var(I)=(-x_2/x_1^2)^2*sd1^2+(-1/x_1)^2*sd2^2


#Q6
n <- 14
qchisq(0.975,n-1)
qchisq(0.025,n-1)


#Q7 
# Just read it


#Q8
#BMI=W/H^2
#Var(Log(BMI))=(1/W)^2*sdW^2+(2/H)^2*sdH^2
# The sd is found by taking the sqrt of the above