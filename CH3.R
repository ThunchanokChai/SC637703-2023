##CH3
#The Plausibility of as a Value for a Normal
#A single sample with several variable measure on each sampling unite

##Ex 1
#H0 : mu == matrix(c(4,50,10),3,1) VS H1 : mu != matrix(c(4,50,10),3,1)  
#input
n = 20
p = 3
alpha = 0.05
Xbar = matrix(c(4.64,45.4,9.965),3,1)
print(Xbar)

S=matrix(c(2.879,10.01,-1.81,10.01,199.788,-5.64,-1.81,-5.64,3.628),3,3)
print(S)

mu=matrix(c(4,50,10),3,1)
print(mu)

T2 = n*t(Xbar-mu)%*%solve(S)%*%(Xbar-mu)
paste("Hotelling’s T2 is:",T2)

T2_cri=(((n-1)*p)/(n-p))*qf(alpha,p,n-p,lower.tail = FALSE)
paste("Critical value: ",T2_cri)

##Ex 2
#H0 : mu == matrix(c(9,5),2,1) VS H1 : mu != matrix(c(9,5),2,1)
#input
n = 3
p = 2
alpha = 0.05
mu = matrix(c(9,5),2,1)
print(mu)

X = matrix(c(6,10,8,9,6,3),3,2)
print(X)

Xbar = colMeans(X)
print(Xbar)

Xbar = matrix(Xbar,2,1) # Change Xbar into vector form.
print(Xbar)

S = cov(X)
print(S)

T2 = n*t(Xbar-mu)%*%solve(S)%*%(Xbar-mu)
paste("Hotelling’s T2:",T2)

Cri_val = (((n-1)*p)/(n-p))*qf(alpha,p,n-p,lower.tail = FALSE)
paste("Critical value: ",Cri_val)

#T2 is 0.777777777777778 less than T2_cri is 798.
#Therefore we do not reject H0 at level of significance alpha = 0.05

#2) Confidence Regions and Simultaneous Comparisons of Component Means
##Ex 3
##input
n = 20
p = 3
alpha = 0.05
Xbar = matrix(c(4.64,45.4,9.965),3,1)
print(Xbar)
S = matrix(c(2.879,10.01,-1.81,10.01,199.788,-5.64,-1.81,-5.64,3.628),3,3)
print(S)

#1 The Lower bound and The Upper bound for X1 and S11
L1 = Xbar[1,1]-(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[1,1]/n))
U1 = Xbar[1,1]+(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[1,1]/n))
paste("The lower bound L1: ",L1, "/ The upper bound U1: ",U1)

#2 The Lower bound and The Upper bound for X2 and S22
L2 = Xbar[2,1]-(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[2,2]/n))
U2 = Xbar[2,1]+(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[2,2]/n))
paste("The lower bound L2: ",L2, "/ The upper bound U2: ",U2)

#3 The Lower bound and The Upper bound for X3 and S33
L3 = Xbar[3,1]-(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[3,3]/n))
U3 = Xbar[3,1]+(sqrt(((n-1)*p)/(n-p)*qf(1-alpha , p , n-p))*sqrt(S[3,3]/n))
paste("The lower bound L3: ",L3, "/ The upper bound U3: ",U3)


##Ex4
#1 The Lower bound and The Upper bound for X1 and S11
L1 = Xbar[1,1]-((qt(1-(alpha / (2*p)),n-1))*sqrt(S[1,1]/n))
U1 = Xbar[1,1]+((qt(1-(alpha / (2*p)),n-1))*sqrt(S[1,1]/n))
paste("The lower bound L1: ",L1, "/ The upper bound U1: ",U1)

#2 The Lower bound and The Upper bound for X2 and S22
L2 = Xbar[2,1]-((qt(1-(alpha / (2*p)),n-1))*sqrt(S[2,2]/n))
U2 = Xbar[2,1]+((qt(1-(alpha / (2*p)),n-1))*sqrt(S[2,2]/n))
paste("The lower bound L2: ",L2, "/ The upper bound U2: ",U2)

#3 The Lower bound and The Upper bound for X3 and S33
L3 = Xbar[3,1]-((qt(1-(alpha / (2*p)),n-1))*sqrt(S[3,3]/n))
U3 = Xbar[3,1]+((qt(1-(alpha / (2*p)),n-1))*sqrt(S[3,3]/n))
paste("The lower bound L3: ",L3, "/ The upper bound U3: ",U3)


# Comparing Mean Vectors from Two Populations
##Ex5
#input
n1 = 50
n2 = 50
p = 2
alpha = 0.05

s1 = matrix(c(2,1,1,6),2,2)
print(s1)

s2 = matrix(c(2,1,1,4),2,2)
print(s2)

sp = (((n1-1)/(n1+n2-2))*s1)+(((n2-1)/(n1+n2-2))*s2)
print(sp)


xbar1 = matrix(c(8.3,4.1),2,1)
print(xbar1)

xbar2 = matrix(c(10.2,3.9),2,1)
print(xbar2)

t2 = t(xbar1-xbar2)%*%solve(((1/n1)+(1/n2))*sp)%*%(xbar1-xbar2)
paste("The T2 value: ",t2)

cri_val = ((n1+n2-2)*p)/(n1+n2-p-1)*qf(1-alpha,p,n1+n2-p-1)
paste("The critical value: ",cri_val)

###T2 is 2.47222 greater than Critical Value is 6.244089. 
###Then reject H0 at level of significance alpha is 0.05

##Ex6
#1 The Lower bound and The Upper bound for Xbar[1,1] and Sp[1,1]
L1 = (xbar1[1,1]-xbar2[1,1])-(sqrt(cri_val)*sqrt(((1/n1)+(1/n2))*sp[1,1]))
U1 = (xbar1[1,1]-xbar2[1,1])+(sqrt(cri_val)*sqrt(((1/n1)+(1/n2))*sp[1,1]))
paste("The lower bound L1: ",L1, "/ The upper bound U1: ",U1)

#2 The Lower bound and The Upper bound for Xbar[2,1] and Sp[2,2]
L2 = (xbar1[2,1]-xbar2[2,1])-(sqrt(cri_val)*sqrt(((1/n1)+(1/n2))*sp[2,2]))
U2 = (xbar1[2,1]-xbar2[2,1])+(sqrt(cri_val)*sqrt(((1/n1)+(1/n2))*sp[2,2]))
paste("The lower bound L2: ",L2, "/ The upper bound U2: ",U2)

##Ex7
#The Bonferroni 100(1-0.05)% simultaneous confidence intervals for the differences in the mean component
qt(1-(alpha/(2*p)),n1+n2-2)

#The Bonferroni 95% simultaneous confidence intervals for the population difference are
#1 The Lower bound and The Upper bound for Xbar[1,1] and Sp[1,1]
L1 = (xbar1[1,1]-xbar2[1,1])-(qt(1-(alpha/(2*p)),n1+n2-2)*sqrt(((1/n1)+(1/n2))*sp[1,1]))
U1 = (xbar1[1,1]-xbar2[1,1])+(qt(1-(alpha/(2*p)),n1+n2-2)*sqrt(((1/n1)+(1/n2))*sp[1,1]))
paste("The lower bound L1: ",L1, "/ The upper bound U1: ",U1)

#2 The Lower bound and The Upper bound for Xbar[2,1] and Sp[2,2]
L2 = (xbar1[2,1]-xbar2[2,1])-(qt(1-(alpha/(2*p)),n1+n2-2)*sqrt(((1/n1)+(1/n2))*sp[2,2]))
U2 = (xbar1[2,1]-xbar2[2,1])+(qt(1-(alpha/(2*p)),n1+n2-2)*sqrt(((1/n1)+(1/n2))*sp[2,2]))
paste("The lower bound L2: ",L2, "/ The upper bound U2: ",U2)

#Ex8
#The two-sample situation when Σ1 ≠ Σ2
n1 = 45
n2 = 55

S1 = matrix(c(13825.3,23823.4,23823.4,73107.4),2,2)
print(S1)

S2 = matrix(c(8632.0,19616.7,19616.7,55964.5),2,2)
print(S2)

S = (1/n1)*S1 + (1/n2)*S2
print(S)

Xbar1 = matrix(c(204.4,556.6),2,1)
Xbar2 = matrix(c(130.0,355.0),2,1)
t(Xbar1-Xbar2)%*%solve(S)%*%(Xbar1-Xbar2)

qchisq(1-alpha,p)

##Testing of Equality of Covariance Matrices
#Ex10
n1 = 50
n2 = 50
g = 2
p = 2
S1 = matrix(c(2,1,1,6),2,2)
S2 = matrix(c(2,1,1,4),2,2)
Sp = ((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
lnM = ((1/2)*(((n1-1)*log(det(S1)))+(n2-1)*log(det(S2)))) - ((1/2)*(n1-1+n2-1)*log(det(Sp)))
paste("lnM : ",lnM)

c1 = ((1/(n1-1))+(1/(n2-1))-(1/(n1-1+n2-1)))*(((2*p^2)+3*p-1)/(6*(p+1)*(g-1)))
paste("c1 : ",c1)

U = -2*(1-c1)*lnM
cri_val = qchisq(1-alpha,(1/2)*(g-1)*p*(p+1))

if ( U < cri_val){
  paste("Since U =",U," < critical value = ",cri_val,",then do not reject H0.")
} else if(U == cri_val){
  paste("Since U =",U," = critical value = ",cri_val,",then do not reject H0.")
} else {
  paste("Since U =",U," > critical value = ",cri_val,",then reject H0.")
}

c2 = (((1/(n1-1)^2)-(1/(n2-1)^2))-(1/((n1-1)+(n2-1))^2))*((p-1)*(p+2)/(6*(g-1)))
a1 = (1/2)*(g-1)*p*(p+1)
a2 = (a1+2)/abs(c2-c1^2)
b1 = (1-c1-(a1/a2))/a1
b2 = (1-c1+(2/a2))/a2

if (c2 > c1^2){
  F = -2*b1*lnM
} else if(c2 < c1^2){
  F = (-2*a2*b2*lnM)/(a1*(1+(2*b2*lnM)))
} else {
  F = NULL
}

cri_val = qf(1-alpha,a1,a2)

if(is.null(F)==0){
  if(F < cri_val){
    paste("Since F =",F," < critical value = ",cri_val,",then do not reject H0.")
  } else if(F == cri_val){
    paste("Since F =",F," = critical value = ",cri_val,",then do not reject H0.")
  } else {
    paste("Since F =",F," > critical value = ",cri_val,",then reject H0.")
  }
}else{
  print("We cannot conclude the distribution of F.")
}



#Paired Comparison
#Ex11
n = 11
X1 = matrix(c(6,6,18,8,11,34,28,71,43,33,20,27,23,64,44,30,75,26,124,54,30,14),11,2)
X2 = matrix(c(25,28,36,35,15,44,42,54,34,29,39,15,13,22,29,31,64,30,64,56,20,21),11,2)
d = X1-X2
dmean = colMeans(d)
dbar = matrix(c(dmean[1],dmean[2]),2,1)
print(dbar)

sd = cov(d)
print(sd)

T2 = n*t(dbar)%*%solve(sd)%*%dbar
print(T2)

((n-1)*p/(n-p))*qf(1-alpha,p,n-p)


L1 = dbar[1,1]-(sqrt(((n-1)*2/(n-p))*qf(1-alpha,p,n-p))*sqrt(sd[1,1]/n))
U1 = dbar[1,1]+(sqrt(((n-1)*2/(n-p))*qf(1-alpha,p,n-p))*sqrt(sd[1,1]/n))
paste(L1," <= mu_d1 <= ",U1)

L2 = dbar[2,1]-(sqrt(((n-1)*2/(n-p))*qf(1-alpha,p,n-p))*sqrt(sd[2,2]/n))
U2 = dbar[2,1]+(sqrt(((n-1)*2/(n-p))*qf(1-alpha,p,n-p))*sqrt(sd[2,2]/n))
paste(L2," <= mu_d1 <= ",U2)

L1 = dbar[1,1]-(qt(1-(alpha/(2*p)),n-1)*sqrt(sd[1,1]/n))
U1 = dbar[1,1]+(qt(1-(alpha/(2*p)),n-1)*sqrt(sd[1,1]/n))
paste(L1," <= mu_d1 <= ",U1)

L2 = dbar[2,1]-(qt(1-(alpha/(2*p)),n-1)*sqrt(sd[2,2]/n))
U2 = dbar[2,1]+(qt(1-(alpha/(2*p)),n-1)*sqrt(sd[2,2]/n))
paste(L2," <= mu_d1 <= ",U2)

















