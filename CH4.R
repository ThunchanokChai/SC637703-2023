#CH4
#Comparison of Several Multivariate Means
##Comparing Several Multivariate Population Means (One-Way MANOVA)
#Ex4.1
#input part
X1bar = matrix(c(2.006,0.48,0.082,0.36),4,1)
X2bar = matrix(c(2.167,0.596,0.124,0.418),4,1)
X3bar = matrix(c(2.273,0.521,0.125,0.383),4,1)
S1 = matrix(c(0.291,-0.001,0.002,0.01,-0.001,0.011,
            0.001,0.003,0.002,0.001,0.001,0.001,
            0.01,0.003,0.001,0.01),4,4)
S2 = matrix(c(0.561,0.011,0.001,0.037,0.011,0.025,
            0.004,0.007,0.001,0.004,0.005,0.002,
            0.037,0.007,0.002,0.019),4,4)
S3 = matrix(c(0.261,0.03,0.003,0.018,0.03,0.017,
            -0.001,0.006,0.003,-0.001,0.004,
            0.001,0.018,0.006,0.001,0.013),4,4)
n1 = 271
n2 = 138 
n3 = 107
##computing part
W = (n1-1)*S1 + (n2-1)*S2 + (n3-1)*S3
W
xbar = (n1*X1bar+n2*X2bar+n3*X3bar)/(n1+n2+n3)
xbar
B = (n1*(X1bar - xbar)%*%t(X1bar - xbar)) + (n2*(X2bar - xbar)%*%t(X2bar - xbar)) 
+ (n3*(X3bar - xbar)%*%t(X3bar - xbar)) 
B

p = 4
lamda = det(W)/det(B+W)
lamda

comp_val = ((n1 + n2 + n3 - p - 2) / p)*((1 - sqrt(lamda)) / sqrt(lamda))
comp_val

qf(1 - 0.05,2*p,2*(n1+n2+n3))
# 18.62007 > 1.947359 so Reject H0

#EX4.2
#input part
X1 = matrix(c(21,25,20,24,12,8,12,10),4,2)
X2 = matrix(c(31,23,24,28,9,12,13,10),4,2)
X3 = matrix(c(34,29,35,32,10,14,11,13),4,2)
X4 = matrix(c(33,38,34,35,14,12,13,13),4,2)

X1mean = colMeans(X1)
paste('X1mean[1] = ',X1mean[1],' || X1mean[2] = ',X1mean[2],sep = "")
X2mean = colMeans(X2)
paste('X2mean[1] = ',X2mean[1],' || X2mean[2] = ',X2mean[2],sep = "")
X3mean = colMeans(X3)
paste('X3mean[1] = ',X3mean[1],' || X3mean[2] = ',X3mean[2],sep = "")
X4mean = colMeans(X4)
paste('X4mean[1] = ',X4mean[1],' || X4mean[2] = ',X4mean[2],sep = "")

n1 = length(X1[,1])
n2 = length(X2[,1])
n3 = length(X3[,1])
n4 = length(X4[,1])
p = length(X1[1,])
S1 = cov(X1)
S2 = cov(X2)
S3 = cov(X3)
S4 = cov(X4)
W = (n1-1)*S1 + (n2-1)*S2 + (n3-1)*S3 + (n4-1)*S4
print(W)
xbar = (n1*X1mean + n2*X2mean + n3*X3mean + n4*X4mean)/(n1+n2+n3+n4)
xbar
B = (n1*(X1mean-xbar) %*% t(X1mean-xbar)) + (n2*(X2mean-xbar) %*% t(X2mean-xbar)) +
  (n3*(X3mean-xbar) %*% t(X3mean-xbar)) + (n4*(X4mean-xbar) %*% t(X4mean-xbar))
print(B)

Lamda = det(W)/det(B+W)
paste("Lamda = ",Lamda)

statistic = (((n1+n2+n3+n4)-4-1)/(4-1))*((1-sqrt(Lamda))/sqrt(Lamda))
paste("statistic = ",statistic)

cri_val = qf(1-alpha,6,27)
paste("Critical value = ",cri_val)


## Simultaneous Confidence Intervals for Treatment Effects
#EX4.3
#pairwise comparisons, the Bonferroni 
#show that 
#H0: mu1=mu2 ; H1: mu1=!mu2
#H0: mu1=mu3 ; H1: mu1=!mu3
#H0: mu2=mu3 ; H1: mu2=!mu3
#find Mu1 ,Mu2 ,M3
Mu1 = X1bar - xbar
Mu1

Mu2 = X2bar - xbar
Mu2

Mu3 = X3bar - xbar
Mu3

#H0: mu1=mu3 ; H1: mu1=!mu3
#show that 
Mu1
Mu3
W

alpha = 0.05 
m=p
n=n1+n2+n3
p = 4 
g = 3 
t_value=qt((alpha/(p*g*(g-1))), 513, lower.tail=FALSE)
t_value

#H0: mu1=mu3 ; H1: mu1=!mu3
diff_Mu1_M3 = Mu1 - Mu3
diff_Mu1_M3
diff_Mu1_M3[3,1]
W[3,3]
A = t_value * sqrt((1.379/(n-g))*((1/n1)+(1/n3)))
Confidence_up_13_33 = -0.043 + A
Confidence_up_13_33
Confidence_low_13_33 = -0.043 - A
Confidence_low_13_33
#The 95% simultaneous confidence of Mu1 and Mu3 (-0.06,-0.02)
#so there is  different 

#H0: mu1=mu2 ; H1: mu1=!mu2
diff_Mu1_M2 = Mu1 - Mu2
diff_Mu1_M2
b = t_value * sqrt((1.379 / (n-g)) * ((1/n1)+(1/n2)))
Confidence_up_13_23 = diff_Mu1_M2[3,1] + b
Confidence_up_13_23
Confidence_low_13_23 = diff_Mu1_M2[3,1] - b
Confidence_low_13_23
#The 95% simultaneous confidence of Mu1 and Mu2 (-0.05,-0.02)
#so there is different 

#H0: mu2=mu3 ; H1: mu2=!mu3
diff_Mu2_M3 = Mu2 - Mu3
diff_Mu2_M3
c = t_value * sqrt((1.379/(n-g)) * ((1/n2) + (1/n3)))
Confidence_up_23_33 = diff_Mu2_M3[3,1] + c
Confidence_up_23_33
Confidence_low_23_33 = diff_Mu2_M3[3,1] - c
Confidence_low_23_33
#The 95% simultaneous confidence of Mu2 and Mu3 (-0.02,-0.018)
#so there is no different 







