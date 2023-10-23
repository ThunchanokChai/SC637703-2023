###14/07/2023
##CH2
## Data exploration by DataExplorer package 
setwd("/Users/thunchanok/Documents/SC637703/Data")
install.packages('DataExplorer')
library('DataExplorer')

Data=read.csv("CreditApproval.csv")
Data

results = apply(is.na(Data),2,which) #Check missing data

introduce(Data) #สรุปๆของdata
plot_missing(Data) #show % of missing Variable 

###Filter out the missing values by drop_na
library(tidyr)
DataC = drop_na(Data) #Data1 that already drop NA

plot_bar(Data[,c('A1','A4','A5')]) 
plot_scatterplot(Data[,c('A2','A3')],by='A2')

Data1= Data[1:200,c('A1','A4','A5')]

summary(Data)

plot_histogram(Data[,'A8'])

#### Simulating multivariate normal distribution 
install.packages("MVN")
library("MASS")
library("ggplot2")
library("MVN")

# Ref: https://www.rdocumentation.org/packages/rockchalk/versions/1.8.110/topics/mvrnorm 
# mvrnorm(n , mu, Sigma[, tol = 1e-06, empirical = FALSE]) 
# From # MASS package.   
# ======== Input:   
# n =>  a number of samples.   
# mu => a mean vector of variables.   
# Sigma => a covariance matrix.   
# tol => tolerance (relative to largest variance)             
# for numerical lack of positive-definiteness in Sigma.   
# empirical => logical. If true, mu and Sigma specify               
# the empirical not population mean and covariance matrix.

n = 100 # set a number of samples
Mu= c(0,0) # create a zero mean vector
sigma =  matrix(c(10,3,3,2),2,2)
sigma
set.seed(1)
data1 = mvrnorm(n,Mu,sigma)

#Normality test for multivariate variables
# H0 : data1 is multivariate normal dist. 
#VS H1 : data1 is not multivariate normal dist.
#For conclustion, we will reject H0 if p-value < 0.05 
results = mvn(data1,mvnTest = 'energy', univariatePlot = 'qqplot') 
results = mvn(data1,mvnTest = 'energy', multivariatePlot = 'qq') 

#qq plot คือ แกน Y คือ ค่าจริง แกน X คือ 
#ทฤษฎี ถ้ากราฟเป็นไปตามเส้นแปลว่ามันเป็นไปในทางเดียวกัน(เป็นการดูการแจกแจงว่ามันเป็นnormal dis. หรือไม่)

results 
#the results show that p-value > 0.05 so don't reject H0

#iris data
iris = read.csv('iris_xcel.csv')
data2 = iris[,c(1:4)]
plot_qq(data2)
results = mvn(data2,mvnTest = 'energy', univariatePlot = 'qqplot') 
results = mvn(data2,mvnTest = 'energy', multivariatePlot = 'qq') 
results = mvn(data2,mvnTest = 'energy')
results




