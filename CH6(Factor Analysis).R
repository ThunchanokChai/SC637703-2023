##CH6
##Factor Analysis
#Install packages
install.packages('psych')
install.packages('GPArotation')
library(psych)
library(GPArotation)
library(readr)

#1 Check that we can apply FA to this data
#Create Data
EFA <- read_csv('EFA.csv')
head(EFA)

#Kaiser-Meyer-Olkin (KMO)
datcorr = cor(EFA)
datKMO = KMO(datcorr)
print(datKMO)
print(datKMO$MSA)

#Bartlett’s Sphericity
#H0 : variable no corelation (independent)
#H1 : variable corelation (dependent)

datcorr = cor(EFA)
datKMO =KMO(datcorr)
n=90
datBart=cortest.bartlett(datcorr,n) #Bartlett’s Sphericity
print(datBart$p.value)

#2 Determine the number of factors
datEig = eigen(datcorr)
print(datEig$values)

datEigmean = mean(datEig$values)
print(datEigmean)

#3 Extract Factors
#Extract Factors by principal component method
datFa_p = principal(datcorr,nfactors=4,rotate="none")
print(datFa_p)

#Extract Factors by principal factor method
datFa_f = fa(datcorr,nfactors=4,rotate="none",fm = 'pa')
print(datFa_f)

#Extract Factors by Maximum likelihood method
datFa_m = fa(datcorr,nfactors=4,rotate="none",fm = 'ml')
print(datFa_m)

#Rotation by vairmax methond
#fa(r,nfactors = #factors , rotate= Rorationtyp, fm = method)
datFa_p = principal(datcorr,nfactors=4,rotate="varimax")
print(datFa_p)
