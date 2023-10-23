##CH5
### PCA in R 13/09/2023
### install package 
### data
install.packages(c("ggplot2","factoextra"))
library(ggplot2)
library(factoextra)

### calling the data set
data("decathlon2")
head("decathlon2")

### select the data that use PCA
decathlon2_active = decathlon2[1:23, 1:10]
head(decathlon2_active[, 1:6], 4)
#### you have to know the meaning of the data that you use 

### Explore data
## plot 
pairs(decathlon2_active[,1:10], pch = 19, lower.panel = NULL) 
#### pch = graph size , lower.panel = not show loer past

### Conducting PCA
res.pca = prcomp(decathlon2_active, scale = TRUE)
print(summary(res.pca))
### scale = true is ....

eigenvec = res.pca$rotation[1:3,]
eigenvec

SD_e = res.pca$sdev
eigenvalue = SD_e^2
eigenvalue

#### 100m and PC2 = 0.132306
eigenvec[1,2] ###eigen

res.pca$sdev[2]

eigenvec[1,2]*res.pca$sdev[2]

### data new space 
new_coor = predict(res.pca,newdata = decathlon2_active )
new_coor[,1:4]

### find mean of eigenvalue 
mean_eigen = mean(eigenvalue)

mean_eigen


res_eig = get_eigenvalue(res.pca)
res_eig$eigenvalue

#new_coor in 3 dim
new_coor[1:3,1:4]
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

## 20/09/23
var = get_pca_var(res.pca) #get variable 
var$coord    #to create scatter plot


#eigen value 
SD_e[1:1]
var$coord[1:1]

#e11 จาก eigen vector (res.pca$rotation)
e11 = res.pca$rotation[1,1]
e11

correlation_1 = (e11)*(res.pca$sdev[1]) #Pc1 and X

correlation_1

e12 = res.pca$rotation[2,1]
e12

correlation_2 = (e12)*(res.pca$sdev[1]) #Pc1 and X

correlation_2

B = res.pca$rotation[,1]

correlation_pc1 = B*res.pca$sdev[1]
correlation_pc1

#####Correlation circle
fviz_pca_var(res.pca, col.var = "black") #plot 2 dimention


#### Corrplot
library("corrplot")
corrplot(var$cos2, is.corr = FALSE) #use coor^2

###Individuals Graph
ind = get_pca_ind(res.pca)  #individuals value
fviz_pca_ind(res.pca, col.ind ="cos2",gradient.cols = c("#00AFBB","#E7B800","#FC4E07")
             ,repel =TRUE)# Avoid text overlapping (slow if many points)


### show the coord for individual 
ind$coord
head(ind$coord)
res.pca$x   # x = new coord of the new variable


#how to apply PCA to the new points
ind.sup = decathlon2[24:27,1:10]
ind.sup.coord = predict(res.pca,newdata=ind.sup)
ind.sup.coord
