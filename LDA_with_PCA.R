# LDA with PCA
setwd("/Users/thunchanok/Documents/SC637703")

##นำเข้าข้อมูล
rice = read.csv("riceClassification.csv")

###Data Explor
library('DataExplorer')
head(rice)
summary(rice)
introduce(rice) 

#ลบตัวแปร ID ออกเนื่องจากไม่ได้ใช้ในการวิเคราะห์
rice_new = rice[,2:12]
head(rice_new)
introduce(rice_new)

### Split data into X and Y
X = rice_new[,1:10]
head(X)

Y = rice_new[,11]
head(Y)

### Check the Correlation
pairs(X, pch = 19, lower.panel = NULL)

# PCA
library("ggplot2")
library("factoextra")
res.pca = prcomp(X,scale = TRUE)

#แสดงผล PCA
print(summary(res.pca)) ## เลือก 2 PC เนื่องจาก 2 PC Cumulative proportion > 0.8

#สร้างตัวแปรเก็บค่าต่างๆของ PCA
Sd_PC = res.pca$sdev
eigval = Sd_PC^2
eigvec = res.pca$rotation
eigval_mean = mean(eigval)
#แสดงค่าeigenvalue
eigval

#สร้างกราฟ วงกลม 1 หน่วย
fviz_pca_var(res.pca,col.var="cos2", gradient.cols=c("#A1D0BE","#F68D71","#90A9D7"))

#สร้างตัวแปรเก็บค่า ความแปรปรวนของ PCA
var = get_pca_var(res.pca)

#สร้างกราฟความสัมพันธ์ระหว่าง
#เรียกใช้งาน Library ที่ใช้ในการสร้างกราฟ
library(corrplot)

#สร้าง corrplot
corrplot(var$cos2,method = "color", type = "full",  diag = TRUE,tl.col = "black", 
         bg ="white", col = NULL)       

#จะได้
PCA = res.pca$x[,1:2]

## สร้าง df ใหม่ที่รวม X_train และ Y_train
PCA_X_data <- cbind(PCA,Y)


## เปลี่ยนชื่อ Y_train เป็น Class
colnames(PCA_X_data)[colnames(PCA_X_data)=="Y"] = "Class"

head(PCA_X_data)

## ทำให้เป็นdata frame จะได้ data ใหม่
PCA_data = as.data.frame(PCA_X_data)
head(PCA_data)

### Split data into training (80%) and test (20%) data
library(caret)
library(dplyr) 
set.seed(555)
PCA_training_samples = PCA_data$Class %>% createDataPartition(p = 0.8, list = FALSE)

PCA_train_data = PCA_data[PCA_training_samples, ]
head(PCA_train_data)

PCA_test_data = PCA_data[-PCA_training_samples, ]
head(PCA_test_data)


##LDA with PCA data
#Fit the LDA Model
library(MASS)
model = lda(Class~., PCA_train_data)

#view model output
model

#use LDA model to make predictions on test data
predicted = predict(model, PCA_test_data)


#view predicted class
names(predicted)

#view linear discriminants
head(predicted$x)
head(predicted$class)

#find accuracy of model
mean(predicted$class==PCA_test_data$Class)

#Visualize the Results 
ggplot(PCA_test_data, aes(PC1,PC2)) +
  geom_point(aes(color = Class))
