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
#pairs(X, pch = 19, lower.panel = NULL)

# PCA
library("ggplot2")
library("factoextra")
res.pca = prcomp(X,scale = TRUE)

#แสดงผล PCA
print(summary(res.pca)) ## เลือก 2 PC เนื่องจาก 2 PC Cumulative proportion > 0.8

#สร้างตัวแปรเก็บค่าต่างๆของ PCA
#Sd_PC = res.pca$sdev
#eigval = Sd_PC^2
#eigvec = res.pca$rotation
#eigval_mean = mean(eigval)
#แสดงค่าeigenvalue
#eigval

#สร้างกราฟ วงกลม 1 หน่วย
#fviz_pca_var(res.pca,col.var="cos2", gradient.cols=c("#A1D0BE","#F68D71","#90A9D7"))

#สร้างตัวแปรเก็บค่า ความแปรปรวนของ PCA
#var = get_pca_var(res.pca)

#สร้างกราฟความสัมพันธ์ระหว่าง
#เรียกใช้งาน Library ที่ใช้ในการสร้างกราฟ
#library(corrplot)

#สร้าง corrplot
#corrplot(var$cos2,method = "color", type = "full",  diag = TRUE,tl.col = "black", 
#         bg ="white", col = NULL)       

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


##Logistic regression with PCA data
## LOGISTIC REGRESSION - FULL MODEL
library(MASS)
full_model = glm(Class ~., data = PCA_train_data, family = binomial)

summary(full_model)

## LOGISTIC REGRESSION - STEPWISE MODEL
stepwise_model = glm(Class ~., data= PCA_train_data, family = binomial) %>% stepAIC(trace = FALSE)

summary(stepwise_model)


## COMPARE FULL and STEPWISE MODEL
### Full Model
#### Prediction of Probability
prob_full = predict(full_model, PCA_test_data , type = "response")
head(prob_full)

#### Predicted Class
predicted_full = ifelse(prob_full > 0.5, "1", "0")
head(predicted_full)

#### Accuracy
mean(predicted_full == PCA_test_data$Class)

### Stepwise Model
#### Prediction of Probability
prob_stepwise = predict(stepwise_model, PCA_test_data, type = "response")
head(prob_stepwise)

#### Predicted Class
predicted_stepwise = ifelse(prob_stepwise > 0.5, "1", "0")
head(predicted_stepwise)

#### Accuracy
mean(predicted_stepwise == PCA_test_data$Class)

####STEPWISE MODEL is better than FULL MODEL

### Goodness of Fit of STEPWISE MODEL
library(performance)
performance_hosmer(stepwise_model, n_bins = 10)

### Assumption and Diagnostic Checking 
#### Linearity
library(tidyverse)
library(ggplot2)
library(broom)
mydata = PCA_train_data %>% dplyr :: select_if(is.numeric)

probabilities_stepwise = predict(stepwise_model, PCA_train_data, type = "response")
logit_sw = log(probabilities_stepwise/(1 - probabilities_stepwise))
mydata2 = mydata %>% mutate(logit_sw) %>% gather(key = "predictors", value = "predictors.value",-logit_sw)

head(mydata2,10)

ggplot(mydata2, aes(logit_sw,predictors.value)) + geom_point(size = 0.5, alpha = 0.5) + geom_smooth(method="loess") + theme_bw() + facet_wrap(~predictors,scales="free_y")

#### Influential Values 
model_sw_data = augment(stepwise_model) %>% mutate(index=1:n())

model_sw_data %>% top_n(3,.cooksd)

ggplot(model_sw_data,aes(index,.std.resid)) + geom_point(aes(color = Class),alpha = 5) + theme_bw()

#### Multicolinearity 
library(car)
vif(stepwise_model)

## ODD RATIO 
model.OR <- exp(stepwise_model$coefficients)
round(model.OR, 3)


