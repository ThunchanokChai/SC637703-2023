# LDA without PCA
##นำเข้าข้อมูล
rice = read.csv("riceClassification.csv")

##Data Explor
library(DataExplorer)
head(rice)
summary(rice)
introduce(rice) 

#ลบตัวแปร ID ออกเนื่องจากไม่ได้ใช้ในการวิเคราะห์
rice_new = rice[,2:12]
introduce(rice_new)

# Split the data into training (80%) and test set (20%)
library(caret)
library(dplyr)
set.seed(555)
training.samples = rice_new$Class %>% createDataPartition(p = 0.8, list = FALSE)
train_data = rice_new[training.samples, ]
head(train_data)
test_data = rice_new[-training.samples, ]
head(test_data)

#Fit the LDA Model
library(MASS)
model = lda(Class~., data=train_data)

#view model output
model

#use LDA model to make predictions on test data
predicted = predict(model, test_data)


#view predicted class
names(predicted)

#view linear discriminants
head(predicted$x)
head(predicted$class)

#find accuracy of model
mean(predicted$class==test_data$Class)

#Visualize the Results 
library(ggplot2)
ggplot(test_data, aes(Area,MajorAxisLength)) +
  geom_point(aes(color = Class))

