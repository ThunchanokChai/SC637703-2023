#LOGISTIC REGRESSION
## Import Data
rice = read.csv("riceClassification.csv")

###Explor data
library('DataExplorer')
head(rice)
summary(rice)
introduce(rice) 

#ลบตัวแปร ID ออกเนื่องจากไม่ได้ใช้ในการวิเคราะห์
rice_new = rice[,2:12]
head(rice_new)
introduce(rice_new)

### Split data into training (80%) and test (20%) data
library(caret)
library(dplyr) 
set.seed(555)
training_samples = rice_new$Class %>% createDataPartition(p = 0.8, list = FALSE)

train_data = rice_new[training_samples, ]
head(train_data)

test_data = rice_new[-training_samples, ]
head(test_data)

## LOGISTIC REGRESSION - FULL MODEL
library(MASS)
full_model = glm(Class ~., data= train_data, family = binomial)

summary(full_model)

## LOGISTIC REGRESSION - STEPWISE MODEL
stepwise_model = glm(Class ~., data= train_data, family = binomial) %>% stepAIC(trace = FALSE)

summary(stepwise_model)


## COMPARE FULL and STEPWISE MODEL
### Full Model
#### Prediction of Probability
prob_full = predict(full_model, test_data, type = "response")
head(prob_full)

#### Predicted Class
predicted_full = ifelse(prob_full > 0.5, "1", "0")
head(predicted_full)

#### Accuracy
mean(predicted_full == test_data$Class)

### Stepwise Model
#### Prediction of Probability
prob_stepwise = predict(stepwise_model, test_data, type = "response")
head(prob_stepwise)

#### Predicted Class
predicted_stepwise = ifelse(prob_stepwise > 0.5, "1", "0")
head(predicted_stepwise)

#### Accuracy
mean(predicted_stepwise == test_data$Class)

####STEPWISE MODEL is better than FULL MODEL

### Goodness of Fit of STEPWISE MODEL
library(performance)
performance_hosmer(stepwise_model, n_bins = 10)

### Assumption and Diagnostic Checking 
#### Linearity
library(tidyverse)
library(ggplot2)
library(broom)
mydata = train_data %>% dplyr :: select_if(is.numeric)

probabilities_stepwise = predict(stepwise_model, train_data, type = "response")
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


