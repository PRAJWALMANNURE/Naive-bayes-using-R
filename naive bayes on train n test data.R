# load the data
# already the train and test files were given 
train_data <- read.csv(choose.files()) 
test_data <- read.csv(choose.files())

#EDA
summary(train_data)
summary(test_data)
boxplot(train_data)
boxplot(test_data)

colnames(train_data)
colnames(test_data)
sal_train <- ifelse(train_data$Salary==' >50K','high','low')
sal_test <- ifelse(test_data$Salary==' >50K','high','low')

train_data <- data.frame(train_data,sal_train)
test_data <- data.frame(test_data,sal_test)

#train and test data
train_data <- (train_data[-14]) # excluding the salary column for analysis
test_data <- (test_data[-14])


#model creation
library(e1071) # for naive bayes function 
library(caret)

model <- naiveBayes(sal_train~.-educationno-maritalstatus-relationship-capitalloss
                    -hoursperweek-native,data = train_data)
#as some of the attributes were not important in classification, hence they were excluded
model

pred <- predict(model,test_data) # prediction on test data
confusionMatrix(table(pred,test_data$sal_test)) #  Accuracy : 0.7891
