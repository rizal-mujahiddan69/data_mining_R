library(e1071)
library(caret)

data_raisin <- read.csv("Raisin_Dataset.csv")
data_raisin$Class <- as.factor(data_raisin$Class)

set.seed(123)
ind <- sample(2,nrow(data_raisin),
              replace = TRUE, prob =c(0.7,0.3))

trainData <- data_raisin[ind == 1,]
testData <- data_raisin[ind == 2,]

write.csv(trainData,"trainData_LKP8.csv")
write.csv(testData,"testData_LKP8.csv")

svm_model <- svm(Class ~ . , data=trainData,gamma=10,cost=1)
svm_pred <- predict(svm_model,testData)
confuse <- confusionMatrix(svm_pred,testData$Class)
confuse