#input dataset
train <- read_csv("C:/Kaggle/Titanic/Titanic/Data/train.csv")
test <- read_csv("C:/Kaggle/Titanic/Titanic/Data/test.csv")
#pretreat of dataset
train$pclass <-factor(train$pclass,levels = c(3, 2, 1), ordered = TRUE)
test$pclass <-factor(test$pclass,levels = c(3, 2, 1), ordered = TRUE)
train$Gender <- ifelse(train$Sex == "male",1,0)
test$Gender <- ifelse(test$Sex == "male",1,0)
train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = TRUE)
test$Age[is.na(test$Age)] <- mean(test$Age, na.rm = TRUE)
train$Embarked_C = ifelse(train$Embarked == "C", 1, 0)
train$Embarked_Q = ifelse(train$Embarked == "Q", 1, 0)
test$Embarked_C = ifelse(test$Embarked == "C", 1, 0)
test$Embarked_Q = ifelse(test$Embarked == "Q", 1, 0)
#predicted model
logistic.model<- glm(Survived~Pclass+Gender+Age+SibSp+Parch+Fare+Embarked_C+Embarked_Q,data=train,family =binomial)
summary(logistic.model)
step.model<-step(logistic.model)
logistic_MSE = mean(logistic.model$residuals^2)
step_MSE = mean(step.model$residuals^2)
#prediction
test_predictions = predict(step.model, test, type = "response")
test$predictions<-predict(step.model,newdata = test,type = "response")
test$survival = ifelse(test$predictions > 0.5, 1, 0)
#Creating CSV for Kaggle Submission
submission <- cbind(test$PassengerId, test$survival)
colnames(submission) <- c("PassengerId", "Survived")
write.csv(submission, file = "C:/Kaggle/Titanic/Titanic/Submission/submission_steplogistic.csv",row.names = FALSE)
 




