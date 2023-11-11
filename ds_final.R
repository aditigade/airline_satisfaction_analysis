#FRESH 

#importing----
getwd()
setwd("D:/data science")
f = read.csv("train.csv")

#Handling missing data----
cat("Before handling missing values \n")
na <- sum(is.na(f))
print(na)
colna <- colSums(is.na(f))
print(colna)
f <- replace(f, is.na(f), 0)
cat("After handling missing values \n")
na <- sum(is.na(f))
print(na)

#converting into factor----
Gender_levels <- c("Male", "Female")
f$Gender <- factor(f$Gender,levels = Gender_levels)

Customer.Type_levels <- c("Loyal Customer", "disloyal Customer")
f$Customer.Type <- factor(f$Customer.Type, levels = Customer.Type_levels)

Type.of.Travel_levels <- c("Personal Travel", "Business travel")
f$Type.of.Travel <- factor(f$Type.of.Travel, levels = Type.of.Travel_levels)

f$Class <-as.factor(f$Class)

f$satisfaction <-as.factor(f$satisfaction)

for (x in 1:103904){
  if (f$Arrival.Delay.in.Minutes[x] > 0) {
    f$Arrival.Delay.in.Minutes[x] <- 1
  }
}

f$Arrival.Delay.in.Minutes <- as.factor(f$Arrival.Delay.in.Minutes)

for (x in 1:103904){
  if (f$Departure.Delay.in.Minutes[x] > 0) {
    f$Departure.Delay.in.Minutes[x] <- 1
  }
}

f$Departure.Delay.in.Minutes <- as.factor(f$Departure.Delay.in.Minutes)

f = subset(f, select = -c(X,id,Gender,Age) )

cat("Class Distribution in the dataset")
#counts the occurrences of each unique value in the "satisfaction" column
class_distribution <- table(f$satisfaction) 
# Print the class distribution
print(class_distribution)

#Splitting into train and test---- 
library(caret)
set.seed(100)
sample <- sample(c(TRUE, FALSE), nrow(f), replace=TRUE, prob=c(0.8,0.2))
train  <- f[sample, ]
test   <- f[!sample, ]

cat("Class Distribution in train")
#counts the occurrences of each unique value in the "satisfaction" column
class_distribution <- table(train$satisfaction) 
# Print the class distribution
print(class_distribution)

#undersampling----
neutral.or.dissatisfied <- which(train$satisfaction == "neutral or dissatisfied")
satisfied <- which(train$satisfaction == "satisfied")

# if you want all elements of the smaller class, could be:
nsamp <- min(length(neutral.or.dissatisfied), length(satisfied))

pick_neutral.or.dissatisfied <- sample(neutral.or.dissatisfied, nsamp)
pick_satisfied <- sample(satisfied, nsamp)

new_train <- train[c(pick_neutral.or.dissatisfied, pick_satisfied), ]

cat("Class distribution after undersampling \n")
#counts the occurrences of each unique value in the "satisfaction" column
class_distribution <- table(new_train$satisfaction) 
# Print the class distribution
print(class_distribution)


#standardization----
#mean is close to 0 and standard deviation is 1 
train_standardized <- new_train
# applying scale function
train_standardized[4 : 18] <- as.data.frame(scale(train_standardized[4 : 18]))
train_standardized <- train_standardized[-21]

test_standardized <- test
# applying scale function
test_standardized[4 : 18] <- as.data.frame(scale(test_standardized[4 : 18]))
test_standardized <-test_standardized[-21]

# #SVM----
# library(e1071)
# 
# svm_train_standardized <- new_train
# # applying scale function
# svm_train_standardized[4 : 18] <- as.data.frame(scale(train_standardized[4 : 18]))
# 
# classifier = svm(formula = satisfaction ~ .,
#                  data = svm_train_standardized,
#                  kernel = 'linear',
#                  type = 'nu-classification')
# 
# print(classifier)

#KNN----
library(class)

knntrain_standardized <- train_standardized
knntrain_standardized$Customer.Type <- as.integer(knntrain_standardized$Customer.Type)
knntrain_standardized$Type.of.Travel <- as.integer(knntrain_standardized$Type.of.Travel)
knntrain_standardized$Class <- as.integer(knntrain_standardized$Class)
knntrain_standardized$Departure.Delay.in.Minutes <- as.integer(knntrain_standardized$Departure.Delay.in.Minutes)
knntrain_standardized$Arrival.Delay.in.Minutes <- as.integer(knntrain_standardized$Arrival.Delay.in.Minutes)

knntest_standardized <- test_standardized
knntest_standardized$Customer.Type <- as.integer(knntest_standardized$Customer.Type)
knntest_standardized$Type.of.Travel <- as.integer(knntest_standardized$Type.of.Travel)
knntest_standardized$Class <- as.integer(knntest_standardized$Class)
knntest_standardized$Departure.Delay.in.Minutes <- as.integer(knntest_standardized$Departure.Delay.in.Minutes)
knntest_standardized$Arrival.Delay.in.Minutes <- as.integer(knntest_standardized$Arrival.Delay.in.Minutes)

knnnew_train <- new_train
knnnew_train$Customer.Type <- as.integer(knnnew_train$Customer.Type)
knnnew_train$Type.of.Travel <- as.integer(knnnew_train$Type.of.Travel)
knnnew_train$Class <- as.integer(knnnew_train$Class)
knnnew_train$Departure.Delay.in.Minutes <- as.integer(knnnew_train$Departure.Delay.in.Minutes)
knnnew_train$Arrival.Delay.in.Minutes <- as.integer(knnnew_train$Arrival.Delay.in.Minutes)

actual <- test$satisfaction

knn_model_7 <- knn( train = knntrain_standardized,
                    test = knntest_standardized,
                    cl = knnnew_train$satisfaction,
                    k  = 7)

actual <- test$satisfaction

cm <-table(actual, knn_model_7)
cm

accuracy <- sum(diag(cm))/length(actual)
sprintf("Accuracy: %.2f%%", accuracy*100)

knn_model_9 <- knn( train = knntrain_standardized,
                    test = knntest_standardized,
                    cl = knnnew_train$satisfaction,
                    k  = 9)

actual <- test$satisfaction

cm <-table(actual, knn_model_9)
cm

accuracy <- sum(diag(cm))/length(actual)
sprintf("Accuracy: %.2f%%", accuracy*100)


#Naive Bayes----
library(naivebayes)
nb_train_standardized <- new_train
# applying scale function
nb_train_standardized[4 : 18] <- as.data.frame(scale(train_standardized[4 : 18]))
naive_bayes_model <- naive_bayes(satisfaction ~ ., data = nb_train_standardized, usekernel = T)
predictions <- predict(naive_bayes_model, newdata = test_standardized, what = "response")
cm <- table(test$satisfaction, predictions)
cm
accuracy <- sum(diag(cm))/length(test$satisfaction)
sprintf("Accuracy: %.2f%%", accuracy*100)


#Logistic Regression----
logreg_train_standardized <- new_train
# applying scale function
logreg_train_standardized[4 : 18] <- as.data.frame(scale(train_standardized[4 : 18]))

logreg = glm(satisfaction ~ .,
             data = logreg_train_standardized,
             family = "binomial")

logreg_pred = predict.glm(logreg,
                          newdata = test_standardized,
                          what = "response")

logreg_pred2 = ifelse(logreg_pred > 0.5, "Yes", "No")
head(logreg_pred2)

cm <- table(logreg_pred2, test$satisfaction)
cm
accuracy <- sum(diag(cm))/length(test$satisfaction)
sprintf("Accuracy: %.2f%%", accuracy*100)


#
#Decision Trees----
library(rpart)
library(rpart.plot)
fit <- rpart(satisfaction~., data = new_train, method = 'class')
#extra=106 class model with a binary response
rpart.plot(fit, extra = 106)
predict_unseen <-predict(fit, test, type = 'class')

cm <- table(test$satisfaction, predict_unseen)
cm
accuracy <- sum(diag(cm))/length(test$satisfaction)
sprintf("Accuracy: %.2f%%", accuracy*100)

#Random Forest----
library(ggplot2)
library(cowplot)
library(randomForest)
# Train a Random Forest model
model1 <- randomForest(satisfaction ~ ., data = new_train, ntree = 100, mtry = 4, importance = TRUE)
# Make binary predictions
binary_predictions <- predict(model1, newdata = test[-21], type = "response")
# binary_predictions <- ifelse(binary_predictions > 0.500, 1, 0)
# Create a confusion matrix
rf_confusion_matrix <- table(actual = test$satisfaction, predicted = binary_predictions)
# Print the results
cat("\nRandom Forest\n")
print(rf_confusion_matrix)
# Calculate accuracy
rf_accuracy <- sum(diag(rf_confusion_matrix)) / sum(rf_confusion_matrix)
cat("\nAccuracy by Random Forest is:", rf_accuracy)