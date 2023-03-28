install.packages("devtools")
library(devtools)
install_github("cran/DMwr") #SMOTE algorithm
install.packages("caTools") #Sampling
install.packages("caret") #Grid tuning system
install.packages("rpart") #CART - single trees
install.packages("e1071") #Maching learning application
install.packages("randomForest") #Random forest

library(DMwR)
library(caTools)
library(caret)
library(rpart)
library(e1071)
library(randomForest)

set.seed(21)

str(clean_data)
counts <- table(clean_data$B_Outcome)
counts
barplot(counts)


clean_data$Num_Startups <- scale(clean_data$Num_Startups)
clean_data$Num_Startups <- numeric(clean_data$Num_Startups)

#library(caTools)
sample =sample.split(clean_data, SplitRatio = .8)
train = subset(clean_data, sample == TRUE)
test = subset(clean_data, sample == FALSE)

install.packages("BAR")
library(BAR)

# Decision Tree
cart_model <- rpart(B_Outcome ~., data = train, method ="class")
plot(cart_model)
text(cart_model, digits = 3)

# Random Forest
control <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
metric = "Accuracy"
tuneGrid = expand.grid(.mtry = c(1:10))


rf_model <- train(B_Outcome ~.,
                  data = train,
                  method = "rf",
                  metric = metric,
                  tuneGrid = tuneGrid,
                  trControl = control)

varImp(rf_model)

#Prediction
pred_rf <- predict(rf_model, test)

#Confusion Matrix

confusionMatrix(data = pred_rf, reference = test$B_Outcome)


#ROC Curve
library(pROC)

prediction_for_ROC <- predict(rf_model, test, type = "prob")

#Prediction rep. of our success cases

ROC_rf <- roc(test$B_Outcome, prediction_for_ROC[,2])
ROC_rf_AUC <- auc(ROC_rf)

##############################################################

#Compare this with a logistic regression ....

log_mod <- glm(B_Outcome ~., data = train, family = 'binomial')
log_pred <- predict(log_mod, test, type = "response")

# Logistic regression

ROC_log <- roc(test$B_Outcome, log_pred)
ROC_log_auc <- auc(ROC_log)


###

plot(ROC_rf, main = "ROC for RF (GREEN) vs LG for (RED)", col = "green")
lines(ROC_log, col ="red")
paste("Area Under Curve of Random Forest: ", ROC_rf_AUC)
paste("Area Under Curve of Logistic Regression: ", ROC_log_auc)



