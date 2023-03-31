
#install required packages and utilize libraries
install.packages("devtools")
library(devtools)
install_github("cran/DMwr") #SMOTE algorithm
install.packages("caTools") #Sampling
install.packages("caret") #Grid tuning system
install.packages("rpart") #CART - single trees
install.packages("e1071") #Maching learning application
install.packages("randomForest") #Random forest
install.packages("pROC")
install.packages("SHAP")

library(DMwR)
library(caTools)
library(caret)
library(rpart)
library(e1071)
library(randomForest)
library(pROC)
library(dplyr)
library(ggplot2)
library(rpart.plot)
library(mice)

# code success = 0; failure = 1
clean_data_RF$B_Outcome <- ifelse(clean_data_RF$B_Outcome == "Success",0,1)
train$B_Outcome <- ifelse(train$B_Outcome == "Success",0,1)
test$B_Outcome <- ifelse(test$B_Outcome == "Success",0,1)

#Set the scale of dataset samples for each columns
clean_data_RF$B_Outcome <- as.factor(clean_data_RF$B_Outcome)
clean_data_RF$P_Work_Exp <- as.factor(clean_data_RF$P_Work_Exp)
clean_data_RF$P_Startup_Exp <- as.factor(clean_data_RF$P_Startup_Exp)
clean_data_RF$Num_Startups <- as.numeric(clean_data_RF$Num_Startups)

#Data preparation
#1.) Impute missing values using random forest
set.seed(150)

impute_data<- mice(data_set_RF, m=5, method="rf")
clean_data_RF <- complete(impute_data, 5)

#2.) Data splitting
sample =sample.split(clean_data_RF, SplitRatio = .5)
train = subset(clean_data_RF, sample == TRUE)
test = subset(clean_data_RF, sample == FALSE)

#Set the scale of training and testing samples for each columns
#Note: this is optional since this was performed earlier from the sample dataset
train$B_Outcome <- as.factor(train$B_Outcome)
train$P_Work_Exp <- as.factor(train$P_Work_Exp)
train$P_Startup_Exp <- as.factor(train$P_Startup_Exp)
train$Num_Startups <- as.numeric(train$Num_Startups)

test$B_Outcome <- as.factor(test$B_Outcome)
test$P_Work_Exp <- as.factor(test$P_Work_Exp)
test$P_Startup_Exp <- as.factor(test$P_Startup_Exp)
test$Num_Startups <- as.numeric(test$Num_Startups)

#Decision tree model from RPART
cart_model <- rpart(B_Outcome ~., data = train, method ="class")
plot(cart_model)
text(cart_model, digits = 5)

#Decision tree plot
rpart.plot(cart_model, type = 2, extra = 100, yesno=2, shadow.col = "gray")

#Random forest classification model
control <- trainControl(method = "repeatedcv", number = 5, repeats = 2)
metric = "Accuracy"
tuneGrid = expand.grid(.mtry = c(1:10))

rf_model <- train(B_Outcome ~.,
                  data = train,
                  method = "rf",
                  metric = metric,
                  tuneGrid = tuneGrid,
                  trControl = control,
                  importance = TRUE)

#random forest accuracy graph
ggplot(rf_model)

#OOB Error estimation graph from the random forest classification model
layout(matrix(c(1,2),nrow=1),
       width=c(2.5,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf_model$finalModel, log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf_model$finalModel$err.rate),col=1:4,cex=0.8,fill=1:4)

#Variable importance
varImp(rf_model)

importance(rf_model$finalModel)

#Random forest predictive model
pred_rf <- predict(rf_model, test)

#Confusion Matrix from the random forest predictive model
confusionMatrix(data = pred_rf, reference = test$B_Outcome)

#Random forest predictive model for ROC-AUC
prediction_for_ROC <- predict(rf_model, test, type = "prob")

#ROC from success class prediction probabilities (column probabilities at 1 = success; 2 = failure)
ROC_rf <- roc(test$B_Outcome, prediction_for_ROC[,1], percent = TRUE, digits = 4)
ROC_rf_AUC <- auc(ROC_rf)

#Logistic regression predictive model
log_mod <- glm(B_Outcome ~., data = train, family = 'binomial')
log_pred <- predict(log_mod, test, type = "response")

# Logistic regression predictive model for ROC-AUC
ROC_log <- roc(test$B_Outcome, log_pred, percent = TRUE, digits = 4)
ROC_log_auc <- auc(ROC_log)

#Plotting ROC curves from two predictive models
plot(ROC_rf, col = "black", legacy.axes = TRUE, print.auc = TRUE)
plot(ROC_log, col = "dark gray", add = TRUE, print.auc = TRUE, print.auc.y = 40)

legend("bottomright", legend = c("Random Forest", "Logistic Regression"),
       col = c("black", "dark gray"), lwd = 4)

paste("Area Under Curve of Random Forest: ", ROC_rf_AUC)
paste("Area Under Curve of Logistic Regression: ", ROC_log_auc)

#Class probabilities of success in the random forest predictive model
roc.df <- data.frame(
  TPP = ROC_rf$sensitivities,
  FPP = (100 - ROC_rf$specificities),
  Thresholds = ROC_rf$thresholds)

#Log-odds probabilities of logistic regression predictive model
roc.df <- data.frame(
  TPP = ROC_log$sensitivities,
  FPP = (100 - ROC_log$specificities),
  Thresholds = ROC_log$thresholds)


# Summary statistics (without missing values imputation)
 #Prior work experience
TAB1 <- table(clean_data_RF$B_Outcome, clean_data_RF$P_Work_Exp)
FRQ1 <- prop.table(TAB1)*100
FRQ1

 #Prior startup experience
TAB2 <- table(clean_data_RF$B_Outcome, clean_data_RF$P_Startup_Exp)
FRQ2 <- prop.table(TAB2)*100
FRQ2

 #Number of previously founded startups
TAB3 <- table(clean_data_RF$B_Outcome, clean_data_RF$Num_Startups)
FRQ3 <- prop.table(TAB3)*100
FRQ3

