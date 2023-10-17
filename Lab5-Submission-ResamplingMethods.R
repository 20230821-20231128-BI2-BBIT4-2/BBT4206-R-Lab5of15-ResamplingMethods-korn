
# STEP 1. Install and Load the Required Packages ----
## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## klaR ----
if (require("klaR")) {
  require("klaR")
} else {
  install.packages("klaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## readr ----
if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## LiblineaR ----
if (require("LiblineaR")) {
  require("LiblineaR")
} else {
  install.packages("LiblineaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

## naivebayes ----
if (require("naivebayes")) {
  require("naivebayes")
} else {
  install.packages("naivebayes", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}

##Install mlbench
install.packages("mlbench")
require("mlbench")
require("caret")
data("PimaIndiansDiabetes")
summary("PimaIndiansDiabetes")

  
  dim(PimaIndiansDiabetes)
  y <- PimaIndiansDiabetes$diabetes ;#focuses on the diabetes column 
  cbind(freq=table(y), percentage=prop.table(table(y))*100);
  
  ##summary of each column in the dataset
  par(mfrow=c(1,4));
  for(i in 1:4){
    hist(PimaIndiansDiabetes[,i], main=names(PimaIndiansDiabetes)[i])
  }
  
  library(lattice)
  #create a layout of simpler density plots by attribute
  par(mfrow=c(1,4))
  for (i in 1:4) {
    plot(density(PimaIndiansDiabetes[,i]), main=names(PimaIndiansDiabetes)[i])}

  par(mfrow=c(1,4))
  for(i in 1:4) {
    boxplot(PimaIndiansDiabetes[,i], main=names(PimaIndiansDiabetes)[i])
  }  

  par(mfrow=c(1,4))
  for(i in 1:4){
    counts <- table(PimaIndiansDiabetes[,i])
    name <-names(PimaIndiansDiabetes)[i]
    barplot(counts,main=name)
  }  
  
  ##split the dataset
  train_index = sample(c(T, F), nrow(PimaIndiansDiabetes), prob = c(0.8, 0.2), replace = TRUE)
  train_data <- PimaIndiansDiabetes[train_index,]
  test_data <- PimaIndiansDiabetes[!train_index,]
  
  ## 2. Regression: Linear Model ----
  

  train_index <- createDataPartition(PimaIndiansDiabetes$`diabetes`, # nolint
                                     p = 0.80, list = FALSE)
  PimaIndians_dataset_train <- PimaIndiansDiabetes[train_index, ]
  PimaIndians_dataset_test <- PimaIndiansDiabetes[-train_index, ] 
  

  train_control <- trainControl(method = "boot", number = 500)
  
  fit <- glm(formula = diabetes ~ ., family = binomial, data = PimaIndians_dataset_train)
  
  test.predict = ifelse(predict(fit, test_data, type = "response") > 0.5, "Yes", "No")
  cm1 <- table(test.predict, test_data$diabetes)
  sum(diag(cm1)) / sum(cm1)
  
  
  
  
  ## 3. Classification: LDA with k-fold Cross Validation ----
  
  ### 3.a. LDA classifier based on a 5-fold cross validation ----
  # We train a Linear Discriminant Analysis (LDA) classifier based on a 5-fold
  # cross validation train control but this time, using the churn variable for
  # classification, not the customer value variable for regression.
  train_control <- trainControl(method = "cv", number = 5)
  
  Pima_Indians_dateset_model_lda <-
    caret::train(diabetes ~ ., data = PimaIndians_dataset_train,
                 trControl = train_control, na.action = na.omit, method = "lda2",
                 metric = "Accuracy")
  
  ### 3.b. Test the trained LDA model using the testing dataset ----
  predictions_lda <- predict(Pima_Indians_dateset_model_lda,
                             PimaIndians_dataset_test[, 1:8])
  
  ### 3.c. View the summary of the model and view the confusion matrix ----
  print(Pima_Indians_dateset_model_lda)
  caret::confusionMatrix(predictions_lda, PimaIndians_dataset_test$diabetes)
  
  ## 4. Classification: Naive Bayes with Repeated k-fold Cross Validation ----
  ### 4.a. Train an e1071::naive Bayes classifier based on the churn variable ----
  Pima_Indians_dateset_model_nb <-
    e1071::naiveBayes(`diabetes` ~ ., data = PimaIndians_dataset_train)
  
  ### 4.b. Test the trained naive Bayes classifier using the testing dataset ----
  Pima_Indians_predictions_nb_e1071 <-
    predict(Pima_Indians_dateset_model_nb, PimaIndians_dataset_test[, 1:8])
  
  
  ### 4.c. View a summary of the naive Bayes model and the confusion matrix ----
  print(Pima_Indians_dateset_model_nb)
  caret::confusionMatrix(Pima_Indians_predictions_nb_e1071, PimaIndians_dataset_test$diabetes)
  
  ## 5. Classification: SVM with Repeated k-fold Cross Validation ----
  ### 5.a. SVM Classifier using 5-fold cross validation with 3 reps ----
  
  
  train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
  
  Pima_Indians_model_svm <-
    caret::train(`diabetes` ~ ., data = PimaIndians_dataset_train,
                 trControl = train_control, na.action = na.omit,
                 method = "svmLinearWeights2", metric = "Accuracy")
  
  ### 5.b. Test the trained SVM model using the testing dataset ----
  predictions_svm <- predict(Pima_Indians_model_svm, PimaIndians_dataset_test[, 1:8])
  
  ### 5.c. View a summary of the model and view the confusion matrix ----
  print(Pima_Indians_model_svm)
  caret::confusionMatrix(predictions_svm, PimaIndians_dataset_test$diabetes)
  ## 6. Classification: Naive Bayes with Leave One Out Cross Validation ----
  # In Leave One Out Cross-Validation (LOOCV), a data instance is left out and a
  # model constructed on all other data instances in the training set. This is
  # repeated for all data instances.
  
  ### 6.a. Train a Naive Bayes classifier based on an LOOCV ----
  train_control <- trainControl(method = "LOOCV")
  
  Pima_Indians_dateset_model_nb_loocv <-
    caret::train(`diabetes` ~ ., data = PimaIndians_dataset_train,
                 trControl = train_control, na.action = na.omit,
                 method = "naive_bayes", metric = "Accuracy")
  
  ### 6.b. Test the trained model using the testing dataset ====
  predictions_nb_loocv <-
    predict(Pima_Indians_dateset_model_nb_loocv, PimaIndians_dataset_test[, 1:8])
  
  ### 6.c. View the confusion matrix ====
  print(Pima_Indians_dateset_model_nb_loocv)
  caret::confusionMatrix(predictions_nb_loocv, PimaIndians_dataset_test$diabetes)
  
  
  
    