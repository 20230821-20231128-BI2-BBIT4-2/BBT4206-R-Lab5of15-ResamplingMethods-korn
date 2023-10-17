Business Intelligence Project
================
korn
18/10/23

- [Student Details](#student-details)
- [Setup Chunk](#setup-chunk)
  - [LAB 5](#lab-5)
  - [STEP 1. INSTALL PACKAGES](#step-1-install-packages)
  - [STEP 2. LOAD THE DATASET](#step-2-load-the-dataset)
  - [STEP 3. DATA ANALYSIS](#step-3-data-analysis)
  - [STEP 4. SPLIT THE DATASET](#step-4-split-the-dataset)
  - [STEP 5. TRAINING AND TESTING](#step-5-training-and-testing)
    - [Regression: Linear Mode](#regression-linear-mode)
  - [3. Classification: LDA with k-fold Cross Validation
    —-](#3-classification-lda-with-k-fold-cross-validation--)
    - [3.a. LDA classifier based on a 5-fold cross validation
      —-](#3a-lda-classifier-based-on-a-5-fold-cross-validation--)
  - [4. Classification: Naive Bayes with Repeated k-fold Cross
    Validation
    —-](#4-classification-naive-bayes-with-repeated-k-fold-cross-validation--)
    - [4.a. Train an e1071::naive Bayes classifier based on the churn
      variable
      —-](#4a-train-an-e1071naive-bayes-classifier-based-on-the-churn-variable--)
  - [5. Classification: SVM with Repeated k-fold Cross Validation
    —-](#5-classification-svm-with-repeated-k-fold-cross-validation--)
    - [5.a. SVM Classifier using 5-fold cross validation with 3 reps
      —-](#5a-svm-classifier-using-5-fold-cross-validation-with-3-reps--)
  - [6. Classification: Naive Bayes with Leave One Out Cross Validation
    —-](#6-classification-naive-bayes-with-leave-one-out-cross-validation--)

# Student Details

<table>
<colgroup>
<col style="width: 53%" />
<col style="width: 46%" />
</colgroup>
<tbody>
<tr class="odd">
<td><strong>Student ID Numbers and Names of Group Members</strong></td>
<td><ol type="1">
<li><p>134644 - C - Sebastian Muramara</p></li>
<li><p>136675 - C - Bernard Otieno</p></li>
<li><p>131589 - C - Agnes Anyango</p></li>
<li><p>131582 - C - Njeri Njuguna</p></li>
<li><p>136009 - C- Sera Ndabari</p></li>
</ol></td>
</tr>
<tr class="even">
<td><strong>GitHub Classroom Group Name</strong></td>
<td>Korn</td>
</tr>
<tr class="odd">
<td><strong>Course Code</strong></td>
<td>BBT4206</td>
</tr>
<tr class="even">
<td><strong>Course Name</strong></td>
<td>Business Intelligence II</td>
</tr>
<tr class="odd">
<td><strong>Program</strong></td>
<td>Bachelor of Business Information Technology</td>
</tr>
<tr class="even">
<td><strong>Semester Duration</strong></td>
<td>21<sup>st</sup> August 2023 to 28<sup>th</sup> November 2023</td>
</tr>
</tbody>
</table>

# Setup Chunk

**Note:** the following KnitR options have been set as the global
defaults: <BR>
`knitr::opts_chunk$set(echo = TRUE, warning = FALSE, eval = TRUE, collapse = FALSE, tidy = TRUE)`.

More KnitR options are documented here
<https://bookdown.org/yihui/rmarkdown-cookbook/chunk-options.html> and
here <https://yihui.org/knitr/options/>.

## LAB 5

## STEP 1. INSTALL PACKAGES

``` r
if (require("languageserver")) {
  require("languageserver")
} else {
  install.packages("languageserver", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
```

    ## Loading required package: languageserver

``` r
## caret ----
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
```

    ## Loading required package: caret

    ## Loading required package: ggplot2

    ## Loading required package: lattice

``` r
## klaR ----
if (require("klaR")) {
  require("klaR")
} else {
  install.packages("klaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
```

    ## Loading required package: klaR

    ## Loading required package: MASS

``` r
## e1071 ----
if (require("e1071")) {
  require("e1071")
} else {
  install.packages("e1071", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
```

    ## Loading required package: e1071

``` r
## readr ----
if (require("readr")) {
  require("readr")
} else {
  install.packages("readr", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
```

    ## Loading required package: readr

``` r
## LiblineaR ----
if (require("LiblineaR")) {
  require("LiblineaR")
} else {
  install.packages("LiblineaR", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
```

    ## Loading required package: LiblineaR

``` r
## naivebayes ----
if (require("naivebayes")) {
  require("naivebayes")
} else {
  install.packages("naivebayes", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
```

    ## Loading required package: naivebayes

    ## naivebayes 0.9.7 loaded

## STEP 2. LOAD THE DATASET

``` r
if (require("mlbench")) {
  require("mlbench")
} else {
  install.packages("mlbench", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
```

    ## Loading required package: mlbench

``` r
if (require("caret")) {
  require("caret")
} else {
  install.packages("caret", dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
##Install mlbench
##install.packages("mlbench")
##require("mlbench")
##require("caret")
data("PimaIndiansDiabetes")
summary("PimaIndiansDiabetes")
```

    ##    Length     Class      Mode 
    ##         1 character character

## STEP 3. DATA ANALYSIS

``` r
##summary of each column in the dataset
  par(mfrow=c(1,4));
  for(i in 1:4){
    hist(PimaIndiansDiabetes[,i], main=names(PimaIndiansDiabetes)[i])
  }
```

![](Lab-Submission-Markdown_files/figure-gfm/DATA%20ANALYSIS-1.png)<!-- -->

``` r
  library(lattice)
  #create a layout of simpler density plots by attribute
  par(mfrow=c(1,4))
  for (i in 1:4) {
    plot(density(PimaIndiansDiabetes[,i]), main=names(PimaIndiansDiabetes)[i])}
```

![](Lab-Submission-Markdown_files/figure-gfm/DATA%20ANALYSIS-2.png)<!-- -->

``` r
  par(mfrow=c(1,4))
  for(i in 1:4) {
    boxplot(PimaIndiansDiabetes[,i], main=names(PimaIndiansDiabetes)[i])
  }  
```

![](Lab-Submission-Markdown_files/figure-gfm/DATA%20ANALYSIS-3.png)<!-- -->

``` r
  par(mfrow=c(1,4))
  for(i in 1:4){
    counts <- table(PimaIndiansDiabetes[,i])
    name <-names(PimaIndiansDiabetes)[i]
    barplot(counts,main=name)
  }  
```

![](Lab-Submission-Markdown_files/figure-gfm/DATA%20ANALYSIS-4.png)<!-- -->

## STEP 4. SPLIT THE DATASET

``` r
  ##split the dataset
  train_index = sample(c(T, F), nrow(PimaIndiansDiabetes), prob = c(0.8, 0.2), replace = TRUE)
  train_data <- PimaIndiansDiabetes[train_index,]
  test_data <- PimaIndiansDiabetes[!train_index,]
```

## STEP 5. TRAINING AND TESTING

### Regression: Linear Mode

``` r
  train_index <- createDataPartition(PimaIndiansDiabetes$`diabetes`, # nolint
                                     p = 0.80, list = FALSE)
  PimaIndians_dataset_train <- PimaIndiansDiabetes[train_index, ]
  PimaIndians_dataset_test <- PimaIndiansDiabetes[-train_index, ] 
  

  train_control <- trainControl(method = "boot", number = 500)
  
  fit <- glm(formula = diabetes ~ ., family = binomial, data = PimaIndians_dataset_train)
  
  test.predict = ifelse(predict(fit, test_data, type = "response") > 0.5, "Yes", "No")
  cm1 <- table(test.predict, test_data$diabetes)
  sum(diag(cm1)) / sum(cm1)
```

    ## [1] 0.79375

## 3. Classification: LDA with k-fold Cross Validation —-

### 3.a. LDA classifier based on a 5-fold cross validation —-

``` r
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
```

    ## Linear Discriminant Analysis 
    ## 
    ## 615 samples
    ##   8 predictor
    ##   2 classes: 'neg', 'pos' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 492, 492, 492, 492, 492 
    ## Resampling results:
    ## 
    ##   Accuracy   Kappa    
    ##   0.7853659  0.5074669
    ## 
    ## Tuning parameter 'dimen' was held constant at a value of 1

``` r
  caret::confusionMatrix(predictions_lda, PimaIndians_dataset_test$diabetes)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction neg pos
    ##        neg  85  25
    ##        pos  15  28
    ##                                           
    ##                Accuracy : 0.7386          
    ##                  95% CI : (0.6615, 0.8062)
    ##     No Information Rate : 0.6536          
    ##     P-Value [Acc > NIR] : 0.01536         
    ##                                           
    ##                   Kappa : 0.3959          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.15473         
    ##                                           
    ##             Sensitivity : 0.8500          
    ##             Specificity : 0.5283          
    ##          Pos Pred Value : 0.7727          
    ##          Neg Pred Value : 0.6512          
    ##              Prevalence : 0.6536          
    ##          Detection Rate : 0.5556          
    ##    Detection Prevalence : 0.7190          
    ##       Balanced Accuracy : 0.6892          
    ##                                           
    ##        'Positive' Class : neg             
    ## 

## 4. Classification: Naive Bayes with Repeated k-fold Cross Validation —-

### 4.a. Train an e1071::naive Bayes classifier based on the churn variable —-

``` r
Pima_Indians_dateset_model_nb <-
    e1071::naiveBayes(`diabetes` ~ ., data = PimaIndians_dataset_train)
  
  ### 4.b. Test the trained naive Bayes classifier using the testing dataset ----
  Pima_Indians_predictions_nb_e1071 <-
    predict(Pima_Indians_dateset_model_nb, PimaIndians_dataset_test[, 1:8])
  
  
  ### 4.c. View a summary of the naive Bayes model and the confusion matrix ----
  print(Pima_Indians_dateset_model_nb)
```

    ## 
    ## Naive Bayes Classifier for Discrete Predictors
    ## 
    ## Call:
    ## naiveBayes.default(x = X, y = Y, laplace = laplace)
    ## 
    ## A-priori probabilities:
    ## Y
    ##       neg       pos 
    ## 0.6504065 0.3495935 
    ## 
    ## Conditional probabilities:
    ##      pregnant
    ## Y         [,1]     [,2]
    ##   neg 3.285000 2.980097
    ##   pos 4.883721 3.772808
    ## 
    ##      glucose
    ## Y         [,1]     [,2]
    ##   neg 109.4550 26.45321
    ##   pos 142.3023 30.59651
    ## 
    ##      pressure
    ## Y         [,1]     [,2]
    ##   neg 68.36250 18.13238
    ##   pos 70.53953 22.25945
    ## 
    ##      triceps
    ## Y         [,1]     [,2]
    ##   neg 19.65500 14.81755
    ##   pos 22.49302 17.87963
    ## 
    ##      insulin
    ## Y         [,1]     [,2]
    ##   neg 69.41250 100.3522
    ##   pos 99.32558 135.9505
    ## 
    ##      mass
    ## Y         [,1]     [,2]
    ##   neg 30.41300 7.489380
    ##   pos 35.37953 7.122496
    ## 
    ##      pedigree
    ## Y          [,1]      [,2]
    ##   neg 0.4287525 0.2916734
    ##   pos 0.5645209 0.3871761
    ## 
    ##      age
    ## Y         [,1]     [,2]
    ##   neg 30.85500 11.30945
    ##   pos 36.77674 10.99432

``` r
  caret::confusionMatrix(Pima_Indians_predictions_nb_e1071, PimaIndians_dataset_test$diabetes)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction neg pos
    ##        neg  81  27
    ##        pos  19  26
    ##                                         
    ##                Accuracy : 0.6993        
    ##                  95% CI : (0.62, 0.7707)
    ##     No Information Rate : 0.6536        
    ##     P-Value [Acc > NIR] : 0.1342        
    ##                                         
    ##                   Kappa : 0.3116        
    ##                                         
    ##  Mcnemar's Test P-Value : 0.3020        
    ##                                         
    ##             Sensitivity : 0.8100        
    ##             Specificity : 0.4906        
    ##          Pos Pred Value : 0.7500        
    ##          Neg Pred Value : 0.5778        
    ##              Prevalence : 0.6536        
    ##          Detection Rate : 0.5294        
    ##    Detection Prevalence : 0.7059        
    ##       Balanced Accuracy : 0.6503        
    ##                                         
    ##        'Positive' Class : neg           
    ## 

## 5. Classification: SVM with Repeated k-fold Cross Validation —-

### 5.a. SVM Classifier using 5-fold cross validation with 3 reps —-

``` r
  train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
  
  Pima_Indians_model_svm <-
    caret::train(`diabetes` ~ ., data = PimaIndians_dataset_train,
                 trControl = train_control, na.action = na.omit,
                 method = "svmLinearWeights2", metric = "Accuracy")
  
  ### 5.b. Test the trained SVM model using the testing dataset ----
  predictions_svm <- predict(Pima_Indians_model_svm, PimaIndians_dataset_test[, 1:8])
  
  ### 5.c. View a summary of the model and view the confusion matrix ----
  print(Pima_Indians_model_svm)
```

    ## L2 Regularized Linear Support Vector Machines with Class Weights 
    ## 
    ## 615 samples
    ##   8 predictor
    ##   2 classes: 'neg', 'pos' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold, repeated 3 times) 
    ## Summary of sample sizes: 492, 492, 492, 492, 492, 492, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   cost  Loss  weight  Accuracy   Kappa     
    ##   0.25  L1    1       0.5956640  0.11287224
    ##   0.25  L1    2       0.6607046  0.15845104
    ##   0.25  L1    3       0.6747967  0.18420961
    ##   0.25  L2    1       0.7360434  0.37063656
    ##   0.25  L2    2       0.7544715  0.48861965
    ##   0.25  L2    3       0.5165312  0.15881092
    ##   0.50  L1    1       0.6184282  0.05152322
    ##   0.50  L1    2       0.6325203  0.13256195
    ##   0.50  L1    3       0.6417344  0.12296375
    ##   0.50  L2    1       0.7349593  0.36977964
    ##   0.50  L2    2       0.7398374  0.46344122
    ##   0.50  L2    3       0.5165312  0.15881092
    ##   1.00  L1    1       0.6325203  0.10078999
    ##   1.00  L1    2       0.6379404  0.16453647
    ##   1.00  L1    3       0.5956640  0.13579898
    ##   1.00  L2    1       0.7392954  0.37973006
    ##   1.00  L2    2       0.7479675  0.47807551
    ##   1.00  L2    3       0.5176152  0.15933307
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were cost = 0.25, Loss = L2 and weight = 2.

``` r
  caret::confusionMatrix(predictions_svm, PimaIndians_dataset_test$diabetes)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction neg pos
    ##        neg  68  15
    ##        pos  32  38
    ##                                           
    ##                Accuracy : 0.6928          
    ##                  95% CI : (0.6132, 0.7648)
    ##     No Information Rate : 0.6536          
    ##     P-Value [Acc > NIR] : 0.1753          
    ##                                           
    ##                   Kappa : 0.3692          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.0196          
    ##                                           
    ##             Sensitivity : 0.6800          
    ##             Specificity : 0.7170          
    ##          Pos Pred Value : 0.8193          
    ##          Neg Pred Value : 0.5429          
    ##              Prevalence : 0.6536          
    ##          Detection Rate : 0.4444          
    ##    Detection Prevalence : 0.5425          
    ##       Balanced Accuracy : 0.6985          
    ##                                           
    ##        'Positive' Class : neg             
    ## 

## 6. Classification: Naive Bayes with Leave One Out Cross Validation —-

``` r
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
```

    ## Naive Bayes 
    ## 
    ## 615 samples
    ##   8 predictor
    ##   2 classes: 'neg', 'pos' 
    ## 
    ## No pre-processing
    ## Resampling: Leave-One-Out Cross-Validation 
    ## Summary of sample sizes: 614, 614, 614, 614, 614, 614, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   usekernel  Accuracy   Kappa    
    ##   FALSE      0.7739837  0.4937372
    ##    TRUE      0.7626016  0.4605587
    ## 
    ## Tuning parameter 'laplace' was held constant at a value of 0
    ## Tuning
    ##  parameter 'adjust' was held constant at a value of 1
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final values used for the model were laplace = 0, usekernel = FALSE
    ##  and adjust = 1.

``` r
  caret::confusionMatrix(predictions_nb_loocv, PimaIndians_dataset_test$diabetes)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction neg pos
    ##        neg  81  27
    ##        pos  19  26
    ##                                         
    ##                Accuracy : 0.6993        
    ##                  95% CI : (0.62, 0.7707)
    ##     No Information Rate : 0.6536        
    ##     P-Value [Acc > NIR] : 0.1342        
    ##                                         
    ##                   Kappa : 0.3116        
    ##                                         
    ##  Mcnemar's Test P-Value : 0.3020        
    ##                                         
    ##             Sensitivity : 0.8100        
    ##             Specificity : 0.4906        
    ##          Pos Pred Value : 0.7500        
    ##          Neg Pred Value : 0.5778        
    ##              Prevalence : 0.6536        
    ##          Detection Rate : 0.5294        
    ##    Detection Prevalence : 0.7059        
    ##       Balanced Accuracy : 0.6503        
    ##                                         
    ##        'Positive' Class : neg           
    ## 
