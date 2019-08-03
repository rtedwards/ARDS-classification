#########################################
## Author: Robert Edwards
## Model Training and Evaluation
#########################################


#########################################
## Set Project Directory
setwd("~/OneDrive - University of Glasgow/University of Glasgow/ARDS-classification/project/_data_preprocessing")
#########################################

#########################################
## Run dependent files 
source("../_settings/libraries.R")
source("../_settings/imputation.R")
#########################################


#########################################
## Load Data
ards <- read.csv(file = "../data/data-imputed-listwise.csv", header = TRUE)
#data.df <- read.csv(file = "../data/data-imputed-median.csv", header = TRUE)
#data.df <- read.csv(file = "../data/data-imputed-knn.csv", header = TRUE)
#data.df <- read.csv(file = "../data/data-imputed-mice.csv", header = TRUE)
#########################################



#########################################
## Data Splitting 
#########################################
set.seed(21)
trainIndex <- createDataPartition(ards$ECMO_Survival, 
                                  p = .75,      ## 75% data in training set
                                  list = FALSE, ## avoids returning data as a list
                                  times = 1)
head(trainIndex)

train <- ards[ trainIndex,]
test  <- ards[-trainIndex,]


#folds <- createFolds(factor(train$ECMO_Survival), k = 5, returnTrain = T, list = T)

# verify stratification: Every fold has 2 observations of each species
#train$fold <- folds
#summary(train$ECMO_Survival[folds$Fold1==1])


#########################################
## Train Control
#########################################

ctrl <- caret::trainControl(## 10-fold CV
#  method = "repeatedcv",
  method = "cv",
  number = 10,
  returnResamp = "all",
  classProbs = TRUE, 
  summaryFunction = twoClassSummary,
  ## repeated ten times
#  repeats = 1
  )



#########################################
## Logistic Regression + LASSO
#########################################
set.seed(21)

tuneGrid = expand.grid(alpha = 1, ## LASSO regularization
                       lambda = seq(0.001,0.1,by = 0.001))

lasso_model <- caret::train(ECMO_Survival ~ ., 
                            data = train, 
                            method = "glmnet", 
                            metric = "ROC",
                            trControl = ctrl, 
                            preProcess = c('center', 'scale'),
                            ## parameters for glmnet()
                            family = "binomial",
                            tuneGrid = tuneGrid)

lasso_model

## Plot LogLambda
# oldpar <- par(no.readonly=TRUE)
# par(mfrow=c(1, 2))
# 
# plot(lasso_cv_model, xvar="lambda", label = TRUE)
# plot(lasso_cv_model$results, main="LASSO") # alpha = 1
# par(oldpar)



#########################################
## Linear Discriminant Analysis
#########################################
set.seed(21)
lda_model <- caret::train(ECMO_Survival ~ ., 
                          data = train,
                          method = "lda", 
                          metric = "ROC",
                          trControl = ctrl)
lda_model


#########################################
## Quadratic Discriminant Analysis
#########################################
set.seed(21)
qda_model <- caret::train(ECMO_Survival ~ ., 
                          data = train, 
                          method = "qda", 
                          metric = "ROC",
                          trControl = ctrl)
qda_model



#########################################
## ## Weighted K-Nearest Neighbors
## https://cran.r-project.org/web/packages/kknn/kknn.pdf
#########################################
set.seed(21)
tuneGrid <- expand.grid(kmax = seq(3, 15, by = 2),            # allows to test a range of k values
                        distance = seq(1, 1, by = 1), # allows to test a range of distance values
                        kernel = c('gaussian',  # different weighting types in kknn
                                   'triangular',
                                   'rectangular',
                                   'epanechnikov',
                                   'optimal'))

knn_model <- caret::train(ECMO_Survival ~ ., 
                          data = train, 
                          method = "kknn", 
                          metric = "ROC",
                          trControl = ctrl,
                          ## parameters for knn()
                          tuneGrid = tuneGrid,
                          tuneLength = 10)
knn_model

ggplot(knn_model)
knnPredict <- predict(knn_model, newdata = test )
confusionMatrix(knnPredict, test$ECMO_Survival )



#########################################
## Random Forest
## https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
#########################################
set.seed(21)
tuneGrid <- expand.grid()

rf_model <- caret::train(ECMO_Survival ~ ., 
                          data = train, 
                          method = "rf", 
                          metric = "ROC",
                          trControl = ctrl
#                          tuneGrid = tuneGrid,
#                          tuneLength = 10
                          )
rf_model


#########################################
## Support Vector Classifier
## https://cran.r-project.org/web/packages/kknn/kknn.pdf
#########################################



#########################################
## Cleanup variables
#########################################
rm(ards)
rm(train)
rm(test)
rm(trainIndex)
rm(tuneGrid)
rm(ctrl)
rm(lasso_model)
rm(lda_model)
rm(qda_model)
rm(knn_model)
rm(rf_model)
rm(svm_model)





