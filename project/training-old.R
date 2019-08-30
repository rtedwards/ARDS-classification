#########################################
## Author: Robert Edwards
## Model Training and Evaluation
#########################################

## Start fresh
rm(list = ls())

#########################################
## Set Project Directory
#########################################
setwd("~/OneDrive - University of Glasgow/University of Glasgow/ARDS-classification/project/src")


#########################################
## Run dependent files 
#########################################
source("../_settings/libraries.R")
source("../_data_preprocessing/preprocess.R")


#########################################
## Set up Parallel Processing 
## https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
## 
## system("sysctl hw.ncpu") # total number of cores
## system("sysctl  hw.physicalcpu")  # number of physical CPUs
#########################################
numCores <- parallel::detectCores()
clusters <- makePSOCKcluster(numCores) # Leave some for other important tasks, like browsing reddit
registerDoParallel(clusters)
#registerDoSEQ() ## register the sequential backend


#########################################
## Load Data
#########################################
load("../data/imputed-data.RData")

# imputation_method <- "complete-case"
# train <- train.complete.df

# imputation_method <- "mean"
# train <- train.mean.df

imputation_method <- "pmm5"
train <- train.pmm.df


#########################################
## Training Settings
#########################################
set.seed(123)

## K-Fold Cross Validation
trControl <- caret::trainControl(
#  method = "none",
  method = "repeatedcv",  # repeated K-fold cross-validation
  number = 10,             # k = 5
  repeats = 10,           # repeat 10 times
  returnResamp = "all",
  classProbs = TRUE,      # Should be TRUE if metric = "ROC" Can skip if metric = "Kappa"
  savePredictions = TRUE,
  allowParallel = TRUE,   # Allow parallel processing
  summaryFunction = twoClassSummary
)


## Model evaluation metric
metric <- "ROC"
#metric = "kappa"   # good for imbalanced data
#metric <- "Accuracy"

## Create a variable to store training times
training_times <- list()


#########################################
## Logistic Regression 
#########################################
tuneGrid = expand.grid(alpha = 1,   ## LASSO regularization
                       lambda = 0)  ## No regularization


training_times[1] <- system.time({
logit_model <- caret::train(ECMO_Survival ~ ., 
                            data = train, 
                            method = "glmnet", 
                            metric = metric,
                            trControl = trControl, 
#                            preProcess = preProcess,
                            family = "binomial",
                            tuneGrid = tuneGrid
#                            tuneLength = 10
                            )
})[3]
training_times[1]
logit_model


#########################################
## Logistic Regression + LASSO
#########################################
set.seed(123)

## TuneGrid - It means user has to specify a tune grid manually. In the grid, 
## each algorithm parameter can be specified as a vector of possible values. 
## These vectors combine to define all the possible combinations to try.
tuneGrid = expand.grid(alpha = 1, ## LASSO regularization
                       lambda = seq(0.001,0.1,by = 0.001))

training_times[2] <- system.time({
lasso_model <- caret::train(ECMO_Survival ~ ., 
                            data = train, 
                            method = "glmnet", 
                            metric = metric,
                            trControl = trControl, 
#                            preProcess = preProcess,
                            family = "binomial",
                            tuneGrid = tuneGrid)
})[3]
training_times[2]
lasso_model


#########################################
## Linear Discriminant Analysis
#########################################
set.seed(123)
training_times[3] <- system.time({
lda_model <- caret::train(ECMO_Survival ~ ., 
                          data = train,
                          method = "lda", 
                          metric = metric,
#                          preProcess = preProcess,
                          trControl = trControl)
})[3]
training_times[3]
lda_model


#########################################
## Quadratic Discriminant Analysis
## "Rank deficiency in group N" https://stats.stackexchange.com/questions/35071/what-is-rank-deficiency-and-how-to-deal-with-it
#########################################
set.seed(123)
training_times[4] <- system.time({
qda_model <- caret::train(ECMO_Survival ~ ., 
                          data = train, 
                          method = "qda", 
                          metric = metric,
#                          preProcess = preProcess,
                          preProcess = c("corr"),
                          trControl = trControl)
})[3]
training_times[4]
qda_model



#########################################
## ## Weighted K-Nearest Neighbors
## https://cran.r-project.org/web/packages/kknn/kknn.pdf
#########################################
set.seed(123)
tuneGrid <- expand.grid(kmax = seq(5, 15, by = 2),  # allows to test a range of k values
                        distance = c(1, 2), # various Minkowski distances
                        kernel = c('gaussian',      # different weighting types in kknn
                                   'triangular',
                                   'rectangular'))

training_times[5] <- system.time({
knn_model <- caret::train(ECMO_Survival ~ ., 
                          data = train, 
                          method = "kknn", 
                          metric = metric,
#                          preProcess = preProcess,
                          trControl = trControl,
                          tuneGrid = tuneGrid
                          )
})[3]
training_times[5]
knn_model



#########################################
## Random Forest
## https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
#########################################
set.seed(123)
tuneGrid <- expand.grid(mtry = seq(1, 10, by = 2))

training_times[6] <- system.time({
rf_model <- caret::train(ECMO_Survival ~ .,
                         data = train, 
                         method = "rf", 
                         metric = metric,
#                         preProcess = preProcess,
                         trControl = trControl,
                         tuneGrid = tuneGrid
#                         ntree = 1000 
#                         tuneLength = 10
                          )
})[3]
training_times[6]
rf_model


#########################################
## Support Vector Classifier
## 
#########################################
set.seed(123)

## I. Fit a Linear SVM kernel
tuneGrid <- expand.grid(C = c(2^(-1), 2^0, 2^1, 2^3)) # seq(0.2, 1, by = 0.2)

training_times[7] <- system.time({
svmLinear_model <- caret::train(ECMO_Survival ~ ., 
                         data = train, 
                         method = "svmLinear", 
                         metric = metric,
#                         preProcess = preProcess,
                         trControl = trControl,
                         tuneGrid = tuneGrid
#                         tuneLength = 10
                         )
})[3]
training_times[7]
svmLinear_model



## II. Fit a Radial SVM kernel
tuneGrid <- expand.grid(sigma = c(2^(-1), 2^0, 2^1, 2^3), 
                        C = c(2^(-1), 2^0, 2^1, 2^3))
training_times[8] <- system.time({
  svmRadial_model <- caret::train(ECMO_Survival ~ ., 
                                  data = train, 
                                  method = "svmRadial", 
                                  metric = metric,
#                                  preProcess = preProcess,
                                  trControl = trControl,
                                  tuneGrid = tuneGrid
#                                  tuneLength = 10
                                )
})[3]
training_times[8]
svmRadial_model



## III. Fit a Polynomial SVM kernel
tuneGrid <- expand.grid(degree = 1:4,
                        scale = seq(0.25, 1, by = 0.25),   # seq(0.1, 1, by = 0.1), 
                        C = c(2^(-1), 2^0, 2^1, 2^3)       # seq(0.2, 1, by = 0.2)
                        )

training_times[9] <- system.time({
svmPoly_model <- caret::train(ECMO_Survival ~ ., 
                                data = train, 
                                method = "svmPoly", 
                                metric = metric,
#                                preProcess = preProcess,
                                trControl = trControl,
                                tuneGrid = tuneGrid
#                                tuneLength = 10
                              )
})[3]
training_times[9]
svmPoly_model


## Name for each of the training times
names(training_times) <- c("logit", "LASSO", "LDA", "QDA", "KNN", "RF", "svmLinear", "svmRadial", "svmPoly")


#########################################
## Save Trained Models 
#########################################
# Save multiple objects
file_name <- paste0("../_trained-models/trained-models-", imputation_method, ".RData")
save(file = file_name,
     train,
     training_times,
     logit_model,
     lasso_model,
     lda_model,
     qda_model,
     knn_model,
     rf_model,
     svmLinear_model,
     svmPoly_model,
     svmRadial_model
     )


#########################################
## Cleanup 
#########################################

## Cleaning variables
rm(ards)
rm(train)
rm(test)
rm(tuneGrid)
rm(trControl)
rm(training_times)
rm(imputation_method)

## Parallel Computing
stopCluster(clusters)  ## Stop the cluster
registerDoSEQ() ## register the sequential backend
rm(numCores)
rm(clusters)

rm(tuneGrid)
rm(metric)

rm(logit_model)
rm(lasso_model)
rm(lda_model)
rm(qda_model)
rm(knn_model)
rm(rf_model)
rm(svmLinear_model)
rm(svmPoly_model)
rm(svmRadial_model)

rm(file_name)






