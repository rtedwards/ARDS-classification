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
  method = "none",
  #  method = "repeatedcv",  # repeated K-fold cross-validation
  #  number = 10,             # k = 5
  #  repeats = 10,           # repeat 10 times
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


#########################################
## Logistic Regression 
#########################################

logit_model = list()
logit = 1
for (i in logit) {
  tuneGrid = expand.grid(alpha = 1,   ## LASSO regularization
                         lambda = 0)  ## No regularization
  
  logit_model$models[[i]] <- caret::train(ECMO_Survival ~ ., 
                              data = train, 
                              method = "glmnet", 
                              metric = metric,
                              trControl = trControl, 
                              family = "binomial",
                              tuneGrid = tuneGrid
  )
}
logit_model


#########################################
## Logistic Regression + LASSO
#########################################
set.seed(123)

lasso_model = list()
lambda = seq(0.001, 0.1, by = 0.001)
for (i in 1:length(lambda)) {
  tuneGrid = expand.grid(alpha = 1, ## LASSO regularization
                         lambda = lambda[i])
  
  lasso_model$lambda <- lambda[i]
  lasso_model$models[[i]] <- caret::train(ECMO_Survival ~ ., 
                              data = train, 
                              method = "glmnet", 
                              metric = metric,
                              trControl = trControl, 
                              family = "binomial",
                              tuneGrid = tuneGrid)
}
lasso_model


#########################################
## Linear Discriminant Analysis
#########################################
set.seed(123)

lda_model = list()
lda = 1
for (i in lda) {
lda_model$models[[i]] <- caret::train(ECMO_Survival ~ ., 
                          data = train,
                          method = "lda", 
                          metric = metric,
                          trControl = trControl)
}
lda_model


#########################################
## Quadratic Discriminant Analysis
## "Rank deficiency in group N" https://stats.stackexchange.com/questions/35071/what-is-rank-deficiency-and-how-to-deal-with-it
#########################################
set.seed(123)
qda_model = list()
qda = 1
for (i in qda) {
  qda_model$models[[i]] <- caret::train(ECMO_Survival ~ ., 
                            data = train, 
                            method = "qda", 
                            metric = metric,
                            preProcess = c("corr"),
                            trControl = trControl)
}
qda_model



#########################################
## ## Weighted K-Nearest Neighbors
## https://cran.r-project.org/web/packages/kknn/kknn.pdf
#########################################
set.seed(123)

knn_model = list()
kmax = seq(3, 19, by = 2)
for (i in 1:length(kmax)) {
  tuneGrid <- expand.grid(kmax = kmax[i],  # allows to test a range of k values
                          distance = 2, # various Minkowski distances
                          kernel = 'gaussian')
  
  knn_model$k <- kmax[i]
  knn_model$models[[i]] <- caret::train(ECMO_Survival ~ ., 
                            data = train, 
                            method = "kknn", 
                            metric = metric,
                            trControl = trControl,
                            tuneGrid = tuneGrid
  )
}
knn_model



#########################################
## Random Forest
## https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
#########################################
set.seed(123)
rf_model = list()
mtry = seq(1, 33, by = 2)
for (i in 1:length(mtry)) {
  tuneGrid <- expand.grid(mtry = mtry[i])
  
  rf_model$mtry <- mtry[i]
  rf_model$models[[i]] <- caret::train(ECMO_Survival ~ .,
                           data = train, 
                           method = "rf", 
                           metric = metric,
                           trControl = trControl,
                           tuneGrid = tuneGrid
  )
}
rf_model



#########################################
## Save Trained Models 
#########################################
# Save multiple objects
file_name <- paste0("../_trained-models/trained2-models-", imputation_method, ".RData")
save(file = file_name,
     train,
     training_times,
     logit_model,
     lasso_model,
     lda_model,
     qda_model,
     knn_model,
     rf_model
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






