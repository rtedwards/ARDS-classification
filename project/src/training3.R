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
#setwd(getwd())


#########################################
## Run dependent files 
#########################################
source("../_settings/libraries.R")
source("../_settings/functions.R")
source("../_data_preprocessing/preprocess.R")
#source("../src/training.R")


#########################################
## Load Data
#########################################
load("../data/processed-data.RData")


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
## Settings
#########################################

num_folds <- 10   # Cross-Validation Settings
metric = "kappa" # good for imbalanced data

## Imputation Settings
# impute_method <- "complete-case"
# impute_method <- "mean"
impute_method <- "pmm"
# impute_method <- "norm"
# impute_method = "rf"

imputeSettings <- list(
  method = impute_method, 
  m = 5,      # Number of imputed datasets to create
  maxit = 5,  # max number of iterations for imputation convergence
  seed = 123
)

## Settings for trainControl
trControl <- caret::trainControl(
  method = "none",        # No cross validation
  returnResamp = "all",
  classProbs = TRUE,     # Should be TRUE if metric = "ROC" Can skip if metric = "Kappa"
  savePredictions = TRUE,
  allowParallel = TRUE,   # Allow parallel processing
  summaryFunction = twoClassSummary
)

## Training settings
trainSettings <- list(formula = reformulate(".", response = "ECMO_Survival"), 
                      trControl = trControl,
                      metric = metric
)





#########################################
## Logistic Regression 
#########################################

kappa_avg <- data.frame(matrix(data = NA, nrow = 1, ncol = 6))
colnames(kappa_avg) <-  c("Logit", "LDA", "QDA", "KNN", "RF")


## Model to fit
trainSettings$method <- "glmnet"

## Step in the grid search for model tuning
trainSettings$tuneGrid = expand.grid(alpha = 1,   ## LASSO regularization
                                     lambda = 0)  ## No regularization

kappa_logit <- crossValidate(train, K = num_folds, 
                                 trainSettings = trainSettings, 
                                 imputeSettings = imputeSettings)


#########################################
## Linear Discriminant Analysis 
#########################################
## Model to fit
trainSettings$method <- "lda"

kappa_lda <- crossValidate(train, K = num_folds, 
                               trainSettings = trainSettings, 
                               imputeSettings = imputeSettings)


#########################################
## Quadratic Discriminant Analysis 
#########################################
## Model to fit
trainSettings$method <- "qda"

kappa_qda <- crossValidate(train, K = num_folds, 
                               trainSettings = trainSettings, 
                               imputeSettings = imputeSettings)



#########################################
## Weighted K-Nearest Neighbors
#########################################
## Model to fit
trainSettings$method <- "kknn"
kmax <- seq(3, 15, by = 2)   # mtry=7 x K=5 x m=99 x ~sec=3 ~= 180min
kappa_knn <- data.frame(matrix(data = NA, nrow = length(kmax), ncol = 2))
colnames(kappa_knn) <- c("kmax", "kappa")

for (i in 1:length(kmax)) {
  print( paste0("----- k = ", i, " -----") )
  ## Step in the grid search for model tuning
  trainSettings$tuneGrid = expand.grid(kmax = kmax[i],              # seq(5, 15, by = 2),  # allows to test a range of k values
                                       distance = c(2),       # various Minkowski distances
                                       kernel = c('gaussian') # different weighting types in kknn))
  ) 
  
  kappa_knn[i, 1] <- kmax[[i]]  ## Store k value
  kappa_knn[i, 2] <- crossValidate(train, K = num_folds, 
                                   trainSettings = trainSettings, 
                                   imputeSettings = imputeSettings)
}

#########################################
## Random Forests
#########################################
## Model to fit
trainSettings$method <- "rf"
mtry <- seq(3, 15, by = 2) # mtry=7 x K=5 x m=99 x ~sec=3 ~= 180min
kappa_rf <- data.frame(matrix(data = NA, nrow = length(mtry), ncol = 2))
colnames(kappa_rf) <- c("mtry", "kappa")

for (i in 1:length(mtry)) {
  print( paste0("----- mtry = ", i, " -----") )
  ## Step in the grid search for model tuning
  trainSettings$tuneGrid = expand.grid(mtry = mtry[i])    # seq(1, 10, by = 2))
  
  kappa_rf[i, 1] <- mtry[[i]]  ## Store k value
  kappa_rf[i, 2] <- crossValidate(train, K = num_folds, 
                                  trainSettings = trainSettings, 
                                  imputeSettings = imputeSettings)
}

kappa_avg



#########################################
## Save Trained Models 
#########################################
# Save multiple objects
file_name <- paste0("../_trained-models/trained-models-", impute_method, ".RData")
save(file = file_name,
     train,
     kappa_logit,
     kappa_lda,
     kappa_qda,
     kappa_knn,
     kappa_rf
)



#########################################
## Cleanup 
#########################################

## Cleaning variables
rm(train)
rm(test)

## Parallel Computing
stopCluster(clusters)  ## Stop the cluster
registerDoSEQ() ## register the sequential backend
rm(numCores)
rm(clusters)

# Kappa values
rm(kappa_logit)
rm(kappa_lda)
rm(kappa_qda)
rm(kappa_knn)
rm(kappa_rf)


# # Models
# rm(logit_model)
# rm(lasso_model)
# rm(lda_model)
# rm(qda_model)
# rm(knn_model)
# rm(rf_model)
# rm(svmLinear_model)
# rm(svmPoly_model)
# rm(svmRadial_model)







