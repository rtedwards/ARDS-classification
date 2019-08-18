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
#########################################
numCores <- parallel::detectCores()
#clusters <- makePSOCKcluster(numCores) # Leave some for other important tasks, like browsing reddit
#registerDoParallel(clusters)
#stopCluster(clusters)  ## Stop the cluster
#registerDoSEQ() ## register the sequential backend


#########################################
## Settings
#########################################

num_folds <- 10   # Cross-Validation Settings
metric = "kappa" # good for imbalanced data

## Imputation Settings
#impute_method <- "complete-case"
#impute_method <- "mean"
impute_method <- "pmm"

imputeSettings <- list(
  method = impute_method, 
  m = 9,      # Number of imputed datasets to create
  maxit = 5,  # max number of iterations for imputation convergence
  seed = 123,
  numCores = numCores
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
## Model to fit
trainSettings$method <- "glmnet"

## Step in the grid search for model tuning
trainSettings$tuneGrid = expand.grid(alpha = 1,   ## LASSO regularization
                                     lambda = 0)  ## No regularization

table_logit <- crossValidate(train, K = num_folds, 
                                 trainSettings = trainSettings, 
                                 imputeSettings = imputeSettings)


#########################################
## Linear Discriminant Analysis 
#########################################
## Model to fit
trainSettings$method <- "lda"

table_lda <- crossValidate(train, K = num_folds, 
                               trainSettings = trainSettings, 
                               imputeSettings = imputeSettings)


#########################################
## Quadratic Discriminant Analysis 
#########################################
## Model to fit
trainSettings$method <- "qda"

table_qda <- crossValidate(train, K = num_folds, 
                               trainSettings = trainSettings, 
                               imputeSettings = imputeSettings)


#########################################
## Weighted K-Nearest Neighbors
#########################################
## Model to fit
trainSettings$method <- "kknn"
kmax <- seq(3, 19, by = 2)   # mtry=7 x K=5 x m=99 x ~sec=3 ~= 180min
table_knn_all <- list(kmax = double(),
                  xtabs = list())
table_knn <- list()

for (i in 1:length(kmax)) {
  print( paste0("----- k = ", i, " -----") )
  ## Step in the grid search for model tuning
  trainSettings$tuneGrid = expand.grid(kmax = kmax[i],              # seq(5, 15, by = 2),  # allows to test a range of k values
                                       distance = c(2),       # various Minkowski distances
                                       kernel = c('gaussian') # different weighting types in kknn))
  ) 
  
  table_knn_all$kmax[[i]] <- kmax[[i]]  ## Store k value
  table_knn_all$xtabs[[i]] <- crossValidate(train, K = num_folds, 
                                   trainSettings = trainSettings, 
                                   imputeSettings = imputeSettings)
}

temp.df <- data.frame(matrix(unlist(table_knn_all$xtabs), nrow=length(table_knn_all$xtabs), byrow=T))
temp.df <- cbind(kmax, temp.df) 
colnames(temp.df) <- c("kmax", names(table_knn_all$xtabs[[1]]))
table_knn$kmax <- table_knn_all$kmax[[which.max(temp.df$kappa)]]
table_knn$xtabs <- table_knn_all$xtabs[[which.max(temp.df$kappa)]]


#########################################
## Random Forests
#########################################
## Model to fit
trainSettings$method <- "rf"
mtry <- seq(3, 15, by = 2) # mtry=7 x K=5 x m=99 x ~sec=3 ~= 180min
table_rf_all <- list(mtry = double(),
                    xtabs = list())
table_rf <- list()

for (i in 1:length(mtry)) {
  print( paste0("----- mtry = ", i, " -----") )
  ## Step in the grid search for model tuning
  trainSettings$tuneGrid = expand.grid(mtry = mtry[i])    # seq(1, 10, by = 2))

  table_rf_all$mtry[[i]] <- mtry[[i]]  ## Store k value
  table_rf_all$xtabs[[i]] <- crossValidate(train, K = num_folds,
                                  trainSettings = trainSettings,
                                  imputeSettings = imputeSettings)
}

temp.df <- data.frame(matrix(unlist(table_rf_all$xtabs), nrow=length(table_rf_all$xtabs), byrow=T))
temp.df <- cbind(mtry, temp.df) 
colnames(temp.df) <- c("mtry", names(table_rf_all$xtabs[[1]]))
table_rf$mtry <- table_rf_all$mtry[[which.max(temp.df$kappa)]]
table_rf$xtabs <- table_rf_all$xtabs[[which.max(temp.df$kappa)]]



#########################################
## Save Trained Models 
#########################################
# Save multiple objects
file_name <- paste0("../_trained-models/trained-models-", impute_method, ".RData")
save(file = file_name,
     table_logit,
     table_lda,
     table_qda,
     table_knn,
     table_rf
)



#########################################
## Cleanup 
#########################################

## Cleaning variables
rm(train)
rm(test)

## Parallel Computing
#stopCluster(clusters)  ## Stop the cluster
registerDoSEQ() ## register the sequential backend
rm(numCores)
#rm(clusters)

# Table of metrics
rm(table_logit)
rm(table_lda)
rm(table_qda)
rm(table_knn)
rm(table_rf)









