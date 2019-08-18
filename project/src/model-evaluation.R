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

#########################################
## Load Data
#########################################
load("../data/processed-data.RData")

#load("../_trained-models/trained-models-complete-case.RData")
#impute_method <- "complete-case"

#load("../_trained-models/trained-models-mean.RData")
#impute_method <- "mean"

load("../_trained-models/trained-models-pmm.RData")
impute_method <- "pmm"

#########################################
## Settings
#########################################
numCores <- parallel::detectCores()
metric = "kappa" # good for imbalanced data


## Imputation Settings
imputeSettings <- list(
  method = impute_method, 
  m = 99,      # Number of imputed datasets to create
  maxit = 10,  # max number of iterations for imputation convergence
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
## PreProcess / Impute
#########################################
## Preprocess the data
set.seed(123)
train <- train %>%
  preProcess( method = c("center", 
                         "scale",
                         "YeoJohnson"  ## Transformation method
  )) %>%
  predict(train) ## Generate new dataframe


set.seed(123)
test <- test %>%
  preProcess( method = c("center", 
                         "scale",
                         "YeoJohnson"  ## Transformation method
  )) %>%
  predict(test) ## Generate new dataframe

## Impute the training and test sets
imputed_data <- imputeData(train, test, 
                           method = imputeSettings$method, 
                           m = imputeSettings$m, 
                           maxit = imputeSettings$maxit, 
                           seed = imputeSettings$seed,
                           numCores = imputeSettings$numCores
) 

train_imputed <- imputed_data$train 
tests_imputed <- imputed_data$test



#########################################
## Validate Against Test Data 
#########################################

valid_table <- list()



#########################################
## Logistic Regression 
#########################################
## Model to fit
trainSettings$method <- "glmnet"

## Step in the grid search for model tuning
trainSettings$tuneGrid = expand.grid(alpha = 1,   ## LASSO regularization
                                     lambda = 0)  ## No regularization

try({ ## handles errors thrown from rank deficiency in QDA models
  ## Fit model to imputed training set
  ## Validate fitted model against each test set and vote
  fit_model <- fitModel(train_imputed, settings = trainSettings)
  valid_table <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
  logit_metrics <- createTable(valid_table)
})




#########################################
## Linear Discriminant Analysis 
#########################################
trainSettings$method <- "lda"

try({ ## handles errors thrown from rank deficiency in QDA models
  ## Fit model to imputed training set
  ## Validate fitted model against each test set and vote
  fit_model <- fitModel(train_imputed, settings = trainSettings)
  valid_table <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
  lda_metrics <- createTable(valid_table)
})


#########################################
## Quadratic Discriminant Analysis 
#########################################
trainSettings$method <- "qda"

try({ ## handles errors thrown from rank deficiency in QDA models
  ## Fit model to imputed training set
  ## Validate fitted model against each test set and vote
  fit_model <- fitModel(train_imputed, settings = trainSettings)
  valid_table <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
  qda_metrics <- createTable(valid_table)
})

#########################################
## Weighted K-Nearest Neighbors
#########################################
trainSettings$method <- "kknn"
trainSettings$tuneGrid = expand.grid(kmax = table_knn$kmax,     
                                     distance = c(2),       # Euclidean distances
                                     kernel = c('gaussian') # Kernel type
                                     ) 

try({ ## handles errors thrown from rank deficiency in QDA models
  ## Fit model to imputed training set
  ## Validate fitted model against each test set and vote
  fit_model <- fitModel(train_imputed, settings = trainSettings)
  valid_table <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
  knn_metrics <- createTable(valid_table)
})
  



#########################################
## Random Forests
#########################################
## Model to fit
trainSettings$method <- "rf"
trainSettings$tuneGrid = expand.grid(mtry = table_rf$mtry)

try({ ## handles errors thrown from rank deficiency in QDA models
  ## Fit model to imputed training set
  ## Validate fitted model against each test set and vote
  fit_model <- fitModel(train_imputed, settings = trainSettings)
  valid_table <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
  rf_metrics <- createTable(valid_table)
})



#########################################
## Combine Metrics
#########################################

metrics_table <- rbind(logit_metrics, lda_metrics, qda_metrics, knn_metrics, rf_metrics)
rownames(metrics_table) <- c("Logit", "LDA", "QDA", "KNN", "RF")



#########################################
## Save Table
#########################################
# Save multiple objects
file_name <- paste0("../_metrics/metrics-", impute_method, "99.RData")
save(file = file_name,
     metrics_table
)


#########################################
## Cleanup 
#########################################

rm(list = ls())







