#########################################
## Author: Robert Edwards
## Model Training and Evaluation
#########################################


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
#ards <- read.csv(file = "../data/data-imputed-listwise.csv", header = TRUE)
#imputation_method <- "listwise"
#ards <- ards %>% 
#  select(-"PreECMO_Albumin")

#ards <- read.csv(file = "../data/data-imputed-median.csv", header = TRUE)
#imputation_method <- "median"

#ards <- read.csv(file = "../data/data-imputed-knn.csv", header = TRUE)
#imputation_method <- "knn"

ards <- read.csv(file = "../data/data-imputed-pmm10.csv", header = TRUE)
imputation_method <- "pmm10"



#########################################
## Data Splitting 
#########################################
set.seed(21)
trainIndex <- createDataPartition(ards$ECMO_Survival[1:450], 
                                  p = .8,      ## 75% data in training set
                                  list = FALSE, ## avoids returning data as a list
                                  times = 1)

## set the same trainIndex across the m imputated datasets
trainIndex <- rbind(trainIndex, 
                    trainIndex+450, 
                    trainIndex+(450*2), 
                    trainIndex+(450*3), 
                    trainIndex+(450*4),
                    trainIndex+(450*5),
                    trainIndex+(450*6),
                    trainIndex+(450*7),
                    trainIndex+(450*8),
                    trainIndex+(450*9)
                    )

head(trainIndex)

train <- ards[ trainIndex,]
test  <- ards[-trainIndex,]


#########################################
## Training Settings
#########################################
set.seed(21)

## Pre-Compute CV folds so we can use the same ones for all models
CV_folds <- createMultiFolds(train$ECMO_Survival, 
                             k = 5,     # 5-fold cross validation
                             times = 10 # 10 times
                             )

## K-Fold Cross Validation
trControl <- caret::trainControl(
  method = "repeatedcv",  # repeated K-fold cross-validation
  index = CV_folds,
  returnResamp = "all",
  classProbs = TRUE,      # Should be TRUE if metric = "ROC" Can skip if metric = "Kappa"
  savePredictions = TRUE,
#  sampling = "down",      # For imbalanced datasets c("down", "up", "rose", "smote")
  summaryFunction = twoClassSummary
)

## How is the data preprocessed?
# preProcess <- c("center",    # mean centered
#                "scale",      # sd = 1
#                "YeoJohnson", # Trnasform variables
#                "corr"        # remove correlated variables
#                )

## Model evaluation metric
metric <- "ROC"
#metric <- "Accuracy"

## Create a variable to store training times
training_times <- list()
colnames(training_times) <- c("logit", "LASSO", "LDA", "QDA", "KNN", "RF", "svmLinear", "svmRadial", "svmPoly")


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
set.seed(21)

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
set.seed(21)
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
set.seed(21)
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
set.seed(21)
tuneGrid <- expand.grid(kmax = seq(3, 15, by = 2),            # allows to test a range of k values
                        distance = seq(1, 1, by = 1), # allows to test a range of distance values
                        kernel = c('gaussian',  # different weighting types in kknn
                                   'triangular',
                                   'rectangular',
                                   'epanechnikov',
                                   'optimal'))

ptime <- system.time({
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

ggplot(knn_model)
knnPredict <- predict(knn_model, newdata = test )
confusionMatrix(knnPredict, test$ECMO_Survival )



#########################################
## Random Forest
## https://cran.r-project.org/web/packages/randomForest/randomForest.pdf
#########################################
set.seed(21)
tuneGrid <- expand.grid(mtry = c(1:10))
#mtry <- sqrt(ncol(train))

ptime <- system.time({
rf_model <- caret::train(ECMO_Survival ~ .,
                         data = train, 
                         method = "rf", 
                         metric = metric,
#                         preProcess = preProcess,
                         trControl = trControl,
                         tuneGrid = tuneGrid,
                         ntree = 1000 
#                         tuneLength = 10
                          )
})[3]
training_times[6]
rf_model


#########################################
## Support Vector Classifier
## 
#########################################
set.seed(21)

## I. Fit a Linear SVM kernel
tuneGrid <- expand.grid(C = seq(0.2, 1, by = 0.2))

svmTime1 <- system.time({
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
tuneGrid <- expand.grid(sigma = seq(0.2, 1, by = 0.2), 
                        C = seq(0.2, 1, by = 0.2))
svmTime2 <- system.time({
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
                        scale = seq(0.2, 1, by = 0.2),   # seq(0.1, 1, by = 0.1), 
                        C = seq(0.2, 1, by = 0.2)       # seq(0.1, 1, by = 0.1)
                        )

svmTime3 <- system.time({
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







#########################################
## Model Comparison 
#########################################
# resamps <- resamples(list(Linear = L_model, Poly = P_model, Radial = R_model))
# summary(resamps)
# bwplot(resamps, metric = "Accuracy")
# densityplot(resamps, metric = "Accuracy")
# 
# #Test a model's predictive accuracy Using Area under the ROC curve
# #Ideally, this should be done with a SEPERATE test set
# pSpecies <- predict(L_model,x,type='prob')
# colAUC(pSpecies,y,plot=TRUE)




#########################################
## Save Trained Models 
#########################################
# Save multiple objects
file_name <- paste0("../_trained-models/trained-models-", imputation_method, ".RData")
save(file = file_name,
     train,
     test,
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

# To load the data again
#load(file_name)

#########################################
## Cleanup 
#########################################

## Cleaning variables
rm(ards)
rm(train)
rm(test)
rm(trainIndex)
rm(tuneGrid)
rm(ctrl)

## Parallel Computing
stopCluster(clusters)  ## Stop the cluster
registerDoSEQ() ## register the sequential backend
rm(numCores)
rm(clusters)

rm(CV_folds)
rm(tuneGrid)

rm(lasso_model)
rm(lda_model)
rm(qda_model)
rm(knn_model)
rm(rf_model)
rm(svmLinear_model)
rm(svmPoly_model)
rm(svmRadial_model)

rm(file_name)






