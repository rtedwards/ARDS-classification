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
  m = 99,      # Number of imputed datasets to create
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
colnames(kappa_avg) <-  c("Logit", "LASSO", "LDA", "QDA", "KNN", "RF")


## Model to fit
trainSettings$method <- "glmnet"

## Step in the grid search for model tuning
trainSettings$tuneGrid = expand.grid(alpha = 1,   ## LASSO regularization
                                     lambda = 0)  ## No regularization

kappa_avg$Logit <- crossValidate(train, K = num_folds, 
                           trainSettings = trainSettings, 
                           imputeSettings = imputeSettings)


#########################################
## Linear Discriminant Analysis 
#########################################
## Model to fit
trainSettings$method <- "lda"

## Step in the grid search for model tuning
trainSettings$tuneGrid = expand.grid(alpha = 1,   ## LASSO regularization
                                     lambda = 0)  ## No regularization

kappa_avg$LDA <- crossValidate(train, K = num_folds, 
                           trainSettings = trainSettings, 
                           imputeSettings = imputeSettings)


#########################################
## Quadratic Discriminant Analysis 
#########################################
## Model to fit
trainSettings$method <- "qda"

## Step in the grid search for model tuning
trainSettings$tuneGrid = expand.grid(alpha = 1,   ## LASSO regularization
                                     lambda = 0)  ## No regularization

kappa_avg$QDA <- crossValidate(train, K = num_folds, 
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
  print("----- k = ", i, " -----")
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
  print("----- mtry = ", i, " -----")
  ## Step in the grid search for model tuning
  trainSettings$tuneGrid = expand.grid(mtry = mtry[i])    # seq(1, 10, by = 2))
 
  kappa_rf[i, 1] <- mtry[[i]]  ## Store k value
  kappa_rf[i, 2] <- crossValidate(train, K = num_folds, 
                                trainSettings = trainSettings, 
                                imputeSettings = imputeSettings)
}

kappa_avg

## Calculate accuracy metrics
# model$sensitivity <- sensitivity(vote, obs)
# model$specificity <- specificity(vote, obs)


#########################################
## ROC Plot 
#########################################



specificities <- as.data.frame(cbind(logit_roc$specificities,
                                     lasso_roc$specificities,
                                     lda_roc$specificities,
                                     qda_roc$specificities,
                                     knn_roc$specificities,
                                     rf_roc$specificities,
                                     svmLinear_roc$specificities,
                                     svmRadial_roc$specificities,
                                     svmPoly_roc$specificities
))
colnames(specificities) <- cbind("Logit", 
                                 "LASSO",
                                 "LDA",
                                 "QDA",
                                 "KNN",
                                 "RF",
                                 "svmLinear",
                                 "svmRadial",
                                 "svmPoly"
)
sensitivities <- as.data.frame(cbind(logit_roc$sensitivities,
                                     lasso_roc$sensitivities,
                                     lda_roc$sensitivities,
                                     qda_roc$sensitivities,
                                     knn_roc$sensitivities,
                                     rf_roc$sensitivities,
                                     svmLinear_roc$sensitivities,
                                     svmRadial_roc$sensitivities,
                                     svmPoly_roc$sensitivities
))
colnames(sensitivities) <- cbind("Logit", 
                                 "LASSO",
                                 "LDA",
                                 "QDA",
                                 "KNN",
                                 "RF",
                                 "svmLinear",
                                 "svmRadial",
                                 "svmPoly"
)

specificities <- specificities %>%
  gather(model, specificity, factor_key = TRUE)

sensitivities <- sensitivities %>%
  gather(model, sensitivity, factor_key = TRUE)

roc_df <- cbind(specificities, sensitivities[, 2])
colnames(roc_df) <- c("model", "specificity", "sensitivity")


library(ggplot2)
library(plotROC)

# Select a parameter setting
selectedIndices <- rf_model$pred$mtry == 2
ggplot(rf_model$pred[selectedIndices, ], 
       aes(m = M, d = factor(ECMO_Survival, levels = c("Y", "N")))) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()


roc_df %>% 
  group_by(model) %>%
  ggplot() + 
  aes(specificity, sensitivity) + 
  geom_roc(n.cuts=0) + 
  coord_equal() +
  style_roc()

g + annotate("text", x=0.75, y=0.25, label=paste("AUC =", round((calc_auc(g))$AUC, 4)))

#########################################
## Model Comparison 
#########################################
# resamps <- resamples(list(Linear = L_model, Poly = P_model, Radial = R_model))
# summary(resamps)
# bwplot(resamps, metric = "Accuracy")
# densityplot(resamps, metric = "Accuracy")
# 
# #test a model's predictive accuracy Using Area under the ROC curve
# #Ideally, this should be done with a SEPERATE test set
# pSpecies <- predict(L_model,x,type='prob')
# colAUC(pSpecies,y,plot=TRUE)




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

# Models
rm(logit_model)
rm(lasso_model)
rm(lda_model)
rm(qda_model)
rm(knn_model)
rm(rf_model)
rm(svmLinear_model)
rm(svmPoly_model)
rm(svmRadial_model)







