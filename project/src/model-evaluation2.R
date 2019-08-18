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
#source("../_data_preprocessing/preprocess.R")
#source("../src/training.R")


#########################################
## Kappa
#########################################
load("../_trained-models/trained-models-complete-case.RData")
kappa_cc <- as.data.frame(cbind(kappa_logit, kappa_lda, kappa_qda, max(kappa_knn$kappa), max(kappa_rf$kappa)) )

load("../_trained-models/trained-models-mean.RData")
kappa_mean <- as.data.frame(cbind(kappa_logit, kappa_lda, kappa_qda, max(kappa_knn$kappa), max(kappa_rf$kappa)) )

load("../_trained-models/trained-models-pmm99.RData")
kappa_pmm <- as.data.frame(cbind(kappa_logit, kappa_lda, kappa_qda, max(kappa_knn$kappa), max(kappa_rf$kappa)) )

kappa_table <- rbind(kappa_cc, kappa_mean, kappa_pmm)
kappa_table <- round(kappa_table, 3)
colnames(kappa_table) <- c("Logit", "LDA", "QDA", "KNN", "RF")
rownames(kappa_table) <- c("Complete Case", "Mean", "PMM")


#########################################
## Sensitivity
#########################################
# load("../_trained-models/trained-models-complete-case.RData")
# kappa_cc <- as.data.frame(cbind(kappa_logit, kappa_lda, kappa_qda, max(kappa_knn$kappa), max(kappa_rf$kappa)) )
# 
# load("../_trained-models/trained-models-mean.RData")
# kappa_mean <- as.data.frame(cbind(kappa_logit, kappa_lda, kappa_qda, max(kappa_knn$kappa), max(kappa_rf$kappa)) )
# 
# load("../_trained-models/trained-models-pmm99.RData")
# kappa_pmm <- as.data.frame(cbind(kappa_logit, kappa_lda, kappa_qda, max(kappa_knn$kappa), max(kappa_rf$kappa)) )
# 
# kappa_table <- rbind(kappa_cc, kappa_mean, kappa_pmm)
# colnames(kappa_table) <- c("Logit", "LDA", "QDA", "KNN", "RF")
# rownames(kappa_table) <- c("Complete Case", "Mean", "PMM")



fit_model <- fitModel(train_imputed, settings = trainSettings)

## Validate fitted model against each test set
## Using the observed classes in the first test set
#      validate[i, ] <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])

valid_table[[i]] <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
#      print("xtab: ")
#      print(valid_table[[i]])
#      print("Kappa: ")
#      print(valid_table[[i]]$overall[[2]])

validated[i,1 ] <- valid_table[[i]]$overall[[1]] # Extract Accuracy from returned table
validated[i, 2] <- valid_table[[i]]$overall[[2]] # Extract Kappa from returned table
validated[i, 3] <- valid_table[[i]]$byClass[[1]] # Extract Sensitivity from returned table
validated[i, 4] <- valid_table[[i]]$byClass[[2]] # Extract Specificity from returned table
validated[i, 5] <- valid_table[[i]]$byClass[[3]] # Extract Pos Pred Value from returned table
validated[i, 6] <- valid_table[[i]]$byClass[[4]] # Extract Neg Pred Value from returned table
validated[i, 7] <- valid_table[[i]]$byClass[[5]] # Extract Precision from returned table
validated[i, 8] <- valid_table[[i]]$byClass[[6]] # Extract Recall from returned table
validated[i, 9] <- valid_table[[i]]$byClass[[7]] # Extract F1 from returned table
validated[i, 10] <- valid_table[[i]]$byClass[[8]] # Extract Prevalence from returned table
validated[i, 11] <- valid_table[[i]]$byClass[[9]] # Extract Detection Rate from returned table
validated[i, 12] <- valid_table[[i]]$byClass[[10]] # Extract Detection Prevalence from returned table
validated[i, 13] <- valid_table[[i]]$byClass[[11]] # Extract Balanced Accuracy from returned table









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
stopCluster(clusters)  ## Stop the cluster
registerDoSEQ() ## register the sequential backend


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

for (i in 1:length(table_knn$kmax)) {a[[i]] <- mean(table_knn$xtabs[[i]]$kappa)}

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







