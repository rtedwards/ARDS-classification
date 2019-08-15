
####################################################
## imputeData()
####################################################
imputeData <- function(train, test, method, m = 5, maxit = 10, seed = 123) {

  set.seed(seed)
  
  if (method == "complete-case") {
    train_complete.df <- train %>%
      select(-"PreECMO_Albumin") %>% # remove variables with >40% missing obs
      drop_na()  # drop rows with missing obs
    
    test_complete.df <- test %>%
      select(-"PreECMO_Albumin") %>% # remove variables with >40% missing obs
      drop_na()  # drop rows with missing obs
    
    ## Create imputed data object
    imputed_data <- list(train = train_complete.df,
                         test = list(test_complete.df) 
                         )
    
    return(imputed_data)
  }
  
  ## Selects predictors according to simple statistics to speed up imputation
  predictor_matrix <-  quickpred(train)
  
  ## Impute Training Set    
  train_imputed <- mice(train, 
                     nnodes = 8,
                     meth = method,
                     m = m, 
                     maxit = maxit, 
                     seed = seed, 
                     printFlag = FALSE,
                     predictorMatrix = predictor_matrix
                     )
  
  # Stack the training sets into one long dataset
  train_imputed.df <- complete(train_imputed, action = "stacked")
  
  # ## Making predictor matrix for test set
  # predictor_matrix <- train_imputed$predictorMatrix
  # predictor_matrix[, 1] <- 0 # Do not use outcome variable ECMO_Survival
  
  ## Concatenate Test Set with imputed Training Set
  concate.df <- rbind(train_imputed.df, test)
  
  ## Selects predictors according to simple statistics to speed up imputation
  predictor_matrix <-  quickpred(concate.df)
  predictor_matrix[, 1] <- 0 # Do not use outcome variable ECMO_Survival

  ## Impute concatenated train and test sets
  concatenated <- mice(concate.df, 
                       meth = method, 
                       m = m,
                       maxit = maxit, 
                       seed = seed, 
                       printFlag = FALSE,
                       predictorMatrix = predictor_matrix
                       )
  
  concatenated.df <- complete(concatenated, action = "all") # create list of imputed datasets
  
  ## Extract the 5 imputed Test Sets
  test_imputed.df <- list()
  for (i in 1:length(concatenated.df)) {
    test_imputed.df[[i]] <- concatenated.df[[i]] %>% # for each element in the list
      slice(-1:-nrow(train_imputed.df))         # select only the rows that are the test set
  }
  
  ## Create imputed data object
  imputed_data <- list(train = train_imputed.df,
                       test = test_imputed.df)
  
  return(imputed_data)
}



#########################################
## validate()
#########################################
validate <- function(model, tests, obs) {
  ## The model is validated against each imputed test set
  ## The prediction is the majority vote of the combined test sets
  ## We can validate against any of the m test sets because the outcome has 0% missingness
  
  pred <- list()
  for (i in 1:length(tests)) {
    ## Validate against each imputed test dataset
    pred[[i]] <- predict(model, tests[i], type = "raw")
  }
  
  ## Vote for final predictions
  vote <- vote(predictions = pred)
  
  ## Create dataframe of predictions and observations to validate
  validation <- cbind( as.data.frame(vote), obs )
  colnames(validation) <- c("pred", "obs")
  
  validation <- validation %>%  ## Change variables to factors
    mutate(pred = factor(pred)) %>%
    mutate(obs = factor(obs))
  
  ## Create confusion matrix with "N" taken as the positive level (event = death)
  xtab <- confusionMatrix(data = validation$pred, reference = validation$obs, positive = "N" )
  
  ## Extract kappa for use as accuracy metric in cross-validation
  kappa <- xtab$overall[[2]]
  
  return(kappa)
} ## end validate()


#########################################
## vote()
#########################################
vote <- function(predictions) {
  ## Takes a list of multiple predictions and tallys votes (across rows) for the 
  ## final prediction used. 

  ## Create a n x m dataframe of predictions for each model fit
  predictions.df <- as.data.frame(as.data.frame(predictions))
  colnames(predictions.df) <- NULL  # strip colnames
  
  vote_Y <- rowSums( as.matrix(predictions.df) == "Y" ) # Sum number of "Y" in across test sets for each case
  vote_N <- rowSums( as.matrix(predictions.df) == "N" ) # Sum number of "Y" in across test sets for each case
  
  final_vote <- factor(ifelse( vote_Y > vote_N, "Y", "N"))
  
  return(final_vote)
} ## end vote()



#########################################
## fitModel()
#########################################
fitModel <- function(train, settings) {
  ## Call the caret::train() method here because Imputation needs to happen during 
  ## Cross-Validation.  This function allows for fitting a new model on each 
  ## k-validation set during CV. 
  
  set.seed(settings$seed)
  
  trControl <- caret::trainControl(
    method = "none",        # No cross-validation (because imputation needs to be done on every validation set)
    returnResamp = "all",
    classProbs = TRUE,      # Should be TRUE if metric = "ROC" Can skip if metric = "Kappa"
    savePredictions = TRUE,
    allowParallel = TRUE,   # Allow parallel processing
    summaryFunction = twoClassSummary
  )
  
  
  if (settings$method == "glmnet") {
    ## Used for Logit and LASSO models
    fit_model <- caret::train(settings$formula, 
                              data = train, 
                              method = settings$method, 
                              metric = settings$metric,
                              trControl = trControl, 
                              family = "binomial",
                              tuneGrid = settings$tuneGrid
                              )
    return(fit_model)
  } ## end if
  
  if (settings$method == "lda") {
    fit_model <- caret::train(settings$formula, 
                              data = train,
                              method = settings$method, 
                              metric = settings$metric,
                              trControl = trControl)
    return(fit_model)
  } ## end if
  
  if (settings$method == "qda") {
#    print(cor(train[, 4:ncol(train)]))
    fit_model <- caret::train(settings$formula, 
                              data = train,
                              method = settings$method, 
                              metric = settings$metric,
                              trControl = settings$trControl
#                              preProcess = c("corr", "zv", "nzv")  ## Drop correlated variables
                              )
    return(fit_model)
  } ## end if
  
  if (settings$method == "kknn") {
    fit_model <- caret::train(settings$formula, 
                              data = train,
                              method = settings$method, 
                              metric = settings$metric,
                              trControl = settings$trControl)
    return(fit_model)
  } ## end if
  
  if (settings$method == "rf") {
    fit_model <- caret::train(settings$formula, 
                              data = train,
                              method = settings$method, 
                              metric = settings$metric,
                              trControl = settings$trControl,
                              tuneGrid = settings$tuneGrid
                              )
    return(fit_model)
  } ## end if
  
} ## end fitModel


#########################################
## crossValidate()
#########################################
crossValidate <- function(data, K, trainSettings, imputeSettings) {
  ## Will split training set into K folds and 
  
  ## Create K-folds
  folds <- caret::createFolds(data[, 1], K)
  
  ## Create K Train and Test sets using the generated folds
  test_cv <- lapply(folds, function(ind, dat) dat[ind,], dat = data)
  train_cv <- lapply(folds, function(ind, dat) dat[-ind,], dat = data)
  
  ## Create a dataframe to store kappa values
  validate <- data.frame(Kappa = double()) ## create empty dataframe
  
  ## For each fold: 
  ## - impute the train and test, 
  ## - fit a model on the training set
  ## - validate the model against each of the test sets
  ## - calculate accuracy scoree (kappa) for each validation
  for (i in 1:length(test_cv)) {
    
    ## Impute the training and test sets
    imputation_time <- system.time({
    imputed_data <- imputeData(train_cv[[i]], test_cv[[i]], 
               method = imputeSettings$method, 
               m = imputeSettings$m, 
               maxit = imputeSettings$maxit, 
               seed = imputeSettings$seed
               ) 
    })[3]
    print( paste0("CV ", i, " - imputation time: ", round(imputation_time, 3)) )
    
    train_imputed <- imputed_data$train 
    tests_imputed <- imputed_data$test
    
    
    try({ ## handles errors thrown from rank deficiency in QDA models
      ## Fit model to imputed training set
      fit_model <- fitModel(train_imputed, settings = trainSettings)
      
      ## Validate fitted model against each test set
      ## Using the observed classes in the first test set
      validate[i, ] <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
    })


  }## end for  
  
  averaged_metric <- mean(validate$Kappa)
  
  return(averaged_metric)
    
}## end crossValidate()










# #########################################
# ## Testing CrossValidate()
# #########################################
# 
# data <- train
# K <- 10
# 
# folds <- caret::createFolds(data[, 1], K)
# 
# ## Create K Train and Test sets using the generated folds
# test_cv <- lapply(folds, function(ind, dat) dat[ind,], dat = data)
# train_cv <- lapply(folds, function(ind, dat) dat[-ind,], dat = data)
# 
# 
# ## For each fold: 
# ## - impute the train and test, 
# ## - fit a model on the training set
# ## - validate the model against each of the test sets
# ## - calculate accuracy scoree (kappa) for each validation
# for (i in 1:length(test_cv)) {
#   
#   print("Imputing")
#   ## Impute the training and test sets
#   imputed_data <- imputeData(train_cv[[i]], test_cv[[i]], 
#                              method = imputeSettings$method, 
#                              m = 5, 
#                              maxit = imputeSettings$maxit, 
#                              seed = imputeSettings$seed
#   ) 
# 
#   train_imputed <- imputed_data$train 
#   tests_imputed <- imputed_data$test
# 
#   ## Training settings
#   ## *** This can't be passed in with the trainSettings for some reason ***
#   trControl <- caret::trainControl(
#     method = "none",        # No cross-validation (because imputation needs to be done on every validation set)
#     returnResamp = "all",
#     savePredictions = TRUE,
#     allowParallel = TRUE,   # Allow parallel processing
#     summaryFunction = twoClassSummary
#   )
#   
#   tuneGrid = expand.grid(alpha = 1,   ## LASSO regularization
#                          lambda = 0)  ## No regularization
#   ## Fit model to imputed training set
#   fit_model <- caret::train(ECMO_Survival ~ ., 
#                             data = train_imputed, 
#                             method = "glmnet", 
#                             metric = metric,
#                             trControl = trControl, 
#                             #                            preProcess = preProcess,
#                             family = "binomial",
#                             tuneGrid = tuneGrid
#                             )
#   
# #  fit_model <- fitModel(train_imputed, settings = trainSettings)
#   print("Model Fit")
#   
#   ## Validate fitted model against each test set
#   ## Using the observed classes in the first test set
#   kappa <- data.frame(Kappa = double()) ## create empty dataframe
#   
#   pred <- list()
#   for (i in 1:length(tests_imputed)) {
#     ## Validate against each imputed test dataset
#     pred[[i]] <- predict(fit_model, tests_imputed[i], type = "raw")
#   }
#   
# 
#   
#   print("Validating Model")
#   kappa[1, ] <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[i]][, 1])
#   print("Model Validated")
#   
# }## end for  
# 
# averaged_metric <- mean(validate$Kappa)
# 
# 
# #########################################
# ## Testing Area
# #########################################
# 
# folds <- caret::createFolds(train$ECMO_Survival, k = 10)
# test.cv <- lapply(folds, function(ind, dat) dat[ind,], dat = train)
# train.cv <- lapply(folds, function(ind, dat) dat[-ind,], dat = train)
# 
# unlist(lapply(train.cv, ncol))
# 
# 
# train_settings <- list()
# train_settings$formula <- as.formula("ECMO_Survival ~ .") 
# 
# 
# ## List of settings to pass through to caret::train()
# settings <- list()
# settings$method <- "lda"
# settings$trControl <- trControl
# settings$tuneGrid  <- tuneGrid
# settings$preProcess <- preProcess
# settings$metric <- "kappa"
# settings$seed <- 123
# 
# 
# 
# kappa <- data.frame(Kappa=double()) ## create empty dataframe
# kappa[1, ] <- validate(logit_model[[1]], test.impute.df, test.impute.df[[1]]$ECMO_Survival)
# kappa[2, ] <- validate(lda_model[[1]], test.impute.df, test.impute.df[[1]]$ECMO_Survival)
# kappa[3, ] <- validate(qda_model[[1]], test.impute.df, test.impute.df[[1]]$ECMO_Survival)
# 



