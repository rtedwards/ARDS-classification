
####################################################
## imputeData()
####################################################
imputeData <- function(train, test, method, m = 5, maxit = 10, seed = 123, numCores = 8) {

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
  train_imputed <- micemd::mice.par(train, 
                     nnodes = numCores,
                     meth = method,
                     m = m, 
                     maxit = maxit, 
                     seed = seed, 
                     printFlag = FALSE,
                     predictorMatrix = predictor_matrix
                     )
  
  # Stack the training sets into one long dataset
  train_imputed.df <- complete(train_imputed, action = "stacked")
  
  ## Concatenate Test Set with imputed Training Set
  concate.df <- rbind(train_imputed.df, test)
  
  ## Selects predictors according to simple statistics to speed up imputation
  predictor_matrix <-  quickpred(concate.df)
  predictor_matrix[, 1] <- 0 # Do not use outcome variable ECMO_Survival

  ## Impute concatenated train and test sets
  concatenated <- micemd::mice.par(concate.df, 
                           nnodes = numCores,
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
  
  return(xtab)
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
  
#  print("Vote Yes")
#  print(vote_Y)

  
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
#    returnResamp = "all",
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
    fit_model <- caret::train(settings$formula, 
                              data = train,
                              method = settings$method, 
                              metric = settings$metric,
                              trControl = settings$trControl,
                              preProcess = c("corr")  ## Drop correlated variables
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
  
  if (settings$method == "svmRadial") {
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
  validated <- data.frame(accuracy = double(),
                          kappa = double(),
                          sensitivity = double(),
                          specificity = double(),
                          posPredValue = double(),
                          negPredValue = double(),
                          precision = double(),
                          recall = double(),
                          F1 = double(),
                          prevalence = double(),
                          detectionRate = double(),
                          detectionPrevalence = double(),
                          balancedAccuracy = double()) ## create empty dataframe
  
  valid_table <- list()
  
  ## For each fold: 
  ## - impute the train and test, 
  ## - fit a model on the training set
  ## - validate the model against each of the test sets
  ## - calculate accuracy scoree (kappa) for each validation
  for (i in 1:length(test_cv)) {
    
    ## Preprocess the data
    set.seed(123)
    train_cv[[i]] <- train_cv[[i]] %>%
      preProcess( method = c("center", 
                             "scale",
                             "YeoJohnson"  ## Transformation method
      )) %>%
      predict(train_cv[[i]]) ## Generate new dataframe
    
    
    set.seed(123)
    test_cv[[i]] <- test_cv[[i]] %>%
      preProcess( method = c("center", 
                             "scale",
                             "YeoJohnson"  ## Transformation method
      )) %>%
      predict(test_cv[[i]]) ## Generate new dataframe
    
    ## Impute the training and test sets
    imputation_time <- system.time({
    imputed_data <- imputeData(train_cv[[i]], test_cv[[i]], 
               method = imputeSettings$method, 
               m = imputeSettings$m, 
               maxit = imputeSettings$maxit, 
               seed = imputeSettings$seed,
               numCores = imputeSettings$numCores
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
#      validate[i, ] <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
      
      valid_table[[i]] <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
      
      validated[i, 1] <- valid_table[[i]]$overall[[1]] # Extract Accuracy from returned table
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
    })


  }## end for  
  
  ## Average the K folds
  avg_validated <- sapply(validated, FUN = mean, MARGIN = 2, na.rm = TRUE)

  return(avg_validated)  
}## end crossValidate()



#########################################
## testModel()
#########################################
createTable <- function(xtab) {
  ## Create a dataframe to store kappa values
  tested <- data.frame(accuracy = double(),
                          kappa = double(),
                          sensitivity = double(),
                          specificity = double(),
                          posPredValue = double(),
                          negPredValue = double(),
                          precision = double(),
                          recall = double(),
                          F1 = double(),
                          prevalence = double(),
                          detectionRate = double(),
                          detectionPrevalence = double(),
                          balancedAccuracy = double()) ## create empty dataframe
  
  tested[1, 1] <- xtab$overall[[1]] # Extract Accuracy from returned table
  tested[1, 2] <- xtab$overall[[2]] # Extract Kappa from returned table
  tested[1, 3] <- xtab$byClass[[1]] # Extract Sensitivity from returned table
  tested[1, 4] <- xtab$byClass[[2]] # Extract Specificity from returned table
  tested[1, 5] <- xtab$byClass[[3]] # Extract Pos Pred Value from returned table
  tested[1, 6] <- xtab$byClass[[4]] # Extract Neg Pred Value from returned table
  tested[1, 7] <- xtab$byClass[[5]] # Extract Precision from returned table
  tested[1, 8] <- xtab$byClass[[6]] # Extract Recall from returned table
  tested[1, 9] <- xtab$byClass[[7]] # Extract F1 from returned table
  tested[1, 10] <- xtab$byClass[[8]] # Extract Prevalence from returned table
  tested[1, 11] <- xtab$byClass[[9]] # Extract Detection Rate from returned table
  tested[1, 12] <- xtab$byClass[[10]] # Extract Detection Prevalence from returned table
  tested[1, 13] <- xtab$byClass[[11]] # Extract Balanced Accuracy from returned table
  
  return(tested)
} ## end createTable()






# #########################################
# ## Testing CrossValidate()
# #########################################

# data <- train
# K <- 10
# 
# folds <- caret::createFolds(data[, 1], K)
# 
# ## Create K Train and Test sets using the generated folds
# test_cv <- lapply(folds, function(ind, dat) dat[ind,], dat = data)
# train_cv <- lapply(folds, function(ind, dat) dat[-ind,], dat = data)
# 
# ## Create a dataframe to store kappa values
# validated <- data.frame(accuracy = double(),
#                        kappa = double(),
#                        sensitivity = double(),
#                        specificity = double(),
#                        posPredValue = double(),
#                        negPredValue = double(),
#                        precision = double(),
#                        recall = double(),
#                        F1 = double(),
#                        prevalence = double(),
#                        detectionRate = double(),
#                        detectionPrevalence = double(),
#                        balancedAccuracy = double()) ## create empty dataframe
# 
# valid_table <- list()
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
#   
#   print("Fitting Model")
#   fit_model <- fitModel(train_imputed, settings = trainSettings)
#   ## Fit model to imputed training set
#   # fit_model <- caret::train(ECMO_Survival ~ .,
#   #                           data = train_imputed,
#   #                           method = "glmnet",
#   #                           metric = "kappa",
#   #                           trControl = trControl,
#   #                           #                            preProcess = preProcess,
#   #                           family = "binomial",
#   #                           tuneGrid = tuneGrid
#   #                           )
# 
# #  fit_model <- fitModel(train_imputed, settings = trainSettings)
# 
#   ## Validate fitted model against each test set
#   ## Using the observed classes in the first test set
#   kappa <- data.frame(Kappa = double()) ## create empty dataframe
# 
#   pred <- list()
#   for (j in 1:length(tests_imputed)) {
#     ## Validate against each imputed test dataset
#     pred[[j]] <- predict(fit_model, tests_imputed[j], type = "raw")
#   }
# 
# 
#   valid_table[[i]] <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[1]][, 1])
#   # print("xtab: ")
#   # print(valid_table[[i]])
#   # print("Kappa: ")
#   # print(valid_table[[i]]$overall[[2]])
# 
#   validated[i,1 ] <- valid_table[[i]]$overall[[1]] # Extract Accuracy from returned table
#   validated[i, 2] <- valid_table[[i]]$overall[[2]] # Extract Kappa from returned table
#   validated[i, 3] <- valid_table[[i]]$byClass[[1]] # Extract Sensitivity from returned table
#   validated[i, 4] <- valid_table[[i]]$byClass[[2]] # Extract Specificity from returned table
#   validated[i, 5] <- valid_table[[i]]$byClass[[3]] # Extract Pos Pred Value from returned table
#   validated[i, 6] <- valid_table[[i]]$byClass[[4]] # Extract Neg Pred Value from returned table
#   validated[i, 7] <- valid_table[[i]]$byClass[[5]] # Extract Precision from returned table
#   validated[i, 8] <- valid_table[[i]]$byClass[[6]] # Extract Recall from returned table
#   validated[i, 9] <- valid_table[[i]]$byClass[[7]] # Extract F1 from returned table
#   validated[i, 10] <- valid_table[[i]]$byClass[[8]] # Extract Prevalence from returned table
#   validated[i, 11] <- valid_table[[i]]$byClass[[9]] # Extract Detection Rate from returned table
#   validated[i, 12] <- valid_table[[i]]$byClass[[10]] # Extract Detection Prevalence from returned table
#   validated[i, 13] <- valid_table[[i]]$byClass[[11]] # Extract Balanced Accuracy from returned table
# 
#   # print("Validating Model")
#   # kappa[1, ] <- validate(model = fit_model, tests = tests_imputed, obs = tests_imputed[[i]][, 1])
#   # print("Model Validated")
# 
# }## end for
# 
# averaged_metric <- mean(validated$Kappa)
# 
# 



