
####################################################
## imputeData()
####################################################
imputeData <- function(train, test, method, m, maxit = 10, seed = 123) {

  set.seed(123)
  
  ## Impute Training Set    
  train.imputed <- mice(train, 
                     meth = method,
                     m = m, 
                     maxit = maxit, 
                     seed = seed, 
                     printFlag = FALSE)
  
  # Stack the training sets into one long dataset
  train.imputed.df <- complete(train.imputed, action = "stacked")
  
  ## Making predictor matrix for test set
  predictor_matrix <- train.imputed$predictorMatrix
  predictor_matrix[, 1] <- 0 # Do not use outcome variable ECMO_Survival
  
  ## Concatenate Test Set with imputed Training Set
  concate.df <- rbind(train.imputed.df, test)
  
  ## Impute concatenated train and test sets
  concatenated <- mice(concate.df, 
                       meth = method, 
                       m = m,
                       maxit = maxit, 
                       seed = seed, 
                       printFlag = FALSE,
                       predictorMatrix = predictor_matrix)
  
  concatenated.df <- complete(concatenated, action = "all") # create list of imputed datasets
  
  ## Extract the 5 imputed Test Sets
  test.imputed.df <- list()
  for (i in 1:length(concatenated.df)) {
    test.imputed.df[[i]] <- concatenated.df[[i]] %>% # for each element in the list
      slice(-1:-nrow(train.imputed.df))         # select only the rows that are the test set
  }
  
  
  return(list(train.imputed.df, test.imputed.df))
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
  vote <- vote(pred)

  ## Calculate accuracy metrics
  model$sensitivity <- sensitivity(vote, obs)
  model$specificity <- specificity(vote, obs)
  
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
fitModel <- function(train, response, settings) {
  ## Call the caret::train() method here because Imputation needs to happen during 
  ## Cross-Validation.  This function allows for fitting a new model on each 
  ## k-validation set during CV. 
  
  set.seed(settings$seed)
  
  if (settings$method == "glmnet") {
    ## Used for Logit and LASSO models
    fit_model <- caret::train(response ~ ., 
                 data = train, 
                 method = settings$method, 
                 metric = settings$metric,
                 trControl = settings$trControl, 
                 family = "binomial",
                 tuneGrid = settings$tuneGrid
                 )
    return(fit_model)
  } ## end if
  
  if (settings$method == "lda") {
    fit_model <- caret::train(reponse ~ ., 
                 data = train,
                 method = settings$method, 
                 metric = settings$metric,
                 trControl = settings$trControl)
    return(fit_model)
  } ## end if
  
  if (settings$method == "qda") {
    fit_model <- caret::train(response ~ ., 
                              data = train,
                              method = settings$method, 
                              metric = settings$metric,
                              trControl = trControl)
    return(fit_model)
  } ## end if
  
  if (settings$method == "kknn") {
    fit_model <- caret::train(response ~ ., 
                              data = train,
                              method = settings$method, 
                              metric = metric,
                              trControl = trControl)
    return(fit_model)
  } ## end if
  
  if (settings$method == "rf") {
    fit_model <- caret::train(response ~ ., 
                              data = train,
                              method = settings$method, 
                              metric = settings$metric,
                              trControl = settings$trControl)
    return(fit_model)
  } ## end if
  
} ## end fitModel


#########################################
## crossValidate()
#########################################
crossValidate <- function(data, K, settings) {
  ## Will split training set into K folds and 
  
  ## Create K-folds
  folds <- caret::createFolds(data$ECMO_Survival, K)
  
  ## Create K Train and Test sets using the generated folds
  test_cv <- lapply(folds, function(ind, dat) dat[ind,], dat = data)
  train_cv <- lapply(folds, function(ind, dat) dat[-ind,], dat = data)
  
  
}

folds <- caret::createFolds(train$ECMO_Survival, k = 10)
test.cv <- lapply(folds, function(ind, dat) dat[ind,], dat = train)
train.cv <- lapply(folds, function(ind, dat) dat[-ind,], dat = train)

unlist(lapply(train.cv, ncol))



## List of settings to pass through to caret::train()
settings <- list()
settings$method <- "lda"
settings$trControl <- trControl
settings$tuneGrid  <- tuneGrid
settings$preProcess <- preProcess
settings$metric <- "kappa"
settings$seed <- 123


#########################################
## Testing Area
#########################################
kappa <- data.frame(Kappa=double()) ## create empty dataframe
kappa[1, ] <- validate(logit_model[[1]], test.impute.df, test.impute.df[[1]]$ECMO_Survival)
kappa[2, ] <- validate(lda_model[[1]], test.impute.df, test.impute.df[[1]]$ECMO_Survival)
kappa[3, ] <- validate(qda_model[[1]], test.impute.df, test.impute.df[[1]]$ECMO_Survival)




