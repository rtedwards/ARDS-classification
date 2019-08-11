
####################################################
## Imputation
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



validate <- function(models, test) {
  ## Each trained model is validated against each imputed test set
  ## The prediction of each trained model is the majority vote of the combined test sets
  ## The  
  
  pred <- list()
  vote = list()
  
  for (i in 1:length(models)) {
    ## For each model trained
    for (j in 1:length(test)) {
      ## Validate against each imputed test dataset
      pred[[j]] <- predict(models[i], test[j], type = "raw")
    }
    
    ## Create a n x m dataframe of predictions for each model fit
    pred.df <- as.data.frame(as.data.frame(pred))
    colnames(pred.df) <- NULL  # strip colnames
    vote_Y <- rowSums( as.matrix(pred.df) == "Y" ) # Sum number of "Y" in across test sets for each case
    vote_N <- rowSums( as.matrix(pred.df) == "N" ) # Sum number of "Y" in across test sets for each case
    
    vote[i] <- factor(ifelse( vote_Y > vote_N, "Y", "N"))
  
  }
  
}