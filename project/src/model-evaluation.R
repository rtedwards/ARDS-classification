#########################################
## Author: Robert Edwards
## Model Training and Evaluation
#########################################


#########################################
## Set Project Directory
#########################################
setwd("~/OneDrive - University of Glasgow/University of Glasgow/ARDS-classification/project/src")
#setwd(getwd())

#########################################
## Run dependent files 
#########################################
#source("../_settings/libraries.R")
#source("../_data_preprocessing/impute.R")
#source("../src/training.R")


#########################################
## Load Data
#########################################
load("../_trained-models/trained-models-listwise.RData")

models <- list( logit = logit_model,
                lasso = lasso_model,
                lda = lda_model,
                qda = qda_model,
                knn = knn_model,
                rf = rf_model,
                svmLinear = svmLinear_model,
                svmRadial = svmRadial_model,
                svmPoly = svmPoly_model
              )

#########################################
## Predictions on train Set 
#########################################
predictions <- list()
for (i in 1:length(models)) {
  predictions[i] <- predict(models[i], test, type = "raw")
}


logit_pred <- predict(logit_model, test, type = "raw")
lasso_pred <- predict(lasso_model, test, type = "raw")
lda_pred <- predict(lda_model, test, type = "raw")
qda_pred <- predict(qda_model, test, type = "raw")
knn_pred <- predict(knn_model, test, type = "raw")
rf_pred <- predict(rf_model, test, type = "raw")
svmLinear_pred <- predict(svmLinear_model, test, type = "raw")
svmRadial_pred <- predict(svmRadial_model, test, type = "raw")
svmPoly_pred <- predict(svmPoly_model, test, type = "raw")

#########################################
## Confusion Matrices 
#########################################
xtab <- list()
for (i in 1:length(predictions)) {
  xtab[i] <- confusionMatrix(data = predictions[[i]], reference = test$ECMO_Survival)
}

logit_table <- confusionMatrix(data = logit_pred, reference = test$ECMO_Survival)
lasso_table <- confusionMatrix(data = lasso_pred, reference = test$ECMO_Survival)
lda_table <- confusionMatrix(data = lda_pred, reference = test$ECMO_Survival)
qda_table <- confusionMatrix(data = qda_pred, reference = test$ECMO_Survival)
knn_table <- confusionMatrix(data = knn_pred, reference = test$ECMO_Survival)
rf_table <- confusionMatrix(data = rf_pred, reference = test$ECMO_Survival)
svmLinear_table <- confusionMatrix(data = svmLinear_pred, reference = test$ECMO_Survival)
svmRadial_table <- confusionMatrix(data = svmRadial_pred, reference = test$ECMO_Survival)
svmPoly_table <- confusionMatrix(data = svmPoly_pred, reference = test$ECMO_Survival)








#########################################
## ROC Plot 
#########################################
library(ggplot2)
library(plotROC)

# Select a parameter setting
selectedIndices <- rf_model$pred$mtry == 2
ggplot(rf_model$pred[selectedIndices, ], 
       aes(m = M, d = factor(ECMO_Survival, levels = c("Y", "N")))) + 
  geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()


g <- ggplot(lasso_model$pred[lasso_model$bestTune$lambda ], 
            aes(m = M, d = factor(obs, levels = c("Y", "N")))
            ) + 
  geom_roc(n.cuts=0) + 
  coord_equal() +
  style_roc()

g + annotate("text", x=0.75, y=0.25, label=paste("AUC =", round((calc_auc(g))$AUC, 4)))



testROC <- function(model, data) {
  library(pROC)
  roc_obj <- roc(data$ECMO_Survival, 
                 predict(model, data, type = "prob")[, "Y"],
                 levels = c("N", "Y"))
#  ci(roc_obj)
}

logit_roc <- testROC(logit_model, test)
lasso_roc <- testROC(lasso_model, test)
lda_roc <- testROC(lda_model, test)
qda_roc <- testROC(qda_model, test)
knn_roc <- testROC(knn_model, test)
rf_roc <- testROC(rf_model, test)
svmLinear_roc <- testROC(svmLinear_model, test)
svmRadial_roc <- testROC(svmRadial_model, test)
svmPoly_roc <- testROC(svmPoly_model, test)

plot(logit_roc, col = "darkred")
plot(lasso_roc, add = TRUE, col = "red")
plot(lda_roc, add = TRUE, col = "orange")
plot(qda_roc, add = TRUE, col = "gold")
plot(knn_roc, add = TRUE, col = "green")
plot(rf_roc, add = TRUE, col = "blue")
plot(svmLinear_roc, add = TRUE, col = "purple")
plot(svmRadial_roc, add = TRUE, col = "brown")
plot(svmPoly_roc, add = TRUE, col = "black")


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







