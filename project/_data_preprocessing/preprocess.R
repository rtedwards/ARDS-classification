#########################################
## Author: Robert Edwards
## Data Preprocessing:  Cleaning, Standardizing, Imputation
#########################################


#########################################
## Set Project Directory
setwd("~/OneDrive - University of Glasgow/University of Glasgow/ARDS-classification/project/_data_preprocessing")
#########################################

#########################################
## Config 
library(config)
config <- config::get()

#source("_settings/functions.R")
#load_libraries(config$settings$libraries)
#load_sources(config$settings$sources)

#datasets <- load_datasets(config$settings$data_folder)
#########################################

#########################################
## Load theme 
source("../_settings/theme.R")
#########################################


#########################################
## Load libraries 
source("../_settings/libraries.R")
#########################################


#########################################
## Data Cleaning
#########################################

## Load data from .csv 
data.raw.df <- read.csv(file = "../data/ARDSdata_reorder.csv", header = TRUE)

##Rename first column
colnames(data.raw.df)[1] <- c("Pt_ID")

data.raw.df <- data.raw.df %>%
  dplyr::select("ECMO_Survival",
                "Gender",
                "Indication",
                "Age",
                "PreECMO_RR", 
                "PreECMO_Vt", 
                "PreECMO_FiO2", 
                "PreECMO_Ppeak", 
                "PreECMO_Pmean", 
                "PreECMO_PEEP", 
                "PreECMO_PF", 
                "PreECMO_SpO2", 
                "PreECMO_PaCO2", 
                "PreECMO_pH", 
                "PreECMO_BE", 
                "PreECMO_Lactate", 
                "PreECMO_NAdose", 
                "PreECMO_MAP", 
                "PreECMO_Creatinine", 
                "PreECMO_Urea", 
                "PreECMO_CK", 
                "PreECMO_Bilirubin", 
                "PreECMO_Albumin", 
                "PreECMO_CRP", 
                "PreECMO_Fibrinogen", 
                "PreECMO_Ddimer", 
                "PreECMO_ATIII", 
                "PreECMO_Leukocytes", 
                "PreECMO_Platelets", 
                "PreECMO_TNFa", 
                "PreECMO_IL6", 
                "PreECMO_IL8", 
                "PreECMO_siIL2") 


## Select the first 450 rows.  The others are blank
data.df <- data.raw.df %>%
  dplyr::slice(1:450) 


## Make categorical variables factors
data.df <- data.df %>%
  mutate(Indication = factor(Indication)) %>% 
  mutate(ECMO_Survival = factor(ECMO_Survival)) %>%
  mutate(Gender = factor(Gender))


#########################################
## Imputation 
## library("caret")
## https://topepo.github.io/caret/pre-processing.html#imputation
## Imputation (or list-weise deletion) should be done before scaling/standardizing
#########################################


#########################################
## PreProcessing
#########################################

data.clean.df <- data.df %>%
  select(-"PreECMO_Albumin") %>% # remove variables with >40% missing obs
  drop_na()  # drop rows with missing obs


## Listwise Deletion
set.seed(123)
data.imputed.listwise.df <- data.clean.df %>%
  preProcess( method = c("center", 
                         "scale",
                         "YeoJohnson"  ## Transformation method
                         )) %>%
  predict(data.clean.df) ## Generate new dataframe

## Sanity Checks
colMeans(data.imputed.listwise.df[, 4:ncol(data.imputed.listwise.df)]) ## Check that we get mean of 0
apply(data.imputed.listwise.df[, 4:ncol(data.imputed.listwise.df)], 2, sd) ## Check that we get sd of 1
sum(is.na(data.imputed.listwise.df)) ## Check for any missing observations
nrow(data.imputed.listwise.df) ## Check number of rows



## Median Imputation
set.seed(123)
data.imputed.median.df <- data.df %>%
  preProcess( method = c("center", 
                         "scale", 
                         "YeoJohnson",  ## Transformation method
                         "medianImpute")) %>%
  predict(data.df) ## Generate new dataframe

## Sanity Checks
colMeans(data.imputed.median.df[, 4:ncol(data.imputed.median.df)]) ## Check that we get mean of 0
apply(data.imputed.median.df[, 4:ncol(data.imputed.median.df)], 2, sd) ## Check that we get sd of 1
sum(is.na(data.imputed.median.df)) ## Check for any missing observations
nrow(data.imputed.median.df) ## Check number of rows



## KNN Imputation
set.seed(123)
data.imputed.knn.df <- data.df %>%
  preProcess( method = c("center", 
                         "scale", 
                         "YeoJohnson",  ## Transformation method
                         "knnImpute"),
              k = 5) %>%
  predict(data.df) ## Generate new dataframe

## Sanity Checks
colMeans(data.imputed.knn.df[, 4:ncol(data.imputed.knn.df)]) ## Check that we get mean of 0
apply(data.imputed.knn.df[, 4:ncol(data.imputed.knn.df)], 2, sd) ## Check that we get sd of 1
sum(is.na(data.imputed.knn.df)) ## Check for any missing observations
nrow(data.imputed.knn.df) ## Check number of rows


#########################################
## Predictive Mean Matching Imputation
#########################################
set.seed(123)
data.preprocess.df <- data.df %>%
  preProcess( method = c("center", 
                         "scale", 
                         "YeoJohnson"
                         )
            ) %>%
  predict(data.df) ## Generate new dataframe
              
imputed.pmm <- mice(data.preprocess.df, meth = "pmm", m = 10, maxit = 20, seed = 123, printFlag = FALSE)
data.imputed.pmm.df <- complete(imputed.pmm, action = "stacked")


## Sanity Checks
## https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
imputed.pmm$method ## imputation method on each variable


## Check if variables have converged
plot(imputed.pmm)

## Compare distributions of original and imputed data
xyplot(imputed.pmm, PreECMO_Albumin ~ PreECMO_RR+PreECMO_Ppeak+PreECMO_Pmean+PreECMO_PEEP+PreECMO_Bilirubin+PreECMO_Ddimer+PreECMO_ATIII+PreECMO_Leukocytes, pch=18, cex=.5)

xyplot(imputed.pmm, PreECMO_Albumin ~ PreECMO_Vt+PreECMO_CRP+PreECMO_Fibrinogen+PreECMO_TNFa, pch=18, cex=.5)

xyplot(imputed.pmm, PreECMO_Albumin ~ PreECMO_IL8+PreECMO_siIL2, pch=18, cex=.5)


densityplot(imputed.pmm)
stripplot(imputed.pmm, pch = c(1, 20), cex = 0.5)

modelFit1 <- with(imputed.pmm, glm(ECMO_Survival ~ PreECMO_RR+PreECMO_Ppeak+PreECMO_Pmean+PreECMO_PEEP+PreECMO_Bilirubin+PreECMO_Ddimer+PreECMO_ATIII+PreECMO_Leukocytes+PreECMO_Vt+PreECMO_CRP+PreECMO_Fibrinogen+PreECMO_TNFa+PreECMO_IL8+PreECMO_siIL2, family = "binomial"))
summary(pool(modelFit1))





#########################################
## Save Datasets
#########################################
write.csv(data.imputed.listwise.df, "../data/data-imputed-listwise.csv",
          row.names=FALSE)
write.csv(data.imputed.median.df, "../data/data-imputed-median.csv",
          row.names=FALSE)
write.csv(data.imputed.knn.df, "../data/data-imputed-knn.csv",
          row.names=FALSE)
write.csv(data.imputed.pmm.df, "../data/data-imputed-pmm5.csv",
          row.names=FALSE)
write.csv(data.imputed.pmm.df, "../data/data-imputed-pmm10.csv",
          row.names=FALSE)


#########################################
## Cleanup variables
#########################################
rm(data.raw.df)
rm(data.df)
rm(data.clean.df)
rm(data.categorical.df)
rm(data.continuous.df)
rm(data.imputed.listwise.df)
rm(data.imputed.median.df)
rm(data.imputed.knn.df)
rm(data.imputed.pmm.df)

