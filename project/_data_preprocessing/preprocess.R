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
#config <- config::get()

#source("_settings/functions.R")
#load_libraries(config$settings$libraries)
#load_sources(config$settings$sources)
#datasets <- load_datasets(config$settings$data_folder)
#
#########################################

#########################################
## Load theme 
#source("../_settings/theme.R")
#########################################


#########################################
## Load libraries 
source("../_settings/libraries.R")
source("../_settings/functions.R")
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
## Scale Data
#########################################
set.seed(123)
data.scaled.df <- data.df %>%
  preProcess( method = c("center", 
                         "scale",
                         "YeoJohnson"  ## Transformation method
  )) %>%
  predict(data.df) ## Generate new dataframe


#########################################
## Data Splitting 
#########################################
set.seed(123)
train_index <- createDataPartition(data.scaled.df$ECMO_Survival[1:nrow(data.scaled.df)], 
                                  p = .75,      ## 75% data in training set
                                  list = FALSE, ## avoids returning data as a list
                                  times = 1)


train <- data.scaled.df[ train_index, ]
test  <- data.scaled.df[-train_index, ]


#########################################
## Imputation 
## library("caret")
## https://topepo.github.io/caret/pre-processing.html#imputation
## Imputation (or list-weise deletion) should be done before scaling/standardizing
#########################################



# #########################################
# ## Complete Case (Listwise)
# #########################################
# 
# train.complete.df <- train %>%
#   select(-"PreECMO_Albumin") %>% # remove variables with >40% missing obs
#   drop_na()  # drop rows with missing obs
# 
# test.complete.df <- test %>%
#   select(-"PreECMO_Albumin") %>% # remove variables with >40% missing obs
#   drop_na()  # drop rows with missing obs
# 
# 
# #########################################
# ## Mean Imputation 
# #########################################
# set.seed(123)
# imputed_data <- imputeData(train, test, method = "mean", m = 1)
# 
# train.mean.df <- imputed_data$train
# test.mean.df <- imputed_data$test
# 
# 
# #########################################
# ## (5) Predictive Mean Matching Imputation 
# #########################################
# set.seed(123)
# imputed_data <- imputeData(train, test, method = "pmm", m = 5)
# 
# train.mean.df <- imputed_data$train
# test.mean.df <- imputed_data$test



#########################################
## Save Objects 
#########################################
# Save multiple objects
save(file = "../data/processed-data.RData",
     train_index,
     train,
     test
     # train.complete.df,
     # test.complete.df,
     # train.mean.df,
     # test.mean.df,
     # train.pmm.df,
     # test.pmm.df
)




#########################################
## Cleanup variables
#########################################
rm(data.raw.df)
rm(data.df)
rm(data.scaled.df)
rm(train_index)
rm(train)
rm(test)
# rm(train.complete.df)
# rm(test.complete.df)
# rm(train.mean.df)
# rm(test.mean.df)
# rm(train.pmm.df)
# rm(test.pmm.df)
# rm(imputed_data)
        

