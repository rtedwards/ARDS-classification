## Robert Edwards
## Importing and imputing missing data

library(ggplot2)
library(dplyr)
library(moderndive)
library(skimr)
library(tidyr)
library(kableExtra)
library(gridExtra)
library(xtable)
library(knitr)
library(GGally)
library(broom)

ARDSdata.df <- read.csv(file = "data/ARDSdata.csv", header = TRUE)

Day1_vars <- c("Day1ECMO_RR", 
               "Day1ECMO_Vt", 
               "Day1ECMO_FiO2", 
               "Day1ECMO_Ppeak", 
               "Day1ECMO_Pmean", 
               "Day1ECMO_PEEP", 
               "Day1ECMO_PF", 
               "Day1ECMO_SpO2", 
               "Day1ECMO_PaCO2", 
               "Day1ECMO_pH", 
               "Day1ECMO_BE", 
               "Day1ECMO_Lactate", 
               "Day1ECMO_NAdose", 
               "Day1ECMO_MAP", 
               "Day1ECMO_Creatinine", 
               "Day1ECMO_Urea", 
               "Day1ECMO_CK", 
               "Day1ECMO_Bilirubin", 
               "Day1ECMO_Albumin", 
               "Day1ECMO_CRP", 
               "Day1ECMO_Fibrinogen", 
               "Day1ECMO_Ddimer", 
               "Day1ECMO_ATIII", 
               "Day1ECMO_Leukocytes", 
               "Day1ECMO_Platelets", 
               "Day1ECMO_TNFa", 
               "Day1ECMO_IL6", 
               "Day1ECMO_IL8", 
               "Day1ECMO_siIL2")
data <- data.raw.df[, -which(names(data.raw.df) %in% Day1_vars)]

data %>%
  





