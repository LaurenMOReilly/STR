# PROGRAM: STR_Initialization.R
# STUDY: STR - Short-Term Risk for SB 
# DESCRIPTION: Initialization: load packages, load data, set local macros
# INPUTS: Z:/Projects/STR/Posted/dxrx_parentd_merge_export.sas7bdat
# OUTPUTS: N/A
# DATE CREATED: 2021-09-21
# COMPLETED:
# AUTHOR: LMO ( loreilly at iu dot edu ) 
# NOTE: 1mo outcome is not included in manuscript; main outcome=12mo, sensitivity analysis=6mo

###Set working directory###
setwd ("Z:/Projects/STR/Progress")

###Load packages###
if (0) { # deprecated
  install.packages('haven') #Load SAS file
  library(haven)
  install.packages('groupdata2') #Split
  library(groupdata2)
  install.packages('scales') #Scale/center
  library('scales')
  install.packages('dplyr') #Quartile
  library(dplyr)
  install.packages("usethis") #SMOTE
  library(usethis)
  install.packages("devtools")
  library(devtools)
  install.packages('pROC') #AUC plots
  library(pROC)
  install.packages('PRROC') #PRAUC plots
  library(PRROC)
  install.packages('glmnet')
  library(glmnet)
  install.packages('ROCR')
  library(ROCR)
  install.packages('caret')
  library(caret)
  install.packages('randomForest')
  library(randomForest)
  install.packages('epiR') #To calculate Kappa
  library(epiR) 
  install.packages('smotefamily')
  library(smotefamily)
  install.packages('xtable')
  library(xtable)
  install.packages('xgboost')
  library(xgboost)
  install.packages('e1071')
  library(e1071)
  install.packages('measures')
  library(measures)
  install.packages('predtools')
  library(predtools)
}

#Package for ICI
install.packages("remotes")
remotes::install_github("gweissman/gmish")
devtools::install_github("srhaile/mscpredmodel")

###Call to/run loadLibraries file (loads libaries)###
source('Z:/Projects/STR/R Code/STR_loadLibraries.R')

###Read in initial file from SAS###
#Ensure no missing data
DATA = read_sas(data_file = "Z:/Projects/STR/Posted/dxrx_parentd_merge_export.sas7bdat")
DATA  = na.omit(DATA)
#Convert LOPNR to factor, as required in groupdata2::partition code
DATA$LOPNR = factor(DATA$LOPNR)

###Set seeds###
SPLITSEED   = 123
SCALESEED   = 234
SMOTSEED    = 345
RIDGESEED   = 456
LASSOSEED   = 567
ENSEED      = 678
RFSEED 		  = 789
MARSSEED    = 8910
BOOSTSEED   = 91011
SVMSEED     = 101112
ROSESEED    = 111213
GLMSEED     = 121314

###Set global variables###
OUTCOME_1MO       = 'sb_1mo'
OUTCOME_6MO       = 'sb_6mo'
OUTCOME_12MO      = 'sb_12mo'
VAR_PARTITION_ID  = 'LOPNR'
TRAINSIZE         = 0.70
TESTSIZE          = 0.30