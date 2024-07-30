# PROGRAM:        STR_loadLibraries.R
# STUDY:          STR - Short-Term Risk for SB
# DESCRIPTION:    Utility code that dynamically installs and/or loads project packages
# INPUTS:
# OUTPUTS:
# DATE CREATED:   2021-10-01
# COMPLETED:      Yes
# AUTHOR:         MER (rickertm at indiana dot edu)

###Specify libraries###
packages = c(
    'haven'       #read_sas()
  , 'groupdata2'  #partition() into training and holdout samples 
  , 'scales'      #rescale() to 0-1
  , 'dplyr'       #ntile(), mutate(), %>%, etc
  , 'usethis' 
  , 'devtools'
  , 'pROC'        #full, partial AUC + plots
  , 'glmnet'      #ridge, lasso, elastic net
  , 'ROCR'        #plotting 
  , 'PRROC'       #PRAUC plots
  , 'smotefamily' #SMOTE
  , 'randomForest'#RF
  , 'caret'       #Caret
  , 'epiR'        #Kappa
  , 'xtable'	    #Variable importance ensemble
  , 'xgboost'     #boosting package
  , 'e1071'       #SVM package
  , 'measures'    #Brier score
  , 'predtools'   #Plots
)

###Load or install&load###
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)