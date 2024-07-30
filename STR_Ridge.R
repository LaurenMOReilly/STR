# PROGRAM: STR_Ridge.R
# STUDY: STR - Short-Term Risk for SB 
# DESCRIPTION: Ridge regression, variable selection
# INPUTS: train.ROSE.1mo.data, train.ROSE.6mo.data, train.ROSE.12mo, holdout.quart.1mo, holdout.quart.6mo, holdout.quart.12mo
# OUTPUTS: models: ridge.cv.1mo, ridge.cv.6mo, ridge.cv.12mo
# DATE CREATED: 2021-09-10
# COMPLETED: No
# AUTHOR: LMO ( loreilly at iu dot edu ) 
# NOTE: 1mo outcome is not included in manuscript; main outcome=12mo, sensitivity analysis=6mo

###RIDGE### 
#Ridge does not set variable coefficients to zero; does not perform variable selection

#Load library
library(glmnet)

#Set seed
set.seed(RIDGESEED)

#Create separate datasets for predictors and outcome (by each outcome)
#Holdout sample is not oversampled
#Predictors and outcome can be amended
  #1 month
train.ridge.1mo.preds = as.matrix(train.ROSE.1mo.data[ ,c(1:60,62:210)]) 
train.ridge.1mo.y = train.ROSE.1mo.data[[61]]  

holdout.ridge.1mo.preds = as.matrix(holdout.quart.1mo[ ,c(1:60,62:210)]) 
holdout.ridge.1mo.y = holdout.quart.1mo[[61]]  

  #6 month
train.ridge.6mo.preds = as.matrix(train.ROSE.6mo.data[ ,c(1:60,62:210)])
train.ridge.6mo.y = train.ROSE.6mo.data[[61]]  

holdout.ridge.6mo.preds = as.matrix(holdout.quart.6mo[ ,c(1:60,62:210)])
holdout.ridge.6mo.y = holdout.quart.6mo[[61]]  

  #12 month
train.ridge.12mo.preds = as.matrix(train.ROSE.12mo.data[ ,c(1:60,62:210)])
train.ridge.12mo.y = train.ROSE.12mo.data[[61]]  

holdout.ridge.12mo.preds = as.matrix(holdout.quart.12mo[ ,c(1:60,62:210)])
holdout.ridge.12mo.y = holdout.quart.12mo[[61]] 

###SET UP MODELS###
#Tuning
#10-fold cross validation to find lambda
lambdas_try = 10^seq(-3, 5, length.out=100) #implements lambdas from values ranging 10^-3 to 10^5

#Model in training set
  #1 month
ridge.cv.1mo = cv.glmnet(train.ridge.1mo.preds
                         , train.ridge.1mo.y
                         , alpha=0 #alpha=0 sets to ridge
                         , lambda=lambdas_try
                         , type.measure="mse" #CV on mse (deviance)
                         , family="binomial"
                         , standardize=FALSE
                         , nfolds=10 
)

  #6 months
ridge.cv.6mo = cv.glmnet(train.ridge.6mo.preds
                         , train.ridge.6mo.y
                         , alpha=0 #alpha=0 sets to ridge
                         , lambda=lambdas_try
                         , type.measure="mse" #CV on mse (deviance)
                         , family="binomial"
                         , standardize=FALSE
                         , nfolds=10 
)

  #12 months
ridge.cv.12mo = cv.glmnet(train.ridge.12mo.preds
                         , train.ridge.12mo.y
                         , alpha=0  #alpha=0 sets to ridge
                         , lambda=lambdas_try
                         , type.measure="mse" #CV on mse (deviance)
                         , family="binomial"
                         , standardize=FALSE
                         , nfolds=10
)

# Plot cross-validation results
dim(coef(ridge.cv.1mo)) #check matrix dimensions
plot(ridge.cv.1mo)
plot(coef(ridge.cv.1mo, s = "lambda.min"), type="l")
points(coef(ridge.cv.1mo, s = "lambda.1se"), type="l", col = 2)

dim(coef(ridge.cv.6mo)) #check matrix dimensions
plot(ridge.cv.6mo)
plot(coef(ridge.cv.6mo, s = "lambda.min"), type="l")
points(coef(ridge.cv.6mo, s = "lambda.1se"), type="l", col = 2)

dim(coef(ridge.cv.12mo)) #check matrix dimensions
plot(ridge.cv.12mo)
plot(coef(ridge.cv.12mo, s = "lambda.min"), type="l")
points(coef(ridge.cv.12mo, s = "lambda.1se"), type="l", col = 2)

# Best cross-validated lambda (lambda that minimizes MSE within 1 SE - aids in variable selection)
lambda.cv.ridge.1mo  = ridge.cv.1mo$lambda.1se
lambda.cv.ridge.6mo  = ridge.cv.6mo$lambda.1se
lambda.cv.ridge.12mo = ridge.cv.12mo$lambda.1se

#Fit final model in holdout
  #1 month
model.ridge.1mo = glmnet (train.ridge.1mo.preds, train.ridge.1mo.y, alpha=0, family="binomial", lambda=lambda.cv.ridge.1mo)
y.hat.ridge.1mo = predict(model.ridge.1mo, s=lambda.cv.ridge.1mo, newx=holdout.ridge.1mo.preds)

  #6 months
model.ridge.6mo = glmnet (train.ridge.6mo.preds, train.ridge.6mo.y, alpha=0, family="binomial", lambda=lambda.cv.ridge.6mo)
y.hat.ridge.6mo = predict(model.ridge.6mo, s=lambda.cv.ridge.6mo, newx=holdout.ridge.6mo.preds)

  #12 months
model.ridge.12mo = glmnet (train.ridge.12mo.preds, train.ridge.12mo.y, alpha=0, family="binomial", lambda=lambda.cv.ridge.12mo)
y.hat.ridge.12mo = predict(model.ridge.12mo, s=lambda.cv.ridge.12mo, newx=holdout.ridge.12mo.preds)

#Model summaries
coef(model.ridge.1mo)
print(model.ridge.1mo)
summary(model.ridge.1mo)  

coef(model.ridge.6mo)
print(model.ridge.6mo)
summary(model.ridge.6mo)  

coef(model.ridge.12mo)
print(model.ridge.12mo)
summary(model.ridge.12mo)  

#Turn model prediction into probabilities in holdout
probabilities.ridge.1mo  = predict(model.ridge.1mo, type="response", newx=holdout.ridge.1mo.preds)
probabilities.ridge.6mo  = predict(model.ridge.6mo, type="response", newx=holdout.ridge.6mo.preds)
probabilities.ridge.12mo = predict(model.ridge.12mo, type="response", newx=holdout.ridge.12mo.preds)

#Confusion matrix
#First variables=predicted; second="truth"
#Round at 0.5 criterion
library(caret)
class.pred.ridge.1mo = as.factor(round(stats::predict(model.ridge.1mo, type="response", newx=holdout.ridge.1mo.preds)))
caret::confusionMatrix(class.pred.ridge.1mo, as.factor(holdout.ridge.1mo.y), positive="1", mode="everything")

class.pred.ridge.6mo = as.factor(round(stats::predict(model.ridge.6mo, type="response", newx=holdout.ridge.6mo.preds)))
caret::confusionMatrix(class.pred.ridge.6mo, as.factor(holdout.ridge.6mo.y), positive="1", mode="everything")

class.pred.ridge.12mo = as.factor(round(stats::predict(model.ridge.12mo, type="response", newx=holdout.ridge.12mo.preds)))
caret::confusionMatrix(class.pred.ridge.12mo, as.factor(holdout.ridge.12mo.y), positive="1", mode="everything")

str(class.pred.ridge.1mo)
summary(y.hat.ridge.1mo)

str(class.pred.ridge.6mo)
summary(y.hat.ridge.6mo)

str(class.pred.ridge.12mo)
summary(y.hat.ridge.12mo)

#Change cutoff to 1%
class.pred.ridge.1mo.01  = as.factor(ifelse(stats::predict(model.ridge.1mo,  newx=holdout.ridge.1mo.preds,  type="response")>=0.01,"1","0"))
caret::confusionMatrix(class.pred.ridge.1mo.01, as.factor(holdout.ridge.1mo.y), positive="1")

class.pred.ridge.6mo.01  = as.factor(ifelse(stats::predict(model.ridge.6mo,  newx=holdout.ridge.6mo.preds,  type="response")>=0.01,"1","0"))
caret::confusionMatrix(class.pred.ridge.6mo.01, as.factor(holdout.ridge.6mo.y), positive="1")

class.pred.ridge.12mo.01 = as.factor(ifelse(stats::predict(model.ridge.12mo, newx=holdout.ridge.12mo.preds, type="response")>=0.01,"1","0"))
caret::confusionMatrix(class.pred.ridge.12mo.01, as.factor(holdout.ridge.12mo.y), positive="1")

#Change cutoff to 5%
class.pred.ridge.1mo.05  = as.factor(ifelse(stats::predict(model.ridge.1mo,  newx=holdout.ridge.1mo.preds,  type="response")>=0.05,"1","0"))
caret::confusionMatrix(class.pred.ridge.1mo.05, as.factor(holdout.ridge.1mo.y), positive="1")

class.pred.ridge.6mo.05  = as.factor(ifelse(stats::predict(model.ridge.6mo,  newx=holdout.ridge.6mo.preds,  type="response")>=0.05,"1","0"))
caret::confusionMatrix(class.pred.ridge.6mo.05, as.factor(holdout.ridge.6mo.y), positive="1")

class.pred.ridge.12mo.05 = as.factor(ifelse(stats::predict(model.ridge.12mo, newx=holdout.ridge.12mo.preds, type="response")>=0.05,"1","0"))
caret::confusionMatrix(class.pred.ridge.12mo.05, as.factor(holdout.ridge.12mo.y), positive="1")

#Change cutoff to 10%
class.pred.ridge.1mo.10  = as.factor(ifelse(stats::predict(model.ridge.1mo,  newx=holdout.ridge.1mo.preds,  type="response")>=0.10,"1","0"))
caret::confusionMatrix(class.pred.ridge.1mo.10, as.factor(holdout.ridge.1mo.y), positive="1", mode="everything")

class.pred.ridge.6mo.10  = as.factor(ifelse(stats::predict(model.ridge.6mo,  newx=holdout.ridge.6mo.preds,  type="response")>=0.10,"1","0"))
caret::confusionMatrix(class.pred.ridge.6mo.10, as.factor(holdout.ridge.6mo.y), positive="1", mode="everything")

class.pred.ridge.12mo.10 = as.factor(ifelse(stats::predict(model.ridge.12mo, newx=holdout.ridge.12mo.preds, type="response")>=0.10,"1","0"))
caret::confusionMatrix(class.pred.ridge.12mo.10, as.factor(holdout.ridge.12mo.y), positive="1", mode="everything")

#Change cutoff to 25%
class.pred.ridge.1mo.25  = as.factor(ifelse(stats::predict(model.ridge.1mo,  newx=holdout.ridge.1mo.preds,  type="response")>=0.25,"1","0"))
caret::confusionMatrix(class.pred.ridge.1mo.25, as.factor(holdout.ridge.1mo.y), positive="1", mode="everything")

class.pred.ridge.6mo.25  = as.factor(ifelse(stats::predict(model.ridge.6mo,  newx=holdout.ridge.6mo.preds,  type="response")>=0.25,"1","0"))
caret::confusionMatrix(class.pred.ridge.6mo.25, as.factor(holdout.ridge.6mo.y), positive="1", mode="everything")

class.pred.ridge.12mo.25 = as.factor(ifelse(stats::predict(model.ridge.12mo, newx=holdout.ridge.12mo.preds, type="response")>=0.25,"1","0"))
caret::confusionMatrix(class.pred.ridge.12mo.25, as.factor(holdout.ridge.12mo.y), positive="1", mode="everything")


#AUC 
library(pROC)
roc1.ridge.1mo = roc(holdout.ridge.1mo.y, probabilities.ridge.1mo)
auc(roc1.ridge.1mo)
ci.auc(roc1.ridge.1mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc1.ridge.1mo)

roc1.ridge.6mo = roc(holdout.ridge.6mo.y, probabilities.ridge.6mo)
auc(roc1.ridge.6mo)
ci.auc(roc1.ridge.6mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc1.ridge.6mo)

roc1.ridge.12mo = roc(holdout.ridge.12mo.y, probabilities.ridge.12mo)
auc(roc1.ridge.12mo)
ci.auc(roc1.ridge.12mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc1.ridge.12mo)

  #partial AUC
auc(roc1.ridge.1mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.ridge.6mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.ridge.12mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)


#PRAUC
library(PRROC)
  #1 month
prob_yes_ridge.1mo =  probabilities.ridge.1mo[holdout.ridge.1mo.y==1]
prob_no_ridge.1mo  =  probabilities.ridge.1mo[holdout.ridge.1mo.y==0]

roc2.ridge.1mo =   roc.curve(scores.class0 = prob_yes_ridge.1mo, scores.class1=prob_no_ridge.1mo, curve=T)
plot(roc2.ridge.1mo)
prauc.ridge.1mo = pr.curve(scores.class0 = prob_yes_ridge.1mo, scores.class1=prob_no_ridge.1mo, curve=T)
prauc.ridge.1mo
plot(prauc.ridge.1mo)

  #6 months
prob_yes_ridge.6mo =  probabilities.ridge.6mo[holdout.ridge.6mo.y==1]
prob_no_ridge.6mo  =  probabilities.ridge.6mo[holdout.ridge.6mo.y==0]

roc2.ridge.6mo =   roc.curve(scores.class0 = prob_yes_ridge.6mo, scores.class1=prob_no_ridge.6mo, curve=T)
plot(roc2.ridge.6mo)
prauc.ridge.6mo = pr.curve(scores.class0 = prob_yes_ridge.6mo, scores.class1=prob_no_ridge.6mo, curve=T)
prauc.ridge.6mo
plot(prauc.ridge.6mo)

  #12 months
prob_yes_ridge.12mo =  probabilities.ridge.12mo[holdout.ridge.12mo.y==1]
prob_no_ridge.12mo  =  probabilities.ridge.12mo[holdout.ridge.12mo.y==0]

roc2.ridge.12mo =   roc.curve(scores.class0 = prob_yes_ridge.12mo, scores.class1=prob_no_ridge.12mo, curve=T)
plot(roc2.ridge.12mo)
prauc.ridge.12mo = pr.curve(scores.class0 = prob_yes_ridge.12mo, scores.class1=prob_no_ridge.12mo, curve=T)
prauc.ridge.12mo
plot(prauc.ridge.12mo)


#Brier score
negative = 0
positive = 1
measures::Brier(probabilities.ridge.1mo, holdout.ridge.1mo.y, negative, positive)
measures::Brier(probabilities.ridge.6mo, holdout.ridge.6mo.y, negative, positive)
measures::Brier(probabilities.ridge.12mo, holdout.ridge.12mo.y, negative, positive)

#ICI
library(mscpredmodel)
ici.data.ridge.1mo = data.frame(probabilities.ridge.1mo, holdout.quart.1mo$sb_1mo)
mscpredmodel::int_calib_index(ici.data.ridge.1mo, holdout.quart.1mo.sb_1mo ~ s0)

ici.data.ridge.6mo = data.frame(probabilities.ridge.6mo, holdout.quart.6mo$sb_6mo)
mscpredmodel::int_calib_index(ici.data.ridge.6mo, holdout.quart.6mo.sb_6mo ~ s0)

ici.data.ridge.12mo = data.frame(probabilities.ridge.12mo, holdout.quart.12mo$sb_12mo)
mscpredmodel::int_calib_index(ici.data.ridge.12mo, holdout.quart.12mo.sb_12mo ~ s0)

#Net benefit
p_t = 0.10
nb.ici.1mo = gmish::nb(probabilities.ridge.1mo, holdout.ridge.1mo.y, p_t) 
nb.ici.1mo

nb.ici.6mo = gmish::nb(probabilities.ridge.6mo, holdout.ridge.6mo.y, p_t) 
nb.ici.6mo

nb.ici.12mo = gmish::nb(probabilities.ridge.12mo, holdout.ridge.12mo.y, p_t) 
nb.ici.12mo

#Save models
saveRDS(ridge.cv.1mo,  "ridge.cv.1mo.rds")
saveRDS(ridge.cv.6mo,  "ridge.cv.6mo.rds")
saveRDS(ridge.cv.12mo, "ridge.cv.12mo.rds")

#Drop models to clear global environment
rm(train.ridge.1mo.preds, train.ridge.1mo.y, train.ridge.6mo.preds, train.ridge.6mo.y, train.ridge.12mo.preds, train.ridge.12mo.y,
   model.ridge.1mo, model.ridge.6mo, model.ridge.12mo, 
   ridge.cv.1mo, ridge.cv.6mo, ridge.cv.12mo, 
   prauc.ridge.1mo, prauc.ridge.6mo, prauc.ridge.12mo,
   y.hat.ridge.1mo, y.hat.ridge.6mo, y.hat.ridge.12mo,
   holdout.ridge.1mo.preds, holdout.ridge.1mo.y, holdout.ridge.6mo.preds, holdout.ridge.6mo.y, holdout.ridge.12mo.preds, holdout.ridge.12mo.y,
   probabilities.ridge.1mo, probabilities.ridge.6mo, probabilities.ridge.12mo,
   roc.ridge.1mo, roc.ridge.6mo, roc.ridge.12mo,
   roc2.ridge.1mo, roc2.ridge.6mo, roc2.ridge.12mo,
   roc1.ridge.1mo, roc1.ridge.6mo, roc1.ridge.12mo, class.pred.ridge.1mo, class.pred.ridge.6mo, class.pred.ridge.12mo,
   lambda.cv.ridge.1mo, lambda.cv.ridge.6mo, lambda.cv.ridge.12mo, prob_no_ridge.1mo, prob_no_ridge.6mo, prob_no_ridge.12mo,
   prob_yes_ridge.1mo, prob_yes_ridge.6mo, prob_yes_ridge.12mo,
   class.pred.ridge.1mo.01, class.pred.ridge.6mo.01, class.pred.ridge.12mo.01
)