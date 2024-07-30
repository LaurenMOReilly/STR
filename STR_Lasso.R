# PROGRAM: STR_Lasso.R
# STUDY: STR - Short-Term Risk for SB 
# DESCRIPTION: LASSO regression, variable selection
# INPUTS: train.ROSE.1mo.data, train.ROSE.6mo.data, train.ROSE.12mo, holdout.quart.1mo, holdout.quart.6mo, holdout.quart.12mo
# OUTPUTS: models: lasso.cv.1mo, lasso.cv.6mo, lasso.cv.12mo
# DATE CREATED: 2021-09-28
# COMPLETED: 
# AUTHOR: LMO ( loreilly at iu dot edu ) 
# NOTE: 1mo outcome is not included in manuscript; main outcome=12mo, sensitivity analysis=6mo

###LASSO###

#Set seed
set.seed(LASSOSEED)

#Create separate datasets for predictors and outcome (by each outcome)
#Holdout sample is not oversampled
#Predictors and outcome can be amended
  #1 month
train.lasso.1mo.preds = as.matrix(train.ROSE.1mo.data[ ,c(1:60,62:210)]) 
train.lasso.1mo.y = train.ROSE.1mo.data[[61]] 

holdout.lasso.1mo.preds = as.matrix(holdout.quart.1mo[ ,c(1:60,62:210)]) 
holdout.lasso.1mo.y = holdout.quart.1mo[[61]]  

  #6 month
train.lasso.6mo.preds = as.matrix(train.ROSE.6mo.data[ ,c(1:60,62:210)])
train.lasso.6mo.y = train.ROSE.6mo.data[[61]]  

holdout.lasso.6mo.preds = as.matrix(holdout.quart.6mo[ ,c(1:60,62:210)]) 
holdout.lasso.6mo.y = holdout.quart.6mo[[61]] 

  #12 month
train.lasso.12mo.preds = as.matrix(train.ROSE.12mo.data[ ,c(1:60,62:210)]) 
train.lasso.12mo.y = train.ROSE.12mo.data[[61]]  

holdout.lasso.12mo.preds = as.matrix(holdout.quart.12mo[ ,c(1:60,62:210)]) 
holdout.lasso.12mo.y = holdout.quart.12mo[[61]] 

###SET UP MODELS###
#Tuning
#10-fold cross validation to find lambda
lambdas_try = 10^seq(-3, 5, length.out=100) #implements lambdas from values ranging 10^-3 to 10^5

#Model in training set
   #1 month
lasso.cv.1mo = cv.glmnet(train.lasso.1mo.preds
                         , train.lasso.1mo.y
                         , alpha=1
                         , lambda=lambdas_try
                         , type.measure="mse" #CV on mse (deviance)
                         , family="binomial"
                         , standardize=FALSE
                         , nfolds=10 #alpha=1 sets to lasso
)

   #6 months
lasso.cv.6mo = cv.glmnet(train.lasso.6mo.preds
                         , train.lasso.6mo.y
                         , alpha=1
                         , lambda=lambdas_try
                         , type.measure="mse" #CV on mse (deviance)
                         , family="binomial"
                         , standardize=FALSE
                         , nfolds=10 #alpha=1 sets to lasso
)

   #12 months
lasso.cv.12mo = cv.glmnet(train.lasso.12mo.preds
                          , train.lasso.12mo.y
                          , alpha=1
                          , lambda=lambdas_try
                          , type.measure="mse" #CV on mse (deviance)
                          , family="binomial"
                          , standardize=FALSE
                          , nfolds=10 #alpha=1 sets to lasso
)


#Plot cross-validation results
dim(coef(lasso.cv.1mo)) #check matrix dimensions
plot(lasso.cv.1mo)
plot(coef(lasso.cv.1mo, s = "lambda.min"), type="l")
points(coef(lasso.cv.1mo, s = "lambda.1se"), type="l", col = 2)

dim(coef(lasso.cv.6mo)) #check matrix dimensions
plot(lasso.cv.6mo)
plot(coef(lasso.cv.6mo, s = "lambda.min"), type="l")
points(coef(lasso.cv.6mo, s = "lambda.1se"), type="l", col = 2)

dim(coef(lasso.cv.12mo)) #check matrix dimensions
plot(lasso.cv.12mo)
plot(coef(lasso.cv.12mo, s = "lambda.min"), type="l")
points(coef(lasso.cv.12mo, s = "lambda.1se"), type="l", col = 2)

#Best cross-validated lambda (lambda that minimizes MSE within 1 SE)
lambda.cv.lasso.1mo  = lasso.cv.1mo$lambda.1se
lambda.cv.lasso.6mo  = lasso.cv.6mo$lambda.1se
lambda.cv.lasso.12mo = lasso.cv.12mo$lambda.1se

#Fit final model in holdout
   #1 month
model.lasso.1mo = glmnet (train.lasso.1mo.preds, train.lasso.1mo.y, alpha=1, family="binomial", lambda=lambda.cv.lasso.1mo)
y.hat.lasso.1mo = predict(model.lasso.1mo, s=lambda.cv.lasso.1mo, newx=holdout.lasso.1mo.preds)

   #6 months
model.lasso.6mo = glmnet (train.lasso.6mo.preds, train.lasso.6mo.y, alpha=1, family="binomial", lambda=lambda.cv.lasso.6mo)
y.hat.lasso.6mo = predict(model.lasso.6mo, s=lambda.cv.lasso.6mo, newx=holdout.lasso.6mo.preds)

   #12 months
model.lasso.12mo = glmnet (train.lasso.12mo.preds, train.lasso.12mo.y, alpha=1, family="binomial", lambda=lambda.cv.lasso.12mo)
y.hat.lasso.12mo = predict(model.lasso.12mo, s=lambda.cv.lasso.12mo, newx=holdout.lasso.12mo.preds)

#Model summaires
coef(model.lasso.1mo)
print(model.lasso.1mo)
summary(model.lasso.1mo)  

coef(model.lasso.6mo)
print(model.lasso.6mo)
summary(model.lasso.6mo)  

coef(model.lasso.12mo)
print(model.lasso.12mo)
summary(model.lasso.12mo)  

#Turn model prediction into probabilities in holdout
probabilities.lasso.1mo  = predict(model.lasso.1mo, type="response", newx=holdout.lasso.1mo.preds)
probabilities.lasso.6mo  = predict(model.lasso.6mo, type="response", newx=holdout.lasso.6mo.preds)
probabilities.lasso.12mo = predict(model.lasso.12mo, type="response", newx=holdout.lasso.12mo.preds)

###CLASSIFICATION METRICS###
#MSE
mean((y.hat.lasso.1mo - holdout.lasso.1mo.y)^2)
mean((y.hat.lasso.6mo - holdout.lasso.6mo.y)^2)
mean((y.hat.lasso.12mo - holdout.lasso.12mo.y)^2)

#Confusion matrix
#First variables=predicted; second="truth"
#Round at 0.5 criterion
library(caret)
class.pred.lasso.1mo = as.factor(round(stats::predict(model.lasso.1mo, type="response", newx=holdout.lasso.1mo.preds)))
caret::confusionMatrix(class.pred.lasso.1mo, as.factor(holdout.lasso.1mo.y), positive="1", mode="everything")

class.pred.lasso.6mo = as.factor(round(stats::predict(model.lasso.6mo, type="response", newx=holdout.lasso.6mo.preds)))
caret::confusionMatrix(class.pred.lasso.6mo, as.factor(holdout.lasso.6mo.y), positive="1", mode="everything")

class.pred.lasso.12mo = as.factor(round(stats::predict(model.lasso.12mo, type="response", newx=holdout.lasso.12mo.preds)))
caret::confusionMatrix(class.pred.lasso.12mo, as.factor(holdout.lasso.12mo.y), positive="1", mode="everything")

str(class.pred.lasso.1mo)
summary(class.pred.lasso.1mo)

str(class.pred.lasso.6mo)
summary(class.pred.lasso.6mo)

str(class.pred.lasso.12mo)
summary(class.pred.lasso.12mo)

#Change cutoff to 5%
class.pred.lasso.1mo.05  = as.factor(ifelse(stats::predict(model.lasso.1mo,  newx=holdout.lasso.1mo.preds,  type="response")>0.05,"1","0"))
caret::confusionMatrix(class.pred.lasso.1mo.05, as.factor(holdout.lasso.1mo.y), positive="1")

class.pred.lasso.6mo.05  = as.factor(ifelse(stats::predict(model.lasso.6mo,  newx=holdout.lasso.6mo.preds,  type="response")>0.05,"1","0"))
caret::confusionMatrix(class.pred.lasso.6mo.05, as.factor(holdout.lasso.6mo.y), positive="1")

class.pred.lasso.12mo.05 = as.factor(ifelse(stats::predict(model.lasso.12mo, newx=holdout.lasso.12mo.preds, type="response")>0.05,"1","0"))
caret::confusionMatrix(class.pred.lasso.12mo.05, as.factor(holdout.lasso.12mo.y), positive="1")

#Change cutoff to 10%
class.pred.lasso.1mo.10  = as.factor(ifelse(stats::predict(model.lasso.1mo,  newx=holdout.lasso.1mo.preds,  type="response")>0.10,"1","0"))
caret::confusionMatrix(class.pred.lasso.1mo.10, as.factor(holdout.lasso.1mo.y), positive="1", mode="everything")

class.pred.lasso.6mo.10  = as.factor(ifelse(stats::predict(model.lasso.6mo,  newx=holdout.lasso.6mo.preds,  type="response")>0.10,"1","0"))
caret::confusionMatrix(class.pred.lasso.6mo.10, as.factor(holdout.lasso.6mo.y), positive="1", mode="everything")

class.pred.lasso.12mo.10 = as.factor(ifelse(stats::predict(model.lasso.12mo, newx=holdout.lasso.12mo.preds, type="response")>0.10,"1","0"))
caret::confusionMatrix(class.pred.lasso.12mo.10, as.factor(holdout.lasso.12mo.y), positive="1", mode="everything")

#Change cutoff to 25%
class.pred.lasso.1mo.25  = as.factor(ifelse(stats::predict(model.lasso.1mo,  newx=holdout.lasso.1mo.preds,  type="response")>0.25,"1","0"))
caret::confusionMatrix(class.pred.lasso.1mo.25, as.factor(holdout.lasso.1mo.y), positive="1", mode="everything")

class.pred.lasso.6mo.25  = as.factor(ifelse(stats::predict(model.lasso.6mo,  newx=holdout.lasso.6mo.preds,  type="response")>0.25,"1","0"))
caret::confusionMatrix(class.pred.lasso.6mo.25, as.factor(holdout.lasso.6mo.y), positive="1", mode="everything")

class.pred.lasso.12mo.25 = as.factor(ifelse(stats::predict(model.lasso.12mo, newx=holdout.lasso.12mo.preds, type="response")>0.25,"1","0"))
caret::confusionMatrix(class.pred.lasso.12mo.25, as.factor(holdout.lasso.12mo.y), positive="1", mode="everything")


#AUC 
library(pROC)
roc1.lasso.1mo = roc(holdout.lasso.1mo.y, probabilities.lasso.1mo)
auc(roc1.lasso.1mo)
ci.auc(roc1.lasso.1mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc1.lasso.1mo)

roc1.lasso.6mo = roc(holdout.lasso.6mo.y, probabilities.lasso.6mo)
auc(roc1.lasso.6mo)
ci.auc(roc1.lasso.6mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc1.lasso.6mo)

roc1.lasso.12mo = roc(holdout.lasso.12mo.y, probabilities.lasso.12mo)
auc(roc1.lasso.12mo)
ci.auc(roc1.lasso.12mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc1.lasso.12mo)

   #partial AUC
auc(roc1.lasso.1mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.lasso.6mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.lasso.12mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)

#PRAUC
library(PRROC)
   #1 month
prob_yes_lasso.1mo =  probabilities.lasso.1mo[holdout.lasso.1mo.y==1]
prob_no_lasso.1mo  =  probabilities.lasso.1mo[holdout.lasso.1mo.y==0]

roc2.lasso.1mo =   roc.curve(scores.class0 = prob_yes_lasso.1mo, scores.class1=prob_no_lasso.1mo, curve=T)
plot(roc2.lasso.1mo)
prauc.lasso.1mo = pr.curve(scores.class0 = prob_yes_lasso.1mo, scores.class1=prob_no_lasso.1mo, curve=T)
prauc.lasso.1mo
plot(prauc.lasso.1mo)

   #6 months
prob_yes_lasso.6mo =  probabilities.lasso.6mo[holdout.lasso.6mo.y==1]
prob_no_lasso.6mo  =  probabilities.lasso.6mo[holdout.lasso.6mo.y==0]

roc2.lasso.6mo =   roc.curve(scores.class0 = prob_yes_lasso.6mo, scores.class1=prob_no_lasso.6mo, curve=T)
plot(roc2.lasso.6mo)
prauc.lasso.6mo = pr.curve(scores.class0 = prob_yes_lasso.6mo, scores.class1=prob_no_lasso.6mo, curve=T)
prauc.lasso.6mo
plot(prauc.lasso.6mo)

   #12 months
prob_yes_lasso.12mo =  probabilities.lasso.12mo[holdout.lasso.12mo.y==1]
prob_no_lasso.12mo  =  probabilities.lasso.12mo[holdout.lasso.12mo.y==0]

roc2.lasso.12mo =   roc.curve(scores.class0 = prob_yes_lasso.12mo, scores.class1=prob_no_lasso.12mo, curve=T)
plot(roc2.lasso.12mo)
prauc.lasso.12mo = pr.curve(scores.class0 = prob_yes_lasso.12mo, scores.class1=prob_no_lasso.12mo, curve=T)
prauc.lasso.12mo
plot(prauc.lasso.12mo)

#Brier score
negative = 0
positive = 1
measures::Brier(probabilities.lasso.1mo, holdout.lasso.1mo.y, negative, positive)
measures::Brier(probabilities.lasso.6mo, holdout.lasso.6mo.y, negative, positive)
measures::Brier(probabilities.lasso.12mo, holdout.lasso.12mo.y, negative, positive)

#ICI
library(mscpredmodel)
ici.data.lasso.1mo = data.frame(probabilities.lasso.1mo, holdout.quart.1mo$sb_1mo)
mscpredmodel::int_calib_index(ici.data.lasso.1mo, holdout.quart.1mo.sb_1mo ~ s0)

ici.data.lasso.6mo = data.frame(probabilities.lasso.6mo, holdout.quart.6mo$sb_6mo)
mscpredmodel::int_calib_index(ici.data.lasso.6mo, holdout.quart.6mo.sb_6mo ~ s0)

ici.data.lasso.12mo = data.frame(probabilities.lasso.12mo, holdout.quart.12mo$sb_12mo)
mscpredmodel::int_calib_index(ici.data.lasso.12mo, holdout.quart.12mo.sb_12mo ~ s0)

#Net benefit
p_t = 0.10
nb.ici.1mo = gmish::nb(probabilities.lasso.1mo, holdout.lasso.1mo.y, p_t) 
nb.ici.1mo

nb.ici.6mo = gmish::nb(probabilities.lasso.6mo, holdout.lasso.6mo.y, p_t) 
nb.ici.6mo

nb.ici.12mo = gmish::nb(probabilities.lasso.12mo, holdout.lasso.12mo.y, p_t) 
nb.ici.12mo

#Save models
saveRDS(lasso.cv.1mo,  "lasso.cv.1mo.rds")
saveRDS(lasso.cv.6mo,  "lasso.cv.6mo.rds")
saveRDS(lasso.cv.12mo, "lasso.cv.12mo.rds")

#Drop models to clear global environment
rm(train.lasso.1mo.preds, train.lasso.1mo.y, train.lasso.6mo.preds, train.lasso.6mo.y, train.lasso.12mo.preds, train.lasso.12mo.y,
   model.lasso.1mo, model.lasso.6mo, model.lasso.12mo, 
   lasso.cv.1mo, lasso.cv.6mo, lasso.cv.12mo, 
   prauc.lasso.1mo, prauc.lasso.6mo, prauc.lasso.12mo,
   y.hat.lasso.1mo, y.hat.lasso.6mo, y.hat.lasso.12mo,
   holdout.lasso.1mo.preds, holdout.lasso.1mo.y, holdout.lasso.6mo.preds, holdout.lasso.6mo.y, holdout.lasso.12mo.preds, holdout.lasso.12mo.y,
   probabilities.lasso.1mo, probabilities.lasso.6mo, probabilities.lasso.12mo,
   roc1.lasso.1mo, roc1.lasso.6mo, roc1.lasso.12mo,
   roc2.lasso.1mo, roc2.lasso.6mo, roc2.lasso.12mo,
   lambda.cv.lasso.1mo, lambda.cv.lasso.6mo, lambda.cv.lasso.12mo, class.pred.lasso.1mo, class.pred.lasso.6mo, class.pred.lasso.12mo,
   class.pred.lasso.1mo, class.pred.lasso.6mo, class.pred.lasso.12mo,
   prob_no_lasso.1mo, prob_no_lasso.6mo, prob_no_lasso.12mo, prob_yes_lasso.1mo, prob_yes_lasso.6mo, prob_yes_lasso.12mo,
   class.pred.lasso.1mo.01, class.pred.lasso.6mo.01, class.pred.lasso.12mo.01
)
