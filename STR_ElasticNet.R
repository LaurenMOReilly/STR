# PROGRAM: STR_ElasticNet.R
# STUDY: STR - Short-Term Risk for SB 
# DESCRIPTION: Elastic Net regression, variable selection
# INPUTS: train.ROSE.1mo.data, train.ROSE.6mo.data, train.ROSE.12mo, holdout.quart.1mo, holdout.quart.6mo, holdout.quart.12mo
# OUTPUTS: models: model.en.1mo, model.en.6mo, model.en.12mo
# DATE CREATED: 2021-10-21
# COMPLETED:
# AUTHOR: LMO ( loreilly at iu dot edu ) 
# NOTE: 1mo outcome is not included in manuscript; main outcome=12mo, sensitivity analysis=6mo

###ELASTIC NET###

#Set library
library(caret)

#Set seed
set.seed(ENSEED)

#Create separate datasets for predictors and outcome (by each outcome)
#Holdout sample is not oversampled
#Predictors and outcome can be amended
  #1 month
train.en.1mo.preds = as.matrix(train.ROSE.1mo.data[ ,c(1:60,62:210)]) 
train.en.1mo.y = train.ROSE.1mo.data[[61]]  

holdout.en.1mo.preds = as.matrix(holdout.quart.1mo[ ,c(1:60,62:210)]) 
holdout.en.1mo.y = holdout.quart.1mo[[61]] 

  #6 month
train.en.6mo.preds = as.matrix(train.ROSE.6mo.data[ ,c(1:60,62:210)])
train.en.6mo.y = train.ROSE.6mo.data[[61]] 

holdout.en.6mo.preds = as.matrix(holdout.quart.6mo[ ,c(1:60,62:210)])
holdout.en.6mo.y = holdout.quart.6mo[[61]]  

  #12 month
train.en.12mo.preds = as.matrix(train.ROSE.12mo.data[ ,c(1:60,62:210)]) 
train.en.12mo.y = train.ROSE.12mo.data[[61]] 

holdout.en.12mo.preds = as.matrix(holdout.quart.12mo[ ,c(1:60,62:210)]) 
holdout.en.12mo.y = holdout.quart.12mo[[61]]

###SET UP MODEL###
#Grid for tuning parameters
alpha.grid = seq(0,1,length=10)
#lambda.grid = 10^seq(2,-2,length=100)
lambda.grid = seq(0.001, 1,length=10)

ENGrid = expand.grid(.alpha = alpha.grid 
                     , .lambda = lambda.grid)

#Set trainControl specifications
#10-fold cross validation to find lambda
#http://appliedpredictivemodeling.com/blog/2014/11/27/vpuig01pqbklmi72b8lcl3ij5hj2qm
#Note that tuneLength is an alternative to specifying a tuneGrid
train_control = trainControl (method = "cv" #cross-validated method
                              , number = 10 #number of k folds
                              , search = "grid" #random vs grid search; grid=default
                              , verboseIter = FALSE #printing training log
)

#Model in training set
model.en.1mo = caret::train (data.frame(train.en.1mo.preds) 
                             , as.factor(train.en.1mo.y) 
                             , method="glmnet" 
                             , tuneGrid = ENGrid
                             , trControl = train_control
                             , family = "binomial"
                             , standardize = FALSE
                             , maxit = 1000000
)

model.en.6mo = caret::train (data.frame(train.en.6mo.preds) 
                             , as.factor(train.en.6mo.y) 
                             , method="glmnet" 
                             , tuneGrid = ENGrid
                             , trControl = train_control
                             , family = "binomial"
                             , standardize = FALSE
)

model.en.12mo = caret::train (data.frame(train.en.12mo.preds) 
                             , as.factor(train.en.12mo.y) 
                             , method="glmnet" 
                             , tuneGrid = ENGrid
                             , trControl = train_control
                             , family = "binomial"
                             , standardize = FALSE
)

#Model summaries
model.en.1mo
model.en.6mo
model.en.12mo

model.en.1mo$bestTune
model.en.6mo$bestTune
model.en.12mo$bestTune

coef(model.en.1mo$finalModel, model.en.1mo$bestTune$lambda)
coef(model.en.6mo$finalModel, model.en.6mo$bestTune$lambda)
coef(model.en.12mo$finalModel, model.en.12mo$bestTune$lambda)

summary(model.en.1mo$finalModel)
summary(model.en.6mo$finalModel)
summary(model.en.12mo$finalModel)

en.bestresult = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

en.bestresult(model.en.1mo)
en.bestresult(model.en.6mo)
en.bestresult(model.en.12mo)

#Fit final model, predict in holdout
y.hat.en.1mo = stats::predict(model.en.1mo, holdout.en.1mo.preds)
y.hat.en.6mo = stats::predict(model.en.6mo, holdout.en.6mo.preds)
y.hat.en.12mo = stats::predict(model.en.12mo, holdout.en.12mo.preds)

#Model summaries
str(y.hat.en.1mo)
summary(y.hat.en.1mo)

str(y.hat.en.6mo)
summary(y.hat.en.6mo)

str(y.hat.en.12mo)
summary(y.hat.en.12mo)

#Model prediction as probabilities in holdout sample
probabilities.en.1mo = stats::predict(model.en.1mo, type="prob", holdout.en.1mo.preds)
probabilities.en.6mo = stats::predict(model.en.6mo, type="prob", holdout.en.6mo.preds)
probabilities.en.12mo = stats::predict(model.en.12mo, type="prob", holdout.en.12mo.preds)

#Confusion matrix
#First variables=predicted; second="truth"
#Round at 0.5 criterion

library(caret)
class.pred.en.1mo = as.factor(round(probabilities.en.1mo[ ,2]))
caret::confusionMatrix(class.pred.en.1mo, as.factor(holdout.en.1mo.y), positive="1", mode="everything")

class.pred.en.6mo = as.factor(round(probabilities.en.6mo[ ,2]))
caret::confusionMatrix(class.pred.en.6mo, as.factor(holdout.en.6mo.y), positive="1", mode="everything")

class.pred.en.12mo = as.factor(round(probabilities.en.12mo[ ,2]))
caret::confusionMatrix(class.pred.en.12mo, as.factor(holdout.en.12mo.y), positive="1", mode="everything")

#Change cutoff to 5%
probabilities.en.1mo.05 = ifelse(stats::predict(model.en.1mo, holdout.en.1mo.preds, type="prob")>=0.05,1,0)
class.pred.en.1mo.01 = as.factor(round(probabilities.en.1mo.05[, 2]))
caret::confusionMatrix(class.pred.en.1mo.01, as.factor(holdout.en.1mo.y), positive="1")

probabilities.en.6mo.05 = ifelse(stats::predict(model.en.6mo, holdout.en.6mo.preds, type="prob")>=0.05,1,0)
class.pred.en.6mo.05 = as.factor(round(probabilities.en.6mo.05[, 2]))
caret::confusionMatrix(class.pred.en.6mo.05, as.factor(holdout.en.6mo.y), positive="1")

probabilities.en.12mo.05 = ifelse(stats::predict(model.en.12mo, holdout.en.12mo.preds, type="prob")>=0.05,1,0)
class.pred.en.12mo.05 = as.factor(round(probabilities.en.12mo.05[, 2]))
caret::confusionMatrix(class.pred.en.12mo.05, as.factor(holdout.en.12mo.y), positive="1")

#Change cutoff to 10%
probabilities.en.1mo.10 = ifelse(stats::predict(model.en.1mo, holdout.en.1mo.preds, type="prob")>=0.10,1,0)
class.pred.en.1mo.10 = as.factor(round(probabilities.en.1mo.10[, 2]))
caret::confusionMatrix(class.pred.en.1mo.10, as.factor(holdout.en.1mo.y), positive="1", mode="everything")

probabilities.en.6mo.10 = ifelse(stats::predict(model.en.6mo, holdout.en.6mo.preds, type="prob")>=0.10,1,0)
class.pred.en.6mo.10 = as.factor(round(probabilities.en.6mo.10[, 2]))
caret::confusionMatrix(class.pred.en.6mo.10, as.factor(holdout.en.6mo.y), positive="1", mode="everything")

probabilities.en.12mo.10 = ifelse(stats::predict(model.en.12mo, holdout.en.12mo.preds, type="prob")>=0.10,1,0)
class.pred.en.12mo.10 = as.factor(round(probabilities.en.12mo.10[, 2]))
caret::confusionMatrix(class.pred.en.12mo.10, as.factor(holdout.en.12mo.y), positive="1", mode="everything")

#Change cutoff to 25%
probabilities.en.1mo.25 = ifelse(stats::predict(model.en.1mo, holdout.en.1mo.preds, type="prob")>=0.25,1,0)
class.pred.en.1mo.25 = as.factor(round(probabilities.en.1mo.25[, 2]))
caret::confusionMatrix(class.pred.en.1mo.25, as.factor(holdout.en.1mo.y), positive="1", mode="everything")

probabilities.en.6mo.25 = ifelse(stats::predict(model.en.6mo, holdout.en.6mo.preds, type="prob")>=0.25,1,0)
class.pred.en.6mo.25 = as.factor(round(probabilities.en.6mo.25[, 2]))
caret::confusionMatrix(class.pred.en.6mo.25, as.factor(holdout.en.6mo.y), positive="1", mode="everything")

probabilities.en.12mo.25 = ifelse(stats::predict(model.en.12mo, holdout.en.12mo.preds, type="prob")>=0.25,1,0)
class.pred.en.12mo.25 = as.factor(round(probabilities.en.12mo.25[, 2]))
caret::confusionMatrix(class.pred.en.12mo.25, as.factor(holdout.en.12mo.y), positive="1", mode="everything")


#AUC
library(pROC)
roc1.en.1mo = roc(holdout.en.1mo.y, probabilities.en.1mo[ ,2], ordered=TRUE)
auc(roc1.en.1mo)
ci.auc(roc1.en.1mo, conf.level=0.95, method="bootstrap", boot.n=10000)
#plot(roc1.en.1mo)

roc1.en.6mo = roc(holdout.en.6mo.y, probabilities.en.6mo[ ,2], ordered=TRUE)
auc(roc1.en.6mo)
ci.auc(roc1.en.6mo, conf.level=0.95, method="bootstrap", boot.n=10000)
#plot(roc1.en.6mo)

roc1.en.12mo = roc(holdout.en.12mo.y, probabilities.en.12mo[ ,2], ordered=TRUE)
auc(roc1.en.12mo)
ci.auc(roc1.en.12mo, conf.level=0.95, method="bootstrap", boot.n=10000)
#plot(roc1.en.12mo)

  #partial AUC
auc(roc1.en.1mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.en.6mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.en.12mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)


#PRAUC
library(PRROC)
   #1 month
prob_yes_en.1mo =  probabilities.en.1mo[ ,2][holdout.en.1mo.y==1]
prob_no_en.1mo  =  probabilities.en.1mo[ ,2][holdout.en.1mo.y==0]

roc2.en.1mo =   roc.curve(scores.class0 = prob_yes_en.1mo, scores.class1=prob_no_en.1mo, curve=T)
plot(roc2.en.1mo)
prauc.en.1mo = pr.curve(scores.class0 = prob_yes_en.1mo, scores.class1=prob_no_en.1mo, curve=T)
prauc.en.1mo
plot(prauc.en.1mo)

   #6 months
prob_yes_en.6mo =  probabilities.en.6mo[ ,2][holdout.en.6mo.y==1]
prob_no_en.6mo  =  probabilities.en.6mo[ ,2][holdout.en.6mo.y==0]

roc2.en.6mo =   roc.curve(scores.class0 = prob_yes_en.6mo, scores.class1=prob_no_en.6mo, curve=T)
plot(roc2.en.6mo)
prauc.en.6mo = pr.curve(scores.class0 = prob_yes_en.6mo, scores.class1=prob_no_en.6mo, curve=T)
prauc.en.6mo
plot(prauc.en.6mo)

   #12 months
prob_yes_en.12mo =  probabilities.en.12mo[ ,2][holdout.en.12mo.y==1]
prob_no_en.12mo  =  probabilities.en.12mo[ ,2][holdout.en.12mo.y==0]

roc2.en.12mo =   roc.curve(scores.class0 = prob_yes_en.12mo, scores.class1=prob_no_en.12mo, curve=T)
plot(roc2.en.12mo)
prauc.en.12mo = pr.curve(scores.class0 = prob_yes_en.12mo, scores.class1=prob_no_en.12mo, curve=T)
prauc.en.12mo
plot(prauc.en.12mo)


#Brier score
negative = 0
positive = 1
measures::Brier(probabilities.en.1mo[ ,2], holdout.en.1mo.y, negative, positive)
measures::Brier(probabilities.en.6mo[ ,2], holdout.en.6mo.y, negative, positive)
measures::Brier(probabilities.en.12mo[ ,2], holdout.en.12mo.y, negative, positive)

#ICI
library(mscpredmodel)
ici.data.en.1mo = data.frame(prob=probabilities.en.1mo[, 2], holdout.quart.1mo$sb_1mo)
mscpredmodel::int_calib_index(ici.data.en.1mo, holdout.quart.1mo.sb_1mo ~ prob)

ici.data.en.6mo = data.frame(prob=probabilities.en.6mo[, 2], holdout.quart.6mo$sb_6mo)
mscpredmodel::int_calib_index(ici.data.en.6mo, holdout.quart.6mo.sb_6mo ~ prob)

ici.data.en.12mo = data.frame(prob=probabilities.en.12mo[, 2], holdout.quart.12mo$sb_12mo)
mscpredmodel::int_calib_index(ici.data.en.12mo, holdout.quart.12mo.sb_12mo ~ prob)

#Net benefit
p_t = 0.10
nb.ici.1mo = gmish::nb(probabilities.en.1mo[ ,2], holdout.en.1mo.y, p_t) 
nb.ici.1mo

nb.ici.6mo = gmish::nb(probabilities.en.6mo[ ,2], holdout.en.6mo.y, p_t) 
nb.ici.6mo

nb.ici.12mo = gmish::nb(probabilities.en.12mo[ ,2], holdout.en.12mo.y, p_t) 
nb.ici.12mo

#Save models
saveRDS(model.en.1mo,  "model.en.1mo.rds")
saveRDS(model.en.6mo,  "model.en.6mo.rds")
saveRDS(model.en.12mo, "model.en.12mo.rds")

#Drop models to clear global environment
rm(train.en.1mo.preds, train.en.1mo.y, train.en.6mo.preds, train.en.6mo.y, train.en.12mo.preds, train.en.12mo.y,
   prauc.en.1mo, prauc.en.6mo, prauc.en.12mo,
   y.hat.en.1mo, y.hat.en.6mo, y.hat.en.12mo,
   holdout.en.1mo.preds, holdout.en.1mo.y, holdout.en.6mo.preds, holdout.en.6mo.y, holdout.en.12mo.preds, holdout.en.12mo.y,
   probabilities.en.1mo, probabilities.en.6mo, probabilities.en.12mo,
   roc1.en.1mo, roc1.en.6mo, roc1.en.12mo,
   roc2.en.1mo, roc2.en.6mo, roc2.en.12mo, 
   prauc.en.1mo, prauc.en.6mo, prauc.en.12mo,
   class.pred.en.1mo, class.pred.en.6mo, class.pred.en.12mo, 
   prob_no_en.1mo, prob_no_en.6mo, prob_no_en.12mo, prob_yes_en.1mo, prob_yes_en.6mo, prob_yes_en.12mo,
   probabilities.en.1mo.01, probabilities.en.6mo.01, probabilities.en.12mo.01,
   class.pred.en.1mo.01, class.pred.en.6mo.01, class.pred.en.12mo.01, model.en.1mo, model.en.6mo, model.en.12mo
)

