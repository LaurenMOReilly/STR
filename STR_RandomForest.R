# PROGRAM: STR_RandomForest.R
# STUDY: STR - Short-Term Risk for SB 
# DESCRIPTION: Random Forest regression, ensemble method
# INPUTS: train.ROSE.1mo.data, train.ROSE.6mo.data, train.ROSE.12mo, holdout.quart.1mo, holdout.quart.6mo, holdout.quart.12mo
# OUTPUTS: models: model.rf.1mo, model.rf.6mo, model.rf.11mo
# DATE CREATED: 2021-10-21
# COMPLETED: 
# AUTHOR: LMO ( loreilly at iu dot edu ) 
# NOTE: 1mo outcome is not included in manuscript; main outcome=12mo, sensitivity analysis=6mo

###RANDOM FOREST###

#Set library
library(caret)

#Set seed
set.seed(RFSEED)

#Create separate datasets for predictors and outcome (by each outcome)
#Holdout sample is not oversampled
#Predictors and outcome can be amended
   #1 month
train.rf.1mo.preds = as.matrix(train.ROSE.1mo.data[ ,c(1:60,62:210)]) 
train.rf.1mo.y = train.ROSE.1mo.data[[61]] 

holdout.rf.1mo.preds = as.matrix(holdout.quart.1mo[ ,c(1:60,62:210)]) 
holdout.rf.1mo.y = holdout.quart.1mo[[61]]  

   #6 month
train.rf.6mo.preds = as.matrix(train.ROSE.6mo.data[ ,c(1:60,62:210)]) 
train.rf.6mo.y = train.ROSE.6mo.data[[61]] 

holdout.rf.6mo.preds = as.matrix(holdout.quart.6mo[ ,c(1:60,62:210)]) 
holdout.rf.6mo.y = holdout.quart.6mo[[61]]  

   #12 month
train.rf.12mo.preds = as.matrix(train.ROSE.12mo.data[ ,c(1:60,62:210)]) 
train.rf.12mo.y = train.ROSE.12mo.data[[61]]  

holdout.rf.12mo.preds = as.matrix(holdout.quart.12mo[ ,c(1:60,62:210)]) 
holdout.rf.12mo.y = holdout.quart.12mo[[61]] 

###SET UP MODEL###
#Set grid for tuning
#Note: max # of predictors = bagging
#Default mtry = sqrt number of columns
#Tuning attemps
#rf.grid.1mo = expand.grid (.mtry = sqrt(ncol(train.rf.1mo.preds)))
#rf.grid.6mo = expand.grid (.mtry = sqrt(ncol(train.rf.6mo.preds)))
#rf.grid.12mo = expand.grid (.mtry = sqrt(ncol(train.rf.12mo.preds)))
#rf.grid.1mo = expand.grid (.mtry = c(1,2,4,10,14))
#rf.grid.6mo = expand.grid (.mtry = c(1,2,5,10))
#rf.grid.12mo = expand.grid (.mtry = c(1,2,4))

rf.grid.1mo = expand.grid (.mtry = c(2))
rf.grid.6mo = expand.grid (.mtry = c(2))
rf.grid.12mo = expand.grid (.mtry = c(2))

# 10-fold cross validation to find lambda
# Note that tuneLength is an alternative to specifying a tuneGrid
train_control = trainControl (method = "cv" #cross-validated method
                              , number = 10 #number of k folds
                              , search = "grid" #random vs grid search; grid=default
                              , verboseIter = FALSE #printing training log
)

#train_control = trainControl (method = "cv" #cross-validated method
#                              , number = 10 #number of k folds
#                              , search = "grid" #random vs grid search; grid=default
#                              , verboseIter = FALSE #printing training log
#                              , classProbs = TRUE
#                              , savePredictions = TRUE
#                              , summaryFunction = twoClassSummary
#)

#Run models in training set
#Tune different ntree parameters

model.rf.1mo = caret::train(train.rf.1mo.preds
                            , as.factor(train.rf.1mo.y) 
                            , method = "rf" 
                            , tuneGrid = rf.grid.1mo
                            , trControl = train_control
                            , importance = TRUE
                            , family = "binomial"
                            , standardize = FALSE
                            , allowParallel = TRUE
                            , ntree = 150
)

model.rf.6mo = caret::train(train.rf.6mo.preds
                            , as.factor(train.rf.6mo.y) 
                            , method = "rf" 
                            , tuneGrid = rf.grid.6mo
                            , trControl = train_control
                            , importance = TRUE
                            , family = "binomial"
                            , standardize = FALSE
                            , allowParallel = TRUE
                            , ntree = 150
)

model.rf.12mo = caret::train(train.rf.12mo.preds
                                 , as.factor(train.rf.12mo.y) 
                                 , method = "rf" 
                                 , tuneGrid = rf.grid.12mo
                                 , trControl = train_control
                                 , importance = TRUE
                                 , family = "binomial"
                                 , standardize = FALSE
                                 , allowParallel = TRUE
                                 , ntree = 150
)

#Plot tuning parameters
plot(model.rf.1mo)
plot(model.rf.1mo, metric ='Kappa')
plot(model.rf.6mo)
plot(model.rf.6mo, metric = 'Kappa')
plot(model.rf.12mo)
plot(model.rf.12mo, metric = 'Kappa')

#Model summaries
model.rf.1mo
model.rf.6mo
model.rf.12mo

model.rf.1mo$bestTune
model.rf.6mo$bestTune
model.rf.12mo$bestTune

summary(model.rf.1mo$finalModel)
summary(model.rf.6mo$finalModel)
summary(model.rf.12mo$finalModel)

rf.bestresult = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

rf.bestresult(model.rf.1mo.tune)
rf.bestresult(model.rf.6mo)
rf.bestresult(model.rf.12mo)

# Variable Importance
library(xtable)
  #1 month
vi.rf.1mo = varImp(model.rf.1mo.tune)
vi.rf.1mo
xtable(vi.rf.1mo$importance)

vi.rf.1mo.2 = caret::varImp(model.rf.1mo, scale=TRUE)
vi.rf.1mo.2

  #6 months
vi.rf.6mo = varImp(model.rf.6mo)
vi.rf.6mo
xtable(vi.rf.6mo$importance)

vi.rf.6mo.2 = caret::varImp(model.rf.6mo, scale=TRUE)
vi.rf.6mo.2

  #12 months
vi.rf.12mo = varImp(model.rf.12mo)
vi.rf.12mo
xtable(vi.rf.12mo$importance)

vi.rf.12mo.2 = caret::varImp(model.rf.12mo, scale=TRUE)
vi.rf.12mo.2

#Predict final model in holdout sample
y.hat.rf.1mo  = stats::predict(model.rf.1mo, holdout.rf.1mo.preds)
y.hat.rf.6mo  = stats::predict(model.rf.6mo, holdout.rf.6mo.preds)
y.hat.rf.12mo = stats::predict(model.rf.12mo, holdout.rf.12mo.preds)

#Model summaries
str(y.hat.rf.1mo)
summary(y.hat.rf.1mo)

str(y.hat.rf.6mo)
summary(y.hat.rf.6mo)

str(y.hat.rf.12mo)
summary(y.hat.rf.12mo)

#Model prediction as probabilities in holdout sample
probabilities.rf.1mo  = stats::predict(model.rf.1mo, type="prob", holdout.rf.1mo.preds)
probabilities.rf.6mo  = stats::predict(model.rf.6mo, type="prob", holdout.rf.6mo.preds)
probabilities.rf.12mo = stats::predict(model.rf.12mo, type="prob", holdout.rf.12mo.preds)

#Confusion matrix
#First variables=predicted; second="truth"
#Round at 0.5 criterion
library(caret)
class.pred.rf.1mo = as.factor(round(probabilities.rf.1mo[ ,2]))
caret::confusionMatrix(class.pred.rf.1mo, as.factor(holdout.rf.1mo.y), positive="1", mode="everything")

class.pred.rf.6mo = as.factor(round(probabilities.rf.6mo[ ,2]))
caret::confusionMatrix(class.pred.rf.6mo, as.factor(holdout.rf.6mo.y), positive="1", mode="everything")

class.pred.rf.12mo = as.factor(round(probabilities.rf.12mo[ ,2]))
caret::confusionMatrix(class.pred.rf.12mo, as.factor(holdout.rf.12mo.y), positive="1", mode="everything")

#Change cutoff
  #0.05 (5%)
probabilities.rf.1mo.05 = ifelse(stats::predict(model.rf.1mo, holdout.rf.1mo.preds, type="prob")>=0.05,1,0)
class.pred.rf.1mo.05 = as.factor(round(probabilities.rf.1mo.05[, 2]))
caret::confusionMatrix(class.pred.rf.1mo.05, as.factor(holdout.rf.1mo.y), positive="1")

probabilities.rf.6mo.05 = ifelse(stats::predict(model.rf.6mo, holdout.rf.6mo.preds, type="prob")>=0.05,1,0)
class.pred.rf.6mo.05 = as.factor(round(probabilities.rf.6mo.05[, 2]))
caret::confusionMatrix(class.pred.rf.6mo.05, as.factor(holdout.rf.6mo.y), positive="1")

probabilities.rf.12mo.05 = ifelse(stats::predict(model.rf.12mo, holdout.rf.12mo.preds, type="prob")>=0.05,1,0)
class.pred.rf.12mo.05 = as.factor(round(probabilities.rf.12mo.05[, 2]))
caret::confusionMatrix(class.pred.rf.12mo.05, as.factor(holdout.rf.12mo.y), positive="1")

  #0.10 (1%)
probabilities.rf.1mo.10 = ifelse(stats::predict(model.rf.1mo, holdout.rf.1mo.preds, type="prob")>=0.10,1,0)
class.pred.rf.1mo.10 = as.factor(round(probabilities.rf.1mo.10[, 2]))
caret::confusionMatrix(class.pred.rf.1mo.10, as.factor(holdout.rf.1mo.y), positive="1", mode="everything")

probabilities.rf.6mo.10 = ifelse(stats::predict(model.rf.6mo, holdout.rf.6mo.preds, type="prob")>=0.10,1,0)
class.pred.rf.6mo.10 = as.factor(round(probabilities.rf.6mo.10[, 2]))
caret::confusionMatrix(class.pred.rf.6mo.10, as.factor(holdout.rf.6mo.y), positive="1", mode="everything")

probabilities.rf.12mo.10 = ifelse(stats::predict(model.rf.12mo, holdout.rf.12mo.preds, type="prob")>=0.10,1,0)
class.pred.rf.12mo.10 = as.factor(round(probabilities.rf.12mo.10[, 2]))
caret::confusionMatrix(class.pred.rf.12mo.10, as.factor(holdout.rf.12mo.y), positive="1", mode="everything")

  #0.25 (1%)
probabilities.rf.1mo.25 = ifelse(stats::predict(model.rf.1mo, holdout.rf.1mo.preds, type="prob")>=0.25,1,0)
class.pred.rf.1mo.25 = as.factor(round(probabilities.rf.1mo.25[, 2]))
caret::confusionMatrix(class.pred.rf.1mo.25, as.factor(holdout.rf.1mo.y), positive="1", mode="everything")

probabilities.rf.6mo.25 = ifelse(stats::predict(model.rf.6mo, holdout.rf.6mo.preds, type="prob")>=0.25,1,0)
class.pred.rf.6mo.25 = as.factor(round(probabilities.rf.6mo.25[, 2]))
caret::confusionMatrix(class.pred.rf.6mo.25, as.factor(holdout.rf.6mo.y), positive="1", mode="everything")

probabilities.rf.12mo.25 = ifelse(stats::predict(model.rf.12mo, holdout.rf.12mo.preds, type="prob")>=0.25,1,0)
class.pred.rf.12mo.25 = as.factor(round(probabilities.rf.12mo.25[, 2]))
caret::confusionMatrix(class.pred.rf.12mo.25, as.factor(holdout.rf.12mo.y), positive="1", mode="everything")


#AUC
library(pROC)
roc1.rf.1mo = roc(holdout.rf.1mo.y, probabilities.rf.1mo[ ,2], ordered=TRUE)
auc(roc1.rf.1mo)
#plot(roc1.rf.1mo)
ci.auc(roc1.rf.1mo, conf.level=0.95, method="bootstrap", boot.n=10000)

roc1.rf.6mo = roc(holdout.rf.6mo.y, probabilities.rf.6mo[ ,2], ordered=TRUE)
auc(roc1.rf.6mo)
#plot(roc1.rf.6mo)
ci.auc(roc1.rf.6mo, conf.level=0.95, method="bootstrap", boot.n=10000)

roc1.rf.12mo = roc(holdout.rf.12mo.y, probabilities.rf.12mo[ ,2], ordered=TRUE)
auc(roc1.rf.12mo)
#plot(roc1.rf.12mo)
ci.auc(roc1.rf.12mo, conf.level=0.95, method="bootstrap", boot.n=10000)

   #partial AUC
auc(roc1.rf.1mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.rf.6mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.rf.12mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)


#PRAUC
library(PRROC)
   #1 month
prob_yes_rf.1mo =  probabilities.rf.1mo[ ,2][holdout.rf.1mo.y==1]
prob_no_rf.1mo  =  probabilities.rf.1mo[ ,2][holdout.rf.1mo.y==0]

roc2.rf.1mo =   roc.curve(scores.class0 = prob_yes_rf.1mo, scores.class1=prob_no_rf.1mo, curve=T)
plot(roc2.rf.1mo)
prauc.rf.1mo = pr.curve(scores.class0 = prob_yes_rf.1mo, scores.class1=prob_no_rf.1mo, curve=T)
prauc.rf.1mo
plot(prauc.rf.1mo)

   #6 months
prob_yes_rf.6mo =  probabilities.rf.6mo[ ,2][holdout.rf.6mo.y==1]
prob_no_rf.6mo  =  probabilities.rf.6mo[ ,2][holdout.rf.6mo.y==0]

roc2.rf.6mo =   roc.curve(scores.class0 = prob_yes_rf.6mo, scores.class1=prob_no_rf.6mo, curve=T)
plot(roc2.rf.6mo)
prauc.rf.6mo = pr.curve(scores.class0 = prob_yes_rf.6mo, scores.class1=prob_no_rf.6mo, curve=T)
prauc.rf.6mo
plot(prauc.rf.6mo)

   #12 months
prob_yes_rf.12mo =  probabilities.rf.12mo[ ,2][holdout.rf.12mo.y==1]
prob_no_rf.12mo  =  probabilities.rf.12mo[ ,2][holdout.rf.12mo.y==0]

roc2.rf.12mo = roc.curve(scores.class0 = prob_yes_rf.12mo, scores.class1=prob_no_rf.12mo, curve=T)
plot(roc2.rf.12mo)
prauc.rf.12mo = pr.curve(scores.class0 = prob_yes_rf.12mo, scores.class1=prob_no_rf.12mo, curve=T)
prauc.rf.12mo
plot(prauc.rf.12mo)

#Brier score
negative = 0
positive = 1
measures::Brier(probabilities.rf.1mo[ ,2], holdout.rf.1mo.y, negative, positive)
measures::Brier(probabilities.rf.6mo[ ,2], holdout.rf.6mo.y, negative, positive)
measures::Brier(probabilities.rf.12mo[ ,2], holdout.rf.12mo.y, negative, positive)

#ICI
library(mscpredmodel)
ici.data.rf.1mo = data.frame(prob=probabilities.rf.1mo[, 2], holdout.quart.1mo$sb_1mo)
mscpredmodel::int_calib_index(ici.data.rf.1mo, holdout.quart.1mo.sb_1mo ~ prob)

ici.data.rf.6mo = data.frame(prob=probabilities.rf.6mo[, 2], holdout.quart.6mo$sb_6mo)
mscpredmodel::int_calib_index(ici.data.rf.6mo, holdout.quart.6mo.sb_6mo ~ prob)

ici.data.rf.12mo = data.frame(prob=probabilities.rf.12mo[, 2], holdout.quart.12mo$sb_12mo)
mscpredmodel::int_calib_index(ici.data.rf.12mo, holdout.quart.12mo.sb_12mo ~ prob)

#Net benefit
p_t = 0.10
nb.ici.1mo = gmish::nb(probabilities.rf.1mo[ ,2], holdout.rf.1mo.y, p_t) 
nb.ici.1mo

nb.ici.6mo = gmish::nb(probabilities.rf.6mo[ ,2], holdout.rf.6mo.y, p_t) 
nb.ici.6mo

nb.ici.12mo = gmish::nb(probabilities.rf.12mo[ ,2], holdout.rf.12mo.y, p_t) 
nb.ici.12mo


#Save models
saveRDS(model.rf.1mo,  "model.rf.1mo.rds")
saveRDS(model.rf.6mo,  "model.rf.6mo.rds")
saveRDS(model.rf.12mo, "model.rf.12mo.rds")

#Drop models to clear global environment
rm(train.rf.1mo.preds, train.rf.1mo.y, train.rf.6mo.preds, train.rf.6mo.y, train.rf.12mo.preds, train.rf.12mo.y,
   prauc.rf.1mo, prauc.rf.6mo, prauc.rf.12mo,
   y.hat.rf.1mo, y.hat.rf.6mo, y.hat.rf.12mo,
   holdout.rf.1mo.preds, holdout.rf.1mo.y, holdout.rf.6mo.preds, holdout.rf.6mo.y, holdout.rf.12mo.preds, holdout.rf.12mo.y,
   probabilities.rf.1mo, probabilities.rf.6mo, probabilities.rf.12mo,
   roc1.rf.1mo, roc1.rf.6mo, roc1.rf.12mo, roc.rf.30.1mo, roc.rf.30.6mo, roc.rf.30.12mo,
   prauc.rf.1mo, prauc.rf.6mo, prauc.rf.12mo, prauc.rf.30.1mo, prauc.rf.30.6mo, prauc.rf.30.12mo,
   rf.grid.1mo, rf.grid.6mo, rf.grid.12mo,
   vi.rf.1mo.2, vi.rf.6mo.2, vi.rf.12mo.2, vi.rf.1mo, vi.rf.6mo, vi.rf.12mo,
   class.pred.rf.1mo, class.pred.rf.6mo, class.pred.rf.12mo,
   probabilities.rf.1mo, probabilities.rf.6mo, probabilities.rf.12mo,
   prob_no_rf.1mo, prob_no_rf.6mo, prob_no_rf.12mo,
   prob_yes_rf.1mo, prob_yes_rf.6mo, prob_yes_rf.12mo,
   prob_no_rf.30.1mo, prob_no_rf.30.6mo, prob_no_rf.30.12mo, prob_yes_rf.30.1mo, prob_yes_rf.30.6mo, prob_yes_rf.30.12mo,
   model.rf.1mo, model.rf.6mo, model.rf.12mo,
   pred.cutoff.30.1mo, pred.cutoff.30.6mo, pred.cutoff.30.12mo
)
