# PROGRAM: STR_Boosting.R
# STUDY: STR - Short-Term Risk for SB 
# DESCRIPTION: Boosting
# INPUTS: train.ROSE.1mo.data, train.ROSE.6mo.data, train.ROSE.12mo, holdout.quart.1mo, holdout.quart.6mo, holdout.quart.12mo
# OUTPUTS: models: model.boost.1mo, model.boost.6mo, model.boost.12mo
# DATE CREATED: 2021-12-09
# COMPLETED:
# AUTHOR: LMO ( loreilly at iu dot edu ) 
# NOTE: 1mo outcome is not included in manuscript; main outcome=12mo, sensitivity analysis=6mo

###GRADIENT BOOSTING###

#Set library
library(caret)

#Set seed
set.seed(BOOSTSEED)

#Create separate datasets for predictors and outcome (by each outcome)
#Holdout sample is not oversampled
#Predictors and outcome can be amended
   #1 month
train.boost.1mo.preds = as.matrix(train.ROSE.1mo.data[ ,c(1:60,62:210)]) 
train.boost.1mo.y = train.ROSE.1mo.data[[61]] 

holdout.boost.1mo.preds = as.matrix(holdout.quart.1mo[ ,c(1:60,62:210)]) 
holdout.boost.1mo.y = holdout.quart.1mo[[61]]  

   #6 month
train.boost.6mo.preds = as.matrix(train.ROSE.6mo.data[ ,c(1:60,62:210)]) 
train.boost.6mo.y = train.ROSE.6mo.data[[61]]  

holdout.boost.6mo.preds = as.matrix(holdout.quart.6mo[ ,c(1:60,62:210)]) 
holdout.boost.6mo.y = holdout.quart.6mo[[61]]  

   #12 month
train.boost.12mo.preds = as.matrix(train.ROSE.12mo.data[ ,c(1:60,62:210)]) 
train.boost.12mo.y = train.ROSE.12mo.data[[61]] 

holdout.boost.12mo.preds = as.matrix(holdout.quart.12mo[ ,c(1:60,62:210)]) 
holdout.boost.12mo.y = holdout.quart.12mo[[61]]


###SET UP MODEL##
#Grid for tuning parameters
#boost_grid =  expand.grid(nrounds = c(50, 100, 150) #number of boosting iterations [default=100]
#                          , max_depth = c(1, 3, 5, 7, 10) #maximum tree depth [default=6]
#                          , eta =0.3 #shrinkage [default=0.3]
#                          , gamma = 0 #minimum loss reduction [default=0]
#                          , colsample_bytree = 1 #subsample ratio of columns [default=1]
#                          , min_child_weight = 1 #minimum sum of instance weight [default=1]
#                          , subsample = 1 #subsample percentage [default=1]
#)

boost_grid =  expand.grid(nrounds = 150 #number of boosting iterations [default=100]
                          , max_depth = 2 #maximum tree depth [default=6]
                          , eta =0.3 #shrinkage [default=0.3]
                          , gamma = 0 #minimum loss reduction [default=0]
                          , colsample_bytree = 1 #subsample ratio of columns [default=1]
                          , min_child_weight = 1 #minimum sum of instance weight [default=1]
                          , subsample = 1 #subsample percentage [default=1]
)

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
   #1 month
model.boost.1mo = caret::train (train.boost.1mo.preds
                            , as.factor(train.boost.1mo.y)
                            , method="xgbTree" 
                            , tuneGrid = boost_grid
                            , trControl = train_control
                            #, family = "binomial"
                            #, standardize = FALSE
                            #, nthread = 1
)

   #6 months
model.boost.6mo = caret::train (train.boost.6mo.preds
                                , as.factor(train.boost.6mo.y)
                                , method="xgbTree" 
                                , tuneGrid = boost_grid
                                , trControl = train_control
                                #, family = "binomial"
                                #, standardize = FALSE
                                #, nthread = 1
)

   #12 months
model.boost.12mo = caret::train (train.boost.12mo.preds
                                 , as.factor(train.boost.12mo.y)
                                 , method="xgbTree" 
                                 , tuneGrid = boost_grid
                                 , trControl = train_control
                                 #, family = "binomial"
                                 #, standardize = FALSE
                                 #, nthread = 1
)

#Check model and besttuning parameters
model.boost.1mo
model.boost.6mo
model.boost.12mo

model.boost.1mo$bestTune
model.boost.6mo$bestTune
model.boost.12mo$bestTune

summary(model.boost.1mo$finalModel)
summary(model.boost.6mo$finalModel)
summary(model.boost.12mo$finalModel)

boost.bestresult = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

boost.bestresult(model.boost.1mo)
boost.bestresult(model.boost.6mo)
boost.bestresult(model.boost.12mo)

# Variable Importance
library(xtable)
   #1 month
vi.boost.1mo = varImp(model.boost.1mo)
vi.boost.1mo
xtable(vi.boost.1mo$importance)

vi.boost.1mo.2 = caret::varImp(model.boost.1mo, scale=FALSE)
vi.boost.1mo.2

   #6 months
vi.boost.6mo = varImp(model.boost.6mo)
vi.boost.6mo
xtable(vi.boost.6mo$importance)

vi.boost.6mo.2 = caret::varImp(model.boost.6mo, scale=FALSE)
vi.boost.6mo.2

   #12 months
vi.boost.12mo = varImp(model.boost.12mo)
vi.boost.12mo
xtable(vi.boost.12mo$importance)

vi.boost.12mo.2 = caret::varImp(model.boost.12mo, scale=FALSE)
vi.boost.12mo.2

#Fit final model, predict in holdout sample
y.hat.boost.1mo  = stats::predict(model.boost.1mo, holdout.boost.1mo.preds)
y.hat.boost.6mo  = stats::predict(model.boost.6mo, holdout.boost.6mo.preds)
y.hat.boost.12mo = stats::predict(model.boost.12mo, holdout.boost.12mo.preds)

#Model summaries
str(y.hat.boost.1mo)
summary(y.hat.boost.1mo)

str(y.hat.boost.6mo)
summary(y.hat.boost.6mo)

str(y.hat.boost.12mo)
summary(y.hat.boost.12mo)

#Model prediction as probabilities in holdout sample
probabilities.boost.1mo  = stats::predict(model.boost.1mo, type="prob", holdout.boost.1mo.preds)
probabilities.boost.6mo  = stats::predict(model.boost.6mo, type="prob", holdout.boost.6mo.preds)
probabilities.boost.12mo = stats::predict(model.boost.12mo, type="prob", holdout.boost.12mo.preds)


#Confusion matrix
#First variables=predicted; second="truth"
#Round at 0.5 criterion
library(caret)
class.pred.boost.1mo = as.factor(round(probabilities.boost.1mo[ ,2]))
caret::confusionMatrix(class.pred.boost.1mo, as.factor(holdout.boost.1mo.y), positive="1", mode="everything")

class.pred.boost.6mo = as.factor(round(probabilities.boost.6mo[ ,2]))
caret::confusionMatrix(class.pred.boost.6mo, as.factor(holdout.boost.6mo.y), positive="1", mode="everything")

class.pred.boost.12mo = as.factor(round(probabilities.boost.12mo[ ,2]))
caret::confusionMatrix(class.pred.boost.12mo, as.factor(holdout.boost.12mo.y), positive="1", mode="everything")

#Change cutoff to 5%
probabilities.boost.1mo.05 = ifelse(stats::predict(model.boost.1mo, holdout.boost.1mo.preds, type="prob")>=0.05,1,0)
class.pred.boost.1mo.05 = as.factor(round(probabilities.boost.1mo.05[, 2]))
caret::confusionMatrix(class.pred.boost.1mo.05, as.factor(holdout.boost.1mo.y), positive="1")

probabilities.boost.6mo.05 = ifelse(stats::predict(model.boost.6mo, holdout.boost.6mo.preds, type="prob")>=0.05,1,0)
class.pred.boost.6mo.05 = as.factor(round(probabilities.boost.6mo.05[, 2]))
caret::confusionMatrix(class.pred.boost.6mo.05, as.factor(holdout.boost.6mo.y), positive="1")

probabilities.boost.12mo.05 = ifelse(stats::predict(model.boost.12mo, holdout.boost.12mo.preds, type="prob")>=0.05,1,0)
class.pred.boost.12mo.05 = as.factor(round(probabilities.boost.12mo.05[, 2]))
caret::confusionMatrix(class.pred.boost.12mo.05, as.factor(holdout.boost.12mo.y), positive="1")

#Change cutoff to 10%
probabilities.boost.1mo.10 = ifelse(stats::predict(model.boost.1mo, holdout.boost.1mo.preds, type="prob")>=0.10,1,0)
class.pred.boost.1mo.10 = as.factor(round(probabilities.boost.1mo.10[, 2]))
caret::confusionMatrix(class.pred.boost.1mo.10, as.factor(holdout.boost.1mo.y), positive="1", mode="everything")

probabilities.boost.6mo.10 = ifelse(stats::predict(model.boost.6mo, holdout.boost.6mo.preds, type="prob")>=0.10,1,0)
class.pred.boost.6mo.10 = as.factor(round(probabilities.boost.6mo.10[, 2]))
caret::confusionMatrix(class.pred.boost.6mo.10, as.factor(holdout.boost.6mo.y), positive="1", mode="everything")

probabilities.boost.12mo.10 = ifelse(stats::predict(model.boost.12mo, holdout.boost.12mo.preds, type="prob")>=0.10,1,0)
class.pred.boost.12mo.10 = as.factor(round(probabilities.boost.12mo.10[, 2]))
caret::confusionMatrix(class.pred.boost.12mo.10, as.factor(holdout.boost.12mo.y), positive="1", mode="everything")

#Change cutoff to 25%
probabilities.boost.1mo.25 = ifelse(stats::predict(model.boost.1mo, holdout.boost.1mo.preds, type="prob")>=0.25,1,0)
class.pred.boost.1mo.25 = as.factor(round(probabilities.boost.1mo.25[, 2]))
caret::confusionMatrix(class.pred.boost.1mo.25, as.factor(holdout.boost.1mo.y), positive="1", mode="everything")

probabilities.boost.6mo.25 = ifelse(stats::predict(model.boost.6mo, holdout.boost.6mo.preds, type="prob")>=0.25,1,0)
class.pred.boost.6mo.25 = as.factor(round(probabilities.boost.6mo.25[, 2]))
caret::confusionMatrix(class.pred.boost.6mo.25, as.factor(holdout.boost.6mo.y), positive="1", mode="everything")

probabilities.boost.12mo.25 = ifelse(stats::predict(model.boost.12mo, holdout.boost.12mo.preds, type="prob")>=0.25,1,0)
class.pred.boost.12mo.25 = as.factor(round(probabilities.boost.12mo.25[, 2]))
caret::confusionMatrix(class.pred.boost.12mo.25, as.factor(holdout.boost.12mo.y), positive="1", mode="everything")

#AUC 
library(pROC)
roc1.boost.1mo = roc(holdout.boost.1mo.y, probabilities.boost.1mo[ ,2], ordered=TRUE)
auc(roc1.boost.1mo)
ci.auc(roc1.boost.1mo, conf.level=0.95, method="bootstrap", boot.n=10000)
#plot(roc1.boost.1mo)

roc1.boost.6mo = roc(holdout.boost.6mo.y, probabilities.boost.6mo[ ,2], ordered=TRUE)
auc(roc1.boost.6mo)
ci.auc(roc1.boost.6mo, conf.level=0.95, method="bootstrap", boot.n=10000)
#plot(roc1.boost.6mo)

roc1.boost.12mo = roc(holdout.boost.12mo.y, probabilities.boost.12mo[ ,2], ordered=TRUE)
auc(roc1.boost.12mo)
ci.auc(roc1.boost.12mo, conf.level=0.95, method="bootstrap", boot.n=10000)
#plot(roc1.boost.12mo)

   #partial AUC
auc(roc1.boost.1mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.boost.6mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc1.boost.12mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)

#PRAUC
library(PRROC)
#1 month
prob_yes_boost.1mo =  probabilities.boost.1mo[ ,2][holdout.boost.1mo.y==1]
prob_no_boost.1mo  =  probabilities.boost.1mo[ ,2][holdout.boost.1mo.y==0]

roc2.boost.1mo =   roc.curve(scores.class0 = prob_yes_boost.1mo, scores.class1=prob_no_boost.1mo, curve=T)
plot(roc2.boost.1mo)
prauc.boost.1mo = pr.curve(scores.class0 = prob_yes_boost.1mo, scores.class1=prob_no_boost.1mo, curve=T)
prauc.boost.1mo
plot(prauc.boost.1mo)

#6 months
prob_yes_boost.6mo =  probabilities.boost.6mo[ ,2][holdout.boost.6mo.y==1]
prob_no_boost.6mo  =  probabilities.boost.6mo[ ,2][holdout.boost.6mo.y==0]

roc2.boost.6mo =   roc.curve(scores.class0 = prob_yes_boost.6mo, scores.class1=prob_no_boost.6mo, curve=T)
plot(roc2.boost.6mo)
prauc.boost.6mo = pr.curve(scores.class0 = prob_yes_boost.6mo, scores.class1=prob_no_boost.6mo, curve=T)
prauc.boost.6mo
plot(prauc.boost.6mo)

#12 months
prob_yes_boost.12mo =  probabilities.boost.12mo[ ,2][holdout.boost.12mo.y==1]
prob_no_boost.12mo  =  probabilities.boost.12mo[ ,2][holdout.boost.12mo.y==0]

roc2.boost.12mo =   roc.curve(scores.class0 = prob_yes_boost.12mo, scores.class1=prob_no_boost.12mo, curve=T)
plot(roc2.boost.12mo)
prauc.boost.12mo = pr.curve(scores.class0 = prob_yes_boost.12mo, scores.class1=prob_no_boost.12mo, curve=T)
prauc.boost.12mo
plot(prauc.boost.12mo)

#Brier score
negative = 0
positive = 1
measures::Brier(probabilities.boost.1mo[ ,2], holdout.boost.1mo.y, negative, positive)
measures::Brier(probabilities.boost.6mo[ ,2], holdout.boost.6mo.y, negative, positive)
measures::Brier(probabilities.boost.12mo[ ,2], holdout.boost.12mo.y, negative, positive)

#ICI
library(mscpredmodel)
ici.data.boost.1mo = data.frame(prob=probabilities.boost.1mo[, 2], holdout.quart.1mo$sb_1mo)
mscpredmodel::int_calib_index(ici.data.boost.1mo, holdout.quart.1mo.sb_1mo ~ prob)

ici.data.boost.6mo = data.frame(prob=probabilities.boost.6mo[, 2], holdout.quart.6mo$sb_6mo)
mscpredmodel::int_calib_index(ici.data.boost.6mo, holdout.quart.6mo.sb_6mo ~ prob)

ici.data.boost.12mo = data.frame(prob=probabilities.boost.12mo[, 2], holdout.quart.12mo$sb_12mo)
mscpredmodel::int_calib_index(ici.data.boost.12mo, holdout.quart.12mo.sb_12mo ~ prob)


#Net benefit
p_t = 0.10
nb.ici.1mo = gmish::nb(probabilities.boost.1mo[ ,2], holdout.boost.1mo.y, p_t) 
nb.ici.1mo

nb.ici.6mo = gmish::nb(probabilities.boost.6mo[ ,2], holdout.boost.6mo.y, p_t) 
nb.ici.6mo

nb.ici.12mo = gmish::nb(probabilities.boost.12mo[ ,2], holdout.boost.12mo.y, p_t) 
nb.ici.12mo


#Save models
saveRDS(model.boost.1mo,  "model.boost.1mo.rds")
saveRDS(model.boost.6mo,  "model.boost.6mo.rds")
saveRDS(model.boost.12mo, "model.boost.12mo.rds")

#Drop models to clear global environment
rm(train.boost.1mo.preds, train.boost.1mo.y, train.boost.6mo.preds, train.boost.6mo.y, train.boost.12mo.preds, train.boost.12mo.y,
   prauc.boost.1mo, prauc.boost.6mo, prauc.boost.12mo,
   y.hat.boost.1mo, y.hat.boost.6mo, y.hat.boost.12mo,
   holdout.boost.1mo.preds, holdout.boost.1mo.y, holdout.boost.6mo.preds, holdout.boost.6mo.y, holdout.boost.12mo.preds, holdout.boost.12mo.y,
   probabilities.boost.1mo, probabilities.boost.6mo, probabilities.boost.12mo,
   roc1.boost.1mo, roc1.boost.6mo, roc1.boost.12mo,
   prauc.boost.1mo, prauc.boost.6mo, prauc.boost.12mo,
   boost.grid.1mo, boost.grid.6mo, boost.grid.12mo,
   vi.boost.1mo.2, vi.boost.6mo.2, vi.boost.12mo.2, vi.boost.1mo, vi.boost.6mo, vi.boost.12mo,
   class.pred.boost.1mo, class.pred.boost.6mo, class.pred.boost.12mo,
   probabilities.boost.1mo, probabilities.boost.6mo, probabilities.boost.12mo,
   prob_no_boost.1mo, prob_no_boost.6mo, prob_no_boost.12mo,
   prob_yes_boost.1mo, prob_yes_boost.6mo, prob_yes_boost.12mo,
   boost_grid,
   model.boost.1mo, model.boost.6mo, model.boost.12mo
)
