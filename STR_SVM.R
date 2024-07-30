# PROGRAM: STR_SVM.R
# STUDY: STR - Short-Term Risk for SB 
# DESCRIPTION: Support vector machine, ensemble method
# INPUTS: train.ROSE.1mo.data, train.ROSE.6mo.data, train.ROSE.12mo, holdout.quart.1mo, holdout.quart.6mo, holdout.quart.12mo
# OUTPUTS: models: svm.linear.1mo, svm.linear.6mo, svm.linear.12mo, svm.radial.1mo, svm.radial.6mo, svm.radial.12mo, svm.poly.1mo, svm.poly.6mo, svm.poly.12mo
# DATE CREATED: 2022-02-01
# COMPLETED: 
# AUTHOR: LMO ( loreilly at iu dot edu ) 
# NOTE: 1mo outcome is not included in manuscript; main outcome=12mo, sensitivity analysis=6mo

###SUPPORT VECTOR MACHINES###

#Set library
library(e1071)

#Create separate datasets for predictors and outcome (by each outcome)
#Holdout sample is not oversampled
#Predictors and outcome can be amended
   #1 month
train.svm.1mo.preds = data.frame(train.ROSE.1mo.data[ ,c(1:60,62:210)]) 
train.svm.1mo.y = train.ROSE.1mo.data[[61]]  

holdout.svm.1mo.preds = as.matrix(holdout.quart.1mo[ ,c(1:60,62:210)]) 
holdout.svm.1mo.y = holdout.quart.1mo[[61]] 

   #6 month
train.svm.6mo.preds = as.matrix(train.ROSE.6mo.data[ ,c(1:60,62:210)])
train.svm.6mo.y = train.ROSE.6mo.data[[61]]  

holdout.svm.6mo.preds = as.matrix(holdout.quart.6mo[ ,c(1:60,62:210)]) 
holdout.svm.6mo.y = holdout.quart.6mo[[61]] 

   #12 month
train.svm.12mo.preds = as.matrix(train.ROSE.12mo.data[ ,c(1:60,62:210)]) 
train.svm.12mo.y = train.ROSE.12mo.data[[61]] 

holdout.svm.12mo.preds = as.matrix(holdout.quart.12mo[ ,c(1:60,62:210)]) 
holdout.svm.12mo.y = holdout.quart.12mo[[61]] 


#SET UP MODELS
#cost = cost of constraints violation (default = 1)
#gamma = needed for all except linear (default = 1/data dimension)
#degree = polynomial kernal (default = 3)
   #Linear
set.seed(SVMSEED)
svm.linear.1mo = tune.svm(as.factor(sb_1mo) ~ . #Amend for outcome
                          , data = train.ROSE.1mo.data
                          , kernel = "linear"
                         #, cost = c(0.1, 1, 5, 10) #tuning
                          , cost = 5
                          , scale = FALSE
                          , type = "C-classification"
                          , probability = TRUE
)

saveRDS(svm.linear.1mo, "svm.lin.1mo.prob.rds")

svm.linear.6mo = tune.svm(as.factor(sb_6mo) ~ . #Amend for outcome
                          , data = train.ROSE.6mo.data
                          , kernel = "linear"
                          #, cost = c(0.1, 1, 5, 10) #tuning
                          , cost = 5
                          , scale = FALSE
                          , type = "C-classification"
                          , probability = TRUE
)

saveRDS(svm.linear.6mo, "svm.lin.6mo.prob.rds")

svm.linear.12mo = tune.svm(as.factor(sb_12mo) ~ . #Amend for outcome
                           , data = train.ROSE.12mo.data
                           , kernel = "linear"
                           #, cost = c(0.1, 1, 5, 10) #tuning
                           , cost = 5
                           , scale = FALSE
                           , type = "C-classification"
                           , probability = TRUE
)

saveRDS(svm.linear.12mo, "svm.lin.12mo.prob.rds")

   #Radial
set.seed(SVMSEED)
svm.radial.1mo = tune.svm(as.factor(sb_1mo) ~ . #Amend for outcome
                          , data = train.ROSE.1mo.data
                          , kernel = "radial"
                          , gamma = c(0.1, 0.5, 1)
                          , cost = c(0.01, 0.015, 0.02)
                          , scale = FALSE
                          , type = "C-classification"
)

saveRDS(svm.radial.1mo, "svm.radial.1mo.rds")

svm.radial.6mo = tune.svm(as.factor(sb_6mo) ~ . #Amend for outcome
                          , data = train.ROSE.6mo.data
                          , kernel = "radial"
                          , gamma = c(0.1, 0.5, 1)
                          , cost =  c(0.01, 0.015, 0.02)
                          , scale = FALSE
                          , type = "C-classification"
)

saveRDS(svm.radial.6mo, "svm.radial.6mo.rds")

svm.radial.12mo = tune.svm(as.factor(sb_12mo) ~ . #Amend for outcome
                           , data = train.ROSE.12mo.data
                           , kernel = "radial"
                           , gamma = c(0.1, 0.5, 1)
                           , cost =  c(0.01, 0.015, 0.02)
                           , scale = FALSE
                           , type = "C-classification"
)

saveRDS(svm.radial.12mo, "svm.radial.12mo.rds")

   #Polynomial
set.seed(SVMSEED)
svm.poly.1mo = tune.svm(as.factor(sb_1mo) ~ . #Amend for outcome
                        , data = train.ROSE.1mo.data
                        , kernel = "polynomial"
                        , degree = c(2, 3, 4)
                        , coef0 = c(from = 0.1, to = 4, by = 0.1)
                        , scale = FALSE
                        , type = "C-classification"
)

saveRDS(svm.poly.1mo, "svm.poly.1mo.rds")

svm.poly.6mo = tune.svm(as.factor(sb_6mo) ~ . #Amend for outcome
                        , data = train.ROSE.6mo.data
                        , kernel = "polynomial"
                        , degree = c(2, 3, 4)
                        , coef0 = c(from = 0.1, to = 4, by = 0.1)
                        , scale = FALSE
                        , type = "C-classification"
)

saveRDS(svm.poly.6mo, "svm.poly.6mo.rds")

svm.poly.12mo = tune.svm(as.factor(sb_12mo) ~ . #Amend for outcome
                         , data = train.ROSE.12mo.data
                         , kernel = "polynomial"
                         , degree = c(2, 3, 4)
                         , coef0 = c(from = 0.1, to = 4, by = 0.1)
                         , scale = FALSE
                         , type = "C-classification"
)

saveRDS(svm.poly.12mo, "svm.poly.12mo.rds")

#Model summaries
summary(svm.linear.1mo)
summary(svm.linear.6mo)
summary(svm.linear.12mo)

summary(svm.radial.1mo)
summary(svm.radial.6mo)
summary(svm.radial.12mo)

summary(svm.poly.1mo)
summary(svm.poly.6mo)
summary(svm.poly.12mo)

#Assign best models
best.svm.linear.1moprob = svm.linear.1mo$best.model
best.svm.linear.6moprob = svm.linear.6mo$best.model
best.svm.linear.12moprob = svm.linear.12mo$best.model

best.svm.radial.1mo  = svm.radial.1mo$best.model
best.svm.radial.6mo  = svm.radial.6mo$best.model
best.svm.radial.12mo = svm.radial.12mo$best.model

best.svm.poly.1mo  = svm.poly.1mo$best.model
best.svm.poly.6mo  = svm.poly.6mo$best.model
best.svm.poly.12mo = svm.poly.12mo$best.model

#Save models
#Linear with prob=true when modeled
saveRDS(svm.linear.1mo,  "best.svm.linear.1moprob")
saveRDS(svm.linear.6mo,  "best.svm.linear.6moprob")
saveRDS(svm.linear.12mo, "best.svm.linear.12moprob")

#Radial
saveRDS(svm.radial.1mo, "svm.radial.1mo.rds")
saveRDS(svm.radial.6mo, "svm.radial.6mo.rds")
saveRDS(svm.radial.12mo, "svm.radial.12mo.rds")

#Plot models
#plot (best.svm.linear, train.ROSE.data)
#plot (best.svm.radial, train.ROSE.data)
#plot (best.svm.poly, train.ROSE.data)

#Fit final model
y.hat.linear.1mo.prob  = stats::predict(best.svm.linear.1moprob, holdout.svm.1mo.preds, probability=TRUE)
y.hat.linear.6mo.prob  = stats::predict(best.svm.linear.6moprob, holdout.svm.6mo.preds, probability=TRUE)
y.hat.linear.12mo.prob  = stats::predict(best.svm.linear.12moprob, holdout.svm.12mo.preds, probability=TRUE)

y.hat.radial.1mo  = stats::predict(best.svm.radial.1mo, holdout.svm.1mo.preds)
y.hat.radial.6mo  = stats::predict(best.svm.radial.6mo, holdout.svm.6mo.preds)
y.hat.radial.12mo = stats::predict(best.svm.radial.12mo, holdout.svm.12mo.preds)

y.hat.poly.1mo  = stats::predict(best.svm.poly.1mo, holdout.svm.1mo.preds)
y.hat.poly.6mo  = stats::predict(best.svm.poly.6mo, holdout.svm.6mo.preds)
y.hat.poly.12mo = stats::predict(best.svm.poly.12mo, holdout.svm.12mo.preds)

#Final model summaries
str(y.hat.linear.1mo)
str(y.hat.linear.6mo)
str(y.hat.linear.12mo)
summary(y.hat.linear.1mo)
summary(y.hat.linear.6mo)
summary(y.hat.linear.12mo)

str(y.hat.radial.1mo)
str(y.hat.radial.6mo)
str(y.hat.radial.12mo)
summary(y.hat.radial.1mo)
summary(y.hat.radial.6mo)
summary(y.hat.radial.12mo)

str(y.hat.poly.1mo)
str(y.hat.poly.6mo)
str(y.hat.poly.12mo)
summary(y.hat.poly.1mo)
summary(y.hat.poly.6mo)
summary(y.hat.poly.12mo)

#Model prediction as probabilities on holdout sample
probabilities.linear.1mo.prob = stats::predict(best.svm.linear.1moprob, type="prob", holdout.svm.1mo.preds, probability=TRUE)
probabilities.linear.6mo.prob = stats::predict(best.svm.linear.6moprob, type="prob", holdout.svm.6mo.preds, probability=TRUE)
probabilities.linear.12mo.prob = stats::predict(best.svm.linear.12moprob, type="prob", holdout.svm.12mo.preds, probability=TRUE)

head(attr(probabilities.linear.1mo.prob, "probabilities"))
head(attr(probabilities.linear.6mo.prob, "probabilities"))
head(attr(probabilities.linear.12mo.prob, "probabilities"))

probabilities.linear.1mo.probcont = as.data.frame(attr(probabilities.linear.1mo.prob, "probabilities"))
probabilities.linear.6mo.probcont = as.data.frame(attr(probabilities.linear.6mo.prob, "probabilities"))
probabilities.linear.12mo.probcont = as.data.frame(attr(probabilities.linear.12mo.prob, "probabilities"))

probabilities.radial.1mo = stats::predict(best.svm.radial.1mo, type="prob", holdout.svm.1mo.preds)
probabilities.radial.6mo = stats::predict(best.svm.radial.6mo, type="prob", holdout.svm.6mo.preds)
probabilities.radial.12mo = stats::predict(best.svm.radial.12mo, type="prob", holdout.svm.12mo.preds)

probabilities.poly.1mo = stats::predict(best.svm.poly.1mo, type="prob", holdout.svm.1mo.preds)
probabilities.poly.6mo = stats::predict(best.svm.poly.6mo, type="prob", holdout.svm.6mo.preds)
probabilities.poly.12mo = stats::predict(best.svm.poly.12mo, type="prob", holdout.svm.12mo.preds)


#Confusion matrix
#First variables=predicted; second="truth"
#Round at 0.5 criterion
library(caret)
#Linear
caret::confusionMatrix(probabilities.linear.1mo.prob, as.factor(holdout.svm.1mo.y), positive="1", mode="everything")
caret::confusionMatrix(probabilities.linear.6mo.prob, as.factor(holdout.svm.6mo.y), positive="1", mode="everything")
caret::confusionMatrix(probabilities.linear.12mo.prob, as.factor(holdout.svm.12mo.y), positive="1", mode="everything")

#Change cutoff to 5%
probabilities.linear.1mo.05 = ifelse(as.numeric(as.character(stats::predict(best.svm.linear.1moprob, holdout.svm.1mo.preds, probability = TRUE)))>=0.05,1,0)
class.pred.lin.1mo.05 = as.factor(probabilities.linear.1mo.05)
caret::confusionMatrix(class.pred.lin.1mo.05, as.factor(holdout.svm.1mo.y), positive="1", mode="everything")

probabilities.linear.6mo.05 = ifelse(as.numeric(as.character(stats::predict(best.svm.linear.6moprob, holdout.svm.6mo.preds, probability = TRUE)))>=0.05,1,0)
class.pred.lin.6mo.05 = as.factor(probabilities.linear.6mo.05)
caret::confusionMatrix(class.pred.lin.6mo.05, as.factor(holdout.svm.6mo.y), positive="1", mode="everything")

probabilities.linear.12mo.05 = ifelse(as.numeric(as.character(stats::predict(best.svm.linear.12moprob, holdout.svm.12mo.preds, probability = TRUE)))>=0.05,1,0)
class.pred.lin.12mo.05 = as.factor(probabilities.linear.12mo.05)
caret::confusionMatrix(class.pred.lin.12mo.05, as.factor(holdout.svm.12mo.y), positive="1", mode="everything")

#Change cutoff to 10%
probabilities.linear.1mo.10 = ifelse(as.numeric(as.character(stats::predict(best.svm.linear.1moprob, holdout.svm.1mo.preds, probability = TRUE)))>=0.10,1,0)
class.pred.lin.1mo.10 = as.factor(probabilities.linear.1mo.10)
caret::confusionMatrix(class.pred.lin.1mo.10, as.factor(holdout.svm.1mo.y), positive="1", mode="everything")

probabilities.linear.6mo.10 = ifelse(as.numeric(as.character(stats::predict(best.svm.linear.6moprob, holdout.svm.6mo.preds, probability = TRUE)))>=0.10,1,0)
class.pred.lin.6mo.10 = as.factor(probabilities.linear.6mo.10)
caret::confusionMatrix(class.pred.lin.6mo.10, as.factor(holdout.svm.6mo.y), positive="1", mode="everything")

probabilities.linear.12mo.10 = ifelse(as.numeric(as.character(stats::predict(best.svm.linear.12moprob, holdout.svm.12mo.preds, probability = TRUE)))>=0.10,1,0)
class.pred.lin.12mo.10 = as.factor(probabilities.linear.12mo.10)
caret::confusionMatrix(class.pred.lin.12mo.10, as.factor(holdout.svm.12mo.y), positive="1", mode="everything")

#Change cutoff to 25%
probabilities.linear.1mo.25 = ifelse(as.numeric(as.character(stats::predict(best.svm.linear.1moprob, holdout.svm.1mo.preds, probability = TRUE)))>=0.25,1,0)
class.pred.lin.1mo.25 = as.factor(probabilities.linear.1mo.25)
caret::confusionMatrix(class.pred.lin.1mo.25, as.factor(holdout.svm.1mo.y), positive="1", mode="everything")

probabilities.linear.6mo.25 = ifelse(as.numeric(as.character(stats::predict(best.svm.linear.6moprob, holdout.svm.6mo.preds, probability = TRUE)))>=0.25,1,0)
class.pred.lin.6mo.25 = as.factor(probabilities.linear.6mo.25)
caret::confusionMatrix(class.pred.lin.6mo.25, as.factor(holdout.svm.6mo.y), positive="1", mode="everything")

probabilities.linear.12mo.25 = ifelse(as.numeric(as.character(stats::predict(best.svm.linear.12moprob, holdout.svm.12mo.preds, probability = TRUE)))>=0.25,1,0)
class.pred.lin.12mo.25 = as.factor(probabilities.linear.12mo.25)
caret::confusionMatrix(class.pred.lin.12mo.25, as.factor(holdout.svm.12mo.y), positive="1", mode="everything")


#Radial
caret::confusionMatrix(probabilities.radial.1mo, as.factor(holdout.svm.1mo.y), positive="1")
caret::confusionMatrix(probabilities.radial.6mo, as.factor(holdout.svm.6mo.y), positive="1")
caret::confusionMatrix(probabilities.radial.12mo, as.factor(holdout.svm.12mo.y), positive="1")

#Change cutoff to 10%
probabilities.radial.1mo.10 = ifelse(as.numeric(as.character(stats::predict(best.svm.radial.1mo, holdout.svm.1mo.preds, probability = TRUE)))>=0.10,1,0)
class.pred.radial.1mo.10 = as.factor(probabilities.radial.1mo.10)
caret::confusionMatrix(class.pred.radial.1mo.10, as.factor(holdout.svm.1mo.y), positive="1", mode="everything")

probabilities.radial.6mo.10 = ifelse(as.numeric(as.character(stats::predict(best.svm.radial.6mo, holdout.svm.6mo.preds, probability = TRUE)))>=0.10,1,0)
class.pred.radial.6mo.10 = as.factor(probabilities.radial.6mo.10)
caret::confusionMatrix(class.pred.radial.6mo.10, as.factor(holdout.svm.6mo.y), positive="1", mode="everything")

probabilities.radial.12mo.10 = ifelse(as.numeric(as.character(stats::predict(best.svm.radial.12mo, holdout.svm.12mo.preds, probability = TRUE)))>=0.10,1,0)
class.pred.radial.12mo.10 = as.factor(probabilities.radial.12mo.10)
caret::confusionMatrix(class.pred.radial.12mo.10, as.factor(holdout.svm.12mo.y), positive="1", mode="everything")

#Poly
caret::confusionMatrix(probabilities.poly.1mo, as.factor(holdout.svm.1mo.y), positive="1")
caret::confusionMatrix(probabilities.poly.6mo, as.factor(holdout.svm.6mo.y), positive="1")
caret::confusionMatrix(probabilities.poly.12mo, as.factor(holdout.svm.12mo.y), positive="1")

#Change cutoff to 10%
probabilities.poly.1mo.10 = ifelse(as.numeric(as.character(stats::predict(best.svm.poly.1mo, holdout.svm.1mo.preds, probability = TRUE)))>=0.10,1,0)
class.pred.poly.1mo.10 = as.factor(probabilities.poly.1mo.10)
caret::confusionMatrix(class.pred.poly.1mo.10, as.factor(holdout.svm.1mo.y), positive="1", mode="everything")

probabilities.poly.6mo.10 = ifelse(as.numeric(as.character(stats::predict(best.svm.poly.6mo, holdout.svm.6mo.preds, probability = TRUE)))>=0.10,1,0)
class.pred.poly.6mo.10 = as.factor(probabilities.poly.6mo.10)
caret::confusionMatrix(class.pred.poly.6mo.10, as.factor(holdout.svm.6mo.y), positive="1", mode="everything")

probabilities.poly.12mo.10 = ifelse(as.numeric(as.character(stats::predict(best.svm.poly.12mo, holdout.svm.12mo.preds, probability = TRUE)))>=0.10,1,0)
class.pred.poly.12mo.10 = as.factor(probabilities.poly.12mo.10)
caret::confusionMatrix(class.pred.poly.12mo.10, as.factor(holdout.svm.12mo.y), positive="1", mode="everything")

#AUC
library(pROC)
  #Linear
roc.linear.1mo = roc(holdout.svm.1mo.y, probabilities.linear.1mo.probcont[ ,2], ordered=TRUE)
auc(roc.linear.1mo)
ci.auc(roc.linear.1mo, conf.level=0.95, method="bootstrap", boot.n=10000)
#plot(roc.linear.1mo)

roc.linear.6mo = roc(holdout.svm.6mo.y, probabilities.linear.6mo.probcont[ ,2], ordered=TRUE)
auc(roc.linear.6mo)
ci.auc(roc.linear.6mo, conf.level=0.95, method="bootstrap", boot.n=10000)
#plot(roc.linear.6mo)

roc.linear.12mo = roc(holdout.svm.12mo.y, probabilities.linear.12mo.probcont[ ,2], ordered=TRUE)
auc(roc.linear.12mo)
ci.auc(roc.linear.12mo, conf.level=0.95, method="bootstrap", boot.n=10000)
#plot(roc.linear.12mo)

  #partial AUC
auc(roc.linear.1mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc.linear.6mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc.linear.12mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)

  #Radial
roc.radial.1mo = roc(holdout.svm.1mo.y, as.ordered(probabilities.radial.1mo))
auc(roc.radial.1mo)
ci.auc(roc.radial.1mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc.radial.1mo)

roc.radial.6mo = roc(holdout.svm.6mo.y, as.ordered(probabilities.radial.6mo))
auc(roc.radial.6mo)
ci.auc(roc.radial.6mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc.radial.6mo)

roc.radial.12mo = roc(holdout.svm.12mo.y, as.ordered(probabilities.radial.12mo))
auc(roc.radial.12mo)
ci.auc(roc.radial.12mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc.radial.12mo)

  #Poly
roc.poly.1mo = roc(holdout.svm.1mo.y, as.ordered(probabilities.poly.1mo))
auc(roc.poly.1mo)
ci.auc(roc.poly.1mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc.poly.1mo)

roc.poly.6mo = roc(holdout.svm.6mo.y, as.ordered(probabilities.poly.6mo))
auc(roc.poly.6mo)
ci.auc(roc.poly.6mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc.poly.6mo)

roc.poly.12mo = roc(holdout.svm.12mo.y, as.ordered(probabilities.poly.12mo))
auc(roc.poly.12mo)
ci.auc(roc.poly.12mo, conf.level=0.95, method="bootstrap", boot.n=10000)
plot(roc.poly.12mo)

#PRAUC
library(PRROC)
  #Linear
prob_yes_lin.1mo =  probabilities.linear.1mo.prob[holdout.svm.1mo.y==1]
prob_no_lin.1mo  =  probabilities.linear.1mo.prob[holdout.svm.1mo.y==0]

prauc.linear.1mo = pr.curve(scores.class0 = prob_yes_lin.1mo, scores.class1=prob_no_lin.1mo, curve=T)
plot(prauc.linear.1mo)

prob_yes_lin.6mo =  probabilities.linear.6mo.prob[holdout.svm.6mo.y==1]
prob_no_lin.6mo  =  probabilities.linear.6mo.prob[holdout.svm.6mo.y==0]

prauc.linear.6mo = pr.curve(scores.class0 = prob_yes_lin.6mo, scores.class1=prob_no_lin.6mo, curve=T)
plot(prauc.linear.6mo)

prob_yes_lin.12mo =  probabilities.linear.12mo.prob[holdout.svm.12mo.y==1]
prob_no_lin.12mo  =  probabilities.linear.12mo.prob[holdout.svm.12mo.y==0]

prauc.linear.12mo = pr.curve(scores.class0 = prob_yes_lin.12mo, scores.class1=prob_no_lin.12mo, curve=T)
plot(prauc.linear.12mo)

  #Radial
prob_yes_rad.1mo =  probabilities.radial.1mo[holdout.svm.1mo.y==1]
prob_no_rad.1mo  =  probabilities.radial.1mo[holdout.svm.1mo.y==0]

prauc.radial.1mo = pr.curve(scores.class0 = prob_yes_rad.1mo, scores.class1=prob_no_rad.1mo, curve=T)
prauc.radial.1mo
plot(prauc.radial.1mo)

prob_yes_rad.6mo =  probabilities.radial.6mo[holdout.svm.6mo.y==1]
prob_no_rad.6mo  =  probabilities.radial.6mo[holdout.svm.6mo.y==0]

prauc.radial.6mo = pr.curve(scores.class0 = prob_yes_rad.6mo, scores.class1=prob_no_rad.6mo, curve=T)
plot(prauc.radial.6mo)

prob_yes_rad.12mo =  probabilities.radial.12mo[holdout.svm.12mo.y==1]
prob_no_rad.12mo  =  probabilities.radial.12mo[holdout.svm.12mo.y==0]

prauc.radial.12mo = pr.curve(scores.class0 = prob_yes_rad.12mo, scores.class1=prob_no_rad.12mo, curve=T)
plot(prauc.radial.12mo)

  #Poly
prob_yes_poly.1mo =  probabilities.poly.1mo[holdout.svm.1mo.y==1]
prob_no_poly.1mo  =  probabilities.poly.1mo[holdout.svm.1mo.y==0]

prauc.poly.1mo = pr.curve(scores.class0 = prob_yes_poly.1mo, scores.class1=prob_no_poly.1mo, curve=T)
prauc.poly.1mo
plot(prauc.poly.1mo)

prob_yes_poly.6mo =  probabilities.poly.6mo[holdout.svm.6mo.y==1]
prob_no_poly.6mo  =  probabilities.poly.6mo[holdout.svm.6mo.y==0]

prauc.poly.6mo = pr.curve(scores.class0 = prob_yes_poly.6mo, scores.class1=prob_no_poly.6mo, curve=T)
prauc.poly.6mo
plot(prauc.poly.6mo)

prob_yes_poly.12mo =  probabilities.poly.12mo[holdout.svm.12mo.y==1]
prob_no_poly.12mo  =  probabilities.poly.12mo[holdout.svm.12mo.y==0]

prauc.poly.12mo = pr.curve(scores.class0 = prob_yes_poly.12mo, scores.class1=prob_no_poly.12mo, curve=T)
prauc.poly.12mo
plot(prauc.poly.12mo)

#Brier score
negative = 0
positive = 1
  #Linear
measures::Brier(as.numeric(levels(probabilities.linear.1mo.prob))[probabilities.linear.1mo.prob], holdout.svm.1mo.y, negative, positive)
measures::Brier(as.numeric(levels(probabilities.linear.6mo.prob))[probabilities.linear.6mo.prob], holdout.svm.6mo.y, negative, positive)
measures::Brier(as.numeric(levels(probabilities.linear.12mo.prob))[probabilities.linear.12mo.prob], holdout.svm.12mo.y, negative, positive)
  #Radial
measures::Brier(as.numeric(levels(probabilities.radial.1mo))[probabilities.radial.1mo], holdout.svm.1mo.y, negative, positive)
measures::Brier(as.numeric(levels(probabilities.radial.6mo))[probabilities.radial.6mo], holdout.svm.6mo.y, negative, positive)
measures::Brier(as.numeric(levels(probabilities.radial.12mo))[probabilities.radial.12mo], holdout.svm.12mo.y, negative, positive)
  #Poly
measures::Brier(as.numeric(levels(probabilities.poly.1mo))[probabilities.poly.1mo], holdout.svm.1mo.y, negative, positive)
measures::Brier(as.numeric(levels(probabilities.poly.6mo))[probabilities.poly.6mo], holdout.svm.6mo.y, negative, positive)
measures::Brier(as.numeric(levels(probabilities.poly.12mo))[probabilities.poly.12mo], holdout.svm.12mo.y, negative, positive)

#ICI
library(mscpredmodel)
  #linear only
ici.data.linear.1mo = data.frame(prob=probabilities.linear.1mo.probcont[, 2], holdout.quart.1mo$sb_1mo)
mscpredmodel::int_calib_index(ici.data.linear.1mo, holdout.quart.1mo.sb_1mo ~ prob)

ici.data.linear.6mo = data.frame(prob=probabilities.linear.6mo.probcont[, 2], holdout.quart.6mo$sb_6mo)
mscpredmodel::int_calib_index(ici.data.linear.6mo, holdout.quart.6mo.sb_6mo ~ prob)

ici.data.linear.12mo = data.frame(prob=probabilities.linear.12mo.probcont[, 2], holdout.quart.12mo$sb_12mo)
mscpredmodel::int_calib_index(ici.data.linear.12mo, holdout.quart.12mo.sb_12mo ~ prob)

#Net benefit
  #linear only
p_t = 0.10
nb.ici.1mo = gmish::nb(probabilities.linear.1mo.probcont[, 2], holdout.svm.1mo.y, p_t) 
nb.ici.1mo

nb.ici.6mo = gmish::nb(probabilities.linear.6mo.probcont[ ,2], holdout.svm.6mo.y, p_t) 
nb.ici.6mo

nb.ici.12mo = gmish::nb(probabilities.linear.6mo.probcont[ ,2], holdout.svm.12mo.y, p_t) 
nb.ici.12mo


#Remove from global directory
rm(best.svm.linear.1mo, best.svm.linear.6mo, best.svm.linear.12mo, 
   best.svm.radial.1mo, best.svm.radial.6mo, best.svm.radial.12mo,
   svm.linear.1mo, svm.radial.6mo, svm.radial.12mo,
   svm.poly.1mo, svm.poly.6mo, svm.poly.12mo,
   svm.radial.1mo, svm.radial.6mo, svm.radial.12mo,
   best.svm.poly.1mo, best.svm.poly.6mo, best.svm.poly.12mo,
   train.svm.1mo.preds, train.svm.1mo.y, train.svm.6mo.preds, train.svm.6mo.y, train.svm.12mo.preds, train.svm.12mo.y,
   holdout.svm.1mo.preds, holdout.svm.1mo.y, holdout.svm.6mo.preds, holdout.svm.6mo.y, holdout.svm.12mo.preds, holdout.svm.12mo.y,
   probabilities.linear.1mo, probabilities.linear.6mo, probabilities.linear.12mo,
   probabilities.poly.1mo, probabilities.poly.6mo, probabilities.poly.12mo,
   probabilities.radial.1mo, probabilities.radial.6mo, probabilities.radial.12mo,
   prauc.linear.1mo, prauc.linear.6mo, prauc.linear.12mo,
   prauc.poly.1mo, prauc.poly.6mo, prauc.poly.12mo,
   prauc.radial.1mo, prauc.radial.6mo, prauc.radial.12mo,
   roc.linear.1mo, roc.linear.6mo, roc.linear.12mo,
   roc.poly.1mo, roc.poly.6mo, roc.poly.12mo,
   roc.radial.1mo, roc.radial.6mo, roc.radial.12mo,
   prob_no_lin.1mo, prob_no_lin.6mo, prob_no_lin.12mo, prob_yes_lin.1mo, prob_yes_lin.6mo, prob_yes_lin.12mo,
   prob_no_poly.1mo, prob_no_poly.6mo, prob_no_poly.12mo, prob_yes_poly.1mo, prob_yes_poly.6mo, prob_yes_poly.12mo,
   prob_no_rad.1mo, prob_no_rad.6mo, prob_no_rad.12mo, prob_yes_rad.1mo, prob_yes_rad.6mo, prob_yes_rad.12mo
)
