# PROGRAM: STR_LR.R
# STUDY: STR - Short-Term Risk for SB 
# DESCRIPTION: GLM for LR models
# INPUTS: train.ROSE.1mo.data, train.ROSE.6mo.data, train.ROSE.12mo, holdout.quart.1mo, holdout.quart.6mo, holdout.quart.12mo
# OUTPUTS: models: LR.1mo, LR.6mo, LR.12mo, LR.1mo.a, LR.6mo.a, LR.12mo.a
# DATE CREATED: 2022-02-10
# COMPLETED: 
# AUTHOR: LMO ( loreilly at iu dot edu ) 
# NOTE: 1mo outcome is not included in manuscript; main outcome=12mo, sensitivity analysis=6mo

#Set seed
set.seed(GLMSEED)

#GLM function
#ON TRAIN DATASET
#Runs bivaraite models with all predictors, 10-fold CV
  #1mo
univariateGLM.1mo = function(x) {
  fit = glm(reformulate(x, response='sb_1mo'), data=train.ROSE.1mo.data, family=binomial)
  cv.fit = boot::cv.glm(train.ROSE.1mo.data, fit, K=10)
  c(  summary(fit)$coefficients[, 1]
      , cv.fit$delta[2]
      , caTools::colAUC(predict(fit, type='response'), train.ROSE.1mo.data[[61]])
  )
}
  #6mo
univariateGLM.6mo = function(x) {
  fit = glm(reformulate(x, response='sb_6mo'), data=train.ROSE.6mo.data, family=binomial)
  cv.fit = boot::cv.glm(train.ROSE.6mo.data, fit, K=10)
  c(  summary(fit)$coefficients[, 1]
      , cv.fit$delta[2]
      , caTools::colAUC(predict(fit, type='response'), train.ROSE.6mo.data[[61]])
  )
}
  #12mo
univariateGLM.12mo = function(x) {
  fit = glm(reformulate(x, response='sb_12mo'), data=train.ROSE.12mo.data, family=binomial)
  cv.fit = boot::cv.glm(train.ROSE.12mo.data, fit, K=10)
  c(  summary(fit)$coefficients[, 1]
      , cv.fit$delta[2]
      , caTools::colAUC(predict(fit, type='response'), train.ROSE.12mo.data[[61]])
  )
}

#Rank by AUC, print top 7
#sapply() function over columns, and save the results

results.1mo = as.data.frame(t(sapply(setdiff(names(train.ROSE.1mo.data), 'sb_1mo'), univariateGLM.1mo)))
colnames(results.1mo) = c('Intercept', 'Estimate', 'Pr(>|z|) (Intercept)', 'AUC')
top7.1mo = results.1mo %>% dplyr::top_n(7) %>% head(7)

results.6mo = as.data.frame(t(sapply(setdiff(names(train.ROSE.6mo.data), 'sb_6mo'), univariateGLM.6mo)))
colnames(results.6mo) = c('Intercept', 'Estimate', 'Pr(>|z|) (Intercept)', 'AUC')
top7.6mo = results.6mo %>% dplyr::top_n(7) %>% head(7)

results.12mo = as.data.frame(t(sapply(setdiff(names(train.ROSE.12mo.data), 'sb_12mo'), univariateGLM.12mo)))
colnames(results.12mo) = c('Intercept', 'Estimate', 'Pr(>|z|) (Intercept)', 'AUC')
top7.12mo = results.12mo %>% dplyr::top_n(7) %>% head(7)

#LR on the top 7
LR.1mo = glm(sb_1mo ~ age_sDate + FEMALE_INDIV + tot2 + indx2 + indx13 + orx5 + orx20
             , data = train.ROSE.1mo.data
             , family = binomial
)

LR.6mo = glm(sb_6mo ~ age_sDate + FEMALE_INDIV + tot2 + indx2 + indx13 + orx17 + orx20
             , data = train.ROSE.6mo.data
             , family = binomial
)

LR.12mo = glm(sb_12mo ~ age_sDate + FEMALE_INDIV + tot2 + cc9 + indx2 + indx13 + orx20
              , data = train.ROSE.12mo.data
              , family = binomial
)

#LR on non-oversampled holdout sample
y.hat.LR.1mo.r  = stats::predict(LR.1mo, holdout.quart.1mo[ ,c(1:60,62:210)])
y.hat.LR.6mo.r  = stats::predict(LR.6mo, holdout.quart.6mo[ ,c(1:60,62:210)])
y.hat.LR.12mo.r = stats::predict(LR.12mo, holdout.quart.12mo[ ,c(1:60,62:210)])

#Model summaries
summary(LR.1mo)
summary(LR.6mo)
summary(LR.12mo)

#Turn estimates into ORs in training set
LR.1mo.OR  = exp(cbind(OR = coef(LR.1mo), confint(LR.1mo)))
LR.6mo.OR  = exp(cbind(OR = coef(LR.6mo), confint(LR.6mo)))
LR.12mo.OR = exp(cbind(OR = coef(LR.12mo), confint(LR.12mo)))

#LR on a priori variables
LR.1mo.a = glm(sb_1mo ~ orx4 + indx9 + cc2 + indx2 + cc1 + indx16 + FEMALE_INDIV + indx13 + cc9
               , data = train.ROSE.1mo.data
               , family = binomial
)

LR.6mo.a = glm(sb_6mo ~ orx4 + indx9 + cc2 + indx2 + cc1 + indx16 + FEMALE_INDIV + indx13 + cc9
               , data = train.ROSE.6mo.data
               , family = binomial
)

LR.12mo.a = glm(sb_12mo ~ orx4 + indx9 + cc2 + indx2 + cc1 + indx16 + FEMALE_INDIV + indx13 + cc9
                , data = train.ROSE.12mo.data
                , family = binomial
)

#Turn estimates into ORs
LR.1mo.a.OR  = exp(cbind(OR = coef(LR.1mo.a), confint(LR.1mo.a)))
LR.6mo.a.OR  = exp(cbind(OR = coef(LR.6mo.a), confint(LR.6mo.a)))
LR.12mo.a.OR = exp(cbind(OR = coef(LR.12mo.a), confint(LR.12mo.a)))

#Confusion matrix
#Un-oversampled holdout sample
predict.1mo = predict(LR.1mo, holdout.quart.1mo[ ,c(1:60,62:210)], type="response")
class.predict.1mo = as.factor(round(predict.1mo))
caret::confusionMatrix(class.predict.1mo, as.factor(holdout.quart.1mo$sb_1mo), positive="1", mode="everything")

predict.6mo = predict(LR.6mo, holdout.quart.6mo[ ,c(1:60,62:210)], type="response")
class.predict.6mo = as.factor(round(predict.6mo))
caret::confusionMatrix(class.predict.6mo, as.factor(holdout.quart.6mo$sb_6mo), positive="1", mode="everything")

predict.12mo = predict(LR.12mo, holdout.quart.12mo[ ,c(1:60,62:210)], type="response")
class.predict.12mo = as.factor(round(predict.12mo))
caret::confusionMatrix(class.predict.12mo, as.factor(holdout.quart.12mo$sb_12mo), positive="1", mode="everything")

#Change cutoff to 5%
class.pred.1mo.05  = as.factor(ifelse(stats::predict(LR.1mo,  holdout.quart.1mo[ ,c(1:60,62:210)],  type="response")>=0.05,"1","0"))
caret::confusionMatrix(class.pred.1mo.05, as.factor(holdout.quart.1mo$sb_1mo), positive="1")

class.pred.6mo.05  = as.factor(ifelse(stats::predict(LR.6mo, holdout.quart.6mo[ ,c(1:60,62:210)],  type="response")>=0.05,"1","0"))
caret::confusionMatrix(class.pred.6mo.05, as.factor(holdout.quart.6mo$sb_6mo), positive="1")

class.pred.12mo.05 = as.factor(ifelse(stats::predict(LR.12mo, holdout.quart.12mo[ ,c(1:60,62:210)], type="response")>=0.05,"1","0"))
caret::confusionMatrix(class.pred.12mo.05, as.factor(holdout.quart.12mo$sb_12mo), positive="1")

#Change cutoff to 10%
class.pred.1mo.10  = as.factor(ifelse(stats::predict(LR.1mo,  holdout.quart.1mo[ ,c(1:60,62:210)],  type="response")>=0.1,"1","0"))
caret::confusionMatrix(class.pred.1mo.10, as.factor(holdout.quart.1mo$sb_1mo), positive="1", mode="everything")

class.pred.6mo.10  = as.factor(ifelse(stats::predict(LR.6mo, holdout.quart.6mo[ ,c(1:60,62:210)],  type="response")>=0.1,"1","0"))
caret::confusionMatrix(class.pred.6mo.10, as.factor(holdout.quart.6mo$sb_6mo), positive="1", mode="everything")

class.pred.12mo.10 = as.factor(ifelse(stats::predict(LR.12mo, holdout.quart.12mo[ ,c(1:60,62:210)], type="response")>=0.1,"1","0"))
caret::confusionMatrix(class.pred.12mo.10, as.factor(holdout.quart.12mo$sb_12mo), positive="1", mode="everything")

#Change cutoff to 25%
class.pred.1mo.25  = as.factor(ifelse(stats::predict(LR.1mo,  holdout.quart.1mo[ ,c(1:60,62:210)],  type="response")>=0.25,"1","0"))
caret::confusionMatrix(class.pred.1mo.25, as.factor(holdout.quart.1mo$sb_1mo), positive="1", mode="everything")

class.pred.6mo.25  = as.factor(ifelse(stats::predict(LR.6mo, holdout.quart.6mo[ ,c(1:60,62:210)],  type="response")>=0.25,"1","0"))
caret::confusionMatrix(class.pred.6mo.25, as.factor(holdout.quart.6mo$sb_6mo), positive="1", mode="everything")

class.pred.12mo.25 = as.factor(ifelse(stats::predict(LR.12mo, holdout.quart.12mo[ ,c(1:60,62:210)], type="response")>=0.25,"1","0"))
caret::confusionMatrix(class.pred.12mo.25, as.factor(holdout.quart.12mo$sb_12mo), positive="1", mode="everything")

#A priori
#Un-oversampled holdout sample
predict.1mo.a = predict(LR.1mo.a, holdout.quart.1mo[ ,c(1:60,62:210)], type="response")
class.predict.1mo.a = as.factor(round(predict.1mo.a))
caret::confusionMatrix(class.predict.1mo.a, as.factor(holdout.quart.1mo$sb_1mo), positive="1", mode="everything")

predict.6mo.a = predict(LR.6mo.a, holdout.quart.6mo[ ,c(1:60,62:210)], type="response")
class.predict.6mo.a = as.factor(round(predict.6mo.a))
caret::confusionMatrix(class.predict.6mo.a, as.factor(holdout.quart.6mo$sb_6mo), positive="1", mode="everything")

predict.12mo.a = predict(LR.12mo.a, holdout.quart.12mo[ ,c(1:60,62:210)], type="response")
class.predict.12mo.a = as.factor(round(predict.12mo.a))
caret::confusionMatrix(class.predict.12mo.a, as.factor(holdout.quart.12mo$sb_12mo), positive="1", mode="everything")

#Change cutoff to 1%
class.pred.1mo.a.01  = as.factor(ifelse(stats::predict(LR.1mo.a,  holdout.quart.1mo[ ,c(1:60,62:210)],  type="response")>=0.01,"1","0"))
caret::confusionMatrix(class.pred.1mo.a.01, as.factor(holdout.quart.1mo$sb_1mo), positive="1")

class.pred.6mo.a.01  = as.factor(ifelse(stats::predict(LR.6mo.a, holdout.quart.6mo[ ,c(1:60,62:210)],  type="response")>=0.01,"1","0"))
caret::confusionMatrix(class.pred.6mo.a.01, as.factor(holdout.quart.6mo$sb_6mo), positive="1")

class.pred.12mo.a.01 = as.factor(ifelse(stats::predict(LR.12mo.a, holdout.quart.12mo[ ,c(1:60,62:210)], type="response")>=0.01,"1","0"))
caret::confusionMatrix(class.pred.12mo.a.01, as.factor(holdout.quart.12mo$sb_12mo), positive="1")

#Change cutoff to 5%
class.pred.1mo.a.05  = as.factor(ifelse(stats::predict(LR.1mo.a,  holdout.quart.1mo[ ,c(1:60,62:210)],  type="response")>=0.05,"1","0"))
caret::confusionMatrix(class.pred.1mo.a.05, as.factor(holdout.quart.1mo$sb_1mo), positive="1")

class.pred.6mo.a.05  = as.factor(ifelse(stats::predict(LR.6mo.a, holdout.quart.6mo[ ,c(1:60,62:210)],  type="response")>=0.05,"1","0"))
caret::confusionMatrix(class.pred.6mo.a.05, as.factor(holdout.quart.6mo$sb_6mo), positive="1")

class.pred.12mo.a.05 = as.factor(ifelse(stats::predict(LR.12mo.a, holdout.quart.12mo[ ,c(1:60,62:210)], type="response")>=0.05,"1","0"))
caret::confusionMatrix(class.pred.12mo.a.05, as.factor(holdout.quart.12mo$sb_12mo), positive="1")

#Change cutoff to 10%
class.pred.1mo.a.10  = as.factor(ifelse(stats::predict(LR.1mo.a,  holdout.quart.1mo[ ,c(1:60,62:210)],  type="response")>=0.10,"1","0"))
caret::confusionMatrix(class.pred.1mo.a.10, as.factor(holdout.quart.1mo$sb_1mo), positive="1", mode="everything")

class.pred.6mo.a.10  = as.factor(ifelse(stats::predict(LR.6mo.a, holdout.quart.6mo[ ,c(1:60,62:210)],  type="response")>=0.10,"1","0"))
caret::confusionMatrix(class.pred.6mo.a.10, as.factor(holdout.quart.6mo$sb_6mo), positive="1", mode="everything")

class.pred.12mo.a.10 = as.factor(ifelse(stats::predict(LR.12mo.a, holdout.quart.12mo[ ,c(1:60,62:210)], type="response")>=0.10,"1","0"))
caret::confusionMatrix(class.pred.12mo.a.10, as.factor(holdout.quart.12mo$sb_12mo), positive="1", mode="everything")

#Change cutoff to 25%
class.pred.1mo.a.25  = as.factor(ifelse(stats::predict(LR.1mo.a,  holdout.quart.1mo[ ,c(1:60,62:210)],  type="response")>=0.25,"1","0"))
caret::confusionMatrix(class.pred.1mo.a.25, as.factor(holdout.quart.1mo$sb_1mo), positive="1", mode="everything")

class.pred.6mo.a.25  = as.factor(ifelse(stats::predict(LR.6mo.a, holdout.quart.6mo[ ,c(1:60,62:210)],  type="response")>=0.25,"1","0"))
caret::confusionMatrix(class.pred.6mo.a.25, as.factor(holdout.quart.6mo$sb_6mo), positive="1", mode="everything")

class.pred.12mo.a.25 = as.factor(ifelse(stats::predict(LR.12mo.a, holdout.quart.12mo[ ,c(1:60,62:210)], type="response")>=0.25,"1","0"))
caret::confusionMatrix(class.pred.12mo.a.25, as.factor(holdout.quart.12mo$sb_12mo), positive="1", mode="everything")

#ROC
library(pROC)
#AUC
#un-oversampled holdout sample
roc.LR.1mo = roc(holdout.quart.1mo$sb_1mo, predict.1mo, ordered=TRUE)
auc(roc.LR.1mo)
ci.auc(roc.LR.1mo, conf.level=0.95, method="bootstrap", boot.n=10000)

roc.LR.6mo = roc(holdout.quart.6mo$sb_6mo, predict.6mo, ordered=TRUE)
auc(roc.LR.6mo)
ci.auc(roc.LR.6mo, conf.level=0.95, method="bootstrap", boot.n=10000)

roc.LR.12mo = roc(holdout.quart.12mo$sb_12mo, predict.12mo, ordered=TRUE)
auc(roc.LR.12mo)
ci.auc(roc.LR.12mo, conf.level=0.95, method="bootstrap", boot.n=10000)

  #partial AUC
auc(roc.LR.1mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc.LR.6mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc.LR.12mo, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)


#A priori
#un-oversampled holdout sample
roc.LR.1mo.a = roc(holdout.quart.1mo$sb_1mo, predict.1mo.a, ordered=TRUE)
auc(roc.LR.1mo.a)
ci.auc(roc.LR.1mo.a, conf.level=0.95, method="bootstrap", boot.n=10000)

roc.LR.6mo.a = roc(holdout.quart.6mo$sb_6mo, predict.6mo.a, ordered=TRUE)
auc(roc.LR.6mo.a)
ci.auc(roc.LR.6mo.a, conf.level=0.95, method="bootstrap", boot.n=10000)

roc.LR.12mo.a = roc(holdout.quart.12mo$sb_12mo, predict.12mo.a, ordered=TRUE)
auc(roc.LR.12mo.a)
ci.auc(roc.LR.12mo.a, conf.level=0.95, method="bootstrap", boot.n=10000)

  #partial AUC
auc(roc.LR.1mo.a, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc.LR.6mo.a, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)
auc(roc.LR.12mo.a, partial.auc=c(1,0.8), partial.auc.focus="sens", partial.auc.correct=TRUE)


#PRAUC
library(PRROC)

#un-oversampled holdout sample
prob_yes_LR_1mo =  class.predict.1mo[holdout.quart.1mo$sb_1mo==1]
prob_no_LR_1mo  =  class.predict.1mo[holdout.quart.1mo$sb_1mo==0]

prauc.LR.1mo = pr.curve(scores.class0 = prob_yes_LR_1mo, scores.class1=prob_no_LR_1mo, curve=T)
print(prauc.LR.1mo)
plot(prauc.LR.1mo)

prob_yes_LR_6mo =  class.predict.6mo[holdout.quart.6mo$sb_6mo==1]
prob_no_LR_6mo  =  class.predict.6mo[holdout.quart.6mo$sb_6mo==0]

prauc.LR.6mo = pr.curve(scores.class0 = prob_yes_LR_6mo, scores.class1=prob_no_LR_6mo, curve=T)
print(prauc.LR.6mo)
plot(prauc.LR.6mo)

prob_yes_LR_12mo =  class.predict.12mo[holdout.quart.12mo$sb_12mo==1]
prob_no_LR_12mo  =  class.predict.12mo[holdout.quart.12mo$sb_12mo==0]

prauc.LR.12mo = pr.curve(scores.class0 = prob_yes_LR_12mo, scores.class1=prob_no_LR_12mo, curve=T)
print(prauc.LR.12mo)
plot(prauc.LR.12mo)

#A priori
#un-oversampled holdout sample
prob_yes_LR_1mo.a =  class.predict.1mo.a[holdout.quart.1mo$sb_1mo==1]
prob_no_LR_1mo.a  =  class.predict.1mo.a[holdout.quart.1mo$sb_1mo==0]

prauc.LR.1mo.a = pr.curve(scores.class0 = prob_yes_LR_1mo.a, scores.class1=prob_no_LR_1mo.a, curve=T)
print(prauc.LR.1mo.a)
plot(prauc.LR.1mo.a)

prob_yes_LR_6mo.a =  class.predict.6mo.a[holdout.quart.6mo$sb_6mo==1]
prob_no_LR_6mo.a  =  class.predict.6mo.a[holdout.quart.6mo$sb_6mo==0]

prauc.LR.6mo.a = pr.curve(scores.class0 = prob_yes_LR_6mo.a, scores.class1=prob_no_LR_6mo.a, curve=T)
print(prauc.LR.6mo.a)
plot(prauc.LR.6mo.a)

prob_yes_LR_12mo.a =  class.predict.12mo.a[holdout.quart.12mo$sb_12mo==1]
prob_no_LR_12mo.a  =  class.predict.12mo.a[holdout.quart.12mo$sb_12mo==0]

prauc.LR.12mo.a = pr.curve(scores.class0 = prob_yes_LR_12mo.a, scores.class1=prob_no_LR_12mo.a, curve=T)
print(prauc.LR.12mo.a)
plot(prauc.LR.12mo.a)


#Brier score
negative = 0
positive = 1
#un-oversampled holdout sample
measures::Brier(predict.1mo, holdout.quart.1mo$sb_1mo, negative, positive)
measures::Brier(predict.6mo, holdout.quart.6mo$sb_6mo, negative, positive)
measures::Brier(predict.12mo, holdout.quart.12mo$sb_12mo, negative, positive)

#ICI
library(gmish)
ici.LR.1mo = gmish::ici(predict.1mo, holdout.quart.1mo$sb_1mo)
ici.LR.1mo

ici.LR.6mo = gmish::ici(predict.6mo, holdout.quart.6mo$sb_6mo)
ici.LR.6mo

ici.LR.12mo = gmish::ici(predict.12mo, holdout.quart.12mo$sb_12mo)
ici.LR.12mo

#Net benefit
p_t = 0.1
nb.LR.1mo = gmish::nb(predict.1mo, holdout.quart.1mo$sb_1mo, p_t) 
nb.LR.1mo

nb.LR.6mo = gmish::nb(predict.6mo, holdout.quart.6mo$sb_6mo, p_t) 
nb.LR.6mo

nb.LR.12mo = gmish::nb(predict.12mo, holdout.quart.12mo$sb_12mo, p_t) 
nb.LR.12mo

#A priori
#un-oversampled holdout sample
measures::Brier(predict.1mo.a, holdout.quart.1mo$sb_1mo, negative, positive)
measures::Brier(predict.6mo.a, holdout.quart.6mo$sb_6mo, negative, positive)
measures::Brier(predict.12mo.a, holdout.quart.12mo$sb_12mo, negative, positive)

#ICI
library(gmish)
ici.LR.1mo.a = gmish::ici(predict.1mo.a, holdout.quart.1mo$sb_1mo)
ici.LR.1mo.a

ici.LR.6mo.a = gmish::ici(predict.6mo.a, holdout.quart.6mo$sb_6mo)
ici.LR.6mo.a

ici.LR.12mo.a = gmish::ici(predict.12mo.a, holdout.quart.12mo$sb_12mo)
ici.LR.12mo.a

#Net benefit
p_t = 0.10
nb.LR.1mo.a = gmish::nb(predict.1mo.a, holdout.quart.1mo$sb_1mo, p_t) 
nb.LR.1mo.a

nb.LR.6mo.a = gmish::nb(predict.6mo.a, holdout.quart.6mo$sb_6mo, p_t) 
nb.LR.6mo.a

nb.LR.12mo.a = gmish::nb(predict.12mo.a, holdout.quart.12mo$sb_12mo, p_t) 
nb.LR.12mo.a

#Save models
#Linear
saveRDS(LR.1mo, "LR.1mo.rds")
saveRDS(LR.6mo,   "LR.6mo.rds")
saveRDS(LR.12mo,  "LR.12mo.rds")

saveRDS(LR.1mo.a, "LR.1mo.a.rds")
saveRDS(LR.6mo.a,  "LR.6mo.a.rds")
saveRDS(LR.12mo.a, "LR.12mo.a.rds")

#Removes datasets, values, functions
rm(LR.1mo, LR.1mo.a, LR.6mo, LR.6mo.a, LR.12mo, LR.12mo.a,
   prauc.full.1mo, prauc.full.1mo.a, prauc.full.6mo, prauc.full.6mo.a, prauc.full.12mo, prauc.full.12mo.a,
   results.1mo, results.6mo, results.12mo,
   roc.full.1mo, roc.full.1mo.a, roc.full.6mo, roc.full.6mo.a, roc.full.12mo, roc.full.12mo.a,
   top7.1mo, top7.6mo, top7.12mo)
rm(class.predict.1mo, class.predict.1mo.a, class.predict.6mo, class.predict.6mo.a, class.predict.12mo, class.predict.12mo.a,
   predict.1mo, predict.1mo.a, predict.6mo, predict.6mo.a, predict.12mo, predict.12mo.a,
   prob_no_full, prob_no_full_1mo, prob_no_full_1mo.a, prob_no_full_6mo, prob_no_full_6mo.a, prob_no_full_12mo, prob_no_full_12mo.a,
   prob_yes_full, prob_yes_full_1mo, prob_yes_full_1mo.a, prob_yes_full_6mo, prob_yes_full_6mo.a, prob_yes_full_12mo, prob_yes_full_12mo.a)
rm(univariateGLM.1mo, univariateGLM.6mo, univariateGLM.12mo)
rm(prauc.LR.1mo, prauc.LR.1mo.a, prauc.LR.6mo, prauc.LR.6mo.a, prauc.LR.12mo, prauc.LR.12mo.a, roc.LR.1mo, roc.LR.1mo.a,
   roc.LR.6mo, roc.LR.6mo.a, roc.LR.12mo, roc.LR.12mo.a, prob_no_LR_1mo, prob_no_LR_1mo.a, prob_no_LR_6mo, prob_no_LR_6mo.a,
   prob_no_LR_12mo, prob_no_LR_12mo.a, prob_yes_LR_1mo, prob_yes_LR_1mo.a, prob_yes_LR_6mo, prob_yes_LR_6mo.a, prob_yes_LR_12mo,
   prob_yes_LR_12mo.a)
rm(y.hat.LR.1mo.r, y.hat.LR.6mo.r, y.hat.LR.12mo.r)