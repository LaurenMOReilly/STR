# PROGRAM: STR_DataPreprocess_ML.R
# STUDY: STR - Short-Term Risk for SB 
# DESCRIPTION: Read in cleaned SAS file, split, final preprocessing, ROSE
# INPUTS: DATA (see STR_Initialization.R file)
# OUTPUTS: train.ROSE.1mo.data, train.ROSE.6mo.data, train.ROSE.12mo, holdout.quart.1mo, holdout.quart.6mo, holdout.quart.12mo
# DATE CREATED: 2021-09-10
# COMPLETED:
# AUTHOR: LMO ( loreilly at iu dot edu ) 
# NOTE: 1mo outcome is not included in manuscript; main outcome=12mo, sensitivity analysis=6mo

###DATA SEPARATION###
#Split 70/30
#Use personal LOPNR as the ID factor so that people will not be separated
#To ensure stratified sampling, specify outcome as num_col  
#Set set
set.seed(SPLITSEED)

#Split dataset for each outcome
data.split.1mo  = partition(DATA, c(TRAINSIZE, TESTSIZE), id_col = VAR_PARTITION_ID, num_col = OUTCOME_1MO) 
data.split.6mo  = partition(DATA, c(TRAINSIZE, TESTSIZE), id_col = VAR_PARTITION_ID, num_col = OUTCOME_6MO) 
data.split.12mo = partition(DATA, c(TRAINSIZE, TESTSIZE), id_col = VAR_PARTITION_ID, num_col = OUTCOME_12MO) 

#Assign partitions to either test or train set for each outcome
  #1 month
train.set.1mo = data.split.1mo[[1]]
nrow(train.set.1mo)
table(train.set.1mo$sb_1mo)

holdout.set.1mo = data.split.1mo[[2]]
nrow(holdout.set.1mo)
table(holdout.set.1mo$sb_1mo)

  #6 month
train.set.6mo = data.split.6mo[[1]] 
nrow(train.set.6mo)
table(train.set.6mo$sb_6mo)

holdout.set.6mo = data.split.6mo[[2]] 
nrow(holdout.set.6mo)
table(holdout.set.6mo$sb_6mo)  

  #12 month
train.set.12mo = data.split.12mo[[1]] 
nrow(train.set.12mo)
table(train.set.12mo$sb_12mo)

holdout.set.12mo = data.split.12mo[[2]] 
nrow(holdout.set.12mo)
table(holdout.set.12mo$sb_12mo)  

#Set datasets to dataframes
  # 1month
train.set.1mo    = as.data.frame(train.set.1mo)
holdout.set.1mo  = as.data.frame(holdout.set.1mo)
  #6 months
train.set.6mo    = as.data.frame(train.set.6mo)
holdout.set.6mo  = as.data.frame(holdout.set.6mo)
  #12 months
train.set.12mo   = as.data.frame(train.set.12mo)
holdout.set.12mo = as.data.frame(holdout.set.12mo)

#Check if the proportion is even between the outcomes, cohort is also equally distributed
prop.table(table(train.set.1mo[, OUTCOME_1MO]))
prop.table(table(holdout.set.1mo[, OUTCOME_1MO]))

prop.table(table(train.set.6mo[, OUTCOME_6MO]))
prop.table(table(holdout.set.6mo[, OUTCOME_6MO]))

prop.table(table(train.set.12mo[, OUTCOME_12MO]))
prop.table(table(holdout.set.12mo[, OUTCOME_12MO]))

###DATA TRANSFORMATION FOR ML###
set.seed(SCALESEED)
#Normalize continuous variables
  #1) Age and neighborhood deprivation: Rescale range to 0-1
    #1 month
train.set.1mo[, c(1,191)]   = lapply(train.set.1mo[, c(1,191)],
                                     function(x) rescale(x, to = c(0,1))
)
holdout.set.1mo[, c(1,191)] = lapply(holdout.set.1mo[, c(1,191)],
                                     function(x) rescale(x, to = c(0,1))
)
    #6 months
train.set.6mo[, c(1,191)]   = lapply(train.set.6mo[, c(1,191)],
                                     function(x) rescale(x, to = c(0,1))
)
holdout.set.6mo[, c(1,191)] = lapply(holdout.set.6mo[, c(1,191)],
                                     function(x) rescale(x, to = c(0,1))
)

    #12 months
train.set.12mo[, c(1,191)]   = lapply(train.set.12mo[, c(1,191)],
                                      function(x) rescale(x, to = c(0,1))
)
holdout.set.12mo[, c(1,191)] = lapply(holdout.set.12mo[, c(1,191)],
                                      function(x) rescale(x, to = c(0,1))
)

  #2) Quartile number of outpatient visits, total treatment days in tx period
    #1 month
train.quart.1mo = train.set.1mo %>%
  mutate(tot1q=ntile(tot1, 4),
         tot4q=ntile(tot4, 4))
holdout.quart.1mo = holdout.set.1mo %>%
  mutate(tot1q=ntile(tot1, 4),
         tot4q=ntile(tot4, 4))
    #6 months
train.quart.6mo = train.set.6mo %>% 
  mutate(tot1q=ntile(tot1, 4),
         tot4q=ntile(tot4, 4))
holdout.quart.6mo = holdout.set.6mo %>% 
  mutate(tot1q=ntile(tot1, 4),
         tot4q=ntile(tot4, 4))
    #12 months
train.quart.12mo = train.set.12mo %>% 
  mutate(tot1q=ntile(tot1, 4),
         tot4q=ntile(tot4, 4))
holdout.quart.12mo = holdout.set.12mo %>% 
  mutate(tot1q=ntile(tot1, 4),
         tot4q=ntile(tot4, 4))

table (train.quart.1mo$tot1q)
table (holdout.quart.1mo$tot4q)
table (train.quart.6mo$tot1q)
table (holdout.quart.6mo$tot4q)
table (train.quart.12mo$tot1q)
table (holdout.quart.12mo$tot4q)

#Dummy code quartiles - train
  #tot1: total number of outpatient tx visits
  #tot4: total number of days in tx periods
    #1 month
#train.quart.1mo$tot1q1 = ifelse(train.quart.1mo$tot1q=='1', 1, 0)
train.quart.1mo$tot1q2 = ifelse(train.quart.1mo$tot1q=='2', 1, 0)
train.quart.1mo$tot1q3 = ifelse(train.quart.1mo$tot1q=='3', 1, 0)
train.quart.1mo$tot1q4 = ifelse(train.quart.1mo$tot1q=='4', 1, 0)
#train.quart.1mo$tot4q1 =  ifelse(train.quart.1mo$tot4q=='1', 1, 0)
train.quart.1mo$tot4q2 = ifelse(train.quart.1mo$tot4q=='2', 1, 0)
train.quart.1mo$tot4q3 = ifelse(train.quart.1mo$tot4q=='3', 1, 0)
train.quart.1mo$tot4q4 = ifelse(train.quart.1mo$tot4q=='4', 1, 0)
    #6 months
#train.quart.6mo$tot1q1 = ifelse(train.quart.6mo$tot1q=='1', 1, 0) 
train.quart.6mo$tot1q2 = ifelse(train.quart.6mo$tot1q=='2', 1, 0)   
train.quart.6mo$tot1q3 = ifelse(train.quart.6mo$tot1q=='3', 1, 0)   
train.quart.6mo$tot1q4 = ifelse(train.quart.6mo$tot1q=='4', 1, 0) 
#train.quart.6mo$tot4q1 =  ifelse(train.quart.6mo$tot4q=='1', 1, 0) 
train.quart.6mo$tot4q2 = ifelse(train.quart.6mo$tot4q=='2', 1, 0)   
train.quart.6mo$tot4q3 = ifelse(train.quart.6mo$tot4q=='3', 1, 0)   
train.quart.6mo$tot4q4 = ifelse(train.quart.6mo$tot4q=='4', 1, 0) 
    #12 months
#train.quart.12mo$tot1q1 = ifelse(train.quart.12mo$tot1q=='1', 1, 0) 
train.quart.12mo$tot1q2 = ifelse(train.quart.12mo$tot1q=='2', 1, 0)   
train.quart.12mo$tot1q3 = ifelse(train.quart.12mo$tot1q=='3', 1, 0)   
train.quart.12mo$tot1q4 = ifelse(train.quart.12mo$tot1q=='4', 1, 0) 
#train.quart.12mo$tot4q1 =  ifelse(train.quart.12mo$tot4q=='1', 1, 0) 
train.quart.12mo$tot4q2 = ifelse(train.quart.12mo$tot4q=='2', 1, 0)   
train.quart.12mo$tot4q3 = ifelse(train.quart.12mo$tot4q=='3', 1, 0)   
train.quart.12mo$tot4q4 = ifelse(train.quart.12mo$tot4q=='4', 1, 0) 

#Dummy code quartiles - holdout
  #tot1: total number of outpatient tx visits
  #tot4: total number of days in tx periods
    #1 month
#holdout.quart.1mo$tot1q1 = ifelse(holdout.quart.1mo$tot1q=='1', 1, 0)
holdout.quart.1mo$tot1q2 = ifelse(holdout.quart.1mo$tot1q=='2', 1, 0)
holdout.quart.1mo$tot1q3 = ifelse(holdout.quart.1mo$tot1q=='3', 1, 0)
holdout.quart.1mo$tot1q4 = ifelse(holdout.quart.1mo$tot1q=='4', 1, 0)
#holdout.quart.1mo$tot4q1 =  ifelse(holdout.quart.1mo$tot4q=='1', 1, 0)
holdout.quart.1mo$tot4q2 = ifelse(holdout.quart.1mo$tot4q=='2', 1, 0)
holdout.quart.1mo$tot4q3 = ifelse(holdout.quart.1mo$tot4q=='3', 1, 0)
holdout.quart.1mo$tot4q4 = ifelse(holdout.quart.1mo$tot4q=='4', 1, 0)
    #6 months
#holdout.quart.6mo$tot1q1 = ifelse(holdout.quart.6mo$tot1q=='1', 1, 0) 
holdout.quart.6mo$tot1q2 = ifelse(holdout.quart.6mo$tot1q=='2', 1, 0)   
holdout.quart.6mo$tot1q3 = ifelse(holdout.quart.6mo$tot1q=='3', 1, 0)   
holdout.quart.6mo$tot1q4 = ifelse(holdout.quart.6mo$tot1q=='4', 1, 0) 
#holdout.quart.6mo$tot4q1 =  ifelse(holdout.quart.6mo$tot4q=='1', 1, 0) 
holdout.quart.6mo$tot4q2 = ifelse(holdout.quart.6mo$tot4q=='2', 1, 0)   
holdout.quart.6mo$tot4q3 = ifelse(holdout.quart.6mo$tot4q=='3', 1, 0)   
holdout.quart.6mo$tot4q4 = ifelse(holdout.quart.6mo$tot4q=='4', 1, 0) 
    #12 months
#holdout.quart.12mo$tot1q1 = ifelse(holdout.quart.12mo$tot1q=='1', 1, 0) 
holdout.quart.12mo$tot1q2 = ifelse(holdout.quart.12mo$tot1q=='2', 1, 0)   
holdout.quart.12mo$tot1q3 = ifelse(holdout.quart.12mo$tot1q=='3', 1, 0)   
holdout.quart.12mo$tot1q4 = ifelse(holdout.quart.12mo$tot1q=='4', 1, 0) 
#holdout.quart.12mo$tot4q1 =  ifelse(holdout.quart.12mo$tot4q=='1', 1, 0) 
holdout.quart.12mo$tot4q2 = ifelse(holdout.quart.12mo$tot4q=='2', 1, 0)   
holdout.quart.12mo$tot4q3 = ifelse(holdout.quart.12mo$tot4q=='3', 1, 0)   
holdout.quart.12mo$tot4q4 = ifelse(holdout.quart.12mo$tot4q=='4', 1, 0) 

#Drop variables not needed for subsequent analyses (LOPNR, tot1, tot1q, tot4, tot4q)
  #1 month
train.quart.1mo =   subset(train.quart.1mo, select = -c(LOPNR,tot1,tot1q,tot4,tot4q))
holdout.quart.1mo = subset(holdout.quart.1mo, select = -c(LOPNR,tot1,tot1q,tot4,tot4q))
  #6 month
train.quart.6mo =   subset(train.quart.6mo, select = -c(LOPNR,tot1,tot1q,tot4,tot4q))
holdout.quart.6mo = subset(holdout.quart.6mo, select = -c(LOPNR,tot1,tot1q,tot4,tot4q))
  #12 month
train.quart.12mo =   subset(train.quart.12mo, select = -c(LOPNR,tot1,tot1q,tot4,tot4q))
holdout.quart.12mo = subset(holdout.quart.12mo, select = -c(LOPNR,tot1,tot1q,tot4,tot4q))

#Table outcomes
table(train.quart.1mo$sb_1mo)
table(holdout.quart.1mo$sb_1mo)
table(train.quart.1mo$sb_6mo)
table(train.quart.1mo$sb_12mo)

###ROSE Oversampling###
#https://journal.r-project.org/archive/2014/RJ-2014-008/RJ-2014-008.pdf

  #1 month
train.ROSE.1mo = ROSE::ovun.sample(sb_1mo ~ .
                                  , train.quart.1mo[ , -c(62:65)]
                                  , method = "over"
                                  , p=0.4
                                  , seed = ROSESEED
)

  #6 months
train.ROSE.6mo = ROSE::ovun.sample(sb_6mo ~ .
                                   , train.quart.6mo[ , -c(61,63:65)]
                                   , method = "over"
                                   , p=0.4
                                   , seed = ROSESEED
)


  #12 months
train.ROSE.12mo = ROSE::ovun.sample(sb_12mo ~ .
                                    , train.quart.12mo[ , -c(61,62,64,65)]
                                    , method = "over"
                                    , p=0.4
                                    , seed = ROSESEED
)


#Assign synthetic dataset
train.ROSE.1mo.data    = train.ROSE.1mo$data

train.ROSE.6mo.data    = train.ROSE.6mo$data

train.ROSE.12mo.data   = train.ROSE.12mo$data

#Examine distribution of synthetic outcome
table(train.ROSE.1mo.data$sb_1mo)
table(train.ROSE.6mo.data$sb_6mo)
table(train.ROSE.12mo.data$sb_12mo)

#Remove datasets not needed in global environment
rm(data.split.1mo, data.split.6mo, data.split.12mo, 
   train.set.1mo, train.set.6mo, train.set.12mo,
   train.quart.1mo, train.quart.6mo, train.quart.12mo,
   holdout.set.1mo, holdout.set.6mo, holdout.set.12mo,
   train.ROSE.1mo, train.ROSE.6mo, train.ROSE.12mo
)

#Output dataset to set working directory
save(train.ROSE.1mo.data, file="train.ROSE.1mo.data.Rdata")
save(train.ROSE.6mo.data, file="train.ROSE.6mo.data.Rdata")
save(train.ROSE.12mo.data, file="train.ROSE.12mo.data.Rdata")

#Drop out variables in holdout sample to prepare to holdout analyses in subsequent datafiles
holdout.quart.1mo = holdout.quart.1mo[ , -c(62:65)]
holdout.quart.6mo = holdout.quart.6mo[ , -c(61,63:65)]
holdout.quart.12mo = holdout.quart.12mo[ , -c(61,62,64,65)]

#Save holdout files (use in future analyses)
save(holdout.quart.1mo, file='holdout.quart.1mo.Rdata')
save(holdout.quart.6mo, file='holdout.quart.6mo.Rdata')
save(holdout.quart.12mo, file='holdout.quart.12mo.Rdata')

table(train.ROSE.1mo.data$sb_1mo)
table(train.ROSE.6mo.data$sb_6mo)
table(train.ROSE.12mo.data$sb_12mo)
