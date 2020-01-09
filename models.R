library(mlr)
library(DMwR)
library(rpart.plot)

#------------------------------------
# Handling imbalanced Data with SMOTE
#------------------------------------

d18_learning = d18_imputed[,-c(1,14)]
d18_balanced <- SMOTE(Flag_cancellation ~ .,d18_learning,perc.over=200,perc.under=300)

#--------------------------
# Create Tasks and Learners
#--------------------------

# Classification Task
task = makeClassifTask(id="churn",data=d18_balanced,target="Flag_cancellation")

# Logistic Regression Learner
learner_lr = makeLearner("classif.logreg")
# Decision Tree Learner
learner_dt = makeLearner("classif.rpart")

#-----------------------
# Test and tune learners
#-----------------------

# 10-fold Cross-Validation
rdesc = makeResampleDesc("CV",iters=10)
rinst = makeResampleInstance(rdesc,task)

# Tune Decision Tree Learner
grid = makeTuneControlGrid()
getParamSet(learner_dt)
params = makeParamSet(
  makeDiscreteParam("cp",values=seq(0.001,0.006,0.002)),
  makeDiscreteParam("minsplit",values=c(1,5,10,50)),
  makeDiscreteParam("maxdepth",values=c(20,30,50)),
  makeDiscreteParam("parms",values=list(a=list(prior=c(.6,.4)),b=list(prior=c(.5,.5))))
)
params_new = makeParamSet(
  makeIntegerParam("minsplit",lower=0,upper=1),
  makeIntegerParam("minbucket",lower=0,upper=1),
  makeNumericParam("cp",lower=0,upper=1),
  makeIntegerParam("maxcompete",lower=0,upper=1),
  makeIntegerParam("maxdepth",lower=0,upper=1),
)
tuned = tuneParams(learner_dt,task,rdesc,control=grid,par.set=params,measures=list(mmce,tpr,tnr))

# Logistic Regression
res_lr = resample(learner_lr,task,rinst,measures=list(mmce,tpr,tnr),models=T)
# Decision Tree
res_dt = resample(learner_dt,task,rinst,measures=list(mmce,tpr,tnr),models=T)

# View models
res_lr$models[[1]]$learner.model
res_dt$models[[1]]$learner.model

#------------------
# Train final model
#------------------

#Logistic Regression
model_lr = train(learner_lr,task)
#Decision Tree
model_dt = train(learner_dt,task)


#--------------------------------
# Interpret and Visualize results -> on training or test data? or both?
#--------------------------------

# Get coefficients from LogReg model
model_lr$learner.model

# Decision Tree Plot
rpart.plot(getLearnerModel(model_dt))


### TODO
# 1: maybe SMOTE optimization? -> needs to be after splitting training and test data
# 2: visualization (https://christophm.github.io/interpretable-ml-book/logistic.html)
# 3: prediction