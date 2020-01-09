library(mlr)
library(DMwR)
library(rpart.plot)
library(FSelector)

#------------------------------------
# Handling imbalanced Data with SMOTE
#------------------------------------

# remove IDs and Opt In Data protection regulations
d18_learning = d18_imputed[,-c(1,11)]
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

# Check feature importance
generateFilterValuesData(task, method = c("information.gain","chi.squared", "gain.ratio")) %>% plotFilterValues()

# 10-fold Cross-Validation
rdesc = makeResampleDesc("CV",iters=10)
rinst = makeResampleInstance(rdesc,task)

# Tune Decision Tree Learner
grid = makeTuneControlGrid()
getParamSet(learner_dt)
params = makeParamSet(
  # min number of observations in node for split attempt (default 20)
  makeIntegerParam("minsplit",lower=1,upper=50),
  # complexity of the tree, 0 = high, 1 = no tree (default 0.01)
  makeNumericParam("cp",lower=0,upper=0.05),
  # max depth of tree (default 30)
  makeIntegerParam("maxdepth",lower=10,upper=50)
)
# note: next line takes about 7 minutes on a decent pc to execute (500 iterations)
dt_tuned = tuneParams(learner_dt,task,rdesc,control=grid,par.set=params,measures=list(mmce,tpr,tnr))
learner_dt <- setHyperPars(learner_dt,par.vals=dt_tuned$x)

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
model_lr = mlr::train(learner_lr,task)
#Decision Tree
model_dt = mlr::train(learner_dt,task)


#--------------------------------
# Interpret and Visualize results
#--------------------------------

# Get coefficients from LogReg model
model_lr$learner.model

# Decision Tree Plot
rpart.plot(getLearnerModel(model_dt))


### TODO
# 1: visualization (https://christophm.github.io/interpretable-ml-book/logistic.html)