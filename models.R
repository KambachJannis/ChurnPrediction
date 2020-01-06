library(mlr)
library(DMwR)

#------------------------------------
# Handling imbalanced Data with SMOTE
#------------------------------------

d18_learning = d18_imputed[,-c(1,14)]
d18_balanced <- SMOTE(Flag_cancellation ~ ., d18_learning, perc.over=200,perc.under=300)

#------------------------
# Create Task and Learner
#------------------------

task = makeClassifTask(id="churn", data=d18_learning, target="Flag_cancellation")
task_SMOTE = makeClassifTask(id="churn", data=d18_balanced, target="Flag_cancellation")
learner = makeLearner("classif.logreg")

#-------------------
# Prepare resampling
#-------------------

rdesc = makeResampleDesc("CV", iters=10)
rinst = makeResampleInstance(rdesc,task)
rinst_SMOTE = makeResampleInstance(rdesc,task_SMOTE)

#------------
# Train model
#------------

res = resample(learner,task_SMOTE,rinst_SMOTE,measures=list(mmce,tpr,tnr),models=T)
res$models[[1]]$learner.model

### TODO
# 1: train one model on entire dataset
# 2: add decision tree (incl. hyperparameter tuning)
# 3: maybe SMOTE optimization? -> needs to be after splitting training and test data
# 4: visualization (https://christophm.github.io/interpretable-ml-book/logistic.html)
# 5: prediction