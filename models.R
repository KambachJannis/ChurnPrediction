library(mlr)
library(DMwR)

#------------------------------------
# Handling imbalanced Data with SMOTE
#------------------------------------

d18_learning = d18_imputed[,-c(1,14)]
d18_learning$Flag_cancellation = as.factor(d18_learning$Flag_cancellation)
d18_balanced <- SMOTE(Flag_cancellation ~ ., d18_learning, perc.over=300,perc.under=1000)

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

res = resample(learner,task,rinst,measures=list(mmce,tpr,tnr))
