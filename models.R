library(mlr)
library(DMwR)

#------------------------------------
# Handling imbalanced Data with SMOTE
#------------------------------------

d18_learning = d18_imputed[,-c(1,14)]
d18_balanced <- SMOTE(Flag_cancellation ~ ., d18_learning, perc.over=200,perc.under=300)

#--------------------------
# Create Tasks and Learners
#--------------------------

# Logistic Regression
task = makeClassifTask(id="churn", data=d18_balanced, target="Flag_cancellation")
learner_lr = makeLearner("classif.logreg")
learner_dt = makeLearner("classif.rpart")

#--------------------------------
# Test learner/task configuration
#--------------------------------

#LogReg with 10-fold Cross-Validation
rdesc = makeResampleDesc("CV", iters=10)
rinst = makeResampleInstance(rdesc,task)
res_lr = resample(learner_lr,task,rinst,measures=list(mmce,tpr,tnr),models=T)
res_dt = resample(learner_dt,task,rinst,measures=list(mmce,tpr,tnr),models=T)
res_lr$models[[1]]$learner.model

#------------
# Train model
#------------

#Logistic Regression
model_lr = train(learner_lr,task_lr)

#--------------------------------
# Interpret and Visualize results -> on training or test data? or both?
#--------------------------------

# Get coefficients from LogReg model
model_lr$learner.model


### TODO
# 1: add decision tree (incl. hyperparameter tuning)
# 2: maybe SMOTE optimization? -> needs to be after splitting training and test data
# 3: visualization (https://christophm.github.io/interpretable-ml-book/logistic.html)
# 4: prediction