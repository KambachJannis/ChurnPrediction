# Customer Churn Prediction

This our group's solution to CRM class project to create a customer churn prediction model in order to run an efficient targeted marketing campaign.
The final model is a decision tree in order to maintain model interpretability, since this is a consulting case project.

## Requirements

* mlr
* dplyr
* tidyr
* ggplot2
* lubridate
* mice
* corrr
* caret

## Usage
The scripts need to be executed in the following order:
- preprocessing
- models
- prediction

d18_imputed is required for the models-script to work and model_dt is required for the prediction-script to work.

## Details

The preprocessing step takes care of most flaws in the data, including: syntactic & semantic preparations, deduplication, one-hot-encoding, missing value imputation and SMOTe in order to account for the class imbalance.