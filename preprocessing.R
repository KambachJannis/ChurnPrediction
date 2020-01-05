library(lubridate)
library(dplyr)
library(mice)
library(corrr)
library(caret)

# Load data
d18 = read.csv2(file="Data_set_Jan18.csv", stringsAsFactors = FALSE)
d19 = read.csv2(file="Data_set_Jan19.csv", stringsAsFactors = FALSE)

# Rename first column because of unicode error
colnames(d18)[1] = "ID"
colnames(d19)[1] = "ID"

#------------------------------
# Syntactic preparation of data 
#------------------------------

# Set all missing values to NA
d18[d18=="-"] = NA
d19[d19=="-"] = NA

# Convert numeric values from strings
d18$Age = as.numeric(d18$Age)
d18$Number_contractual_relationships = as.numeric(d18$Number_contractual_relationships)
d18$Customer_for_years = as.numeric(d18$Customer_for_years)
d18$Credit_rating_score = as.numeric(d18$Credit_rating_score)
d18$Open_item_amount = as.numeric(sub(",",".",d18$Open_item_amount,fixed=T))
d18$Factor_subsequent_payment = as.numeric(sub(",",".",d18$Factor_subsequent_payment,fixed=T))

# Convert boolean values from strings
d18$Flag_e.mail_deposited = as.factor(d18$Flag_e.mail_deposited)
d18$Flag_mobile_deposited = as.factor(d18$Flag_mobile_deposited)
d18$Flag_advertising_permission_e.mail = as.factor(d18$Flag_advertising_permission_e.mail)
d18$Flag_advertising_permission_post = as.factor(d18$Flag_advertising_permission_post)
d18$Flag_advertising_permission_telephone = as.factor(d18$Flag_advertising_permission_telephone)
d18$OptIn_data_protection_regulations = as.factor(d18$OptIn_data_protection_regulations)
d18$OptIn_newsletter = as.factor(d18$OptIn_newsletter)

# Prepare date values
d18$Minimum_contract_term = as.Date(d18$Minimum_contract_term, format="%d.%m.%Y")
d18$Maximum_contract_term = as.Date(d18$Minimum_contract_term, format="%d.%m.%Y")
d18$Vorteilswelt_customer_since = as.Date(d18$Vorteilswelt_customer_since, format="%d.%m.%Y")

# Set open item amount to 0 if NA
d18[which(is.na(d18$Open_item_amount)),]$Open_item_amount = 0

# Remove 1 column where ID is NA
d18 = d18[-which(is.na(d18$ID)),]

#-----------------------------
# Semantic preparation of data
#-----------------------------

# Transform contract duration dates into days left based on time of dataset
basedate = as.Date("01.02.2018","%d.%m.%Y")
d18$Minimum_contract_duration = as.numeric(d18$Minimum_contract_term - basedate, "days")
d18$Maximum_contract_duration = as.numeric(d18$Maximum_contract_term - basedate, "days")

# Transform Vorteilswelt signup date into membership duration based on current date
basedate2 = as.Date("01.01.2020", "%d.%m.%Y")
d18$Vorteilswelt_customer_duraction = as.numeric(basedate2 - d18$Vorteilswelt_customer_since, "days")

# Merge client type and title into single 4-level factor
for(i in 1:nrow(d18)){
  if(d18$Client_type[i]=="N" & !is.na(d18$Title[i])){
    if(d18$Title[i]=="Frau"){
      d18$Client_type[i] = "F"
    }else if(d18$Title[i]=="Herrn"){
      d18$Client_type[i] = "M"
    }
  }
}
d18$Client_type = as.factor(d18$Client_type)

#------------------------------------------
# Remove columns with redundant information
#------------------------------------------

# Title
d18 = d18[,-2]

# Credit rating traffic light
d18 = d18[,-11]

# Invoice shock
d18 = d18[,-20]

# Minimum/Maximum contract terms
d18 = d18[,-8]
d18 = d18[,-8]

# Vorteilswelt customer since
d18 = d18[,-16]

#------------------------
# Deal with duplicate IDs
#------------------------

# If credit score is different, set highest score in all rows
n_occur = data.frame(table(unique(d18[,-8])$ID))
duplicates = n_occur[n_occur$Freq > 1,]
for(i in 1:nrow(duplicates)){
  tempIDs = which(d18$ID == as.character(duplicates[i,]$Var1))
  temp = max(d18[tempIDs,]$Credit_rating_score)
  d18[tempIDs,]$Credit_rating_score = temp
}

# Sum up open item amount of duplicate IDs
d18 = d18 %>% group_by(ID) %>% summarise(Client_type=first(Client_type), Age=first(Age), Flag_e.mail_deposited=first(Flag_e.mail_deposited), Flag_mobile_deposited=first(Flag_mobile_deposited), 
                                         Number_contractual_relationships=first(Number_contractual_relationships), Customer_for_years=first(Customer_for_years), Credit_rating_score=first(Credit_rating_score), 
                                         Total_consumption=first(Total_consumption), Open_item_amount=sum(Open_item_amount), Flag_advertising_permission_e.mail=first(Flag_advertising_permission_e.mail), 
                                         Flag_advertising_permission_post=first(Flag_advertising_permission_post), Flag_advertising_permission_telephone=first(Flag_advertising_permission_telephone), 
                                         OptIn_data_protection_regulations=first(OptIn_data_protection_regulations), OptIn_newsletter=first(OptIn_newsletter), Factor_subsequent_payment=first(Factor_subsequent_payment),Flag_cancellation=first(Flag_cancellation), 
                                         Sum_contribution_margin_2=first(Sum_contribution_margin_2), Minimum_contract_duration=first(Minimum_contract_duration), Maximum_contract_duration=first(Maximum_contract_duration), 
                                         Vorteilswelt_customer_duraction=first(Vorteilswelt_customer_duraction))

#--------------------------
# Prepare data for training
#--------------------------

# One-hot-encoding to eleminiate multi-level factors
dummy = dummyVars(" ~ Client_type", data = d18)
d18_encoded = data.frame(predict(dummy, newdata = d18))
d18 = cbind(d18,d18_encoded)
# Remove original Client type
d18 = d18[,-2]

# !!!!!Remove maximum contract duration and rename minimum to "contract duration" (might need to be removed for prediction data)
d18 = d18[,-19]
colnames(d18)[18] = "Contract_duration"

# Missing value imputation
imputation = mice(d18[,-c(8,9,16)])
d18_imputed = complete(imputation)
d18_imputed = cbind(d18_imputed,d18[,c(8,9,16)])

# Move churn flag from 2019 to 2018 to have a target variable
#d19_churn = d19 %>% select(ID, Flag_cancellation)
#d19_churn = unique(d19_churn)
#colnames(d19_churn)[2] = "Target"
#d18_ready = merge(d18, d19_churn, by = "ID")

#------------------------------------------------------------------------------------------
#-------------------------------- Exploration ---------------------------------------------
#------------------------------------------------------------------------------------------

sum(is.na(d18$Age))
sum(is.na(d18$Credit_rating_score))
sum(is.na(d18$OptIn_data_protection_regulations))
sum(is.na(d18$OptIn_newsletter))
sum(is.na(d18$Factor_subsequent_payment))
sum(is.na(d18$Minimum_contract_duration))
sum(is.na(d18$Maximum_contract_duration))
sum(is.na(d18$Vorteilswelt_customer_duraction))

d18$Flag_e.mail_deposited = as.numeric(d18$Flag_e.mail_deposited)
d18$Flag_mobile_deposited = as.numeric(d18$Flag_mobile_deposited)
d18$Flag_advertising_permission_e.mail = as.numeric(d18$Flag_advertising_permission_e.mail)
d18$Flag_advertising_permission_post = as.numeric(d18$Flag_advertising_permission_post)
d18$Flag_advertising_permission_telephone = as.numeric(d18$Flag_advertising_permission_telephone)
d18$OptIn_data_protection_regulations = as.numeric(d18$OptIn_data_protection_regulations)
d18$OptIn_newsletter = as.numeric(d18$OptIn_newsletter)