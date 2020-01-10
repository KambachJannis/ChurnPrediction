library(lubridate)
library(dplyr)
library(mice)
library(corrr)
library(caret)
set.seed(420)

# Load data
d19t = read.csv2(file="Data_set_Oct19.csv", stringsAsFactors = FALSE)

# Rename first column because of unicode error
colnames(d19)[1] = "ID"

#------------------------------
# Syntactic preparation of data 
#------------------------------

# Set all missing values to NA
d19[d19=="-"] = NA

# Convert numeric values from strings
d19$Age = as.numeric(d19$Age)
d19$Number_contractual_relationships = as.numeric(d19$Number_contractual_relationships)
d19$Customer_for_years = as.numeric(d19$Customer_for_years)
d19$Credit_rating_score = as.numeric(d19$Credit_rating_score)
d19$Open_item_amount = as.numeric(sub(",",".",d19$Open_item_amount,fixed=T))
d19$Factor_subsequent_payment = as.numeric(sub(",",".",d19$Factor_subsequent_payment,fixed=T))

# Convert boolean values from strings
d19$Flag_e.mail_deposited = as.factor(d19$Flag_e.mail_deposited)
d19$Flag_mobile_deposited = as.factor(d19$Flag_mobile_deposited)
d19$Flag_advertising_permission_e.mail = as.factor(d19$Flag_advertising_permission_e.mail)
d19$Flag_advertising_permission_post = as.factor(d19$Flag_advertising_permission_post)
d19$Flag_advertising_permission_telephone = as.factor(d19$Flag_advertising_permission_telephone)
d19$OptIn_data_protection_regulations = as.factor(d19$OptIn_data_protection_regulations)
d19$OptIn_newsletter = as.factor(d19$OptIn_newsletter)

# Prepare date values
d19$Minimum_contract_term = as.Date(d19$Minimum_contract_term, format="%d.%m.%Y")
d19$Maximum_contract_term = as.Date(d19$Maximum_contract_term, format="%d.%m.%Y")
d19$Vorteilswelt_customer_since = as.Date(d19$Vorteilswelt_customer_since, format="%d.%m.%Y")

# Set open item amount to 0 if NA
d19[which(is.na(d19$Open_item_amount)),]$Open_item_amount = 0

# Set OptIns to 0 when NA because they are Opt In
d19[which(is.na(d19$OptIn_data_protection_regulations)),]$OptIn_data_protection_regulations = 0
d19[which(is.na(d19$OptIn_newsletter)),]$OptIn_newsletter = 0

#-----------------------------
# Semantic preparation of data
#-----------------------------

# Transform contract duration dates into days left based on time of dataset
basedate = as.Date("01.01.2019","%d.%m.%Y")
d19$Minimum_contract_duration = as.numeric(d19$Minimum_contract_term - basedate, "days")
d19$Maximum_contract_duration = as.numeric(d19$Maximum_contract_term - basedate, "days")

# Use contract difference difference instead of maximum contract duration
d19$Contract_duration_difference = d19$Maximum_contract_duration - d19$Minimum_contract_duration
# Set contract duration difference to 0 if number of contracts is 1
d19[which(d19$Number_contractual_relationships==1),]$Contract_duration_difference = 0


# Transform Vorteilswelt signup date into membership duration based on current date
basedate2 = as.Date("01.01.2020", "%d.%m.%Y")
d19$Vorteilswelt_customer_duration = as.numeric(basedate2 - d19$Vorteilswelt_customer_since, "days")
# Set duration to 0 if NA
d19[which(is.na(d19$Vorteilswelt_customer_duration)),]$Vorteilswelt_customer_duration = 0

# Merge client type and title into single 4-level factor
for(i in 1:nrow(d19)){
  if(d19$Client_type[i]=="N" & !is.na(d19$Title[i])){
    if(d19$Title[i]=="Frau"){
      d19$Client_type[i] = "F"
    }else if(d19$Title[i]=="Herrn"){
      d19$Client_type[i] = "M"
    }
  }
}
d19$Client_type = as.factor(d19$Client_type)

#------------------------------------------
# Remove columns with redundant information
#------------------------------------------

# Title
d19 = d19[,-2]

# Credit rating traffic light
d19 = d19[,-11]

# Invoice shock
d19 = d19[,-20]

# Minimum/Maximum contract terms + Maximum contract duration
d19 = d19[,-8]
d19 = d19[,-8]
d19 = d19[,-20]

# Vorteilswelt customer since
d19 = d19[,-16]

#------------------------
# Deal with duplicate IDs
#------------------------

# If credit score is different, set highest score in all rows
n_occur = data.frame(table(unique(d19[,-8])$ID))
duplicates = n_occur[n_occur$Freq > 1,]
for(i in 1:nrow(duplicates)){
  tempIDs = which(d19$ID == as.character(duplicates[i,]$Var1))
  temp = max(d19[tempIDs,]$Credit_rating_score)
  d19[tempIDs,]$Credit_rating_score = temp
}

# Sum up open item amount of duplicate IDs
d19 = d19 %>% group_by(ID) %>% summarise(Client_type=first(Client_type), Age=first(Age), Flag_e.mail_deposited=first(Flag_e.mail_deposited), Flag_mobile_deposited=first(Flag_mobile_deposited), 
                                         Number_contractual_relationships=first(Number_contractual_relationships), Customer_for_years=first(Customer_for_years), Credit_rating_score=first(Credit_rating_score), 
                                         Total_consumption=first(Total_consumption), Open_item_amount=sum(Open_item_amount), Flag_advertising_permission_e.mail=first(Flag_advertising_permission_e.mail), 
                                         Flag_advertising_permission_post=first(Flag_advertising_permission_post), Flag_advertising_permission_telephone=first(Flag_advertising_permission_telephone), 
                                         OptIn_data_protection_regulations=first(OptIn_data_protection_regulations), OptIn_newsletter=first(OptIn_newsletter), Factor_subsequent_payment=first(Factor_subsequent_payment), 
                                         Sum_contribution_margin_2=first(Sum_contribution_margin_2), Minimum_contract_duration=first(Minimum_contract_duration), Contract_duration_difference=first(Contract_duration_difference), 
                                         Vorteilswelt_customer_duration=first(Vorteilswelt_customer_duration))

#----------------------------
# Prepare data for prediction
#----------------------------

# One-hot-encoding to eleminiate multi-level factors
dummy_client = dummyVars(" ~ Client_type", data = d19)
client_encoded = data.frame(predict(dummy_client, newdata = d19))
d19 = cbind(d19,client_encoded)

# Remove original Client type and OptIns
d19 = d19[,-2]

# Missing value imputation
imputation = mice(d19[,-c(8,9,10,11,12,13,16)])
d19_imputed = mice::complete(imputation)
d19_imputed = cbind(d19_imputed,d19[,c(8,9,10,11,12,13,16)])

#----------------------------------------
# Do the prediction and export it as .csv
#----------------------------------------

pred = predict(model_dt,newdata=d19_imputed)
export = cbind.data.frame(ID=d19_imputed$ID,Flag_cancellation=pred$data$response)
write.csv2(export,"prediction.csv", row.names = FALSE)