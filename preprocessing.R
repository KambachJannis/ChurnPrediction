library(lubridate)
library(dplyr)
library(mice)
library(corrr)
library(caret)

# Load data
d18 = read.csv2(file="Data_set_Jan18.csv", stringsAsFactors = FALSE)

# Rename first column because of unicode error
colnames(d18)[1] = "ID"

#------------------------------
# Syntactic preparation of data 
#------------------------------

# Set all missing values to NA
d18[d18=="-"] = NA

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
d18$Flag_cancellation = as.factor(d18$Flag_cancellation)

# Prepare date values
d18$Minimum_contract_term = as.Date(d18$Minimum_contract_term, format="%d.%m.%Y")
d18$Maximum_contract_term = as.Date(d18$Maximum_contract_term, format="%d.%m.%Y")
d18$Vorteilswelt_customer_since = as.Date(d18$Vorteilswelt_customer_since, format="%d.%m.%Y")

# Set open item amount to 0 if NA
d18[which(is.na(d18$Open_item_amount)),]$Open_item_amount = 0

# Set OptIns to 0 when NA because they are Opt In
d18[which(is.na(d18$OptIn_data_protection_regulations)),]$OptIn_data_protection_regulations = 0
d18[which(is.na(d18$OptIn_newsletter)),]$OptIn_newsletter = 0

# Remove columns where ID is NA
d18 = d18[-which(is.na(d18$ID)),]

# Change all -1 in customer_for_years to NA
d18[d18$Customer_for_years==-1,]$Customer_for_years = NA


#-----------------------------
# Semantic preparation of data
#-----------------------------

# Transform contract duration dates into days left based on time of dataset
basedate = as.Date("01.01.2018","%d.%m.%Y")
d18$Minimum_contract_duration = as.numeric(d18$Minimum_contract_term - basedate, "days")
d18$Maximum_contract_duration = as.numeric(d18$Maximum_contract_term - basedate, "days")

# Use contract difference difference instead of maximum contract duration
d18$Contract_duration_difference = d18$Maximum_contract_duration - d18$Minimum_contract_duration
# Set contract duration difference to 0 if number of contracts is 1
d18[which(d18$Number_contractual_relationships==1),]$Contract_duration_difference = 0


# Transform Vorteilswelt signup date into membership duration based on current date
basedate2 = as.Date("01.01.2020", "%d.%m.%Y")
d18$Vorteilswelt_customer_duration = as.numeric(basedate2 - d18$Vorteilswelt_customer_since, "days")
# Set duration to 0 if NA
d18[which(is.na(d18$Vorteilswelt_customer_duration)),]$Vorteilswelt_customer_duration = 0

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

# Minimum/Maximum contract terms + Maximum contract duration
d18 = d18[,-8]
d18 = d18[,-8]
d18 = d18[,-21]

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
                                         Sum_contribution_margin_2=first(Sum_contribution_margin_2), Minimum_contract_duration=first(Minimum_contract_duration), Contract_duration_difference=first(Contract_duration_difference), 
                                         Vorteilswelt_customer_duration=first(Vorteilswelt_customer_duration))

#--------------------------
# Prepare data for training
#--------------------------

# One-hot-encoding to eleminiate multi-level factors
dummy_client = dummyVars(" ~ Client_type", data = d18)
client_encoded = data.frame(predict(dummy_client, newdata = d18))
d18 = cbind(d18,client_encoded)

# Remove original Client type and OptIns
d18 = d18[,-2]

# Missing value imputation
imputation = mice(d18[,-c(8,9,16)])
d18_imputed = complete(imputation)
d18_imputed = cbind(d18_imputed,d18[,c(8,9,16)])

#------------------------------------------------------------------------------------------
#-------------------------------- Exploration ---------------------------------------------
#------------------------------------------------------------------------------------------

# Missing Values
sum(is.na(d18$Age))
sum(is.na(d18$Credit_rating_score))
sum(is.na(d18$OptIn_data_protection_regulations))
sum(is.na(d18$OptIn_newsletter))
sum(is.na(d18$Factor_subsequent_payment))
sum(is.na(d18$Minimum_contract_duration))
sum(is.na(d18$Maximum_contract_duration))
sum(is.na(d18$Vorteilswelt_customer_duration))

# Correlation
d18c = d18_imputed[,-1]
d18c$Flag_e.mail_deposited = as.numeric(d18c$Flag_e.mail_deposited)
d18c$Flag_mobile_deposited = as.numeric(d18c$Flag_mobile_deposited)
d18c$Flag_advertising_permission_e.mail = as.numeric(d18c$Flag_advertising_permission_e.mail)
d18c$Flag_advertising_permission_post = as.numeric(d18c$Flag_advertising_permission_post)
d18c$Flag_advertising_permission_telephone = as.numeric(d18c$Flag_advertising_permission_telephone)
d18c$OptIn_data_protection_regulations = as.numeric(d18c$OptIn_data_protection_regulations)
d18c$OptIn_newsletter = as.numeric(d18c$OptIn_newsletter)
d18c$Flag_cancellation = as.numeric(d18c$Flag_cancellation)
findCorrelation(cor(d18c),cutoff=.75,names=T,verbose=T)


### TODO
# 1: final corrlelation matrix
# 2: NA values grid