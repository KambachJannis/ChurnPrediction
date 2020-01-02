library(lubridate)
library(tidyr)
library(dplyr)
library(ploty)
library(readxl)
library(mice)
library(corrr)

# Load data
d18 = read.csv2(file="Data_set_Jan18.csv", stringsAsFactors = FALSE)

# Rename first column because of unicode error
colnames(d18)[1] = "ID"

#------------------------------
# Syntactic preparation of data 
#------------------------------

# Set all missing values to NA
d18[d18=="-"] <- NA

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


#-----------------------------
# Semantic preparation of data
#-----------------------------

# Transform contract duration dates into days left based on time of dataset
basedate = as.Date("01.02.2018","%d.%m.%Y")
d18$Minimum_contract_duration = d18$Minimum_contract_term - basedate
d18$Maximum_contract_duration = d18$Maximum_contract_term - basedate

# Transform Vorteilswelt signup date into membership duration based on current date
basedate2 = as.Date("01.01.2020", "%d.%m.%Y")
d18$Vorteilswelt_customer_duraction = basedate2 - d18$Vorteilswelt_customer_since

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
d18 = d18[,-12]

# Invoice shock
d18 = d18[,-22]