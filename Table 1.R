## Load package
library(tableone) 
library(readxl)

## Load data
data<-read_xlsx("samples_metadata_imm_272S_wide.xlsx")

##Check variables
dput(names(data))
head(data)

##median function
quan<-function(x){
  median<-round(median(x),2)
  quantile<-round(quantile(x,probs=c(0.25,0.75)),2)
  a<-paste(median,"(",quantile[1],", ", quantile[2],")")
  return(a)
}

## Table 1 summary

## Creat a variable list
myVars<-dput(names(data))

catVars<-c("Cohort", "Vac_Group",  "Male", 
           "OWOB", "Obese_bin", "HTC_bin", "DMC_bin", "Allergies_ever_bin", 
           "Diarrhea_within_past_3_months", "Any_other_como_bin", "Antibiotics_3m_current", 
           "Hormone_current", "Immunosuppressant_current", "Probiotic_current", 
           "Vaccines_received_past_year", "Dietary_preference", "Dietary_change", 
           "Alcohol_drinking_past_2w", "Regular_exercise","Dose1_AE_num_cat", "Dose2_AE_num_cat")

conVars<-c("Age","M0_RBD", "BMI", "M1_RBD_100", 
           "AUC_100_12800", "M1_sVNT_10", "M1_sVNT_200")

data[catVars] <- lapply(data[catVars], factor)
data[conVars] <- lapply(data[conVars], as.numeric)

tab.1<-CreateTableOne(data=data,vars=myVars,factorVars = catVars,strata="Vac_Group",addOverall=TRUE)

table.1<-print(tab.1,nonnormal=conVars,exact=catVars,
               showAllLevels = FALSE,catDigits = 1,contDigits = 1)

write.csv(table.1,file="table1.csv")