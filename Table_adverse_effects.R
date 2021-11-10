## Load package
library(tableone) 

##median function
quan<-function(x){
  median<-round(median(x),2)
  quantile<-round(quantile(x,probs=c(0.25,0.75)),2)
  a<-paste(median,"(",quantile[1],", ", quantile[2],")")
  return(a)
}

## Supplementary Table 5. Adverse events after the first dose and second dose
## Load data
dat.t1 <- read.csv("AE.csv")

##Check variables
dput(names(data))
head(data)

## Creat a variable list

myVars.ae<-dput(names(dat.t1))

catVars.ae<-c( "Cohort", "Vac_Group", "VAC_DOSE", 
               "AE_Injection_site_pain", "AE_Fatigue", "AE_Fever", "AE_Injection_site_SEPI", 
               "AE_Myalgia", "AE_Drowsiness", "AE_Headache", "AE_Chills", "AE_Dizziness", 
               "AE_Arthralgia", "AE_Loss_of_appetite", "AE_Abdominal_pain", 
               "AE_Rhinorrhea", "AE_Sore_throat", "AE_Diarrhea", "AE_Pruritus", 
               "AE_Coughing", "AE_Constipation", "AE_Abdominal_distension", 
               "AE_Nausea", "AE_Flushing", "AE_Hypersensitivity", "AE_Muscle_spasms", 
               "AE_Nasal_Congestion", "AE_Edema", "AE_Vomiting", "AE_Tremor", 
               "AE_Eyelid_edema", "AE_Nosebleeds", "AE_Hyposmia", "AE_Ocular_congestion", 
               "AE_number_cat", "AE_number", "AE_Others", "AE_any")

conVars.ae<-c( "AUC.100.12800.", "M1.sVNT.10", "M1.sVNT.200", "M1.sVNT")


dat.t1.1st <- dat.t1[dat.t1$VAC_DOSE==1, ]
dat.t1.2nd <- dat.t1[dat.t1$VAC_DOSE==2, ]

table2.1st<-CreateTableOne(data=dat.t1.1st,vars=myVars.ae,factorVars = catVars.ae,strata="Vac_Group",addOverall=TRUE)
table2.first<-print(table2.1st,nonnormal=conVars.ae,exact=catVars.ae,
                    showAllLevels = FALSE,catDigits = 1,contDigits = 1)
write.csv(table2.first,file="table2.first.csv")

table2.2nd<-CreateTableOne(data=dat.t1.2nd,vars=myVars.ae,factorVars = catVars.ae,strata="Vac_Group",addOverall=TRUE)
table2.second<-print(table2.2nd,nonnormal=conVars.ae,exact=catVars.ae,
                     showAllLevels = FALSE,catDigits = 1,contDigits = 1)
write.csv(table2.second,file="table2.second.csv")


## Supplementary Table 7 The association between immunity response and Adverse events after first dose (Overall)
dput(names(dat.t1))
dat.t1 <- read.csv("AE.csv")
dat.t1.1st <- dat.t1[dat.t1$VAC_DOSE==1, ]
dat.sinovac.ae1 <- dat.t1.1st[dat.t1.1st$Vac_Group=="SinoVac", ]
dat.biontech.ae1 <- dat.t1.1st[dat.t1.1st$Vac_Group=="BioNTech", ]

## The association between AUC_100_12800 and AE in BNT162b2 group
for (i in c( "AE_Injection_site_pain", "AE_Fatigue", "AE_Fever", "AE_Injection_site_SEPI", 
             "AE_Myalgia", "AE_Drowsiness", "AE_Headache", "AE_Chills", "AE_Dizziness", 
             "AE_Arthralgia", "AE_Loss_of_appetite", "AE_Abdominal_pain", 
             "AE_Rhinorrhea", "AE_Sore_throat", "AE_Diarrhea", "AE_Pruritus", 
             "AE_Others", "AE_any")){
  dat.biontech.ae1$c<-dat.biontech.ae1[,colnames(dat.biontech.ae1)==i]
  median<-aggregate(AUC.100.12800. ~ c, data = dat.biontech.ae1, quan)
  t0<-wilcox.test(dat.biontech.ae1$AUC.100.12800.~dat.biontech.ae1$c)
  print(i)
  print(t0$p.value)
  print(median)
}
## The association between M1.sVNT.200 and AE in BNT162b2 group

for (i in c( "AE_Injection_site_pain", "AE_Fatigue", "AE_Fever", "AE_Injection_site_SEPI", 
             "AE_Myalgia", "AE_Drowsiness", "AE_Headache", "AE_Chills", "AE_Dizziness", 
             "AE_Arthralgia", "AE_Loss_of_appetite", "AE_Abdominal_pain", 
             "AE_Rhinorrhea", "AE_Sore_throat", "AE_Diarrhea", "AE_Pruritus", 
             "AE_Others", "AE_any")){
  dat.biontech.ae1$c<-dat.biontech.ae1[,colnames(dat.biontech.ae1)==i]
  median<-aggregate(M1.sVNT.200 ~ c, data = dat.biontech.ae1, quan)
  t0<-wilcox.test(dat.biontech.ae1$M1.sVNT.200~dat.biontech.ae1$c)
  print(i)
  print(t0$p.value)
  print(median)
}

## The association between AUC.100.12800. and AE in CoronaVac group
for (i in c( "AE_Injection_site_pain", "AE_Fatigue", "AE_Fever", "AE_Injection_site_SEPI", 
             "AE_Myalgia", "AE_Drowsiness", "AE_Pruritus", 
             "AE_Others", "AE_any")){
  dat.sinovac.ae1$c<-dat.sinovac.ae1[,colnames(dat.sinovac.ae1)==i]
  median<-aggregate(AUC.100.12800. ~ c, data = dat.sinovac.ae1, quan)
  
  t0<-wilcox.test(dat.sinovac.ae1$AUC.100.12800.~dat.sinovac.ae1$c)
  print(i)
  print(t0$p.value)
  print(median)
}

## The association between M1.sVNT.10 and AE in CoronaVac group
for (i in c( "AE_Injection_site_pain", "AE_Fatigue", "AE_Fever", "AE_Injection_site_SEPI", 
             "AE_Myalgia", "AE_Drowsiness", "AE_Pruritus", 
             "AE_Others", "AE_any")){
  dat.sinovac.ae1$c<-dat.sinovac.ae1[,colnames(dat.sinovac.ae1)==i]
  median<-aggregate(M1.sVNT.10 ~ c, data = dat.sinovac.ae1, quan)
  
  t0<-wilcox.test(dat.sinovac.ae1$M1.sVNT.10~dat.sinovac.ae1$c)
  print(i)
  print(t0$p.value)
  print(median)
}

## Supplementary Table 8. The association between immunity response and Adverse events after second dose (Overall)

dput(names(dat.t1))
dat.t1 <- read.csv("AE.csv")
dat.t1.2st <- dat.t1[dat.t1$VAC_DOSE==2, ]
dat.sinovac.ae2 <- dat.t1.2st[dat.t1.1st$Vac_Group=="SinoVac", ]
dat.biontech.ae2 <- dat.t1.2st[dat.t1.1st$Vac_Group=="BioNTech", ]

## The association between AUC_100_12800 and AE in BNT162b2 group

for (i in c( "AE_Injection_site_pain", "AE_Fatigue", "AE_Fever", "AE_Injection_site_SEPI", 
             "AE_Myalgia", "AE_Drowsiness", "AE_Headache", "AE_Chills", "AE_Dizziness", 
             "AE_Arthralgia", "AE_Loss_of_appetite", "AE_Abdominal_pain", 
             "AE_Rhinorrhea", "AE_Sore_throat", "AE_Diarrhea", "AE_Pruritus", 
             "AE_Coughing", "AE_Constipation", "AE_Abdominal_distension", 
             "AE_Nausea", "AE_Flushing",  
             "AE_Nasal_Congestion",  
             "AE_Others", "AE_any")){
  dat.biontech.ae2$c<-dat.biontech.ae2[,colnames(dat.biontech.ae2)==i]
  median<-aggregate(AUC.100.12800. ~ c, data = dat.biontech.ae2, quan)
  t0<-wilcox.test(dat.biontech.ae2$AUC.100.12800.~dat.biontech.ae2$c)
  print(i)
  print(t0$p.value)
  print(median)
}

## The association between M1.sVNT.200 and AE in BNT162b2 group
for (i in c( "AE_Injection_site_pain", "AE_Fatigue", "AE_Fever", "AE_Injection_site_SEPI", 
             "AE_Myalgia", "AE_Drowsiness", "AE_Headache", "AE_Chills", "AE_Dizziness", 
             "AE_Arthralgia", "AE_Loss_of_appetite", "AE_Abdominal_pain", 
             "AE_Rhinorrhea", "AE_Sore_throat", "AE_Diarrhea", "AE_Pruritus", 
             "AE_Coughing", "AE_Constipation", "AE_Abdominal_distension", 
             "AE_Nausea", "AE_Flushing",  
             "AE_Nasal_Congestion",  
             "AE_Others", "AE_any")){
  dat.biontech.ae2$c<-dat.biontech.ae2[,colnames(dat.biontech.ae2)==i]
  t0<-wilcox.test(dat.biontech.ae2$M1.sVNT.200~dat.biontech.ae2$c)
  print(i)
  print(t0$p.value)
}

## The association between AUC.100.12800. and AE in CoronaVac group
for (i in c( "AE_Injection_site_pain", "AE_Fatigue",  "AE_Injection_site_SEPI", 
             "AE_Myalgia", "AE_Drowsiness", 
             "AE_Rhinorrhea", "AE_Pruritus", 
             "AE_Others", "AE_any")){
  dat.sinovac.ae2$c<-dat.sinovac.ae2[,colnames(dat.sinovac.ae2)==i]
  t0<-wilcox.test(dat.sinovac.ae2$AUC.100.12800.~dat.sinovac.ae2$c)
  print(i)
  print(t0$p.value)
}

## The association between M1.sVNT.10 and AE in CoronaVac group
for (i in c( "AE_Injection_site_pain", "AE_Fatigue",  "AE_Injection_site_SEPI", 
             "AE_Myalgia", "AE_Drowsiness", 
             "AE_Rhinorrhea", "AE_Pruritus", 
             "AE_Others", "AE_any")){
  dat.sinovac.ae2$c<-dat.sinovac.ae2[,colnames(dat.sinovac.ae2)==i]
  t0<-wilcox.test(dat.sinovac.ae2$M1.sVNT.10~dat.sinovac.ae2$c)
  print(i)
  print(t0$p.value)
}