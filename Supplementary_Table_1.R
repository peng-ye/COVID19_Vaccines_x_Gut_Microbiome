## Load package
library(tableone) 
library(readxl)
## Supplementary Table 1 summary
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
## Load data
dat.sinovac <- data[data$Vac_Group=="SinoVac", ]
dat.biontech <- data[data$Vac_Group=="BioNTech", ]

## The association between AUC_100_12800 and predictors in BNT162b2 group
for (i in c("Age","BMI")){
  dat.biontech$c<-dat.biontech[,colnames(dat.biontech)==i]
  dat.biontech$c<-dat.biontech$c[[1]]
  result.pea<-cor.test(dat.biontech$AUC_100_12800,dat.biontech$c,method="spearman")
  print(i)
  print(result.pea)
}


for (i in c(  "Male","OWOB", "Obese_bin", "HTC_bin", "DMC_bin", "Allergies_ever_bin", 
              "Diarrhea_within_past_3_months", "Any_other_como_bin", "Antibiotics_3m_current", 
              "Hormone_current", "Immunosuppressant_current", "Probiotic_current", 
              "Vaccines_received_past_year", "Dietary_preference",  
              "Alcohol_drinking_past_2w", "Regular_exercise", "Dose1_AE_num_cat", "Dose2_AE_num_cat")){
  dat.biontech$c<-dat.biontech[,colnames(dat.biontech)==i]
  dat.biontech$c<-dat.biontech$c[[1]]
  result<-ifelse(nlevels(dat.biontech$c)<2,"null",ifelse(nlevels(dat.biontech$c)==2,round(wilcox.test(dat.biontech$AUC_100_12800~dat.biontech$c)$p.value,3),
                                                         round(kruskal.test(dat.biontech$AUC_100_12800~dat.biontech$c)$p.value,3)))
  median<-aggregate(AUC_100_12800 ~ c, data = dat.biontech, quan)
  print(i)
  print(median)
  print(result)
}

## The association between M1_sVNT_200 and predictors in BNT162b2 group
for (i in c("Age","BMI")){
  dat.biontech$c<-dat.biontech[,colnames(dat.biontech)==i]
  dat.biontech$c<-dat.biontech$c[[1]]
  result.pea<-cor.test(dat.biontech$M1_sVNT_200,dat.biontech$c,method="spearman")
  print(i)
  print(result.pea)
}

for (i in c(  "Male", 
              "OWOB", "Obese_bin", "HTC_bin", "DMC_bin", "Allergies_ever_bin", 
              "Diarrhea_within_past_3_months", "Any_other_como_bin", "Antibiotics_3m_current", 
              "Hormone_current", "Immunosuppressant_current", "Probiotic_current", 
              "Vaccines_received_past_year", "Dietary_preference",  
              "Alcohol_drinking_past_2w", "Regular_exercise", "Dose1_AE_num_cat", "Dose2_AE_num_cat")){
  dat.biontech$c<-dat.biontech[,colnames(dat.biontech)==i]
  dat.biontech$c<-dat.biontech$c[[1]]
  result<-ifelse(nlevels(dat.biontech$c)<2,"null",ifelse(nlevels(dat.biontech$c)==2,round(wilcox.test(dat.biontech$M1_sVNT_200~dat.biontech$c)$p.value,3),
                                                         round(kruskal.test(dat.biontech$M1_sVNT_200~dat.biontech$c)$p.value,3)))
  median<-aggregate(M1_sVNT_200 ~ c, data = dat.biontech, quan)
  print(i)
  print(median)
  print(result)
}

## The association between AUC_100_12800 and predictors in CoronaVac group
for (i in c("Age","BMI")){
  dat.sinovac$c<-dat.sinovac[,colnames(dat.sinovac)==i]
  dat.sinovac$c<-dat.sinovac$c[[1]]
  result.pea<-cor.test(dat.sinovac$AUC_100_12800,dat.sinovac$c,method="spearman")
  print(i)
  print(result.pea)
}

for (i in c(  "Male", "OWOB", "Obese_bin", "HTC_bin", "DMC_bin", "Allergies_ever_bin", 
              "Diarrhea_within_past_3_months", "Any_other_como_bin"
              , "Probiotic_current", 
              "Vaccines_received_past_year", 
              "Alcohol_drinking_past_2w", "Regular_exercise",  "Dose1_AE_num_cat", "Dose2_AE_num_cat")){
  dat.sinovac$c<-dat.sinovac[,colnames(dat.sinovac)==i]
  dat.sinovac$c<-dat.sinovac$c[[1]]
  median<-aggregate(AUC_100_12800 ~ c, data = dat.sinovac, quan)
  
  result<-ifelse(nlevels(dat.sinovac$c)<2,"null",ifelse(nlevels(dat.sinovac$c)==2,round(wilcox.test(dat.sinovac$AUC_100_12800~dat.sinovac$c)$p.value,3),
                                                        round(kruskal.test(dat.sinovac$AUC_100_12800~dat.sinovac$c)$p.value,3)))
  print(i)
  print(median)
  print(result)
}

## The association between M1_sVNT_10 and predictors in CoronaVac group
for (i in c("Age","BMI")){
  dat.sinovac$c<-dat.sinovac[,colnames(dat.sinovac)==i]
  dat.sinovac$c<-dat.sinovac$c[[1]]
  result.pea<-cor.test(dat.sinovac$M1_sVNT_10,dat.sinovac$c,method="spearman")
  print(i)
  print(result.pea)
}

for (i in c(  "Male",  "OWOB", "Obese_bin", "HTC_bin", "DMC_bin", "Allergies_ever_bin", 
              "Diarrhea_within_past_3_months", "Any_other_como_bin" , "Probiotic_current", 
              "Vaccines_received_past_year", 
              "Alcohol_drinking_past_2w", "Regular_exercise",  "Dose1_AE_num_cat", "Dose2_AE_num_cat")){
  dat.sinovac$c<-dat.sinovac[,colnames(dat.sinovac)==i]
  dat.sinovac$c<-dat.sinovac$c[[1]]
  median<-aggregate(M1_sVNT_10 ~ c, data = dat.sinovac, quan)
  
  result<-ifelse(nlevels(dat.sinovac$c)<2,"null",ifelse(nlevels(dat.sinovac$c)==2,round(wilcox.test(dat.sinovac$M1_sVNT_10~dat.sinovac$c)$p.value,3),
                                                        round(kruskal.test(dat.sinovac$M1_sVNT_10~dat.sinovac$c)$p.value,3)))
  print(i)
  print(median)
  print(result)
}
