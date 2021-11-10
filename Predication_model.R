library(pROC)
##Load data
dat.roc<-read_xlsx("modification.xlsx")
dat.roc$Vac_Group<-as.factor(dat.roc$Vac_Group)
dat.roc$sVNT_group[ dat.roc$sVNT_group=="High"]<-1
dat.roc$sVNT_group[ dat.roc$sVNT_group=="Low"]<-0
dat.roc$sVNT_60percent_group[ dat.roc$sVNT_60percent_group=="High"]<-1
dat.roc$sVNT_60percent_group[ dat.roc$sVNT_60percent_group=="Low"]<-0
dat.roc$sVNT_60percent_group<-as.factor(dat.roc$sVNT_60percent_group)
dat.roc$sVNT_group<-as.factor(dat.roc$sVNT_group)

dat.roc.bio<-dat.roc[dat.roc$Vac_Group=="BioNTech",] ##BioNTech dataset
dat.roc.sin<-dat.roc[dat.roc$Vac_Group=="SinoVac",] ##SinoVac dataset

## BNT162b2  group
## predication model for overall, positive and negative biomarker
fit.overall.bio<-glm(sVNT_group~s__Eubacterium_rectale+s__Roseburia_faecis+
                       s__Bacteroides_thetaiotaomicron+s__Bacteroides_sp_OM05_12+
                       s__Fusobacterium_mortiferum+s__Clostridium_saccharolyticum+s__Parabacteroides_merdae,
                     data=dat.roc.bio,family = binomial()) ## build a logistic regression bioNTech overall model
summary(fit.overall.bio)
dat.roc.bio$prob.overall.bio<-predict(fit.overall.bio,newdata=dat.roc.bio,type="response")
roc.overall.bio<-roc(dat.roc.bio$sVNT_group,dat.roc.bio$prob.overall.bio)
roc.overall.bio
ci(roc.overall.bio)

##Positive bacteria##

fit.pos.bio<-glm(sVNT_group~s__Eubacterium_rectale+s__Roseburia_faecis+s__Bacteroides_thetaiotaomicron+s__Bacteroides_sp_OM05_12,
                 data=dat.roc.bio,family = binomial()) ## build a logistic regression bioNTech pos model

summary(fit.pos.bio)
dat.roc.bio$prob.pos.bio<-predict(fit.pos.bio,newdata=dat.roc.bio,type="response")
roc.pos.bio<-roc(dat.roc.bio$sVNT_group,dat.roc.bio$prob.pos.bio)
roc.pos.bio
ci(roc.pos.bio)

##Negative bacteria

fit.neg.bio<-glm(sVNT_group~s__Fusobacterium_mortiferum+s__Clostridium_saccharolyticum+s__Parabacteroides_merdae,
                 data=dat.roc.bio,family = binomial()) ## build a logistic regression bioNTech overall model
summary(fit.neg.bio)
dat.roc.bio$prob.neg.bio<-predict(fit.neg.bio,newdata=dat.roc.bio,type="response")
roc.neg.bio<-roc(dat.roc.bio$sVNT_group,dat.roc.bio$prob.neg.bio)
roc.neg.bio
ci(roc.neg.bio)

## prediction model for individual biomarker 
roc.bio<-data.frame()
for (i in c("s__Eubacterium_rectale","s__Roseburia_faecis","s__Bacteroides_thetaiotaomicron",
            "s__Bacteroides_sp_OM05_12","s__Fusobacterium_mortiferum","s__Clostridium_saccharolyticum",
            "s__Parabacteroides_merdae")) {
  dat.roc.bio$c<-dat.roc.bio[,colnames(dat.roc.bio)==i]
  dat.roc.bio$c<-dat.roc.bio$c[[1]]
  fit.bio.i<-glm(sVNT_group~dat.roc.bio$c,data=dat.roc.bio,family = binomial())
  dat.roc.bio$prob.bio.i<-predict(fit.bio.i,newdata=dat.roc.bio,type="response")
  roc.bio.i<-roc(dat.roc.bio$sVNT_group,dat.roc.bio$prob.bio.i)
  result.roc.bio<-cbind(i,roc.bio.i$auc,ci(roc.bio.i)[1],ci(roc.bio.i)[2])
  roc.bio<-rbind(roc.bio,result.roc.bio)
}

##CoronaVac 
paste( "s__Bifidobacterium_adolescentis","s__Alistipes_putredinis",
       "s__Adlercreutzia_equolifaciens", "s__Oscillibacter_sp_57_20","s__Asaccharobacter_celatus",
       "s__Ruminococcus_sp_CAG_330",  "s__Intestinibacter_bartlettii","s__Lactococcus_petauri",
       "s__Mitsuokella_multacida", "s__Butyricimonas_virosa","s__Blautia_hydrogenotrophica",
       "s__Paraprevotella_xylaniphila", "s__Ruminococcus_gnavus","s__Bacteroides_thetaiotaomicron",
       "s__Bacteroides_vulgatus",sep="+" )

## Prediction model for overall, positive and negative biomarker in SinoVac group
fit.overall.sin<-glm(sVNT_60percent_group~s__Bifidobacterium_adolescentis+s__Alistipes_putredinis+
                       s__Adlercreutzia_equolifaciens+s__Oscillibacter_sp_57_20+s__Asaccharobacter_celatus+
                       s__Ruminococcus_sp_CAG_330+s__Intestinibacter_bartlettii+s__Lactococcus_petauri+
                       s__Mitsuokella_multacida+s__Butyricimonas_virosa+s__Blautia_hydrogenotrophica+
                       s__Paraprevotella_xylaniphila+s__Ruminococcus_gnavus+s__Bacteroides_thetaiotaomicron+s__Bacteroides_vulgatus,
                     data=dat.roc.sin,family = binomial()) ## build a logistic regression sinNTech overall model

summary(fit.overall.sin)
dat.roc.sin$prob.overall.sin<-predict(fit.overall.sin,newdata=dat.roc.sin,type="response")
roc.overall.sin<-roc(dat.roc.sin$sVNT_60percent_group,dat.roc.sin$prob.overall.sin)
roc.overall.sin
ci(roc.overall.sin)

##Positive bacteria##

fit.pos.sin<-glm(sVNT_60percent_group~s__Bifidobacterium_adolescentis+s__Alistipes_putredinis+
                   s__Adlercreutzia_equolifaciens+s__Oscillibacter_sp_57_20+s__Asaccharobacter_celatus+
                   s__Ruminococcus_sp_CAG_330+s__Intestinibacter_bartlettii+s__Lactococcus_petauri+
                   s__Mitsuokella_multacida+s__Butyricimonas_virosa,
                 data=dat.roc.sin,family = binomial()) ## build a logistic regression sinNTech pos model

summary(fit.pos.sin)
dat.roc.sin$prob.pos.sin<-predict(fit.pos.sin,newdata=dat.roc.sin,type="response")
roc.pos.sin<-roc(dat.roc.sin$sVNT_60percent_group,dat.roc.sin$prob.pos.sin)
roc.pos.sin
ci(roc.pos.sin)

##Negative bacteria

fit.neg.sin<-glm(sVNT_60percent_group~s__Blautia_hydrogenotrophica+
                   s__Paraprevotella_xylaniphila+s__Ruminococcus_gnavus+s__Bacteroides_thetaiotaomicron+s__Bacteroides_vulgatus,
                 data=dat.roc.sin,family = binomial()) ## build a logistic regression sinNTech overall model
summary(fit.neg.sin)
dat.roc.sin$prob.neg.sin<-predict(fit.neg.sin,newdata=dat.roc.sin,type="response")
roc.neg.sin<-roc(dat.roc.sin$sVNT_60percent_group,dat.roc.sin$prob.neg.sin)
roc.neg.sin
ci(roc.neg.sin)

## individual Biomarker
roc.sin<-data.frame()
for (i in c("s__Bifidobacterium_adolescentis","s__Alistipes_putredinis",
            "s__Adlercreutzia_equolifaciens", "s__Oscillibacter_sp_57_20","s__Asaccharobacter_celatus",
            "s__Ruminococcus_sp_CAG_330",  "s__Intestinibacter_bartlettii","s__Lactococcus_petauri",
            "s__Mitsuokella_multacida", "s__Butyricimonas_virosa","s__Blautia_hydrogenotrophica",
            "s__Paraprevotella_xylaniphila", "s__Ruminococcus_gnavus","s__Bacteroides_thetaiotaomicron",
            "s__Bacteroides_vulgatus")) {
  dat.roc.sin$c<-dat.roc.sin[,colnames(dat.roc.sin)==i]
  dat.roc.sin$c<-dat.roc.sin$c[[1]]
  fit.sin.i<-glm(sVNT_60percent_group~dat.roc.sin$c,data=dat.roc.sin,family = binomial())
  dat.roc.sin$prob.sin.i<-predict(fit.sin.i,newdata=dat.roc.sin,type="response")
  roc.sin.i<-roc(dat.roc.sin$sVNT_60percent_group,dat.roc.sin$prob.sin.i)
  result.roc.sin<-cbind(i,roc.sin.i$auc,ci(roc.sin.i)[1],ci(roc.sin.i)[2])
  roc.sin<-rbind(roc.sin,result.roc.sin)
}
