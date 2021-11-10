
## effect modification 

### use "mover" Method suggested by described by Zou (2008) [doi: 10.1093/aje/kwn104]
### ""Guang Yong Zou, On the Estimation of Additive Interaction by Use of the Four-by-two Table and Beyond, American Journal of Epidemiology, Volume 168, Issue 2, 15 July 2008, Pages 212¡V224, https://doi.org/10.1093/aje/kwn104"

modification<-function(model,modifier,exposure){
  e1 <- grep(exposure, names(coef(model)), value = TRUE, 
             ignore.case = TRUE)
  e2 <- grep(modifier, names(coef(model)), value = TRUE, 
             ignore.case = TRUE)
  exposure_names <- union(e1, e2)
  beta1 <- exposure_names[1]
  beta2 <- exposure_names[3]
  beta3 <- exposure_names[2]
  varNames <- c(beta1, beta2, beta3)
  z <- qnorm(1 - 0.05/2)
  b1 <- coef(model)[beta1]
  b2 <- coef(model)[beta2]
  b3 <- coef(model)[beta3]
  se_vec <- summary(model)$coefficients[, 2]
  v1 <- se_vec[beta1]^2
  v2 <- se_vec[beta2]^2
  v3 <- se_vec[beta3]^2
  pvals <- summary(model)$coefficients[, "Pr(>|z|)"]
  v_cov <- vcov(model)
  v_cov <- v_cov[varNames, varNames]
  cov12 <- v_cov[beta1, beta2]
  cov13 <- v_cov[beta1, beta3]
  cov23 <- v_cov[beta2, beta3]
  v123 <- v1 + v2 + v3 + (2 * (cov12 + cov13 + cov23))
  v12 <- v1 + v2 + (2 * (cov12))
  v13 <- v1 + v3 + (2 * (cov13))
  v23 <- v2 + v3 + (2 * (cov23))
  OR00 <- 1
  OR10 <- as.numeric(exp(b1))
  l1 <- exp(confint(model)[beta1, 1])
  u1 <- exp(confint(model)[beta1, 2])
  p.OR10 <- pvals[beta1]
  OR01 <- as.numeric(exp(b2))
  l2 <- exp(confint(model)[beta2, 1])
  u2 <- exp(confint(model)[beta2, 2])
  p.OR01 <- pvals[beta2]
  OR11 <- as.numeric(exp(b1 + b2 + b3))
  l3 <- exp(b1 + b2 + b3 - z * sqrt(v123))
  u3 <- exp(b1 + b2 + b3 + z * sqrt(v123))
  q1 <- abs(log(OR11)/sqrt(v123))
  p.OR11 <- exp(-0.717 * q1 - 0.416 * q1^2)
  OR_X1 <- as.numeric(exp(b2 + b3))
  CI.ll_OR_X1 <- exp(b2 + b3 - z * sqrt(v23))
  CI.ul_OR_X1 <- exp(b2 + b3 + z * sqrt(v23))
  q2 <- abs(log(OR_X1)/sqrt(v23))
  p.OR_X1 <- exp(-0.717 * q2 - 0.416 * q2^2)
  OR_A1 <- as.numeric(exp(b1 + b3))
  CI.ll_OR_A1 <- exp(b1 + b3 - z * sqrt(v13))
  CI.ul_OR_A1 <- exp(b1 + b3 + z * sqrt(v13))
  q3 <- abs(log(OR_A1)/sqrt(v13))
  p.OR_A1 <- exp(-0.717 * q3 - 0.416 * q3^2)
  OR_M <- as.numeric(exp(b3))
  CI.ll_OR_M <- exp(confint(model)[beta3, 1])
  CI.ul_OR_M <- exp(confint(model)[beta3, 2])
  p.OR_M <- pvals[beta3]
  ## (ci.type == "mover") ##
  RERI <- OR11 - OR01 - OR10 + 1
  r12 <- (v1 + cov12 + cov13)/sqrt(v1 * v123)
  r13 <- (cov12 + v2 + cov23)/sqrt(v2 * v123)
  r23 <- cov12/sqrt(v1 * v2)
  p1 <- (OR11 - l3)^2 + (u1 - OR10)^2 + (u2 - OR01)^2
  p2 <- 2 * r12 * (OR11 - l3) * (u1 - OR10)
  p3 <- 2 * r13 * (OR11 - l3) * (u2 - OR01)
  p4 <- 2 * r23 * (u1 - OR10) * (u2 - OR01)
  p5 <- p1 - p2 - p3 + p4
  p6 <- p5^0.5
  L <- 1 + OR11 - OR10 - OR01 - p6
  k1 <- (u3 - OR11)^2 + (OR10 - l1)^2 + (OR01 - l2)^2
  k2 <- 2 * r12 * (u3 - OR11) * (OR10 - l1)
  k3 <- 2 * r13 * (u3 - OR11) * (OR01 - l2)
  k4 <- 2 * r23 * (OR10 - l1) * (OR01 - l2)
  k5 <- (k1 - k2 - k3 + k4)^0.5
  U <- 1 + OR11 - OR10 - OR01 + k5
  p.RERI <- NA
  theta1 <- 1/exp(b1 + b2 + b3)
  theta2 <- 1/exp(b2 + b3)
  theta3 <- 1/exp(b1 + b3)
  AP <- theta1 - theta2 - theta3 + 1
  APr12 <- (cov12 + cov13 + v2 + (2 * cov23) + v3)/sqrt(v23 * 
                                                          v123)
  APr13 <- (v1 + cov12 + (2 * cov13) + cov23 + v3)/sqrt(v13 * 
                                                          v123)
  APr23 <- (cov12 + cov23 + cov13 + v3)/sqrt(v23 * v13)
  APl1 <- theta1 * exp(-z * sqrt(v123))
  APu1 <- theta1 * exp(z * sqrt(v123))
  APl2 <- theta2 * exp(-z * sqrt(v23))
  APu2 <- theta2 * exp(z * sqrt(v23))
  APl3 <- theta3 * exp(-z * sqrt(v13))
  APu3 <- theta3 * exp(z * sqrt(v13))
  APp1 <- (theta1 - APl1)^2 + (APu2 - theta2)^2 + (APu3 - 
                                                     theta3)^2
  APp2 <- 2 * APr12 * (theta1 - APl1) * (APu2 - theta2)
  APp3 <- 2 * APr13 * (theta1 - APl1) * (APu3 - theta3)
  APp4 <- 2 * APr23 * (APu2 - theta2) * (APu3 - theta3)
  APp5 <- APp1 - APp2 - APp3 + APp4
  APp6 <- APp5^0.5
  APL <- 1 + theta1 - theta2 - theta3 - APp6
  APk1 <- (APu1 - theta1)^2 + (theta2 - APl2)^2 + (theta3 - 
                                                     APl3)^2
  APk2 <- 2 * APr12 * (APu1 - theta1) * (theta2 - APl2)
  APk3 <- 2 * APr13 * (APu1 - theta1) * (theta3 - APl3)
  APk4 <- 2 * APr23 * (theta2 - APl2) * (theta3 - APl3)
  APk5 <- (APk1 - APk2 - APk3 + APk4)^0.5
  APU <- 1 + theta1 - theta2 - theta3 + APk5
  p.AP <- NA
  SItheta1 <- log((exp(b1 + b2 + b3) - 1))
  SItheta2 <- log((exp(b1) + exp(b2) - 2))
  lnSI <- SItheta1 - SItheta2
  SI <- exp(lnSI)
  vSItheta1 <- (exp(b1 + b2 + b3)/(exp(b1 + b2 + b3) - 
                                     1))^2 * v123
  vSItheta2 <- ((exp(2 * b1) * v1) + (exp(2 * b2) * v2) + 
                  (2 * exp(b1 + b2) * cov12))/(exp(b1) + exp(b2) - 
                                                 2)^2
  SIl1 <- SItheta1 - z * sqrt(vSItheta1)
  SIu1 <- SItheta1 + z * sqrt(vSItheta1)
  SIl2 <- SItheta2 - z * sqrt(vSItheta2)
  SIu2 <- SItheta2 + z * sqrt(vSItheta2)
  SIr <- ((exp(b1) * (v1 + cov12 + cov13)) + (exp(b2) * 
                                                (cov12 + v2 + cov23)))/sqrt(v123 * ((exp(2 * b1) * 
                                                                                       v1) + (exp(2 * b2) * v2) + (2 * exp(b1 + b2) * cov12)))
  lnSIL <- (SItheta1 + (-SItheta2)) - sqrt((SItheta1 - 
                                              SIl1)^2 + ((-SItheta2) - (-SIl2))^2 + (2 * SIr * 
                                                                                       (SItheta1 - SIl1) * ((-SItheta2) - (-SIl2))))
  lnSIU <- (SItheta1 + (-SItheta2)) + sqrt((SIu1 - SItheta1)^2 + 
                                             ((-SIu2) - (-SItheta2))^2 + (2 * SIr * (SIu1 - SItheta1) * 
                                                                            ((-SIu2) - (-SItheta2))))
  SIL <- exp(lnSIL)
  SIU <- exp(lnSIU)
  p.SI <- NA
  d <- data.frame(Measures = c("OR00", "OR01", "OR10", 
                               "OR11", paste("OR(", beta2, " on outcome [", beta1, 
                                             "==0]", sep = ""), paste("OR(", beta2, " on outcome [", 
                                                                      beta1, "==1]", sep = ""), paste("OR(", beta1, 
                                                                                                      " on outcome [", beta2, "==0]", sep = ""), paste("OR(", 
                                                                                                                                                       beta1, " on outcome [", beta2, "==1]", sep = ""), 
                               "Multiplicative scale", "RERI", "AP", "SI"), Estimates = c(OR00, 
                                                                                          OR01, OR10, OR11, OR01, OR_X1, OR10, OR_A1, OR_M, 
                                                                                          RERI, AP, SI), CI.ll = c(NA, l2, l1, l3, l2, CI.ll_OR_X1, 
                                                                                                                   l1, CI.ll_OR_A1, CI.ll_OR_M, L, APL, SIL), CI.ul = c(NA, 
                                                                                                                                                                        u2, u1, u3, u2, CI.ul_OR_X1, u1, CI.ul_OR_A1, CI.ul_OR_M, 
                                                                                                                                                                        U, APU, SIU), p = c(NA, p.OR01, p.OR10, p.OR11, p.OR01, 
                                                                                                                                                                                            p.OR_X1, p.OR10, p.OR_A1, p.OR_M, p.RERI, p.AP, p.SI))
  rownames(d) <- NULL
  print(d)
}

##load data
dat.mod<-read_xlsx("modification.xlsx")
dat.mod$OWOB<-as.factor(dat.mod$OWOB)
dat.mod$sVNT_60percent_group[dat.mod$sVNT_60percent_group=="High"]<-1
dat.mod$sVNT_60percent_group[dat.mod$sVNT_60percent_group=="Low"]<-0
dat.mod$sVNT_60percent_group[dat.mod$sVNT_60percent_group=="High"]<-1
dat.mod$sVNT_60percent_group[dat.mod$sVNT_60percent_group=="Low"]<-0
dat.mod$sVNT_60percent_group<-as.factor(dat.mod$sVNT_60percent_group)
dat.mod$sVNT_60percent_group<-as.factor(dat.mod$sVNT_60percent_group)
dat.mod.sin<-dat.mod[dat.mod$Vac_Group=="SinoVac",] ##SinoVac dataset

for (i in c("s__Bifidobacterium_adolescentis","s__Alistipes_putredinis",
            "s__Adlercreutzia_equolifaciens", "s__Oscillibacter_sp_57_20","s__Asaccharobacter_celatus",
            "s__Ruminococcus_sp_CAG_330",  "s__Intestinibacter_bartlettii","s__Lactococcus_petauri",
            "s__Mitsuokella_multacida", "s__Butyricimonas_virosa","s__Blautia_hydrogenotrophica",
            "s__Paraprevotella_xylaniphila", "s__Ruminococcus_gnavus","s__Bacteroides_thetaiotaomicron",
            "s__Bacteroides_vulgatus") ) {
  dat.mod.sin$mid<-dat.mod.sin[,colnames(dat.mod.sin)==i]
  dat.mod.sin$mid<-ifelse (dat.mod.sin$mid>median(dat.mod.sin$mid),0,1)
  dat.mod.sin[,colnames(dat.mod.sin)==i]<-dat.mod.sin$mid
  dat.mod.sin[,colnames(dat.mod.sin)==i]<-as.factor(dat.mod.sin[,colnames(dat.mod.sin)==i])
}



model<-glm(sVNT_60percent_group~s__Bifidobacterium_adolescentis*OWOB+Age,data = dat.mod.sin,family ="binomial")
model.1<-glm(sVNT_60percent_group~s__Adlercreutzia_equolifaciens*OWOB+Age,data = dat.mod.sin,family ="binomial")
model.2<-glm(sVNT_60percent_group~s__Asaccharobacter_celatus*OWOB+Age,data = dat.mod.sin,family ="binomial")
model.3<-glm(sVNT_60percent_group~s__Butyricimonas_virosa*OWOB+Age,data = dat.mod.sin,family ="binomial")

modification(model,"s__Bifidobacterium_adolescentis","OWOB")
modification(model.1,"s__Adlercreutzia_equolifaciens","OWOB")
modification(model.2,"s__Asaccharobacter_celatus","OWOB")
modification(model.3,"s__Butyricimonas_virosa","OWOB")

