################################################################################
## Project: MH outcomes following IPVA
## Script purpose: Taking dataset (that includes RF and IPVA variables as well 
## as MH), tabulating simple counts, difference-in-difference analysis, and IPW 
## analysis
## Date: 26th September 2019
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

################################################################################
# 0. Locations, packages, functions

# Note that in Cmder, need to set drive:
cd C:\Program Files\R\R-3.5.3\bin
R

# install.packages("readstata13")
# install.packages("varhandle")

packages<-c('digest','readstata13','data.table','tidyr','formattable','tidyverse',
	'dplyr','gdata','foreign','readxl','matrixStats','tableone','Rcmdr','mice',
	'magrittr','varhandle','zoo','mice','backports','olsrr')

source("http://bioconductor.org/biocLite.R")

for(pkg in packages){
  if(!require(pkg,character.only=TRUE)){
    BiocInstaller::biocLite(pkg,suppressUpdates=TRUE)
    library(pkg,character.only=TRUE)
    }
  }

rm(pkg,packages)


################################################################################

## ---- Clear global R environment----------------------------------------------
rm(list=ls())


## ---- Location files----------------------------------------------------------
loc_inp<-'W:/data/'
loc_out<-'C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/IPV project/p5 - MH outcomes/results'
setwd(loc_out)


################################################################################
# 1. Get and prep data  
################################################################################

# Load data, selecting those who do not have missing sex (n=1), 
# and who did not say they were exposed to vic at 0-17 years old (n=487)
cohort.dta <- read.dta13("W:/data/cohort/current_IPVA_cohort_id.dta")
cohort.dta <- cohort.dta[is.na(cohort.dta$kz021_new)==FALSE 
					& cohort.dta$vic_017!=1
					,]
# Declare mental health outcomes, for now
mh <- c("anxiety_f24","depress_ypc","depress_f24","depress_ype")
		#,"suicatt_26",
		#"psych_f24","eatd_ypd","hazalc_ypb","hazalc_f24",
		#"cann_ypb","drug_ypb")

for (m in mh){
	cohort.dta[,m] <- factor(cohort.dta[,m])
	}

#Binary SES
ses <- 	c("bimd2010q5","ccrimd2010q5","ccsimd2010q5","cctimd2010q5","ccrimd2010q5",
		"jan2011imd2010q5_YP","jan2014imd2010q5_YP")

for (s in ses){
	cohort.dta[,paste0(s,"_bin")] <- NA
	cohort.dta[,paste0(s,"_bin")][as.numeric(cohort.dta[,s])>=1 & as.numeric(cohort.dta[,s])<=3] <- 0
	cohort.dta[,paste0(s,"_bin")][as.numeric(cohort.dta[,s])>=4 & as.numeric(cohort.dta[,s])<=5] <- 1
	cohort.dta[,paste0(s,"_bin")] <- factor(cohort.dta[,paste0(s,"_bin")], levels = c(0,1,NA))
	}

#RSB and hospitalisation, get rid of NAs
cohort.dta$rsb[is.na(cohort.dta$rsb)==TRUE]<-0
cohort.dta$hosp[is.na(cohort.dta$hosp)==TRUE]<-0

#List of covariates for models
cov_list <- c("bimd2010q5_bin","ethnicity","sex_min_tf3","mfqtot_ccs",#"depress_ccs",
			"anxiety_tf4",
			#Removing sh_tf3 as only 20 = 1 in girls, none in boys
			#"sh_tf3",
			"asb_ccq","smok_ccs","hazalc_tf4","cann_ccs","drug_ccs","rsb",
			"ext_patmon","hosp","physical_abu","sexual_abuse","emotional_ab",
			"emotional_ne","bullying_0_1","violence_bet","mentl_hlth_p",
			"parental_sep","parent_child_bond_0_16yrs",
			#Now add in risk factors for depression
			"selfest_ccxd","ow_tf4",)

covs <- cov_list[1]
for (c in 2:length(cov_list) ) {
	covs <- paste0(covs," + ",cov_list[c])
	}

#Check distribution of MFQ scores:
hist(cohort.dta$mfqtot_ypc)
hist(log(cohort.dta$mfqtot_ypc))
#Include log(MFQ)
cohort.dta$logish_mfqtot_ypc <- log(cohort.dta$mfqtot_ypc+1)

################################################################################
# 2. Tabulate numbers of outcomes for those exposed and unexposed to vic and per at 18-21
################################################################################
vic_stats <- print(
	           CreateCatTable(vars = c(cov_list,"depress_ccs","depress_ypc","anxiety_f24"), data = cohort.dta[cohort.dta$vic_017!=1,], strata = c("kz021_new","vic_1821"), includeNA = TRUE),
	             showAlllevels = TRUE,
	             quote = FALSE,
	             test = FALSE)
colnames(vic_stats) <- (c("Male, not vic","Female, not vic","Male, vic","Female, vic"))
vic_stats <- vic_stats[,c(1,3,2,4)]

#No longer using perp stuff
# per_stats <- print(
# 	           CreateCatTable(vars = mh, data = cohort.dta, strata = c("kz021_new","per_1821"), includeNA = TRUE),
# 	             showAlllevels = TRUE,
# 	             quote = FALSE,
# 	             test = FALSE)
# colnames(per_stats) <- (c("Male, no per","Female, no per","Male, no per","Female, no per"))
# per_stats <- per_stats[,c(1,3,2,4)]

write.csv(vic_stats, "covs_vic_ns.csv", row.names = TRUE)


################################################################################
# 3. Difference-in-difference analysis (similar to approach taken by Clayton et al 
# with cardiovascular outcomes in pregancy), for causal effect of IPVA on depression 
# (this is possible because we have numerical depression scores, but we don't have 
# numerical anxiety scores)
################################################################################

#Depression scores
#f10 = 10y, tf1 = 12y, tf2 = 13y, ccs = 16y, ccxd = 17.5, cct = 18, ypa = 21, ypb = 22+, ypc = 23+
#mfqtot_f10 mfqtot_tf1 mfqtot_tf2 mfqtot_ccs mfqtot_ccxd mfqtot_cct mfqtot_ypa mfqtot_ypb mfqtot_ypc
#Pull these in in Stata first and then out to R (don't have ccxd, ypa, or ypb at the minute)
#Plot trajectories for the IPVA group and non-IPVA group

#Check relationship between mfqtot_ccs and mfqtot_ypc:
plot(cohort.dta$mfqtot_ccs, cohort.dta$mfqtot_ypc)
summary(lm(mfqtot_ypc ~ mfqtot_ccs, data = cohort.dta[cohort.dta$kz021_new==1,]))
summary(lm(mfqtot_ypc ~ mfqtot_ccs, data = cohort.dta[cohort.dta$kz021_new==2,]))

#want DID where depression is pre- 18-21 and post- 18-21, so use ccs and ypc
#Have put ccs and ypc next to each other as might help with reshape later
cohort.dta_sub <- cohort.dta[,c("aln","qlet","kz021_new",cov_list,#"vic_017",
	 							"vic_1821", #"per_017","per_1821",
	 							"mfqtot_ccs","mfqtot_ypc" #,"mfqtot_f10","mfqtot_tf1","mfqtot_tf2",
	 							#"mfqtot_ccxd","mfqtot_cct","mfqtot_ypa","mfqtot_ypb"
	 							)]

#Log values?
#cohort.dta_sub$ln_mfqtot_ccs <- log(cohort.dta_sub$mfqtot_ccs)
#cohort.dta_sub$ln_mfqtot_ypc <- log(cohort.dta_sub$mfqtot_ypc)

data_long <- gather(cohort.dta_sub, time, depress, mfqtot_ccs:mfqtot_ypc, factor_key=FALSE)
data_long$time[data_long$time=="mfqtot_ccs"] <- 0
data_long$time[data_long$time=="mfqtot_ypc"] <- 1

data_long[,"time"] <- as.numeric(data_long[,"time"])

#Number of models per sex (we'll include CCA and imputed on raw mfq scores, and then CCA and imputed on log mfq scores) 
nmodel <- 4
modelname1 <- "MFQ, CCA"
modelname2 <- "MFQ, MI"
modelname3 <- "logMFQ, CCA"
modelname4 <- "logMFQ, MI"
#Create a table for results
out <- data.frame(matrix(NA, nrow = (nmodel*3)+1, ncol = 12))
colnames(out) <- c(rep("",2),"Males",rep("",4),"Females",rep("",4))
out[1, ] <- c("Model","Variable",rep(c("N obs","Coef","LCI","UCI","p-value"),2))
out[2:((nmodel*3)+1),1] <- c(rep(modelname1,3),rep(modelname2,3),rep(modelname3,3),rep(modelname4,3))
out[2:((nmodel*3)+1),2] <- rep(c("Vic","Time","Vic*Time"),nmodel)
level <- 0

#First model (first 3 rows):
model <- paste0("depress ~ vic_1821*time")
didreg_boys = lm(as.formula(model), data = data_long[data_long$kz021_new==1 & is.na(data_long$aln)==FALSE,])
summary(didreg_boys)
out[2+(level*3),3] <- nobs(didreg_boys)/2
out[2+(level*3),4] <- round(summary(didreg_boys)$coef[2,1],2)
out[2+(level*3),5] <- round(summary(didreg_boys)$coef[2,1]-1.96*summary(didreg_boys)$coef[2,2],2)
out[2+(level*3),6] <- round(summary(didreg_boys)$coef[2,1]+1.96*summary(didreg_boys)$coef[2,2],2)
out[2+(level*3),7] <- round(summary(didreg_boys)$coef[2,4],5)

out[3+(level*3),4] <- round(summary(didreg_boys)$coef[3,1],2)
out[3+(level*3),5] <- round(summary(didreg_boys)$coef[3,1]-1.96*summary(didreg_boys)$coef[3,2],2)
out[3+(level*3),6] <- round(summary(didreg_boys)$coef[3,1]+1.96*summary(didreg_boys)$coef[3,2],2)
out[3+(level*3),7] <- round(summary(didreg_boys)$coef[3,4],5)

out[4+(level*3),4] <- round(summary(didreg_boys)$coef[4,1],2)
out[4+(level*3),5] <- round(summary(didreg_boys)$coef[4,1]-1.96*summary(didreg_boys)$coef[4,2],2)
out[4+(level*3),6] <- round(summary(didreg_boys)$coef[4,1]+1.96*summary(didreg_boys)$coef[4,2],2)
out[4+(level*3),7] <- round(summary(didreg_boys)$coef[4,4],5)

didreg_girls = lm(as.formula(model), data = data_long[data_long$kz021_new==2 & is.na(data_long$aln)==FALSE,])
summary(didreg_girls)
out[2+(level*3),8] <- nobs(didreg_girls)/2
out[2+(level*3),9] <- round(summary(didreg_girls)$coef[2,1],2)
out[2+(level*3),10] <- round(summary(didreg_girls)$coef[2,1]-1.96*summary(didreg_girls)$coef[2,2],2)
out[2+(level*3),11] <- round(summary(didreg_girls)$coef[2,1]+1.96*summary(didreg_girls)$coef[2,2],2)
out[2+(level*3),12] <- round(summary(didreg_girls)$coef[2,4],5)

out[3+(level*3),9] <- round(summary(didreg_girls)$coef[3,1],2)
out[3+(level*3),10] <- round(summary(didreg_girls)$coef[3,1]-1.96*summary(didreg_girls)$coef[3,2],2)
out[3+(level*3),11] <- round(summary(didreg_girls)$coef[3,1]+1.96*summary(didreg_girls)$coef[3,2],2)
out[3+(level*3),12] <- round(summary(didreg_girls)$coef[3,4],5)

out[4+(level*3),9] <- round(summary(didreg_girls)$coef[4,1],2)
out[4+(level*3),10] <- round(summary(didreg_girls)$coef[4,1]-1.96*summary(didreg_girls)$coef[4,2],2)
out[4+(level*3),11] <- round(summary(didreg_girls)$coef[4,1]+1.96*summary(didreg_girls)$coef[4,2],2)
out[4+(level*3),12] <- round(summary(didreg_girls)$coef[4,4],5)

level <- level+1

#Imputation of depress values (including other auxilliary variables)
data_long_girls <- data_long[data_long$kz021_new==2 & is.na(data_long$aln)==FALSE,]
data_long_boys <- data_long[data_long$kz021_new==1 & is.na(data_long$aln)==FALSE,]

ini <- mice(data_long_girls, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet","kz021_new")
pred_mat[,exclude] <- 0
meth[exclude] <- ""

Sys.time()
nit <- 50
imp_girls_long<- mice(data_long_girls, m=nit, maxit=10,
					method=meth, predictorMatrix=pred_mat)
#imp_boys_long<- mice(data_long_boys, m=nit, maxit=10,
#					method=meth, predictorMatrix=pred_mat)
Sys.time()

#Check convergence?
imp_plot <- imp_girls_long[[1]]
for (n in 2:length(imp_girls_long)){
	imp_plot <- ibind(imp_plot, imp_girls_long[[n]])
	}
pdf('check_imp_girls.pdf')
plot(imp_plot)
dev.off()

model <- paste0("depress ~ vic_1821*time")

didreg_boys = pool(with(imp_boys_long, exp = lm(as.formula(model))))
out[2+(level*3),3] <- (dim(imp_boys_long$data)[1])
out[2+(level*3),4] <- round(summary(didreg_boys)[2,2],2)
out[2+(level*3),5] <- round(summary(didreg_boys)[2,2]-1.96*summary(didreg_boys)[2,3],2)
out[2+(level*3),6] <- round(summary(didreg_boys)[2,2]+1.96*summary(didreg_boys)[2,3],2)
out[2+(level*3),7] <- round(summary(didreg_boys)[2,6],5)

out[3+(level*3),4] <- round(summary(didreg_boys)[3,2],2)
out[3+(level*3),5] <- round(summary(didreg_boys)[3,2]-1.96*summary(didreg_boys)[3,3],2)
out[3+(level*3),6] <- round(summary(didreg_boys)[3,2]+1.96*summary(didreg_boys)[3,3],2)
out[3+(level*3),7] <- round(summary(didreg_boys)[3,6],5)

out[4+(level*3),4] <- round(summary(didreg_boys)[4,2],2)
out[4+(level*3),5] <- round(summary(didreg_boys)[4,2]-1.96*summary(didreg_boys)[4,3],2)
out[4+(level*3),6] <- round(summary(didreg_boys)[4,2]+1.96*summary(didreg_boys)[4,3],2)
out[4+(level*3),7] <- round(summary(didreg_boys)[4,6],5)

didreg_girls = pool(with(imp_girls_long, exp = lm(as.formula(model))))
out[2+(level*3),8] <- (dim(imp_girls_long$data)[1])
out[2+(level*3),9] <- round(summary(didreg_girls)[2,2],2)
out[2+(level*3),10] <- round(summary(didreg_girls)[2,2]-1.96*summary(didreg_girls)[2,3],2)
out[2+(level*3),11] <- round(summary(didreg_girls)[2,2]+1.96*summary(didreg_girls)[2,3],2)
out[2+(level*3),12] <- round(summary(didreg_girls)[2,6],5)

out[3+(level*3),9] <- round(summary(didreg_girls)[3,2],2)
out[3+(level*3),10] <- round(summary(didreg_girls)[3,2]-1.96*summary(didreg_girls)[3,3],2)
out[3+(level*3),11] <- round(summary(didreg_girls)[3,2]+1.96*summary(didreg_girls)[3,3],2)
out[3+(level*3),12] <- round(summary(didreg_girls)[3,6],5)

out[4+(level*3),9] <- round(summary(didreg_girls)[4,2],2)
out[4+(level*3),10] <- round(summary(didreg_girls)[4,2]-1.96*summary(didreg_girls)[4,3],2)
out[4+(level*3),11] <- round(summary(didreg_girls)[4,2]+1.96*summary(didreg_girls)[4,3],2)
out[4+(level*3),12] <- round(summary(didreg_girls)[4,6],5)
level <- level+1

#Now with logged outcome
data_long <- gather(cohort.dta_sub, time, depress, mfqtot_ccs:mfqtot_ypc, factor_key=FALSE)
data_long$depress2 <- log(data_long$depress+1)
data_long$time[data_long$time=="mfqtot_ccs"] <- 0
data_long$time[data_long$time=="mfqtot_ypc"] <- 1
data_long[,"time"] <- as.numeric(data_long[,"time"])
data_long_girls <- data_long[data_long$kz021_new==2 & is.na(data_long$aln)==FALSE,]
data_long_boys <- data_long[data_long$kz021_new==1 & is.na(data_long$aln)==FALSE,]

ini <- mice(data_long_girls, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet","kz021_new")
pred_mat[,exclude] <- 0
meth[exclude] <- ""

Sys.time()
nit <- 50
imp_girls_long<- mice(data_long_girls, m=nit, maxit=10,
					method=meth, predictorMatrix=pred_mat)
imp_boys_long<- mice(data_long_boys, m=nit, maxit=10,
					method=meth, predictorMatrix=pred_mat)
Sys.time()

model <- paste0("depress2 ~ vic_1821*time")
didreg_boys = pool(with(imp_boys_long, exp = lm(as.formula(model))))
out[2+(level*3),3] <- (dim(imp_boys_long$data)[1])
out[2+(level*3),4] <- round(exp(summary(didreg_boys)[2,2]),2)
out[2+(level*3),5] <- round(exp(summary(didreg_boys)[2,2]-1.96*summary(didreg_boys)[2,3]),2)
out[2+(level*3),6] <- round(exp(summary(didreg_boys)[2,2]+1.96*summary(didreg_boys)[2,3]),2)
out[2+(level*3),7] <- round(summary(didreg_boys)[2,6],5)

out[3+(level*3),4] <- round(exp(summary(didreg_boys)[3,2]),2)
out[3+(level*3),5] <- round(exp(summary(didreg_boys)[3,2]-1.96*summary(didreg_boys)[3,3]),2)
out[3+(level*3),6] <- round(exp(summary(didreg_boys)[3,2]+1.96*summary(didreg_boys)[3,3]),2)
out[3+(level*3),7] <- round(summary(didreg_boys)[3,6],5)

out[4+(level*3),4] <- round(exp(summary(didreg_boys)[4,2]),2)
out[4+(level*3),5] <- round(exp(summary(didreg_boys)[4,2]-1.96*summary(didreg_boys)[4,3]),2)
out[4+(level*3),6] <- round(exp(summary(didreg_boys)[4,2]+1.96*summary(didreg_boys)[4,3]),2)
out[4+(level*3),7] <- round(summary(didreg_boys)[4,6],5)

didreg_girls = pool(with(imp_girls_long, exp = lm(as.formula(model))))
out[2+(level*3),8] <- (dim(imp_girls_long$data)[1])
out[2+(level*3),9] <- round(exp(summary(didreg_girls)[2,2]),2)
out[2+(level*3),10] <- round(exp(summary(didreg_girls)[2,2]-1.96*summary(didreg_girls)[2,3]),2)
out[2+(level*3),11] <- round(exp(summary(didreg_girls)[2,2]+1.96*summary(didreg_girls)[2,3]),2)
out[2+(level*3),12] <- round(summary(didreg_girls)[2,6],5)

out[3+(level*3),9] <- round(exp(summary(didreg_girls)[3,2]),2)
out[3+(level*3),10] <- round(exp(summary(didreg_girls)[3,2]-1.96*summary(didreg_girls)[3,3]),2)
out[3+(level*3),11] <- round(exp(summary(didreg_girls)[3,2]+1.96*summary(didreg_girls)[3,3]),2)
out[3+(level*3),12] <- round(summary(didreg_girls)[3,6],5)

out[4+(level*3),9] <- round(exp(summary(didreg_girls)[4,2]),2)
out[4+(level*3),10] <- round(exp(summary(didreg_girls)[4,2]-1.96*summary(didreg_girls)[4,3]),2)
out[4+(level*3),11] <- round(exp(summary(didreg_girls)[4,2]+1.96*summary(didreg_girls)[4,3]),2)
out[4+(level*3),12] <- round(summary(didreg_girls)[4,6],5)

#level <- level+1
write.csv(out, "did_vic1821_dep_202007.csv", row.names = FALSE, na = "")


#Did also look at time-points 10, 12, 13, and 16, but decided the earlier ages were a funny time
#Just look at ages 13 and 16
cohort.dta_sub <- cohort.dta[,c("aln","qlet","kz021_new",cov_list, #"vic_017",
							"vic_1821",	"mfqtot_tf2","mfqtot_ccs", #"mfqtot_f10","mfqtot_tf1",
							#"mfqtot_ccxd","mfqtot_cct","mfqtot_ypa","mfqtot_ypb","mfqtot_ypc"
							)]
data_long <- gather(cohort.dta_sub, time, depress, mfqtot_tf2:mfqtot_ccs, factor_key=FALSE)
data_long$time[data_long$time=="mfqtot_tf2"] <- 0
data_long$time[data_long$time=="mfqtot_ccs"] <- 1

data_long[,"time"] <- as.numeric(data_long[,"time"])
nmodel <- 2
out <- data.frame(matrix(NA, nrow = (nmodel*3)+1, ncol = 12))
colnames(out) <- c(rep("",2),"Males",rep("",4),"Females",rep("",4))
out[1, ] <- c("Model","Variable",rep(c("N obs","Coef","LCI","UCI","p-value"),2))
out[2:((nmodel*3)+1),1] <- c(rep("1: Vic 18-21, CCA",3),
								#rep("2: Vic 18-21 no prior vic, CCA",3),
							 rep("2: Vic 18-21, MI",3))
out[2:((nmodel*3)+1),2] <- rep(c("Vic","Time","Vic*Time"),nmodel)
level <- 0
model <- paste0("depress ~ vic_1821*time")
didreg_boys = lm(as.formula(model), data = data_long[data_long$kz021_new==1 & is.na(data_long$aln)==FALSE,])
summary(didreg_boys)
out[2+(level*3),3] <- nobs(didreg_boys)/2
out[2+(level*3),4] <- round(summary(didreg_boys)$coef[2,1],2)
out[2+(level*3),5] <- round(summary(didreg_boys)$coef[2,1]-1.96*summary(didreg_boys)$coef[2,2],2)
out[2+(level*3),6] <- round(summary(didreg_boys)$coef[2,1]+1.96*summary(didreg_boys)$coef[2,2],2)
out[2+(level*3),7] <- round(summary(didreg_boys)$coef[2,4],2)
out[3+(level*3),4] <- round(summary(didreg_boys)$coef[3,1],2)
out[3+(level*3),5] <- round(summary(didreg_boys)$coef[3,1]-1.96*summary(didreg_boys)$coef[3,2],2)
out[3+(level*3),6] <- round(summary(didreg_boys)$coef[3,1]+1.96*summary(didreg_boys)$coef[3,2],2)
out[3+(level*3),7] <- round(summary(didreg_boys)$coef[3,4],2)
out[4+(level*3),4] <- round(summary(didreg_boys)$coef[4,1],2)
out[4+(level*3),5] <- round(summary(didreg_boys)$coef[4,1]-1.96*summary(didreg_boys)$coef[4,2],2)
out[4+(level*3),6] <- round(summary(didreg_boys)$coef[4,1]+1.96*summary(didreg_boys)$coef[4,2],2)
out[4+(level*3),7] <- round(summary(didreg_boys)$coef[4,4],2)
didreg_girls = lm(as.formula(model), data = data_long[data_long$kz021_new==2 & is.na(data_long$aln)==FALSE,])
summary(didreg_girls)
out[2+(level*3),8] <- nobs(didreg_girls)/2
out[2+(level*3),9] <- round(summary(didreg_girls)$coef[2,1],2)
out[2+(level*3),10] <- round(summary(didreg_girls)$coef[2,1]-1.96*summary(didreg_girls)$coef[2,2],2)
out[2+(level*3),11] <- round(summary(didreg_girls)$coef[2,1]+1.96*summary(didreg_girls)$coef[2,2],2)
out[2+(level*3),12] <- round(summary(didreg_girls)$coef[2,4],2)
out[3+(level*3),9] <- round(summary(didreg_girls)$coef[3,1],2)
out[3+(level*3),10] <- round(summary(didreg_girls)$coef[3,1]-1.96*summary(didreg_girls)$coef[3,2],2)
out[3+(level*3),11] <- round(summary(didreg_girls)$coef[3,1]+1.96*summary(didreg_girls)$coef[3,2],2)
out[3+(level*3),12] <- round(summary(didreg_girls)$coef[3,4],2)
out[4+(level*3),9] <- round(summary(didreg_girls)$coef[4,1],2)
out[4+(level*3),10] <- round(summary(didreg_girls)$coef[4,1]-1.96*summary(didreg_girls)$coef[4,2],2)
out[4+(level*3),11] <- round(summary(didreg_girls)$coef[4,1]+1.96*summary(didreg_girls)$coef[4,2],2)
out[4+(level*3),12] <- round(summary(didreg_girls)$coef[4,4],2)
level <- level+1

data_long_girls <- data_long[data_long$kz021_new==2 & is.na(data_long$aln)==FALSE,]
data_long_boys <- data_long[data_long$kz021_new==1 & is.na(data_long$aln)==FALSE,]

## ---- Insert a line here to cut down number of variables used in MI -------------------------------------------------------------------

ini <- mice(data_long_girls, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet","kz021_new")
pred_mat[,exclude] <- 0
meth[exclude] <- ""

Sys.time()
nit <- 50
imp_girls_long<- mice(data_long_girls, m=nit, maxit=10,
					method=meth, predictorMatrix=pred_mat)
imp_boys_long<- mice(data_long_boys, m=nit, maxit=10,
					method=meth, predictorMatrix=pred_mat)
Sys.time()

model <- paste0("depress ~ vic_1821*time")
didreg_boys = pool(with(imp_boys_long, exp = lm(as.formula(model))))
out[2+(level*3),3] <- (dim(imp_boys_long$data)[1])/2
out[2+(level*3),4] <- round(summary(didreg_boys)[2,2],2)
out[2+(level*3),5] <- round(summary(didreg_boys)[2,2]-1.96*summary(didreg_boys)[2,3],2)
out[2+(level*3),6] <- round(summary(didreg_boys)[2,2]+1.96*summary(didreg_boys)[2,3],2)
out[2+(level*3),7] <- round(summary(didreg_boys)[2,6],2)

out[3+(level*3),4] <- round(summary(didreg_boys)[3,2],2)
out[3+(level*3),5] <- round(summary(didreg_boys)[3,2]-1.96*summary(didreg_boys)[3,3],2)
out[3+(level*3),6] <- round(summary(didreg_boys)[3,2]+1.96*summary(didreg_boys)[3,3],2)
out[3+(level*3),7] <- round(summary(didreg_boys)[3,6],2)

out[4+(level*3),4] <- round(summary(didreg_boys)[4,2],2)
out[4+(level*3),5] <- round(summary(didreg_boys)[4,2]-1.96*summary(didreg_boys)[4,3],2)
out[4+(level*3),6] <- round(summary(didreg_boys)[4,2]+1.96*summary(didreg_boys)[4,3],2)
out[4+(level*3),7] <- round(summary(didreg_boys)[4,6],2)

didreg_girls = pool(with(imp_girls_long, exp = lm(as.formula(model))))
out[2+(level*3),8] <- (dim(imp_girls_long$data)[1])/2
out[2+(level*3),9] <- round(summary(didreg_girls)[2,2],2)
out[2+(level*3),10] <- round(summary(didreg_girls)[2,2]-1.96*summary(didreg_girls)[2,3],2)
out[2+(level*3),11] <- round(summary(didreg_girls)[2,2]+1.96*summary(didreg_girls)[2,3],2)
out[2+(level*3),12] <- round(summary(didreg_girls)[2,6],2)

out[3+(level*3),9] <- round(summary(didreg_girls)[3,2],2)
out[3+(level*3),10] <- round(summary(didreg_girls)[3,2]-1.96*summary(didreg_girls)[3,3],2)
out[3+(level*3),11] <- round(summary(didreg_girls)[3,2]+1.96*summary(didreg_girls)[3,3],2)
out[3+(level*3),12] <- round(summary(didreg_girls)[3,6],2)

out[4+(level*3),9] <- round(summary(didreg_girls)[4,2],2)
out[4+(level*3),10] <- round(summary(didreg_girls)[4,2]-1.96*summary(didreg_girls)[4,3],2)
out[4+(level*3),11] <- round(summary(didreg_girls)[4,2]+1.96*summary(didreg_girls)[4,3],2)
out[4+(level*3),12] <- round(summary(didreg_girls)[4,6],2)

#level <- level+1
write.csv(out, "did_vic1821_dep_tf2toccs.csv", row.names = FALSE, na = "")


################################################################################
# 5. IPW - using pre-IPVA covariates to predict IPVA probability, and then
# and then a) inversely weighting IPVA in analyses (outcome either binary anxiety 
# or depression); b) creating a stratifiying variable
################################################################################

## ---- Outcome analysis without the propensity scores first -----------------------
for (c in cov_list){
	#Need to make all factors, while we're here
	cohort.dta[,c] <- factor(cohort.dta[,c])
	model <- paste0("vic_1821 ~", c)
	fit <- glm(as.formula(model), data = cohort.dta[cohort.dta$kz021_new==1,], family = "binomial")
	print(c)
	print(exp(fit$coef[2]))
	fit <- glm(as.formula(model), data = cohort.dta[cohort.dta$kz021_new==2,], family = "binomial")
	print(c)
	print(exp(fit$coef[2]))
	}

#Relationship outcome ~ treatment before weighting
vic_mfq23_fit <- lm(mfqtot_ypc ~ vic_1821, data = cohort.dta[cohort.dta$kz021_new==1,])
summary(vic_mfq23_fit)$coef[2,1]
summary(vic_mfq23_fit)$coef[2,1]-1.96*summary(vic_mfq23_fit)$coef[2,2]
summary(vic_mfq23_fit)$coef[2,1]+1.96*summary(vic_mfq23_fit)$coef[2,2]
round(summary(vic_mfq23_fit)$coef[2,4],5)
ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value

vic_dep23_fit <- glm(depress_ypc ~ vic_1821, data = cohort.dta[cohort.dta$kz021_new==1,], family = "binomial")
exp(vic_dep23_fit$coef[2])
exp(summary(vic_dep23_fit)$coef[2,1]-1.96*summary(vic_dep23_fit)$coef[2,2])
exp(summary(vic_dep23_fit)$coef[2,1]+1.96*summary(vic_dep23_fit)$coef[2,2])
round(summary(vic_dep23_fit)$coef[2,4],5)

vic_mfq23_fit <- lm(mfqtot_ypc ~ vic_1821, data = cohort.dta[cohort.dta$kz021_new==2,])
summary(vic_mfq23_fit)$coef[2,1]
summary(vic_mfq23_fit)$coef[2,1]-1.96*summary(vic_mfq23_fit)$coef[2,2]
summary(vic_mfq23_fit)$coef[2,1]+1.96*summary(vic_mfq23_fit)$coef[2,2]
round(summary(vic_mfq23_fit)$coef[2,4],5)
ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value

vic_dep23_fit <- glm(depress_ypc ~ vic_1821, data = cohort.dta[cohort.dta$kz021_new==2,], family = "binomial")
exp(vic_dep23_fit$coef[2])
exp(summary(vic_dep23_fit)$coef[2,1]-1.96*summary(vic_dep23_fit)$coef[2,2])
exp(summary(vic_dep23_fit)$coef[2,1]+1.96*summary(vic_dep23_fit)$coef[2,2])
round(summary(vic_dep23_fit)$coef[2,4],5)

#Boys: coef = 1.66 (0.64 to 2.68), OR = 1.55 (0.97 to 2.47), 
#Girls: coef = 1.58 (0.87 to 2.30), OR = 1.86 (1.41 to 2.47)


#Relationship outcome ~ treatment with regular adjustment
covs <- cov_list[1]
for (c in 2:length(cov_list) ) {
	covs <- paste0(covs," + ",cov_list[c])
	}
model <- paste0("mfqtot_ypc ~ vic_1821 +",covs)
vic_mfq23_fit <- glm(as.formula(model), data = cohort.dta[cohort.dta$kz021_new==1,], family = "gaussian")
summary(vic_mfq23_fit)$coef[2,1]
summary(vic_mfq23_fit)$coef[2,1]-1.96*summary(vic_mfq23_fit)$coef[2,2]
summary(vic_mfq23_fit)$coef[2,1]+1.96*summary(vic_mfq23_fit)$coef[2,2]

vic_mfq23_fit <- glm(as.formula(model), data = cohort.dta[cohort.dta$kz021_new==2,], family = "gaussian")
summary(vic_mfq23_fit)$coef[2,1]
summary(vic_mfq23_fit)$coef[2,1]-1.96*summary(vic_mfq23_fit)$coef[2,2]
summary(vic_mfq23_fit)$coef[2,1]+1.96*summary(vic_mfq23_fit)$coef[2,2]

model <- paste0("depress_ypc ~ vic_1821 +",covs)
vic_dep23_fit <- glm(as.formula(model), data = cohort.dta[cohort.dta$kz021_new==1,], family = "binomial")
exp(vic_dep23_fit$coef[2])
exp(summary(vic_dep23_fit)$coef[2,1]-1.96*summary(vic_dep23_fit)$coef[2,2])
exp(summary(vic_dep23_fit)$coef[2,1]+1.96*summary(vic_dep23_fit)$coef[2,2])

vic_dep23_fit <- glm(as.formula(model), data = cohort.dta[cohort.dta$kz021_new==2,], family = "binomial")
exp(vic_dep23_fit$coef[2])
exp(summary(vic_dep23_fit)$coef[2,1]-1.96*summary(vic_dep23_fit)$coef[2,2])
exp(summary(vic_dep23_fit)$coef[2,1]+1.96*summary(vic_dep23_fit)$coef[2,2])

#Boys: coef = 1.35 (-0.70 to 3.41), OR = 1.49 (0.34 to 6.60), 
#Girls: coef = 1.66 (0.32 to 3.00), OR = 2.23 (0.89 to 5.57)
#Bear in mind this isn't on the MI results.

#Logistic regression to calculate propensity scores
#'Crude' propensity scores
#Weights without MI
data_boys <- cohort.dta[cohort.dta$kz021_new==1,]
data_girls <- cohort.dta[cohort.dta$kz021_new==2,]

model <- paste0("vic_1821 ~",covs)
fit <- glm(as.formula(model), data=data_girls, family = binomial(link = "logit"))
data_girls$ps <- predict(fit,data_girls,type="response")
data_girls$wgt.ATE <- ifelse(
 		data_girls$vic_1821 == 1, 1/data_girls$ps,
 		1/(1-data_girls$ps)
 	)
fit <- glm(as.formula(model), data=data_boys, family = binomial(link = "logit"))
data_boys$ps <- predict(fit,data_boys,type="response")
data_boys$wgt.ATE <- ifelse(
 		data_boys$vic_1821 == 1, 1/data_boys$ps,
 		1/(1-data_boys$ps)
 	)

#Including Kolmogorv-S test
model <- paste0("mfqtot_ypc ~ vic_1821")
fit <- glm(as.formula(model), data=data_girls, family = "gaussian", weight = (wgt.ATE))
fit <- lm(as.formula(model), data=data_girls, weight = (wgt.ATE))
summary(fit)
#coef: 1.64 (SE: 0.56)
ols_test_normality(fit)$kolmogorv$p.value
data.res <- data.frame(matrix(NA, nrow = length(resid(fit)), ncol = 2))
data.res[,1] <- data_girls$wgt.ATE[is.na(data_girls$wgt.ATE)==FALSE]
data.res[,2] <- resid(fit)
names(data.res) <- c("wgt","res")
ggplot(data.res, aes(x = res, y = ..density..)) +
geom_density()

ggplot(data.res, aes(x = res, y = ..density.., weight = wgt)) +
geom_density()

#p = 9.2 x 10^-9
fit <- glm(as.formula(model), data=data_boys, family = "gaussian", weight = (wgt.ATE))
fit <- lm(as.formula(model), data=data_boys, weight = (wgt.ATE))
summary(fit)
#coef: 2.80 (SE: 0.80)
ols_test_normality(fit)$kolmogorv$p.value
#p = 0.0001

#Log outcome
model <- paste0("logish_mfqtot_ypc ~ vic_1821")
vic_mfq23_fit <- lm(as.formula(model), data=data_girls, weight = (wgt.ATE))
ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
#p = 0.04
exp(summary(vic_mfq23_fit)$coef[2,1])
exp(summary(vic_mfq23_fit)$coef[2,1]-1.96*summary(vic_mfq23_fit)$coef[2,2])
exp(summary(vic_mfq23_fit)$coef[2,1]+1.96*summary(vic_mfq23_fit)$coef[2,2])
#Girls: geometric mean 1.4 times higher (1.2 to 1.6)

vic_mfq23_fit <- lm(as.formula(model), data=data_boys, weight = (wgt.ATE))
ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
#p = 0.03
exp(summary(vic_mfq23_fit)$coef[2,1])
exp(summary(vic_mfq23_fit)$coef[2,1]-1.96*summary(vic_mfq23_fit)$coef[2,2])
exp(summary(vic_mfq23_fit)$coef[2,1]+1.96*summary(vic_mfq23_fit)$coef[2,2])
# Boys: geometric mean 1.5 times higher (1.2 to 1.9)

#Multiple imputation for missing data
#Derive depress_ypc after imputation
data_girls_sub <- data_girls[,c("aln","qlet",cov_list,"vic_1821",
							"mfqtot_ypc","logish_mfqtot_ypc")]
ini <- mice(data_girls_sub, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth

#Excluding binary depression as v. correlated with mfqtot vars anyway
#But not exluding mfqtot values, as mfqtot_ypc is the outcome
exclude<-c("aln","qlet"#,"vic_1821","per_1821_v2","mfqtot_ypc","depress_ypc"
			)
pred_mat[,exclude] <- 0
meth[exclude] <- ""

Sys.time()

nimp <- 50
imp_girls_sub <- mice(data_girls_sub, m=nimp, maxit=10,
						method=meth, predictorMatrix=pred_mat)
Sys.time()
#About five minutes?
#Save for later to avoid imputing all the time
imp_girls_sub2 <- imp_girls_sub

#Now want regular logistic regression dep ~ IPVA
vic_mfq23_fit <- with(imp_girls_sub, lm(mfqtot_ypc ~ vic_1821))
summary(pool(vic_mfq23_fit))[2,2]
summary(pool(vic_mfq23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3]
summary(pool(vic_mfq23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3]
round(summary(pool(vic_mfq23_fit))[2,6],5)
#Coef: 1.49 (0.77 to 2.21)
model <- paste0("mfqtot_ypc ~ vic_1821 + ", covs)
vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(model)))
summary(pool(vic_mfq23_fit))[2,2]
summary(pool(vic_mfq23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3]
summary(pool(vic_mfq23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3]
#0.90 (0.23 to 1.58)

#Logged outcome
model <- paste0("logish_mfqtot_ypc ~ vic_1821")
vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(model)))
exp(summary(pool(vic_mfq23_fit))[2,2])
exp(summary(pool(vic_mfq23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3])
exp(summary(pool(vic_mfq23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3])

model <- paste0("logish_mfqtot_ypc ~ vic_1821 + ",covs)
vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(model)))
exp(summary(pool(vic_mfq23_fit))[2,2])
exp(summary(pool(vic_mfq23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3])
exp(summary(pool(vic_mfq23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3])

a <- complete(imp_girls_sub, action='long',include=TRUE)
a[,"depress_ypc"] <- ifelse(
 		a$mfqtot_ypc <= 12, 0, 
 		1)
imp_girls_sub <- as.mids(a)

vic_dep23_fit <- with(imp_girls_sub, glm(depress_ypc ~ vic_1821, family = "binomial"))
exp(summary(pool(vic_dep23_fit))[2,2])
exp(summary(pool(vic_dep23_fit))[2,2]-1.96*summary(pool(vic_dep23_fit))[2,3])
exp(summary(pool(vic_dep23_fit))[2,2]+1.96*summary(pool(vic_dep23_fit))[2,3])
#OR: 1.59 (1.19 to 2.13)
model <- paste0("depress_ypc ~ vic_1821 + ", covs)
vic_dep23_fit <- with(imp_girls_sub, glm(as.formula(model), family = "binomial"))
exp(summary(pool(vic_dep23_fit))[2,2])
exp(summary(pool(vic_dep23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3])
exp(summary(pool(vic_dep23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3])
#1.37 (0.70 to 2.70)

#Now we want to calculate propensity score per imputed dataset
#http://www.math.umd.edu/~slud/s818M-MissingData/PropensityScoreWeightingR.pdf

#Derive a table including new coefficient and standard error
out <- data.frame(matrix(NA, nrow = nimp, ncol = 13))
colnames(out) <- c("imputation","coeff for vic","SE",
				"min(ps|tr=0)","25pc(ps|tr=0)",
				"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
				"min(ps|tr=1)","25pc(ps|tr=1)",
				"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)")
for (i in 1:nimp){
	a <- complete(imp_girls_sub, i)
	#a[,"id"] <- i
	model <- paste0("vic_1821 ~",covs)
	b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
	#data_girls[,paste0(i,"ps")] <- predict(b,a,type="response")
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	a[,"depress_ypc"] <- ifelse(
 		a$mfqtot_ypc <= 12, 0, 
 		1)
	model <- paste0("depress_ypc ~ vic_1821")
	fit_ipw <- glm(as.formula(model), data=a, family = "binomial", weight = (wgt.ATE))
	out[i,1] <- i
	out[i,2] <- fit_ipw$coef[2]
	out[i,3] <- summary(fit_ipw)$coef[2,2]
	out[i,4:8] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
	out[i,9:13] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
	assign(paste0("imp_girls_",i), a)
	}

meanrow <- rep(NA,13)
meanrow[1] <- "Pooled"
for(i in 2:13){
	meanrow[i] <- mean(out[,i])
	}
#Need to replace SE values
se_sq <- as.numeric(out[,3])^2
vw <- (sum(se_sq))/nimp
vb <- sqrt(sum((as.numeric(out[,2])-mean(as.numeric(out[,2])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[3] <- se_pooled

out <- rbind(out,meanrow)
write.csv(out, "imputed_ps_distn_depress23_girls.csv", row.names = FALSE, na = "")


#Now what about outcome of mfq at 23, not binary depression
out <- data.frame(matrix(NA, nrow = nimp, ncol = 14))
colnames(out) <- c("imputation","coeff for vic","SE","KS p-value",
				"min(ps|tr=0)","25pc(ps|tr=0)",
				"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
				"min(ps|tr=1)","25pc(ps|tr=1)",
				"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)")

for (i in 1:nimp){
	a <- complete(imp_girls_sub, i)
	#a[,"id"] <- i
	model <- paste0("vic_1821 ~",covs)
	b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
	#data_girls[,paste0(i,"ps")] <- predict(b,a,type="response")
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	model <- paste0("mfqtot_ypc ~ vic_1821")
	fit_ipw <- lm(as.formula(model), data=a, weight = (wgt.ATE))
	out[i,1] <- i
	out[i,2] <- fit_ipw$coef[2]
	out[i,3] <- summary(fit_ipw)$coef[2,2]
	out[i,4] <- ols_test_normality(fit_ipw)$kolmogorv$p.value
	out[i,5:9] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
	out[i,10:14] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
	assign(paste0("imp_girls_",i), a)
	}

meanrow <- rep(NA,14)
meanrow[1] <- "Pooled"
for(i in 2:14){
	meanrow[i] <- mean(out[,i])
	}
#Need to replace SE values
se_sq <- as.numeric(out[,3])^2
vw <- (sum(se_sq))/nimp
vb <- sqrt(sum((as.numeric(out[,2])-mean(as.numeric(out[,2])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[3] <- se_pooled

out <- rbind(out,meanrow)
write.csv(out, "imputed_ps_distn_mfq23_girls.csv", row.names = FALSE, na = "")


#Checking balance of propensity scores
for (c in 1:length(cov_list) ) {
	out <- data.frame(matrix(NA, nrow = nimp, ncol = 4))
	colnames(out) <- c("imputation",paste0("coeff for ",cov_list[c]),"SE","standardised diff")

	for (i in 1:nimp){
		a <- complete(imp_girls_sub, i)
		#a[,"id"] <- i
		model <- paste0("vic_1821 ~",covs)
		b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
		#data_girls[,paste0(i,"ps")] <- predict(b,a,type="response")
		a[,"ps"] <- predict(b,a,type="response")
		a[,"wgt.ATE"] <- ifelse(
	 		a$vic_1821 == 1, 1/a$ps,
	 		1/(1-a$ps))
		model <- paste0(cov_list[c]," ~ vic_1821")
		fit_ipw <- glm(as.formula(model), data=a, family = binomial(link = "logit"), weight = (wgt.ATE))
		out[i,1] <- i
		out[i,2] <- fit_ipw$coef[2]
		out[i,3] <- summary(fit_ipw)$coef[2,2]
		a[,"prob"] <- predict(fit_ipw,a,type="response")
		p_treat <- mean(a[a$vic_1821==1,"prob"])
		p_control <- mean(a[a$vic_1821==0,"prob"])
		out[i,4] <- 100*(p_treat-p_control)/(sqrt((p_treat*(1-p_treat)+p_control*(1-p_control))/2))
		assign(paste0("imp_girls_",i), a)
		}
	meanrow <- rep(NA,4)
	meanrow[1] <- "Pooled"
	for(i in 2:4){
		meanrow[i] <- mean(out[,i])
		}
	#Need to replace SE values
	se_sq <- as.numeric(out[,3])^2
	vw <- (sum(se_sq))/nimp
	vb <- sqrt(sum((as.numeric(out[,2])-mean(as.numeric(out[,2])))^2)/(dim(a)[1]-1))
	se_pooled <- sqrt(vw+vb+(vb/nimp))
	meanrow[3] <- se_pooled

	out <- rbind(out,meanrow)
	write.csv(out, paste0("imputed_ps_girls_",cov_list[c],".csv"), row.names = FALSE, na = "")
	}

#Histogram
a <- complete(imp_girls_sub, 1)
model <- paste0("vic_1821 ~",covs)
b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
a[,"ps"] <- predict(b,a,type="response")
a[,"wgt.ATE"] <- ifelse(
		a$vic_1821 == 1, 1/a$ps,
		1/(1-a$ps))
a$vic_1821 <- factor(a$vic_1821)

jpeg(file="ps_density_by_trt.jpeg")
a %>% ggplot(aes(x=ps, fill=vic_1821)) +
    geom_density(color="#e9ecef", alpha=0.6, 
    	position = 'identity', binwidth=0.01)
dev.off()

#Residuals
data.res <- data.frame(matrix(NA, nrow = length(resid(fit)), ncol = 2))
data.res[,1] <- data_girls$wgt.ATE[is.na(data_girls$wgt.ATE)==FALSE]
data.res[,2] <- resid(fit)
names(data.res) <- c("wgt","res")
ggplot(data.res, aes(x = res, y = ..density..)) +
geom_density()

ggplot(data.res, aes(x = res, y = ..density.., weight = wgt)) +
geom_density()

#Now put in logged outcome:
out <- data.frame(matrix(NA, nrow = nimp, ncol = 14))
colnames(out) <- c("imputation","coeff for vic","SE","KS p-value",
				"min(ps|tr=0)","25pc(ps|tr=0)",
				"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
				"min(ps|tr=1)","25pc(ps|tr=1)",
				"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)")

for (i in 1:nimp){
	a <- complete(imp_girls_sub, i)
	#a[,"id"] <- i
	model <- paste0("vic_1821 ~",covs)
	b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
	#data_girls[,paste0(i,"ps")] <- predict(b,a,type="response")
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	model <- paste0("logish_mfqtot_ypc ~ vic_1821")
	fit_ipw <- lm(as.formula(model), data=a, weight = (wgt.ATE))
	out[i,1] <- i
	out[i,2] <- fit_ipw$coef[2]
	out[i,3] <- summary(fit_ipw)$coef[2,2]
	out[i,4] <- ols_test_normality(fit_ipw)$kolmogorv$p.value
	out[i,5:9] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
	out[i,10:14] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
	assign(paste0("imp_girls_",i), a)
	}

meanrow <- rep(NA,14)
meanrow[1] <- "Pooled"
for(i in 2:14){
	meanrow[i] <- mean(out[,i])
	}
#Need to replace SE values
se_sq <- as.numeric(out[,3])^2
vw <- (sum(se_sq))/nimp
vb <- sqrt(sum((as.numeric(out[,2])-mean(as.numeric(out[,2])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[3] <- se_pooled

out <- rbind(out,meanrow)
write.csv(out, "imputed_ps_distn_logmfq23_girls.csv", row.names = FALSE, na = "")

#######################################################
#Repeat in boys

#Multiple imputation for missing data
#Derive depress_ypc after imputation
data_boys_sub <- data_boys[,c("aln","qlet",cov_list,"vic_1821",
	"mfqtot_ypc","logish_mfqtot_ypc")]
ini <- mice(data_boys_sub, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth

#Excluding binary depression as v. correlated with mfqtot vars anyway
#But not exluding mfqtot values, as mfqtot_ypc is the outcome
exclude<-c("aln","qlet")
pred_mat[,exclude] <- 0
meth[exclude] <- ""

Sys.time()

nimp <- 50
imp_boys_sub <- mice(data_boys_sub, m=nimp, maxit=10,
						method=meth, predictorMatrix=pred_mat)
Sys.time()
#About five minutes?
#Save for later to avoid imputing all the time
imp_boys_sub2 <- imp_boys_sub

#Now want regular logistic regression dep ~ IPVA
vic_mfq23_fit <- with(imp_boys_sub, lm(mfqtot_ypc ~ vic_1821))
summary(pool(vic_mfq23_fit))[2,2]
summary(pool(vic_mfq23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3]
summary(pool(vic_mfq23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3]
#Coef: 2.17 (1.10 to 3.24)
model <- paste0("mfqtot_ypc ~ vic_1821 + ", covs)
vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(model)))
summary(pool(vic_mfq23_fit))[2,2]
summary(pool(vic_mfq23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3]
summary(pool(vic_mfq23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3]
#1.66 (0.62 to 2.70)

#Logged outcome
model <- paste0("logish_mfqtot_ypc ~ vic_1821")
vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(model)))
exp(summary(pool(vic_mfq23_fit))[2,2])
exp(summary(pool(vic_mfq23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3])
exp(summary(pool(vic_mfq23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3])

model <- paste0("logish_mfqtot_ypc ~ vic_1821 + ",covs)
vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(model)))
exp(summary(pool(vic_mfq23_fit))[2,2])
exp(summary(pool(vic_mfq23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3])
exp(summary(pool(vic_mfq23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3])

a <- complete(imp_boys_sub, action='long',include=TRUE)
a[,"depress_ypc"] <- ifelse(
 		a$mfqtot_ypc <= 12, 0, 
 		1)
imp_boys_sub <- as.mids(a)

vic_dep23_fit <- with(imp_boys_sub, glm(depress_ypc ~ vic_1821, family = "binomial"))
exp(summary(pool(vic_dep23_fit))[2,2])
exp(summary(pool(vic_dep23_fit))[2,2]-1.96*summary(pool(vic_dep23_fit))[2,3])
exp(summary(pool(vic_dep23_fit))[2,2]+1.96*summary(pool(vic_dep23_fit))[2,3])
#OR: 1.87 (1.15 to 3.04)
model <- paste0("depress_ypc ~ vic_1821 + ", covs)
vic_dep23_fit <- with(imp_boys_sub, glm(as.formula(model), family = "binomial"))
exp(summary(pool(vic_dep23_fit))[2,2])
exp(summary(pool(vic_dep23_fit))[2,2]-1.96*summary(pool(vic_mfq23_fit))[2,3])
exp(summary(pool(vic_dep23_fit))[2,2]+1.96*summary(pool(vic_mfq23_fit))[2,3])
#1.61 (0.57 to 4.56)

#Derive a table including new coefficient and standard error
out <- data.frame(matrix(NA, nrow = nimp, ncol = 13))
colnames(out) <- c("imputation","coeff for vic","SE",
				"min(ps|tr=0)","25pc(ps|tr=0)",
				"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
				"min(ps|tr=1)","25pc(ps|tr=1)",
				"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)")
for (i in 1:nimp){
	a <- complete(imp_boys_sub, i)
	#a[,"id"] <- i
	a$vic_1821 <- factor(a$vic_1821)
	model <- paste0("vic_1821 ~",covs)
	b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
	#data_girls[,paste0(i,"ps")] <- predict(b,a,type="response")
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	a[,"depress_ypc"] <- ifelse(
 		a$mfqtot_ypc <= 12, 0, 
 		1)
	model <- paste0("depress_ypc ~ vic_1821")
	fit_ipw <- glm(as.formula(model), data=a, family = "binomial", weight = (wgt.ATE))
	out[i,1] <- i
	out[i,2] <- fit_ipw$coef[2]
	out[i,3] <- summary(fit_ipw)$coef[2,2]
	out[i,4:8] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
	out[i,9:13] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
	assign(paste0("imp_boys_",i), a)
	}

meanrow <- rep(NA,13)
meanrow[1] <- "Pooled"
for(i in 2:13){
	meanrow[i] <- mean(out[,i])
	}
#Need to replace SE values
se_sq <- as.numeric(out[,3])^2
vw <- (sum(se_sq))/nimp
vb <- sqrt(sum((as.numeric(out[,2])-mean(as.numeric(out[,2])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[3] <- se_pooled

out <- rbind(out,meanrow)
write.csv(out, "imputed_ps_distn_depress23_boys.csv", row.names = FALSE, na = "")


#Now what about outcome of mfq at 23, not binary depression
out <- data.frame(matrix(NA, nrow = nimp, ncol = 14))
colnames(out) <- c("imputation","coeff for vic","SE",
				"min(ps|tr=0)","25pc(ps|tr=0)",
				"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
				"min(ps|tr=1)","25pc(ps|tr=1)",
				"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)")

for (i in 1:nimp){
	a <- complete(imp_boys_sub, i)
	#a[,"id"] <- i
	model <- paste0("vic_1821 ~",covs)
	b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
	#data_girls[,paste0(i,"ps")] <- predict(b,a,type="response")
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	model <- paste0("mfqtot_ypc ~ vic_1821")
	fit_ipw <- lm(as.formula(model), data=a, weight = (wgt.ATE))
	out[i,1] <- i
	out[i,2] <- fit_ipw$coef[2]
	out[i,3] <- summary(fit_ipw)$coef[2,2]
	out[i,4] <- ols_test_normality(fit_ipw)$kolmogorv$p.value
	out[i,5:9] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
	out[i,10:14] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
	assign(paste0("imp_boys_",i), a)
	}

meanrow <- rep(NA,14)
meanrow[1] <- "Pooled"
for(i in 2:14){
	meanrow[i] <- mean(out[,i])
	}
#Need to replace SE values
se_sq <- as.numeric(out[,3])^2
vw <- (sum(se_sq))/nimp
vb <- sqrt(sum((as.numeric(out[,2])-mean(as.numeric(out[,2])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[3] <- se_pooled

out <- rbind(out,meanrow)
write.csv(out, "imputed_ps_distn_mfq23_boys.csv", row.names = FALSE, na = "")

#Logged mfq
out <- data.frame(matrix(NA, nrow = nimp, ncol = 14))
colnames(out) <- c("imputation","coeff for vic","SE","KS p-value",
				"min(ps|tr=0)","25pc(ps|tr=0)",
				"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
				"min(ps|tr=1)","25pc(ps|tr=1)",
				"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)")

for (i in 1:nimp){
	a <- complete(imp_boys_sub, i)
	#a[,"id"] <- i
	model <- paste0("vic_1821 ~",covs)
	b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
	#data_girls[,paste0(i,"ps")] <- predict(b,a,type="response")
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	model <- paste0("logish_mfqtot_ypc ~ vic_1821")
	fit_ipw <- lm(as.formula(model), data=a, weight = (wgt.ATE))
	out[i,1] <- i
	out[i,2] <- fit_ipw$coef[2]
	out[i,3] <- summary(fit_ipw)$coef[2,2]
	out[i,4] <- ols_test_normality(fit_ipw)$kolmogorv$p.value
	out[i,5:9] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
	out[i,10:14] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
	assign(paste0("imp_boys_",i), a)
	}

meanrow <- rep(NA,14)
meanrow[1] <- "Pooled"
for(i in 2:14){
	meanrow[i] <- mean(out[,i])
	}
#Need to replace SE values
se_sq <- as.numeric(out[,3])^2
vw <- (sum(se_sq))/nimp
vb <- sqrt(sum((as.numeric(out[,2])-mean(as.numeric(out[,2])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[3] <- se_pooled

out <- rbind(out,meanrow)
write.csv(out, "imputed_ps_distn_logmfq23_boys.csv", row.names = FALSE, na = "")


## ----  -------------------------------------------------------------------
#Anxiety in girls
data_girls_sub <- data_girls[,c("aln","qlet",cov_list,"vic_1821","anxiety_f24"
								)]
ini <- mice(data_girls_sub, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet")
pred_mat[,exclude] <- 0
meth[exclude] <- ""
Sys.time()
nimp <- 50
imp_girls_sub <- mice(data_girls_sub, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
Sys.time()
imp_girls_sub3 <- imp_girls_sub

out <- data.frame(matrix(NA, nrow = nimp, ncol = 13))
colnames(out) <- c("imputation","coeff for vic","SE",
				"min(ps|tr=0)","25pc(ps|tr=0)",
				"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
				"min(ps|tr=1)","25pc(ps|tr=1)",
				"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)")

for (i in 1:nimp){
	a <- complete(imp_girls_sub, i)
	#a[,"id"] <- i
	model <- paste0("vic_1821 ~",covs)
	b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
	#data_girls[,paste0(i,"ps")] <- predict(b,a,type="response")
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	model <- paste0("anxiety_f24 ~ vic_1821")
	fit_ipw <- glm(as.formula(model), data=a, family = "binomial", weight = (wgt.ATE))
	out[i,1] <- i
	out[i,2] <- fit_ipw$coef[2]
	out[i,3] <- summary(fit_ipw)$coef[2,2]
	out[i,4:8] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
	out[i,9:13] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
	assign(paste0("imp_girls_",i), a)
	}

meanrow <- rep(NA,13)
meanrow[1] <- "Pooled"
for(i in 2:13){
	meanrow[i] <- mean(out[,i])
	}
#Need to replace SE values
se_sq <- as.numeric(out[,3])^2
vw <- (sum(se_sq))/nimp
vb <- sqrt(sum((as.numeric(out[,2])-mean(as.numeric(out[,2])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[3] <- se_pooled

out <- rbind(out,meanrow)
write.csv(out, "imputed_ps_distn_anx24_girls.csv", row.names = FALSE, na = "")

#Anxiety in boys
data_boys_sub <- data_boys[,c("aln","qlet",cov_list,"vic_1821","anxiety_f24"
								)]
ini <- mice(data_boys_sub, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet")
pred_mat[,exclude] <- 0
meth[exclude] <- ""
Sys.time()
nimp <- 50
imp_boys_sub <- mice(data_boys_sub, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
Sys.time()
imp_boys_sub3 <- imp_boys_sub

out <- data.frame(matrix(NA, nrow = nimp, ncol = 13))
colnames(out) <- c("imputation","coeff for vic","SE",
				"min(ps|tr=0)","25pc(ps|tr=0)",
				"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
				"min(ps|tr=1)","25pc(ps|tr=1)",
				"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)")

for (i in 1:nimp){
	a <- complete(imp_boys_sub, i)
	#a[,"id"] <- i
	model <- paste0("vic_1821 ~",covs)
	b <- glm(as.formula(model), data=a, family = binomial(link = "logit"))
	#data_girls[,paste0(i,"ps")] <- predict(b,a,type="response")
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	model <- paste0("anxiety_f24 ~ vic_1821")
	fit_ipw <- glm(as.formula(model), data=a, family = "binomial", weight = (wgt.ATE))
	out[i,1] <- i
	out[i,2] <- fit_ipw$coef[2]
	out[i,3] <- summary(fit_ipw)$coef[2,2]
	out[i,4:8] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
	out[i,9:13] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
	assign(paste0("imp_boys_",i), a)
	}

meanrow <- rep(NA,13)
meanrow[1] <- "Pooled"
for(i in 2:13){
	meanrow[i] <- mean(out[,i])
	}
#Need to replace SE values
se_sq <- as.numeric(out[,3])^2
vw <- (sum(se_sq))/nimp
vb <- sqrt(sum((as.numeric(out[,2])-mean(as.numeric(out[,2])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[3] <- se_pooled

out <- rbind(out,meanrow)
write.csv(out, "imputed_ps_distn_anx24_boys.csv", row.names = FALSE, na = "")