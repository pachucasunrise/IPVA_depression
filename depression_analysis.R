## Project: MH outcomes following IPVA
## Script purpose: Taking dataset (that includes covariates (IPVA RFs and IPVA 
## at 18-21) and depression at age 23 (the outcome), 
## difference-in-difference analysis, 
## show imbalance in covariates between IPVA and no IPVA, and
## IPTW analysis
## Date: 27th July 2019
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

################################################################################
# 0. Locations, packages, functions

# Note that in Cmder, need to set drive:
cd "C:/Program Files/R/R-3.5.3/bin"
R

packages<-c('digest','readstata13','data.table','tidyr','formattable','tidyverse',
	'dplyr','gdata','foreign','readxl','matrixStats','tableone','Rcmdr','mice',
	'magrittr','varhandle','zoo','mice','backports','olsrr','mitml','quantreg',
	'gridExtra','grid','ggplot2','lattice')

source("http://bioconductor.org/biocLite.R")

for(pkg in packages){
  if(!require(pkg,character.only=TRUE)){
    BiocInstaller::biocLite(pkg,suppressUpdates=TRUE)
    library(pkg,character.only=TRUE)
    }
  }

rm(pkg,packages)

setwd('C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/IPV project/p5 - MH outcomes/results')

################################################################################

## ---- Clear global R environment----------------------------------------------
rm(list=ls())


## ---- Location files----------------------------------------------------------
# loc_inp<-'W:/data/'
# loc_out<-'C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/IPV project/p5 - MH outcomes/results'
# setwd(loc_out)


################################################################################
# 1. Get and prep data  
################################################################################

# Load data, selecting those who do not have missing sex (n=1), 
# and who did not say they were exposed to vic at 0-17 years old (n=487)

cohort.dta <- read.dta13("W:/data/cohort/current_IPVA_cohort_id.dta")
#NEED "emotional_ab","physical_abu","sexual_abuse","emotional_ne",
#"bullying_0_1","violence_bet","mentl_hlth_p","substance_ho",
#"parent_convi","parental_sep", but with missing values.  As well as 
#original auxilliary variables from Lotte's scripts.

#cohort.dta <- read.dta13("current_IPVA_cohort_id.dta")
cohort.dta <- cohort.dta[is.na(cohort.dta$kz021_new)==FALSE 
					& cohort.dta$vic_017!=1
					,]

# Binary SES
ses <- 	c("bimd2010q5","ccrimd2010q5","ccsimd2010q5","cctimd2010q5","ccrimd2010q5",
		"jan2011imd2010q5_YP","jan2014imd2010q5_YP")

for (s in ses){
	cohort.dta[,paste0(s,"_bin")] <- NA
	cohort.dta[,paste0(s,"_bin")][as.numeric(cohort.dta[,s])>=1 & as.numeric(cohort.dta[,s])<=3] <- 0
	cohort.dta[,paste0(s,"_bin")][as.numeric(cohort.dta[,s])>=4 & as.numeric(cohort.dta[,s])<=5] <- 1
	cohort.dta[,paste0(s,"_bin")] <- factor(cohort.dta[,paste0(s,"_bin")], levels = c(0,1,NA))
	}

#RSB, hospitalisation, and parental education get rid of NAs
cohort.dta$rsb[is.na(cohort.dta$rsb)==TRUE]<-0
cohort.dta$hosp[is.na(cohort.dta$hosp)==TRUE]<-0
cohort.dta$pareduc[is.na(cohort.dta$pareduc)==TRUE]<-0

#List of covariates for models
cov_list <- c(
			#Risk factors for IPVA and depression
			"mfqtot_ccs",

			"bimd2010q5_bin","ethnicity","sex_min_tf3","anxiety_tf4",
			#Removing sh_tf3 as only 20 = 1 in girls, none in boys
			#"sh_tf3",
			"asb_ccq","smok_ccs","hazalc_tf4","cann_ccs","drug_ccs",
			"ext_patmon",
			#Though RSB and hosp are RFs for IPVA, no evidence that RFs for depression too
			#"rsb","hosp",
			"emotional_ab","physical_abu","sexual_abuse",
			"emotional_ne","bullying_0_1","violence_bet","mentl_hlth_p",
			"substance_ho",
			"parent_convi",
			"parental_sep",
			
			#Now risk factors for at least depression
			"lowselfest_ccxd","ow_tf4",
			#No eating disorder as barely anyone defined as not having one if they responded at 16y
			#"eatd_ccs",
			"sleep_tf4",
			"pareduc")

#Covariates when outcome is logged
cov_list_log <- c("log_mfqtot_ccs",cov_list[cov_list!="mfqtot_ccs"])

#Making sure most of these variables are recognised as factors (all except mfqtot_ccs)
for (c in cov_list[cov_list!="mfqtot_ccs"]){
	cohort.dta[,c] <- factor(cohort.dta[,c])
	}

#Create one term that includes all covariates ready for a model
#(This is used in 'Adjustment 2' - i.e. all covs)
covs <- cov_list[1]
for (c in 2:length(cov_list) ) {
	covs <- paste0(covs," + ",cov_list[c])
	}

covs_log <- cov_list_log[1]
for (c in 2:length(cov_list_log) ) {
	covs_log <- paste0(covs_log," + ",cov_list_log[c])
	}

#Adjustment 1 (subset of covariates)
cov_list2 <- c("mfqtot_ccs",
			"bimd2010q5_bin","ethnicity",
			"emotional_ab","physical_abu","sexual_abuse",
			"emotional_ne")
covs2 <- cov_list2[1]
for (c in 2:length(cov_list2) ) {
covs2 <- paste0(covs2," + ",cov_list2[c])
	}

cov_list3 <- c("log_mfqtot_ccs",
			"bimd2010q5_bin","ethnicity",
			"emotional_ab","physical_abu","sexual_abuse",
			"emotional_ne")
covs3 <- cov_list3[1]
for (c in 2:length(cov_list3) ) {
covs3 <- paste0(covs3," + ",cov_list3[c])
	}

#Check distribution of MFQ scores (mfqtot_ypc is depression score at age 23):
hist(cohort.dta$mfqtot_ypc)
hist(log(cohort.dta$mfqtot_ypc))
#Include log(MFQ), +1 to avoid log(0)
cohort.dta$log_mfqtot_tf2 <- log(cohort.dta$mfqtot_tf2+1)
cohort.dta$log_mfqtot_ccs <- log(cohort.dta$mfqtot_ccs+1)
cohort.dta$log_mfqtot_ypc <- log(cohort.dta$mfqtot_ypc+1)

#Add in vic subtype categories
cohort.dta$vic_sub_1821 <- 0
#Psych vic only
cohort.dta[cohort.dta$vic_emo_1821 == 1 &
	(cohort.dta$vic_phys_1821 != 1 & cohort.dta$vic_sex_1821 != 1)
	,"vic_sub_1821"] <- 1
cohort.dta[cohort.dta$vic_phys_1821 == 1
	& cohort.dta$vic_sex_1821 != 1
	,"vic_sub_1821"] <- 2
#Any sexual victimisation
cohort.dta[cohort.dta$vic_sex_1821 ==1
	,"vic_sub_1821"] <- 3
cohort.dta[,"vic_sub_1821"] <- factor(cohort.dta[,"vic_sub_1821"], levels=c(0,1,2,3))

#Binary depression
cohort.dta$mfqbin_ccs <- NA
cohort.dta$mfqbin_ccs[cohort.dta$mfqtot_ccs<=12] <- 0
cohort.dta$mfqbin_ccs[cohort.dta$mfqtot_ccs>=13] <- 1
cohort.dta$mfqbin_ypc <- NA
cohort.dta$mfqbin_ypc[cohort.dta$mfqtot_ypc<=12] <- 0
cohort.dta$mfqbin_ypc[cohort.dta$mfqtot_ypc>=13] <- 1

#Selecting only those variables that are needed
cohort.dta_sub <- cohort.dta[,c("aln","qlet","kz021_new",
								#Remove ACE variables from cov_list here
								cov_list[cov_list!="mfqtot_ccs"],
								"vic_1821","vic_sub_1821",
								"mfqtot_tf2","mfqtot_ccs","mfqtot_ypc",
								"log_mfqtot_tf2","log_mfqtot_ccs","log_mfqtot_ypc",
								"mfqbin_ccs","mfqbin_ypc"
								#And ACE variables before imputation
								#Plus auxilliary variables
								)]

ini <- mice(cohort.dta_sub, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"mfqbin_ccs","mfq_bin_ypc",
	"log_mfqtot_tf2","log_mfqtot_ccs","log_mfqtot_ypc")
pred_mat[,exclude] <- 0
pre_mat["mfqbin_ccs","mfqbin_ccs"]
meth[exclude] <- ""

#Need to impute log values separately
pred_mat2 <- ini$pred 
meth2 <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"mfqbin_ccs","mfq_bin_ypc",
	"mfqtot_tf2","mfqtot_ccs","mfqtot_ypc")
pred_mat2[,exclude] <- 0
meth2[exclude] <- ""

#Finally binary outcomes
pred_mat3 <- ini$pred 
meth3 <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"mfqtot_tf2","mfqtot_ccs","mfqtot_ypc",
	"log_mfqtot_tf2","log_mfqtot_ccs","log_mfqtot_ypc")
pred_mat3[,exclude] <- 0
meth3[exclude] <- ""

nimp <- 50

cohort.dta_sub_boys <- cohort.dta_sub[cohort.dta_sub$kz021_new==1,]
cohort.dta_sub_girls <- cohort.dta_sub[cohort.dta_sub$kz021_new==2,]

save(cohort.dta_sub_boys,cohort.dta_sub_girls,
	meth,pred_mat,meth2,pred_mat2,meth3,pred_mat3,
	file='W:/data/cohort/data_for_imp_depression_analysis.RData')


################################################################################
# 2. Impute missing data
################################################################################

## ---- The following is code to run as standard (will also provide code for HPC) -------------------------------------------------------------------
# set.seed(136454)
# Sys.time() 
# #12.25
imp_boys_sub <- mice(cohort.dta_sub[cohort.dta_sub$kz021_new==1,], m=nimp, maxit=10,
						method=meth, predictorMatrix=pred_mat)
Sys.time()
#12.30 - 5 minutes!
imp_girls_sub <- mice(cohort.dta_sub[cohort.dta_sub$kz021_new==2,], m=nimp, maxit=10,
						method=meth, predictorMatrix=pred_mat)

#Logged outcome
imp_boys_sub2 <- mice(cohort.dta_sub[cohort.dta_sub$kz021_new==1,], m=nimp, maxit=10,
						method=meth2, predictorMatrix=pred_mat2)
imp_girls_sub2 <- mice(cohort.dta_sub[cohort.dta_sub$kz021_new==2,], m=nimp, maxit=10,
						method=meth2, predictorMatrix=pred_mat2)

#Binary outcome
imp_boys_sub3 <- mice(cohort.dta_sub[cohort.dta_sub$kz021_new==1,], m=nimp, maxit=10,
						method=meth3, predictorMatrix=pred_mat3)
imp_girls_sub3 <- mice(cohort.dta_sub[cohort.dta_sub$kz021_new==2,], m=nimp, maxit=10,
						method=meth3, predictorMatrix=pred_mat3)

# #Will need to impute again for diff-in-diff analysis as long format
data_long <- gather(cohort.dta_sub, time, depress, mfqtot_ccs:mfqtot_ypc, factor_key=FALSE)
data_long$time[data_long$time=="mfqtot_ccs"] <- 0
data_long$time[data_long$time=="mfqtot_ypc"] <- 1
data_long[,"time"] <- as.numeric(data_long[,"time"])
data_long_boys <- data_long[data_long$kz021_new==1 & is.na(data_long$aln)==FALSE,]
data_long_girls <- data_long[data_long$kz021_new==2 & is.na(data_long$aln)==FALSE,]

ini <- mice(data_long_boys, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"log_mfqtot_tf2", "log_mfqtot_ccs", "log_mfqtot_ypc")
pred_mat[,exclude] <- 0
meth[exclude] <- ""

Sys.time()
imp_long_boys <- mice(data_long_boys, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
imp_long_girls <- mice(data_long_girls, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
Sys.time()

#Logged outcome
#14.04
data_long2 <- gather(cohort.dta_sub, time, log_depress, log_mfqtot_ccs:log_mfqtot_ypc, factor_key=FALSE)
data_long2$time[data_long2$time=="log_mfqtot_ccs"] <- 0
data_long2$time[data_long2$time=="log_mfqtot_ypc"] <- 1
data_long2[,"time"] <- as.numeric(data_long2[,"time"])
data_long2_boys <- data_long2[data_long2$kz021_new==1 & is.na(data_long2$aln)==FALSE,]
data_long2_girls <- data_long2[data_long2$kz021_new==2 & is.na(data_long2$aln)==FALSE,]

ini <- mice(data_long2_boys, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"mfqtot_tf2", "mfqtot_ccs", "mfqtot_ypc")
pred_mat[,exclude] <- 0
meth[exclude] <- ""

imp_long2_boys <- mice(data_long2_boys, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
imp_long2_girls <- mice(data_long2_girls, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
Sys.time()
#14.48 - 44 minutes

#Parallel lines, raw values
data_long_par <- gather(cohort.dta_sub, time, depress, mfqtot_tf2:mfqtot_ccs, factor_key=FALSE)
data_long_par$time[data_long_par$time=="mfqtot_tf2"] <- 0
data_long_par$time[data_long_par$time=="mfqtot_ccs"] <- 1
data_long_par[,"time"] <- as.numeric(data_long_par[,"time"])
data_long_boys_par <- data_long_par[data_long_par$kz021_new==1 & is.na(data_long_par$aln)==FALSE,]
data_long_girls_par <- data_long_par[data_long_par$kz021_new==2 & is.na(data_long_par$aln)==FALSE,]

ini <- mice(data_long_boys_par, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"log_mfqtot_tf2", "log_mfqtot_ccs", "log_mfqtot_ypc")
pred_mat[,exclude] <- 0
meth[exclude] <- ""
imp_long_boys_par <- mice(data_long_boys_par, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
imp_long_girls_par <- mice(data_long_girls_par, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
#15.55 ~ 1 hour

#Logged outcome
#16.53
data_long2_par <- gather(cohort.dta_sub, time, log_depress, log_mfqtot_tf2:log_mfqtot_ccs, factor_key=FALSE)
data_long2_par$time[data_long2_par$time=="log_mfqtot_tf2"] <- 0
data_long2_par$time[data_long2_par$time=="log_mfqtot_ccs"] <- 1
data_long2_par[,"time"] <- as.numeric(data_long2_par[,"time"])
data_long2_boys_par <- data_long2_par[data_long2_par$kz021_new==1 & is.na(data_long2_par$aln)==FALSE,]
data_long2_girls_par <- data_long2_par[data_long2_par$kz021_new==2 & is.na(data_long2_par$aln)==FALSE,]

ini <- mice(data_long2_boys_par, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"mfqtot_tf2", "mfqtot_ccs", "mfqtot_ypc")
pred_mat[,exclude] <- 0
meth[exclude] <- ""
imp_long2_boys_par <- mice(data_long2_boys_par, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
imp_long2_girls_par <- mice(data_long2_girls_par, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
#17.15 - 20 minutes...

save(data_long_boys, data_long2_boys,
	data_long_girls, data_long2_girls,

	imp_boys_sub, imp_boys_sub2, imp_boys_sub3,
	imp_girls_sub, imp_girls_sub2, imp_girls_sub3,
	
	imp_long_boys, imp_long2_boys, 
	imp_long_girls, imp_long2_girls, 

	imp_long_boys_par, imp_long2_boys_par,
	imp_long_girls_par, imp_long2_girls_par,
	file='W:/data/cohort/cohort_imp_data_depression_analysis.RData')

# ## ---- Run this part in Blue Crystal as imputations take a long time -------------------------
# library(mice)
# load('data_for_imp_depression_analysis.RData')

# # Change to generic names
# cohort.dta_sub_boys <- cohort.dta_sub_boys[,match(rownames(pred_mat),names(cohort.dta_sub_boys))]
# cohort.dta_sub_girls <- cohort.dta_sub_girls[,match(rownames(pred_mat),names(cohort.dta_sub_girls))]

# #specify nr nodes used
# cores_2_use <- 15

# #Do not change next four lines
# cl <- makeCluster(cores_2_use)
# clusterSetRNGStream(cl, 9956)
# clusterExport(cl,list("cohort.dta_sub_boys",'pred_mat','meth',))
# clusterEvalQ(cl, library(mice))

# imp_pars <-
#   parLapply(cl = cl, X = 1:cores_2_use, fun = function(no){
#     mice(cohort.dta_sub_boys, m = 4, #multiple with cores_2_use to get total nr
#          #imputated datasets, 60
#          maxit=10, printFlag = TRUE,method=meth,predictorMatrix=pred_mat,seed=136454)
#   })
# save(imp_pars,
#      file='imp_long_boys2.RData')
# stopCluster(cl)

# .Random.seed=136454
# imp_boys <- imp_pars[[1]]
# for (n in 2:length(imp_pars)){
#   imp_boys <- 
#     ibind(imp_boys,
#           imp_pars[[n]])
# }
# save(imp_boys,
#      file='imp_long_boys2.RData')

# #Plots to asses convergence (cannot do this after creating object using as.mids)
# pdf('check_imp_boys.pdf')
# plot(imp_boys)
# dev.off()
# q()

# save(cohort.dta_sub,
#   data_long_boys,imp_long_boys,imp_long_boys2,
# 	data_long_girls,imp_long_girls,imp_long_girls2,
# 	file='cohort_imp_data_depression_analysis.RData')


################################################################################
# 3. Numbers of covariates (including missing) between vic and no vic
# (on original dataset, not imputed) - currently Table S1
################################################################################

load('W:/data/cohort/cohort_imp_data_depression_analysis.RData')

#Need to move around order of covariates (or adjust in Excel)
stats <- print(
	           CreateCatTable(vars = cov_list[cov_list!="mfqtot_ccs"], data = cohort.dta, 
	           	 strata = c("vic_1821","kz021_new"), includeNA = TRUE),
	             showAllLevels = TRUE,
	             #quote = TRUE,
	             test = FALSE,
	             format = "f")

out <- data.frame(matrix(NA, nrow = dim(stats)[1]+4, ncol = 10))
colnames(out) <- c("","","Males",rep("",3),"Females",rep("",3))
out[1, ] <- c("","","No vic","","Vic","","No vic","","Vic","")
out[2, ] <- c("Variable","Level",rep(c("n","%"),4))
out[3:(dim(stats)[1]+2),1] <- dimnames(stats)[[1]]
out[(dim(stats)[1]+3),1] <- "mfqtot_ccs"
out[(dim(stats)[1]+3),2] <- "Median (IQR)"
out[(dim(stats)[1]+4),2] <- "Missing"
out[3:(dim(stats)[1]+2),2] <- stats[,1]
out[3:(dim(stats)[1]+4),3:10] <- 0
out[3:(dim(stats)[1]+2),3] <- stats[,2]
out[3:(dim(stats)[1]+2),4] <- round(as.numeric(stats[,2])/as.numeric(stats[1,2])*100,digits=1)
out[3:(dim(stats)[1]+2),5] <- stats[,3]
out[3:(dim(stats)[1]+2),6] <- round(as.numeric(stats[,3])/as.numeric(stats[1,3])*100,digits=1)
out[3:(dim(stats)[1]+2),7] <- stats[,4]
out[3:(dim(stats)[1]+2),8] <- round(as.numeric(stats[,4])/as.numeric(stats[1,4])*100,digits=1)
out[3:(dim(stats)[1]+2),9] <- stats[,5]
out[3:(dim(stats)[1]+2),10] <- round(as.numeric(stats[,5])/as.numeric(stats[1,5])*100,digits=1)

#This following chunk (and next) to be shortened:
#MFQ medians and IQRs
out[(dim(stats)[1]+3),3] <- median(cohort.dta[cohort.dta$kz021_new==1 & 
	cohort.dta$vic_1821==0 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"])
out[(dim(stats)[1]+3),4] <- paste0(quantile(cohort.dta[cohort.dta$kz021_new==1 & 
	cohort.dta$vic_1821==0 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.25)," to ",
	quantile(cohort.dta[cohort.dta$kz021_new==1 & 
	cohort.dta$vic_1821==0 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.75))
out[(dim(stats)[1]+3),5] <- median(cohort.dta[cohort.dta$kz021_new==1 & 
	cohort.dta$vic_1821==1 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"])
out[(dim(stats)[1]+3),6] <- paste0(quantile(cohort.dta[cohort.dta$kz021_new==1 & 
	cohort.dta$vic_1821==1 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.25)," to ",
	quantile(cohort.dta[cohort.dta$kz021_new==1 & 
	cohort.dta$vic_1821==1 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.75))
out[(dim(stats)[1]+3),7] <- median(cohort.dta[cohort.dta$kz021_new==2 & 
	cohort.dta$vic_1821==0 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"])
out[(dim(stats)[1]+3),8] <- paste0(quantile(cohort.dta[cohort.dta$kz021_new==2 & 
	cohort.dta$vic_1821==0 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.25)," to ",
	quantile(cohort.dta[cohort.dta$kz021_new==2 & 
	cohort.dta$vic_1821==0 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.75))
out[(dim(stats)[1]+3),9] <- median(cohort.dta[cohort.dta$kz021_new==2 & 
	cohort.dta$vic_1821==1 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"])
out[(dim(stats)[1]+3),10] <- paste0(quantile(cohort.dta[cohort.dta$kz021_new==2 & 
	cohort.dta$vic_1821==1 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.25)," to ",
	quantile(cohort.dta[cohort.dta$kz021_new==2 & 
	cohort.dta$vic_1821==1 & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.75))

#MFQ row
out[(dim(stats)[1]+4),3] <- count(is.na(cohort.dta[cohort.dta$kz021_new==1 & 
	cohort.dta$vic_1821==0,"mfqtot_ccs"]))
out[(dim(stats)[1]+4),4] <- round(as.numeric(out[(dim(stats)[1]+4),3])/as.numeric(out[3,3])*100,digits=1)
out[(dim(stats)[1]+4),5] <- count(is.na(cohort.dta[cohort.dta$kz021_new==1 & 
	cohort.dta$vic_1821==1,"mfqtot_ccs"]))
out[(dim(stats)[1]+4),6] <- round(as.numeric(out[(dim(stats)[1]+4),5])/as.numeric(out[3,5])*100,digits=1)
out[(dim(stats)[1]+4),7] <- count(is.na(cohort.dta[cohort.dta$kz021_new==2 & 
	cohort.dta$vic_1821==0,"mfqtot_ccs"]))
out[(dim(stats)[1]+4),8] <- round(as.numeric(out[(dim(stats)[1]+4),7])/as.numeric(out[3,7])*100,digits=1)
out[(dim(stats)[1]+4),9] <- count(is.na(cohort.dta[cohort.dta$kz021_new==2 & 
	cohort.dta$vic_1821==1,"mfqtot_ccs"]))
out[(dim(stats)[1]+4),10] <- round(as.numeric(out[(dim(stats)[1]+4),9])/as.numeric(out[3,9])*100,digits=1)

#No empty rows so far...
#out <- out[as.numeric(out[,3])!=0 & as.numeric(out[,5])!=0,]
#NAs introduced by coercion
colnames(out) <- c("","","Males",rep("",3),"Females",rep("",3))
out[1, ] <- c("","","No vic","","Vic","","No vic","","Vic","")
out[2, ] <- c("Variable","Level",rep(c("n","%"),4))
out[out[,2]==0,2] <- "No"
out[out[,2]==1,2] <- "Yes"
out[is.na(out[,2])==TRUE,2] <- "Missing"

#Export table
write.csv(out, "baseline_characs.csv", row.names = FALSE, na = "")


################################################################################
# 3. Average MFQ scores - currently Table 1
################################################################################
out <- data.frame(matrix(NA, nrow = 6, ncol = 7))
colnames(out) <- c("","Median","IQR","Arithmetic mean","95% CI","Geometric mean","95% CI")
out[ ,1] <- c("Men","No vic","Vic","Women","No vic","Vic")

level <- 2
#Struggled to put boys and girls in a loop, wouldn't recognise the list
data <- complete(imp_boys_sub,action='long',include=TRUE)
for (j in 0:1){
	data2 <- data[data$vic_1821==j,]
	data3 <- as.mids(data2)
	out[level,2] <- mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, median)))
	out[level,3] <- paste0(mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, quantile, probs=0.25))),
					" to ",mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, quantile, probs=0.75))))
	out[level,4] <- round(mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, mean))),digits=2)
	out[level,5] <- paste0(round(summary(pool(with(data3, lm(mfqtot_ypc ~ vic_1821))))[1,2]-
							1.96*summary(pool(with(data3, lm(mfqtot_ypc ~ vic_1821))))[1,3],digits=2),
					" to ",round(summary(pool(with(data3, lm(mfqtot_ypc ~ vic_1821))))[1,2]+
							1.96*summary(pool(with(data3, lm(mfqtot_ypc ~ vic_1821))))[1,3],digits=2))
	out[level,6] <- round(exp(summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,2]),digits=2)
	out[level,7] <- paste0(round(exp(summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,2]-
							1.96*summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,3]),digits=2),
					" to ",round(exp(summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,2]+
							1.96*summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,3]),digits=2))
	level <- level+1
	}
level<- level+1
data <- complete(imp_girls_sub,action='long',include=TRUE)
for (j in 0:1){
	data2 <- data[data$vic_1821==j,]
	data3 <- as.mids(data2)
	out[level,2] <- mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, median)))
	out[level,3] <- paste0(mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, quantile, probs=0.25))),
					" to ",mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, quantile, probs=0.75))))
	out[level,4] <- round(mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, mean))),digits=2)
	out[level,5] <- paste0(round(summary(pool(with(data3, lm(mfqtot_ypc ~ vic_1821))))[1,2]-
							1.96*summary(pool(with(data3, lm(mfqtot_ypc ~ vic_1821))))[1,3],digits=2),
					" to ",round(summary(pool(with(data3, lm(mfqtot_ypc ~ vic_1821))))[1,2]+
							1.96*summary(pool(with(data3, lm(mfqtot_ypc ~ vic_1821))))[1,3],digits=2))
	out[level,6] <- round(exp(summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,2]),digits=2)
	out[level,7] <- paste0(round(exp(summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,2]-
							1.96*summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,3]),digits=2),
					" to ",round(exp(summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,2]+
							1.96*summary(pool(with(data3, lm(log_mfqtot_ypc ~ vic_1821))))[1,3]),digits=2))
	level <- level+1
	}
write.csv(out, "mfq_stats.csv", row.names = FALSE, na = "")


################################################################################
# 4. Difference-in-difference analysis (similar to approach taken by Clayton et al 
# with cardiovascular outcomes in pregnancy), for causal effect of IPVA on depression 
# (this is possible because we have numerical depression scores before and after exposure)
# - Forms part of Table 2, 3, 4, and S2
################################################################################

load('W:/data/cohort/cohort_imp_data_depression_analysis.RData')
#Check relationship between mfqtot_ccs (age 16) and mfqtot_ypc (age 23):
plot(cohort.dta_sub$mfqtot_ccs, cohort.dta_sub$mfqtot_ypc)
summary(lm(mfqtot_ypc ~ mfqtot_ccs, data = cohort.dta[cohort.dta$kz021_new==1,]))
summary(lm(mfqtot_ypc ~ mfqtot_ccs, data = cohort.dta[cohort.dta$kz021_new==2,]))

#Number of models per sex (we'll include CCA and imputed on raw mfq scores, 
#and then CCA and imputed on log mfq scores. Will also check lines are parallel at earlier time-points)

nmodel <- 6
model1 <- "MFQ, MI"
model2 <- "logMFQ, MI"
model3 <- "MFQ, CCA"
model4 <- "logMFQ, CCA"
model5 <- "Parallel, MFQ, MI"
model6 <- "Parallel, logMFQ, MI"

#Create a table for results
out <- data.frame(matrix(NA, nrow = (nmodel*4)+1, ncol = 10))
colnames(out) <- c(rep("",2),"Males",rep("",3),"Females",rep("",3))
out[1, ] <- c("Model","Variable",rep(c("N obs","Coef","SE","p-value"),2))
out[2:((nmodel*4)+1),1] <- c(rep(model1,4),rep(model2,4),rep(model3,4),
	rep(model4,4),rep(model5,4),rep(model6,4))
out[2:((nmodel*4)+1),2] <- rep(c("Intercept","Vic","Time","Vic*Time"),nmodel)
level <- 0
digits <- 4

#Read in functions
source("C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/Training/R/did_cca.R")
source("C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/Training/R/did_mi.R")

#Model 1
out <- did_mi(out,level,"depress ~ vic_1821*time",
	imp_long_boys, imp_long_girls, digits)

#Model 2
# a <- complete(imp_long2_boys, action='long',include=TRUE)
# a[,"log_depress"] <- log(a$depress+1)
# imp_long2_boys <- as.mids(a)

# a <- complete(imp_long2_girls, action='long',include=TRUE)
# a[,"log_depress"] <- log(a$depress+1)
# imp_long2_girls <- as.mids(a)

level <- level +1
out <- did_mi(out,level,"log_depress ~ vic_1821*time",
	imp_long2_boys, imp_long2_girls, digits)

#Model 3
level <- level +1
out<- did_cca(out,level,"depress ~ vic_1821*time",
	data_long_boys, data_long_girls, digits)

#Model 4
level <- level +1
out <- did_cca(out,level,"log_depress ~ vic_1821*time",
	data_long2_boys, data_long2_girls, digits)

#Model 5
level <- level +1
out <- did_mi(out,level,"depress ~ vic_1821*time",
	imp_long_boys_par, imp_long_girls_par, digits)

#Model 6
# a <- complete(imp_long_boys2_par, action='long',include=TRUE)
# a[,"log_depress"] <- log(a$depress+1)
# imp_long_boys2 <- as.mids(a)

# a <- complete(imp_long_girls2_par, action='long',include=TRUE)
# a[,"log_depress"] <- log(a$depress+1)
# imp_long_girls2 <- as.mids(a)

level <- level +1
out <- did_mi(out,level,"log_depress ~ vic_1821*time",
	imp_long2_boys_par, imp_long2_girls_par, digits)

write.csv(out, "did_table.csv", row.names = FALSE, na = "")


#Also want to plots estimates from model 2 (may be Figure 1)
group <- expand.grid(c(0,1),c(16,23))
plot_est <- data.frame(cbind(group, matrix(nrow=dim(group)[1], ncol=2)))
colnames(plot_est) <- c("ipva","age","log_mfq_boys","log_mfq_girls")

#Fit model 2 again (easier than using mi function)
didreg_boys = pool(with(imp_long2_boys, exp = lm(log_depress ~ vic_1821*time)))

plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_boys"] <- summary(didreg_boys)[1,2]
plot_est[plot_est$ipva==0 & plot_est$age==23,"log_mfq_boys"] <- summary(didreg_boys)[1,2]+summary(didreg_boys)[3,2]
plot_est[plot_est$ipva==1 & plot_est$age==16,"log_mfq_boys"] <- summary(didreg_boys)[1,2]+summary(didreg_boys)[2,2]
plot_est[plot_est$ipva==1 & plot_est$age==23,"log_mfq_boys"] <- summary(didreg_boys)[1,2]+summary(didreg_boys)[2,2]+
																	+summary(didreg_boys)[3,2]++summary(didreg_boys)[4,2]
#Now filling in for that middle bit...
# plot_est[plot_est$ipva==0 & plot_est$age==18,"log_mfq_boys"] <- plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_boys"]+
# 																(((plot_est[plot_est$ipva==0 & plot_est$age==23,"log_mfq_boys"]-
# 																plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_boys"])/(23-16))*(18-16))
# plot_est[plot_est$ipva==1 & plot_est$age==18,"log_mfq_boys"] <- plot_est[plot_est$ipva==0 & plot_est$age==18,"log_mfq_boys"]+
# 																+summary(didreg_boys)[2,2]

didreg_girls = pool(with(imp_long2_girls, exp = lm(log_depress ~ vic_1821*time)))

plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_girls"] <- summary(didreg_girls)[1,2]
plot_est[plot_est$ipva==0 & plot_est$age==23,"log_mfq_girls"] <- summary(didreg_girls)[1,2]+summary(didreg_girls)[3,2]
plot_est[plot_est$ipva==1 & plot_est$age==16,"log_mfq_girls"] <- summary(didreg_girls)[1,2]+summary(didreg_girls)[2,2]
plot_est[plot_est$ipva==1 & plot_est$age==23,"log_mfq_girls"] <- summary(didreg_girls)[1,2]+summary(didreg_girls)[2,2]+
																	+summary(didreg_girls)[3,2]++summary(didreg_girls)[4,2]
#Now filling in for that middle bit...
# plot_est[plot_est$ipva==0 & plot_est$age==18,"log_mfq_girls"] <- plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_girls"]+
# 																(((plot_est[plot_est$ipva==0 & plot_est$age==23,"log_mfq_girls"]-
# 																plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_girls"])/(23-16))*(18-16))
# plot_est[plot_est$ipva==1 & plot_est$age==18,"log_mfq_girls"] <- plot_est[plot_est$ipva==0 & plot_est$age==18,"log_mfq_girls"]+
# 																+summary(didreg_boys)[2,2]

plot_est$ipva <- factor(plot_est$ipva)
ymin <- min(c(min(plot_est$log_mfq_boys),min(plot_est$log_mfq_girls)))
ymax <- max(c(max(plot_est$log_mfq_boys),max(plot_est$log_mfq_girls)))

#Squash axis
source("C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/Training/R/squash_axis.R")

plot_men <- ggplot(plot_est, aes(x=age, y=log_mfq_boys)) + 
	geom_point(aes(colour = ipva),size=2, shape=18) +
	geom_line(aes(colour = ipva)) +
	ggtitle("Men") +
	xlab("Age") +
	ylab("Log MFQ score") +
	geom_vline(xintercept = c(18,21), linetype = "dashed") +
	#scale_x_discrete(limits=c(16,17,18,19,20,21,22,23))
	ylim(ymin, ymax) +
	theme(axis.title.x=element_blank(),
			legend.position = "none")

plot_women <- ggplot(plot_est, aes(x=age, y=log_mfq_girls)) + 
	geom_point(aes(colour = ipva),size=2, shape=18) +
	geom_line(aes(colour = ipva)) +
	ggtitle("Women") +
	xlab("Age") +
	ylab("Log MFQ score") +
	geom_vline(xintercept = c(18,21), linetype = "dashed") +
	ylim(ymin, ymax) +
	theme(axis.title.x=element_blank(),
			axis.title.y=element_blank(),
			axis.ticks.y=element_blank()) +
	#scale_fill_discrete(name = "IPVA", labels = c("No", "Yes"))
	labs(colour = "IPVA", labels = c("No", "Yes"))

tiff(file="did_plot.tiff", units="in", width=10, height=5, res=300)
grid.arrange(arrangeGrob(plot_men,plot_women,ncol=2,widths=c(0.9,1), 
	bottom=textGrob("Age",hjust=0)))
dev.off()


################################################################################
# 5. Balance in covariates (as standardised differences) before IPTW 
# and distribution of propensity scores
################################################################################

#Create a table for standardised differences before and after weighting
out <- data.frame(matrix(NA, nrow = length(cov_list)+1, ncol = 5))
colnames(out) <- c("var","sd_men","wgt_sd_men","sd_women","wgt_sd_women")
out[1, ] <- c("","men","men","women","women")
out[2:length(cov_list),1] <- cov_list_log[cov_list_log!="log_mfqtot_ccs"]
out[length(cov_list)+1,1] <- "log_mfqtot_ccs"
out[2:(length(cov_list)+1),2:5] <- 0

#Check univarable associations and slot in standardised differences (pre-weighting)
# for (c in cov_list_log[cov_list_log!="log_mfqtot_ccs"]){
# 	model <- paste0("vic_1821 ~", c)
# 	fit <- pool(with(data = imp_boys_sub2, glm(as.formula(model), family = "binomial")))
# 	print(paste0(summary(fit)[2,1],", boys"))
# 	print(exp(summary(fit)[2,2]))
# 	print(exp(summary(fit)[2,2]-1.96*summary(fit)[2,3]))
# 	print(exp(summary(fit)[2,2]+1.96*summary(fit)[2,3]))

# 	fit <- pool(with(data = imp_girls_sub2, glm(as.formula(model), family = "binomial")))
# 	print(paste0(summary(fit)[2,1],", girls"))
# 	print(exp(summary(fit)[2,2]))
# 	print(exp(summary(fit)[2,2]-1.96*summary(fit)[2,3]))
# 	print(exp(summary(fit)[2,2]+1.96*summary(fit)[2,3]))
# 	}

for (c in cov_list_log[cov_list_log!="log_mfqtot_ccs"]){
	model1 <- paste0(c," ~ vic_1821")
	model2 <- paste0("vic_1821 ~",covs_log)
	#Additional model for stablised weights - intercept model
	model3 <- paste0("vic_1821 ~ 1")
		for (i in 1:nimp){
		#Men
		a <- complete(imp_boys_sub2, i)

		fit_uw <- glm(as.formula(model1), data=a, family = binomial(link = "logit"))
		a[,"prob_uw"] <- predict(fit_uw,a,type="response")
		p_treat_uw <- mean(a[a$vic_1821==1,"prob_uw"])
		p_control_uw <- mean(a[a$vic_1821==0,"prob_uw"])
		#Standardised differences before weighting
		#Running sum - will take average later
		# n1 <- length(a$vic_1821[a$vic_1821==1])
		# n2 <- length(a$vic_1821[a$vic_1821==0])
		out[out[,1]==c,2] <- as.numeric(out[out[,1]==c,2])+((p_treat_uw-p_control_uw)/
			(sqrt(((p_treat_uw*(1-p_treat_uw))+(p_control_uw*(1-p_control_uw)))/2)))
		
		#Can't use simpler method as doesn't produce negative differences!
		# #Try a simpler method 
		# tabbefore <- CreateTableOne(vars = c,
		# data = a,
		# strata = "vic_1821",
		# factorVars = c,
		# smd = T)
		# tabbefore <- print(tabbefore,
		# printToggle = FALSE,
		# noSpaces = TRUE, smd=TRUE,
		# quote=T)
		# out[out[,1]==c,2] <- as.numeric(out[out[,1]==c,2])+
		# 						as.numeric(tabbefore[length(tabbefore)])

		b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
		a[,"ps"] <- predict(b,a,type="response")
		a[,"wgt.ATE"] <- ifelse(
	 		a$vic_1821 == 1, 1/a$ps,
	 		1/(1-a$ps))
		#Stabilised weights
		b <- glm(as.formula(model3), data=a, family = binomial(link = "logit"))
		a[,"ps_int"] <- predict(b,a,type="response")
		a[,"wgt.ATE_stab"] <- ifelse(
			a$vic_1821 == 1, a$ps_int/a$ps,
	 		(1-a$ps_int)/(1-a$ps))

		#The following may spring up a warning message, but this is just glm(),
		#being fussy about numbers of successes in model.  It will still fit.
		#Standardised differences after weighting
		fit_w <- glm(as.formula(model1), data=a, family = binomial(link = "logit"), weight = (wgt.ATE_stab))
		a[,"prob_w"] <- predict(fit_w,a,type="response")
		p_treat_w <- mean(a[a$vic_1821==1,"prob_w"])
		p_control_w <- mean(a[a$vic_1821==0,"prob_w"])
		out[out[,1]==c,3] <- as.numeric(out[out[,1]==c,3])+((p_treat_w-p_control_w)/
			(sqrt(((p_treat_w*(1-p_treat_w))+(p_control_w*(1-p_control_w)))/2)))

		#Women
		a <- complete(imp_girls_sub2, i)
		fit_uw <- glm(as.formula(model1), data=a, family = binomial(link = "logit"))
		a[,"prob_uw"] <- predict(fit_uw,a,type="response")
		p_treat_uw <- mean(a[a$vic_1821==1,"prob_uw"])
		p_control_uw <- mean(a[a$vic_1821==0,"prob_uw"])
		out[out[,1]==c,4] <- as.numeric(out[out[,1]==c,4])+((p_treat_uw-p_control_uw)/
			(sqrt(((p_treat_uw*(1-p_treat_uw))+(p_control_uw*(1-p_control_uw)))/2)))
		# tabbefore <- CreateTableOne(vars = c,
		# data = a,
		# strata = "vic_1821",
		# factorVars = c,
		# smd = T)
		# tabbefore <- print(tabbefore,
		# printToggle = FALSE,
		# noSpaces = TRUE, smd=TRUE,
		# quote=T)
		# out[out[,1]==c,4] <- as.numeric(out[out[,1]==c,4])+
		# 						as.numeric(tabbefore[length(tabbefore)])

		b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
		a[,"ps"] <- predict(b,a,type="response")
		a[,"wgt.ATE"] <- ifelse(
	 		a$vic_1821 == 1, 1/a$ps,
	 		1/(1-a$ps))
		b <- glm(as.formula(model3), data=a, family = binomial(link = "logit"))
		a[,"ps_int"] <- predict(b,a,type="response")
		a[,"wgt.ATE_stab"] <- ifelse(
			a$vic_1821 == 1, a$ps_int/a$ps,
	 		(1-a$ps_int)/(1-a$ps))
		fit_w <- glm(as.formula(model1), data=a, family = binomial(link = "logit"), weight = (wgt.ATE_stab))
		a[,"prob_w"] <- predict(fit_w,a,type="response")
		p_treat_w <- mean(a[a$vic_1821==1,"prob_w"])
		p_control_w <- mean(a[a$vic_1821==0,"prob_w"])
		out[out[,1]==c,5] <- as.numeric(out[out[,1]==c,5])+((p_treat_w-p_control_w)/
			(sqrt(((p_treat_w*(1-p_treat_w))+(p_control_w*(1-p_control_w)))/2)))
		}
	}

#Now MFQ CCS separately (as standardised difference in mean not prevalence)
model1 <- paste0("log_mfqtot_ccs ~ vic_1821")

for (i in 1:nimp){
	#Men
	a <- complete(imp_boys_sub2, i)
	#Until re-run, there can me an instance where called mfqtot_ccs.1 instead
	names(a)[names(a)=="mfqtot_ccs.1"]<- "mfqtot_ccs"
	fit_uw <- lm(as.formula(model1), data=a)
	a[,"mean_uw"] <- predict(fit_uw,a,type="response")
	mean_treat_uw <- mean(a[a$vic_1821==1,"mean_uw"])
	mean_control_uw <- mean(a[a$vic_1821==0,"mean_uw"])
	#Need to work out SEM for standardised diff formula
	n1 <- length(a$vic_1821[a$vic_1821==1])
	n2 <- length(a$vic_1821[a$vic_1821==0])
	a[,"x_minus_mean_sq"] <- ifelse(
		a$vic_1821 == 1, (a$log_mfqtot_ccs-mean_treat_uw)^2,
		(a$log_mfqtot_ccs-mean_control_uw)^2)
	var_treat <- sum(a[a$vic_1821==1,"x_minus_mean_sq"])/(n1-1)
	var_control <- sum(a[a$vic_1821==0,"x_minus_mean_sq"])/(n2-1)
	#Standardised differences before weighting
	out[out[,1]=="log_mfqtot_ccs",2] <- as.numeric(out[out[,1]==c,2])+
			((mean_treat_uw-mean_control_uw)/
			(sqrt((var_treat+var_control)/2)))

	# tabbefore <- CreateTableOne(vars = "log_mfqtot_ccs",
	# data = a,
	# strata = "vic_1821",
	# smd = T)
	# tabbefore <- print(tabbefore,
	# printToggle = FALSE,
	# noSpaces = TRUE, smd=TRUE,
	# quote=T)
	# out[out[,1]=="log_mfqtot_ccs",2] <- as.numeric(out[out[,1]==c,2])+
	# 									as.numeric(tabbefore[length(tabbefore)])

	b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	b <- glm(as.formula(model3), data=a, family = binomial(link = "logit"))
	a[,"ps_int"] <- predict(b,a,type="response")
	a[,"wgt.ATE_stab"] <- ifelse(
		a$vic_1821 == 1, a$ps_int/a$ps,
 		(1-a$ps_int)/(1-a$ps))
	fit_w <- lm(as.formula("log_mfqtot_ccs ~ vic_1821"), data=a, weight = (wgt.ATE_stab))
	a[,"mean_w"] <- predict(fit_w,a,type="response")
	mean_treat_w <- mean(a[a$vic_1821==1,"mean_w"])
	mean_control_w <- mean(a[a$vic_1821==0,"mean_w"])
	a[,"x_minus_mean_sq"] <- ifelse(
		a$vic_1821 == 1, (a$log_mfqtot_ccs-mean_treat_w)^2,
		(a$log_mfqtot_ccs-mean_control_w)^2)
	var_treat <- sum(a[a$vic_1821==1,"x_minus_mean_sq"])/(n1-1)
	var_control <- sum(a[a$vic_1821==0,"x_minus_mean_sq"])/(n2-1)
	#Standardised differences after weighting
	out[out[,1]=="log_mfqtot_ccs",3] <- as.numeric(out[out[,1]==c,3])+((mean_treat_w-mean_control_w)/
			(sqrt((var_treat+var_control)/2)))
		
	#Women
	a <- complete(imp_girls_sub2, i)
	names(a)[names(a)=="mfqtot_ccs.1"]<- "mfqtot_ccs"
	fit_uw <- lm(as.formula(model1), data=a)
	a[,"mean_uw"] <- predict(fit_uw,a,type="response")
	mean_treat_uw <- mean(a[a$vic_1821==1,"mean_uw"])
	mean_control_uw <- mean(a[a$vic_1821==0,"mean_uw"])
	n1 <- length(a$vic_1821[a$vic_1821==1])
	n2 <- length(a$vic_1821[a$vic_1821==0])
	a[,"x_minus_mean_sq"] <- ifelse(
		a$vic_1821 == 1, (a$log_mfqtot_ccs-mean_treat_uw)^2,
		(a$log_mfqtot_ccs-mean_control_uw)^2)
	var_treat <- sum(a[a$vic_1821==1,"x_minus_mean_sq"])/(n1-1)
	var_control <- sum(a[a$vic_1821==0,"x_minus_mean_sq"])/(n2-1)
	out[out[,1]=="log_mfqtot_ccs",4] <- as.numeric(out[out[,1]==c,2])+((mean_treat_uw-mean_control_uw)/
			(sqrt((var_treat+var_control)/2)))
	# tabbefore <- CreateTableOne(vars = "log_mfqtot_ccs",
	# data = a,
	# strata = "vic_1821",
	# smd = T)
	# tabbefore <- print(tabbefore,
	# printToggle = FALSE,
	# noSpaces = TRUE, smd=TRUE,
	# quote=T)
	# out[out[,1]=="log_mfqtot_ccs",4] <- as.numeric(out[out[,1]==c,2])+
	# 									as.numeric(tabbefore[length(tabbefore)])

	b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	b <- glm(as.formula(model3), data=a, family = binomial(link = "logit"))
	a[,"ps_int"] <- predict(b,a,type="response")
	a[,"wgt.ATE_stab"] <- ifelse(
		a$vic_1821 == 1, a$ps_int/a$ps,
	 	(1-a$ps_int)/(1-a$ps))
	fit_w <- lm("log_mfqtot_ccs ~ vic_1821", data=a, weight = (wgt.ATE_stab))
	a[,"mean_w"] <- predict(fit_w,a,type="response")
	mean_treat_w <- mean(a[a$vic_1821==1,"mean_w"])
	mean_control_w <- mean(a[a$vic_1821==0,"mean_w"])
	a[,"x_minus_mean_sq"] <- ifelse(
		a$vic_1821 == 1, (a$log_mfqtot_ccs-mean_treat_w)^2,
		(a$log_mfqtot_ccs-mean_control_w)^2)
	var_treat <- sum(a[a$vic_1821==1,"x_minus_mean_sq"])/(n1-1)
	var_control <- sum(a[a$vic_1821==0,"x_minus_mean_sq"])/(n2-1)
	#Standardised differences after weighting
	out[out[,1]=="log_mfqtot_ccs",5] <- as.numeric(out[out[,1]==c,5])+((mean_treat_w-mean_control_w)/
			(sqrt((var_treat+var_control)/2)))
	}

#Finsh and save table
for (i in 2:5){
	out[2:(length(cov_list)+1),i] <- as.numeric(out[2:(length(cov_list)+1),i])/nimp
	}
#Remove row 1 until tidy up later
out <- out[2:length(cov_list)+1,]
write.csv(out, paste0("standardised_differences.csv"), row.names = FALSE, na = "")

###Create a plot
# forestplot <- function(d,x,y,xlab,ylab,colour){
#     require(ggplot2)
#     p <- ggplot(d, aes(x=as.numeric(d[,x]), y=d[,y])) + 
# 		geom_point(size=2, color=colour, shape=18) +
# 		#geom_pointrange() + 
# 		#coord_flip() +
# 		#geom_hline(aes(x=0), lty=2) +
# 		xlab(xlab) +
# 		ylab(ylab) #switch because of the coord_flip() above
#     return(p)
# 	}
# forestplot(out,"sd_men","var","Standardised Difference","Covariate","blue")
# forestplot(out,"wgt_sd_men","var","Standardised Difference","Covariate","red")

#sapply(c("sd_men","wgt_sd_men","sd_women","wgt_sd_women"), function(x) out[,x] <- ifelse(is.na(out[,x]), 0, out[,x]))
xmin <- floor(min(c(min(as.numeric(out[is.na(out[,"sd_men"])==FALSE,"sd_men"])),
					min(as.numeric(out[is.na(out[,"wgt_sd_men"])==FALSE,"wgt_sd_men"])),
					min(as.numeric(out[is.na(out[,"sd_women"])==FALSE,"sd_women"])),
					min(as.numeric(out[is.na(out[,"wgt_sd_women"])==FALSE,"wgt_sd_women"]))
				))*100)/100
xmax <- ceiling(max(c(max(as.numeric(out[is.na(out[,"sd_men"])==FALSE,"sd_men"])),
					max(as.numeric(out[is.na(out[,"wgt_sd_men"])==FALSE,"wgt_sd_men"])),
					max(as.numeric(out[is.na(out[,"sd_women"])==FALSE,"sd_women"])),
					max(as.numeric(out[is.na(out[,"wgt_sd_women"])==FALSE,"wgt_sd_women"]))
				))*100)/100

#So y-axis in right order
out$var=factor(out$var,levels=out$var)

labels <- c("White ethnicity 0y","Sexual minority 15y","Anxiety 17y","Anti-social behaviour 14y",
			"Weekly smoking 16y","Hazardous alcohol use 17y","Weekly cannabis use 16y",
			"Any illicit (non-cannabis) drug use 16y",
			"Extreme parental monitoring 15y",
			"Emotional abuse 16y","Physical abuse 0-16y","Sexual abuse 0-16y","Emotional neglect 0-16y",
			"Bullying 0-16y","Violence between parents 0-16y","Parental mental health problem 0-16y",
			"Household substance abuse 0-16y","Parental conviction 0-16y","Parental separation 0-16y",
			"Low self-esteem 17y","Overweight 17y","Sleep problems 17y","Parent with O-level qualifications 0y",
			"Log MFQ score 16y")

plot_men <- ggplot(out, aes(x=as.numeric(out[,"sd_men"]), y=out[,"var"])) + 
	geom_point(size=2, color="blue", shape=18) +
	ggtitle("Men") +
	xlab("Standardised Difference") +
	ylab("Covariate") +
	xlim(xmin, xmax) +
	scale_y_discrete(limits = rev(levels(out$var)),
		labels=rev(labels)
		) +
	geom_point(x=as.numeric(out[,"wgt_sd_men"]), size=2, color="red", shape=18) +
	geom_vline(xintercept = c(-0.05,0.05), linetype = "dashed") +
	theme(axis.title.x=element_blank())
	

plot_women <- ggplot(out, aes(x=as.numeric(out[,"sd_women"]), y=out[,"var"])) + 
	geom_point(size=2, color="blue", shape=18) +
	ggtitle("Women") +
	xlab("Standardised Difference") +
	ylab("Covariate") +
	xlim(xmin, xmax) +
	#scale_x_discrete(name ="Standardised Difference"
		#,limits=seq(xmin,xmax,by=0.05)
	#	) +
	scale_y_discrete(limits = rev(levels(out$var))) +
	geom_point(x=as.numeric(out[,"wgt_sd_women"]), size=2, color="red", shape=18) +
	geom_vline(xintercept = c(-0.05,0.05), linetype = "dashed") +
	#scale_x_discrete(labels=c("-0.10","-0.05","0.0","0.05","0.10","0.15","0.20","0.25","0.30"))
	theme(axis.title.x=element_blank(),
		axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

tiff(file="standardised_differences.tiff", units="in", width=8, height=5, res=300)
grid.arrange(arrangeGrob(plot_men,plot_women,ncol=2,widths=c(1.8,1)), bottom=textGrob("Standardised Difference",hjust=0))
dev.off()

#Histogram of propensity scores for first imputation
a <- complete(imp_boys_sub, 1)
b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
a[,"ps"] <- predict(b,a,type="response")
a[,"wgt.ATE"] <- ifelse(
		a$vic_1821 == 1, 1/a$ps,
		1/(1-a$ps))
#REPLACE WITH STANDARDISED WEIGHTS
a$vic_1821 <- factor(a$vic_1821)

jpeg(file="ps_density_by_trt_boys.jpeg")
a %>% ggplot(aes(x=ps, fill=vic_1821)) +
    geom_density(color="#e9ecef", alpha=0.6, 
    	position = 'identity')
dev.off()

a <- complete(imp_girls_sub, 1)
b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
a[,"ps"] <- predict(b,a,type="response")
a[,"wgt.ATE"] <- ifelse(
		a$vic_1821 == 1, 1/a$ps,
		1/(1-a$ps))
#REPLACE WITH STANDARDISED WEIGHTS
a$vic_1821 <- factor(a$vic_1821)

jpeg(file="ps_density_by_trt_girls.jpeg")
a %>% ggplot(aes(x=ps, fill=vic_1821)) +
    geom_density(color="#e9ecef", alpha=0.6, 
    	position = 'identity')
dev.off()


################################################################################
# 6. Standard linear regression (crude coefficients), 
# regular adjustment (handful of covariates and all covariates), 
# IPTW (both raw MFQ and logged MFQ)
# In all cases check normality of residuals through Kolmogorov-Smirnof test and
# histogram of residuals
# - Forms part of Tables 2, 3, 4, and S2
################################################################################

#Can make this following block more condensed
models <- c("Crude raw, MI","Adj 1 raw, MI","Adj 2 raw, MI","IPTW raw, MI",
			"Crude logMFQ, MI","Adj 1 logMFQ, MI","Adj 2 logMFQ, MI","IPTW logMFQ, MI",
			"Crude raw, CCA","Adj 1 raw, CCA","Adj 2 raw, CCA","IPTW raw, CCA",
			"Crude logMFQ, CCA","Adj 1 logMFQ, CCA","Adj 2 logMFQ, CCA","IPTW logMFQ, CCA")
nmodel <- length(models)

#Adjustment 1 uses just a subset of covariates (cov_list2 for raw MFQ or cov_list3 for log MFQ)

#Adjustment 2 is all of cov_list or cov_list_log

#Create a table for results
out <- data.frame(matrix(NA, nrow = ((nmodel*2)+1), ncol = 10))
colnames(out) <- c("","","Males",rep("",3),"Females",rep("",3))
out[1, ] <- c("Model","Term",rep(c("Coef","SE","Coef p-value","KS p-value"),2))
out[seq(2,(nmodel*2),by=2),1] <- models
out[2:((nmodel*2)+1),2] <- rep(c("Intercept","Victimisation"),nmodel)
level <- 2
digits <- 2

#Definitely need to condense the following
#Raw MFQ, MI
model_list <- c("mfqtot_ypc ~ vic_1821", 
				paste0("mfqtot_ypc ~ vic_1821 +",covs2),
				paste0("mfqtot_ypc ~ vic_1821 +",covs))

for (m in model_list){
	vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(m)))
	out[level,3] <- summary(pool(vic_mfq23_fit))[1,2]
	out[level,4] <- summary(pool(vic_mfq23_fit))[1,3]
	out[level,5] <- summary(pool(vic_mfq23_fit))[1,6]

	out[level+1,3] <- summary(pool(vic_mfq23_fit))[2,2]
	out[level+1,4] <- summary(pool(vic_mfq23_fit))[2,3]
	out[level+1,5] <- summary(pool(vic_mfq23_fit))[2,6]
	a <- complete(imp_boys_sub, 1)
	a$vic_1821 <- factor(a$vic_1821)
	b <- lm(as.formula(m), data=a)
	a$res <- resid(b)
	#out[level,6] <- ols_test_normality(b)$kolmogorv$p.value
	jpeg(file=paste0(models[level/2],"_boys.jpeg"))
	a %>% ggplot(aes(x=res, fill=vic_1821)) +
	    geom_density(color="#e9ecef", alpha=0.6, 
	    	position = 'identity')
	dev.off()

	vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(m)))
	out[level,7] <- summary(pool(vic_mfq23_fit))[1,2]
	out[level,8] <- summary(pool(vic_mfq23_fit))[1,3]
	out[level,9] <- summary(pool(vic_mfq23_fit))[1,6]

	out[level+1,7] <- summary(pool(vic_mfq23_fit))[2,2]
	out[level+1,8] <- summary(pool(vic_mfq23_fit))[2,3]
	out[level+1,9] <- summary(pool(vic_mfq23_fit))[2,6]
	a <- complete(imp_girls_sub, 1)
	a$vic_1821 <- factor(a$vic_1821)
	b <- lm(as.formula(m), data=a)
	a$res <- resid(b)
	#out[level,10] <- ols_test_normality(b)$kolmogorv$p.value
	jpeg(file=paste0(models[level/2],"_girls.jpeg"))
	a %>% ggplot(aes(x=res, fill=vic_1821)) +
	    geom_density(color="#e9ecef", alpha=0.6, 
	    	position = 'identity')
	dev.off()
	level <- level+2
	}

source("C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/Training/R/iptw_mi.R")
iptw_raw <- iptw_mi("mfqtot_ypc","vic_1821",cov_list,imp_boys_sub,imp_girls_sub)
out[level,3] <- iptw_raw[nimp+2,2]
out[level,4] <- iptw_raw[nimp+2,3]
out[level,5] <- iptw_raw[nimp+2,4]
out[level,6] <- iptw_raw[nimp+2,8]
out[level,7] <- iptw_raw[nimp+2,19]
out[level,8] <- iptw_raw[nimp+2,20]
out[level,9] <- iptw_raw[nimp+2,21]
out[level,10] <- iptw_raw[nimp+2,25]

out[level+1,3] <- iptw_raw[nimp+2,5]
out[level+1,4] <- iptw_raw[nimp+2,6]
out[level+1,5] <- iptw_raw[nimp+2,7]
out[level+1,7] <- iptw_raw[nimp+2,22]
out[level+1,8] <- iptw_raw[nimp+2,23]
out[level+1,9] <- iptw_raw[nimp+2,24]
write.csv(iptw_raw, "imputed_ps_distn_mfq23.csv", row.names = FALSE, na = "")
level <- level+2

#Log MFQ, MI
model_list <- c("log_mfqtot_ypc ~ vic_1821", 
				paste0("log_mfqtot_ypc ~ vic_1821 +",covs3),
				paste0("log_mfqtot_ypc ~ vic_1821 +",covs_log))

for (m in model_list){
	a <- complete(imp_boys_sub, action='long',include=TRUE)
	a[,"log_mfqtot_ypc"] <- log(a$mfqtot_ypc+1)
	imp_boys_sub <- as.mids(a)
	vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(m)))
	out[level,3] <- summary(pool(vic_mfq23_fit))[1,2]
	out[level,4] <- summary(pool(vic_mfq23_fit))[1,3]
	out[level,5] <- summary(pool(vic_mfq23_fit))[1,6]

	out[level+1,3] <- summary(pool(vic_mfq23_fit))[2,2]
	out[level+1,4] <- summary(pool(vic_mfq23_fit))[2,3]
	out[level+1,5] <- summary(pool(vic_mfq23_fit))[2,6]
	a <- complete(imp_boys_sub, 1)
	a$vic_1821 <- factor(a$vic_1821)
	b <- lm(as.formula(m), data=a)
	a$res <- resid(b)
	#out[level,6] <- ols_test_normality(b)$kolmogorv$p.value
	jpeg(file=paste0(models[level/2],"_boys.jpeg"))
	a %>% ggplot(aes(x=res, fill=vic_1821)) +
	    geom_density(color="#e9ecef", alpha=0.6, 
	    	position = 'identity')
	dev.off()

	a <- complete(imp_girls_sub, action='long',include=TRUE)
	a[,"log_mfqtot_ypc"] <- log(a$mfqtot_ypc+1)
	imp_girls_sub <- as.mids(a)
	vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(m)))
	out[level,7] <- summary(pool(vic_mfq23_fit))[1,2]
	out[level,8] <- summary(pool(vic_mfq23_fit))[1,3]
	out[level,9] <- summary(pool(vic_mfq23_fit))[1,6]

	out[level+1,7] <- summary(pool(vic_mfq23_fit))[2,2]
	out[level+1,8] <- summary(pool(vic_mfq23_fit))[2,3]
	out[level+1,9] <- summary(pool(vic_mfq23_fit))[2,6]
	a <- complete(imp_girls_sub, 1)
	a$vic_1821 <- factor(a$vic_1821)
	b <- lm(as.formula(m), data=a)
	a$res <- resid(b)
	#out[level,10] <- ols_test_normality(b)$kolmogorv$p.value
	jpeg(file=paste0(models[level/2],"_girls.jpeg"))
	a %>% ggplot(aes(x=res, fill=vic_1821)) +
	    geom_density(color="#e9ecef", alpha=0.6, 
	    	position = 'identity')
	dev.off()
	level <- level+2
	}

iptw_log <- iptw_mi("log_mfqtot_ypc","vic_1821",cov_list_log,imp_boys_sub,imp_girls_sub)
out[level,3] <- iptw_log[nimp+2,2]
out[level,4] <- iptw_log[nimp+2,3]
out[level,5] <- iptw_log[nimp+2,4]
out[level,6] <- iptw_log[nimp+2,8]
out[level,7] <- iptw_log[nimp+2,19]
out[level,8] <- iptw_log[nimp+2,20]
out[level,9] <- iptw_log[nimp+2,21]
out[level,10] <- iptw_log[nimp+2,25]

out[level+1,3] <- iptw_log[nimp+2,5]
out[level+1,4] <- iptw_log[nimp+2,6]
out[level+1,5] <- iptw_log[nimp+2,7]
out[level+1,7] <- iptw_log[nimp+2,22]
out[level+1,8] <- iptw_log[nimp+2,23]
out[level+1,9] <- iptw_log[nimp+2,24]
write.csv(iptw_log, "imputed_ps_distn_logmfq23.csv", row.names = FALSE, na = "")
level <- level+2

#Raw MFQ, CCA
model_list <- c("mfqtot_ypc ~ vic_1821", 
				paste0("mfqtot_ypc ~ vic_1821 +",covs2),
				paste0("mfqtot_ypc ~ vic_1821 +",covs))

for (m in model_list){
	vic_mfq23_fit <-lm(as.formula(m), data=cohort.dta_sub_boys)
	out[level,3] <- summary(vic_mfq23_fit)$coef[1,1]
	out[level,4] <- summary(vic_mfq23_fit)$coef[1,2]
	out[level,5] <- summary(vic_mfq23_fit)$coef[1,4]

	out[level+1,3] <- summary(vic_mfq23_fit)$coef[2,1]
	out[level+1,4] <- summary(vic_mfq23_fit)$coef[2,2]
	out[level+1,5] <- summary(vic_mfq23_fit)$coef[2,4]
	#out[level,6] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
	vic_mfq23_fit <-lm(as.formula(m), data=cohort.dta_sub_girls)
	out[level,7] <- summary(vic_mfq23_fit)$coef[1,1]
	out[level,8] <- summary(vic_mfq23_fit)$coef[1,2]
	out[level,9] <- summary(vic_mfq23_fit)$coef[1,4]

	out[level+1,7] <- summary(vic_mfq23_fit)$coef[2,1]
	out[level+1,8] <- summary(vic_mfq23_fit)$coef[2,2]
	out[level+1,9] <- summary(vic_mfq23_fit)$coef[2,4]
	#out[level,10] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
	level <- level+2
	}

model1 <- paste0("vic_1821 ~ ",covs)
model2 <- paste0("mfqtot_ypc ~ vic_1821")
b <- glm(as.formula(model1), data=cohort.dta_sub_boys, family = binomial(link = "logit"))
cohort.dta_sub_boys[,"ps"] <- predict(b,cohort.dta_sub_boys,type="response")
cohort.dta_sub_boys[,"wgt.ATE"] <- ifelse(
		cohort.dta_sub_boys$vic_1821 == 1, 1/cohort.dta_sub_boys$ps,
		1/(1-cohort.dta_sub_boys$ps))
vic_mfq23_fit <- lm(as.formula(model2), data=cohort.dta_sub_boys, weight = (wgt.ATE))
out[level,3] <- summary(vic_mfq23_fit)$coef[1,1]
out[level,4] <- summary(vic_mfq23_fit)$coef[1,2]
out[level,5] <- summary(vic_mfq23_fit)$coef[1,4]

out[level+1,3] <- summary(vic_mfq23_fit)$coef[2,1]
out[level+1,4] <- summary(vic_mfq23_fit)$coef[2,2]
out[level+1,5] <- summary(vic_mfq23_fit)$coef[2,4]
#out[level,6] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value

b <- glm(as.formula(model1), data=cohort.dta_sub_girls, family = binomial(link = "logit"))
cohort.dta_sub_girls[,"ps"] <- predict(b,cohort.dta_sub_girls,type="response")
cohort.dta_sub_girls[,"wgt.ATE"] <- ifelse(
		cohort.dta_sub_girls$vic_1821 == 1, 1/cohort.dta_sub_girls$ps,
		1/(1-cohort.dta_sub_girls$ps))
vic_mfq23_fit <- lm(as.formula(model2), data=cohort.dta_sub_girls, weight = (wgt.ATE))
out[level,7] <- summary(vic_mfq23_fit)$coef[1,1]
out[level,8] <- summary(vic_mfq23_fit)$coef[1,2]
out[level,9] <- summary(vic_mfq23_fit)$coef[1,4]

out[level+1,7] <- summary(vic_mfq23_fit)$coef[2,1]
out[level+1,8] <- summary(vic_mfq23_fit)$coef[2,2]
out[level+1,9] <- summary(vic_mfq23_fit)$coef[2,4]
#out[level,10] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
level <- level+2

#Log MFQ, CCA
model_list <- c("log_mfqtot_ypc ~ vic_1821", 
				paste0("log_mfqtot_ypc ~ vic_1821 +",covs3),
				paste0("log_mfqtot_ypc ~ vic_1821 +",covs_log))

for (m in model_list){
	vic_mfq23_fit <-lm(as.formula(m), data=cohort.dta_sub_boys)
	out[level,3] <- summary(vic_mfq23_fit)$coef[1,1]
	out[level,4] <- summary(vic_mfq23_fit)$coef[1,2]
	out[level,5] <- summary(vic_mfq23_fit)$coef[1,4]

	out[level+1,3] <- summary(vic_mfq23_fit)$coef[2,1]
	out[level+1,4] <- summary(vic_mfq23_fit)$coef[2,2]
	out[level+1,5] <- summary(vic_mfq23_fit)$coef[2,4]
	#out[level,6] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
	vic_mfq23_fit <-lm(as.formula(m), data=cohort.dta_sub_girls)
	out[level,7] <- summary(vic_mfq23_fit)$coef[1,1]
	out[level,8] <- summary(vic_mfq23_fit)$coef[1,2]
	out[level,9] <- summary(vic_mfq23_fit)$coef[1,4]

	out[level+1,7] <- summary(vic_mfq23_fit)$coef[2,1]
	out[level+1,8] <- summary(vic_mfq23_fit)$coef[2,2]
	out[level+1,9] <- summary(vic_mfq23_fit)$coef[2,4]
	#out[level,10] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
	level <- level+2
	}

model1 <- paste0("vic_1821 ~ ",covs_log)
model2 <- paste0("log_mfqtot_ypc ~ vic_1821")
b <- glm(as.formula(model1), data=cohort.dta_sub_boys, family = binomial(link = "logit"))
cohort.dta_sub_boys[,"ps"] <- predict(b,cohort.dta_sub_boys,type="response")
cohort.dta_sub_boys[,"wgt.ATE"] <- ifelse(
		cohort.dta_sub_boys$vic_1821 == 1, 1/cohort.dta_sub_boys$ps,
		1/(1-cohort.dta_sub_boys$ps))
vic_mfq23_fit <- lm(as.formula(model2), data=cohort.dta_sub_boys, weight = (wgt.ATE))
out[level,3] <- summary(vic_mfq23_fit)$coef[1,1]
out[level,4] <- summary(vic_mfq23_fit)$coef[1,2]
out[level,5] <- summary(vic_mfq23_fit)$coef[1,4]

out[level+1,3] <- summary(vic_mfq23_fit)$coef[2,1]
out[level+1,4] <- summary(vic_mfq23_fit)$coef[2,2]
out[level+1,5] <- summary(vic_mfq23_fit)$coef[2,4]
#out[level,6] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value

b <- glm(as.formula(model1), data=cohort.dta_sub_girls, family = binomial(link = "logit"))
cohort.dta_sub_girls[,"ps"] <- predict(b,cohort.dta_sub_girls,type="response")
cohort.dta_sub_girls[,"wgt.ATE"] <- ifelse(
		cohort.dta_sub_girls$vic_1821 == 1, 1/cohort.dta_sub_girls$ps,
		1/(1-cohort.dta_sub_girls$ps))
vic_mfq23_fit <- lm(as.formula(model2), data=cohort.dta_sub_girls, weight = (wgt.ATE))
out[level,7] <- summary(vic_mfq23_fit)$coef[1,1]
out[level,8] <- summary(vic_mfq23_fit)$coef[1,2]
out[level,9] <- summary(vic_mfq23_fit)$coef[1,4]

out[level+1,7] <- summary(vic_mfq23_fit)$coef[2,1]
out[level+1,8] <- summary(vic_mfq23_fit)$coef[2,2]
out[level+1,9] <- summary(vic_mfq23_fit)$coef[2,4]
#out[level,10] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value

write.csv(out, paste0("regressions_inc_IPTW.csv"), row.names = FALSE, na = "")

## ---- Sensitivity analyses -------------------------------------------------------------------
#Want to fit model only on those with propensity scores in the lowest quartile, second, third, fourth
 


################################################################################
# 7. Sub-types of IPVA - regression results, logged outcome, MI  
# - Table 3
################################################################################

#Can make this following block more condensed
models <- c("Crude logMFQ, MI","Adj 1 logMFQ, MI","Adj 2 logMFQ, MI","IPTW logMFQ, MI")
nmodel <- length(models)

#Adjustment 1 uses subset of covariates cov_list3 (covs3)
#Adjustment 2 is all of cov_list_log (covs_log)

#Create a table for results
out <- data.frame(matrix(NA, nrow = ((nmodel*4)+1), ncol = 10))
colnames(out) <- c("","","Males",rep("",3),"Females",rep("",3))
out[1, ] <- c("Model","Term",rep(c("Coef","SE","Coef p-value","KS p-value"),2))
out[seq(2,(nmodel*4),by=4),1] <- models
out[2:((nmodel*4)+1),2] <- rep(c("Intercept","Psych only","Phys (no sexual)","Sexual"),nmodel)
level <- 2
digits <- 2

model_list <- c("log_mfqtot_ypc ~ vic_sub_1821", 
				paste0("log_mfqtot_ypc ~ vic_sub_1821 +",covs3),
				paste0("log_mfqtot_ypc ~ vic_sub_1821 +",covs_log))

for (m in model_list){
	a <- complete(imp_boys_sub2, action='long',include=TRUE)
	#a[,"log_mfqtot_ypc"] <- log(a$mfqtot_ypc+1)
	a$vic_sub_1821 <- factor(a$vic_sub_1821, levels=c(0,1,2,3))
	imp_boys_sub2 <- as.mids(a)
	vic_mfq23_fit <- with(imp_boys_sub2, lm(as.formula(m)))
	out[level,3] <- summary(pool(vic_mfq23_fit))[1,2]
	out[level,4] <- summary(pool(vic_mfq23_fit))[1,3]
	out[level,5] <- summary(pool(vic_mfq23_fit))[1,6]

	out[level+1,3] <- summary(pool(vic_mfq23_fit))[2,2]
	out[level+1,4] <- summary(pool(vic_mfq23_fit))[2,3]
	out[level+1,5] <- summary(pool(vic_mfq23_fit))[2,6]

	out[level+2,3] <- summary(pool(vic_mfq23_fit))[3,2]
	out[level+2,4] <- summary(pool(vic_mfq23_fit))[3,3]
	out[level+2,5] <- summary(pool(vic_mfq23_fit))[3,6]

	out[level+3,3] <- summary(pool(vic_mfq23_fit))[4,2]
	out[level+3,4] <- summary(pool(vic_mfq23_fit))[4,3]
	out[level+3,5] <- summary(pool(vic_mfq23_fit))[4,6]

	# a <- complete(imp_boys_sub, 1)
	# a$vic_sub_1821 <- factor(a$vic_sub_1821)
	# b <- lm(as.formula(m), data=a)
	# a$res <- resid(b)
	# #out[level,6] <- ols_test_normality(b)$kolmogorv$p.value
	# jpeg(file=paste0(models[level/2],"_boys.jpeg"))
	# a %>% ggplot(aes(x=res, fill=vic_sub_1821)) +
	#     geom_density(color="#e9ecef", alpha=0.6, 
	#     	position = 'identity')
	# dev.off()

	a <- complete(imp_girls_sub2, action='long',include=TRUE)
	#a[,"log_mfqtot_ypc"] <- log(a$mfqtot_ypc+1)
	a$vic_sub_1821 <- factor(a$vic_sub_1821, levels=c(0,1,2,3))
	imp_girls_sub2 <- as.mids(a)
	vic_mfq23_fit <- with(imp_girls_sub2, lm(as.formula(m)))
	out[level,7] <- summary(pool(vic_mfq23_fit))[1,2]
	out[level,8] <- summary(pool(vic_mfq23_fit))[1,3]
	out[level,9] <- summary(pool(vic_mfq23_fit))[1,6]

	out[level+1,7] <- summary(pool(vic_mfq23_fit))[2,2]
	out[level+1,8] <- summary(pool(vic_mfq23_fit))[2,3]
	out[level+1,9] <- summary(pool(vic_mfq23_fit))[2,6]

	out[level+2,7] <- summary(pool(vic_mfq23_fit))[3,2]
	out[level+2,8] <- summary(pool(vic_mfq23_fit))[3,3]
	out[level+2,9] <- summary(pool(vic_mfq23_fit))[3,6]

	out[level+3,7] <- summary(pool(vic_mfq23_fit))[4,2]
	out[level+3,8] <- summary(pool(vic_mfq23_fit))[4,3]
	out[level+3,9] <- summary(pool(vic_mfq23_fit))[4,6]

	# a <- complete(imp_girls_sub, 1)
	# a$vic_sub_1821 <- factor(a$vic_sub_1821)
	# b <- lm(as.formula(m), data=a)
	# a$res <- resid(b)
	# #out[level,10] <- ols_test_normality(b)$kolmogorv$p.value
	# jpeg(file=paste0(models[level/2],"_girls.jpeg"))
	# a %>% ggplot(aes(x=res, fill=vic_sub_1821)) +
	#     geom_density(color="#e9ecef", alpha=0.6, 
	#     	position = 'identity')
	# dev.off()
	level <- level+4
	}

write.csv(out, paste0("regressions_subtype.csv"), row.names = FALSE, na = "")

#Will have to do this without the iptw_mi function as that is based on a binary exposure
#Have a lot of reading to do - e.g. McCaffrey, about how you do this with multiple treatment groups
iptw_log_cat <- iptw_mi_cat("log_mfqtot_ypc","vic_1821",cov_list_log,imp_boys_sub,imp_girls_sub)

#COLUMN NUMBERS OF IPTW_LOG_CAT WILL CHANGE
# out[level,3] <- iptw_log_cat[nimp+2,2]
# out[level,4] <- iptw_log_cat[nimp+2,3]
# out[level,5] <- iptw_log_cat[nimp+2,4]
# out[level,6] <- iptw_log_cat[nimp+2,8]
# out[level,7] <- iptw_log_cat[nimp+2,19]
# out[level,8] <- iptw_log_cat[nimp+2,20]
# out[level,9] <- iptw_log_cat[nimp+2,21]
# out[level,10] <- iptw_log_cat[nimp+2,25]

# out[level+1,3] <- iptw_log_cat[nimp+2,5]
# out[level+1,4] <- iptw_log_cat[nimp+2,6]
# out[level+1,5] <- iptw_log_cat[nimp+2,7]
# out[level+1,7] <- iptw_log_cat[nimp+2,22]
# out[level+1,8] <- iptw_log_cat[nimp+2,23]
# out[level+1,9] <- iptw_log_cat[nimp+2,24]

out[level+2,3] <- iptw_log_cat[nimp+2,5]
out[level+2,4] <- iptw_log_cat[nimp+2,6]
out[level+2,5] <- iptw_log_cat[nimp+2,7]
out[level+2,7] <- iptw_log_cat[nimp+2,22]
out[level+2,8] <- iptw_log_cat[nimp+2,23]
out[level+2,9] <- iptw_log_cat[nimp+2,24]

out[level+3,3] <- iptw_log_cat[nimp+2,5]
out[level+3,4] <- iptw_log_cat[nimp+2,6]
out[level+3,5] <- iptw_log_cat[nimp+2,7]
out[level+3,7] <- iptw_log_cat[nimp+2,22]
out[level+3,8] <- iptw_log_cat[nimp+2,23]
out[level+3,9] <- iptw_log_cat[nimp+2,24]
