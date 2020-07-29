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
cd C:\Program Files\R\R-3.5.3\bin
R

install.packages("readstata13")
install.packages("varhandle")

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
# loc_inp<-'W:/data/'
# loc_out<-'C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/IPV project/p5 - MH outcomes/results'
# setwd(loc_out)


################################################################################
# 1. Get and prep data  
################################################################################

# Load data, selecting those who do not have missing sex (n=1), 
# and who did not say they were exposed to vic at 0-17 years old (n=487)
cohort.dta <- read.dta13("W:/data/cohort/current_IPVA_cohort_id.dta")
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

#Making sure most of these variables are recognised as factors (all except mfqtot_ccs)
for (c in cov_list[cov_list!="mfqtot_ccs"]){
	cohort.dta[,c] <- factor(cohort.dta[,c])
	}

#Create one term that includes all covariates ready for a model
covs <- cov_list[1]
for (c in 2:length(cov_list) ) {
	covs <- paste0(covs," + ",cov_list[c])
	}

#Check distribution of MFQ scores (mfqtot_ypc is depression score at age 23):
hist(cohort.dta$mfqtot_ypc)
hist(log(cohort.dta$mfqtot_ypc))
#Include log(MFQ), +1 to avoid log(0)
cohort.dta$log_mfqtot_tf2 <- log(cohort.dta$mfqtot_tf2+1)
cohort.dta$log_mfqtot_ccs <- log(cohort.dta$mfqtot_ccs+1)
cohort.dta$log_mfqtot_ypc <- log(cohort.dta$mfqtot_ypc+1)

#Selecting only those variables that are needed
cohort.dta_sub <- cohort.dta[,c("aln","qlet","kz021_new",
								cov_list,"vic_1821",
								"mfqtot_tf2","mfqtot_ccs","mfqtot_ypc",
								"log_mfqtot_tf2","log_mfqtot_ccs","log_mfqtot_ypc"
	 							)]

ini <- mice(cohort.dta_sub, maxit=0)
pred_mat <- ini$pred 
meth <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"log_mfqtot_tf2","log_mfqtot_ccs","log_mfqtot_ypc")
pred_mat[,exclude] <- 0
meth[exclude] <- ""
cohort.dta_sub_boys <- cohort.dta_sub[cohort.dta_sub$kz021_new==1,]
cohort.dta_sub_girls <- cohort.dta_sub[cohort.dta_sub$kz021_new==2,]

save(cohort.dta_sub_boys,cohort.dta_sub_girls,meth,pred_mat
	file='W:/data/cohort/data_for_imp_depression_analysis.RData')


################################################################################
# 2. Impute missing data
################################################################################

## ---- The following is code to run as standard (will also provide code for HPC) -------------------------------------------------------------------
set.seed(136454)
Sys.time() 
#14.58
nimp <- 50
imp_boys_sub <- mice(cohort.dta_sub[cohort.dta_sub$kz021_new==1,], m=nimp, maxit=10,
						method=meth, predictorMatrix=pred_mat)
Sys.time()
#16.02 - 1.5 hours
imp_girls_sub <- mice(cohort.dta_sub[cohort.dta_sub$kz021_new==2,], m=nimp, maxit=10,
						method=meth, predictorMatrix=pred_mat)

#Will need to impute again for diff-in-diff analysis as long format
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

#Parallel lines, raw values
data_long <- gather(cohort.dta_sub, time, depress, mfqtot_tf2:mfqtot_ccs, factor_key=FALSE)
data_long$time[data_long$time=="mfqtot_tf2"] <- 0
data_long$time[data_long$time=="mfqtot_ccs"] <- 1
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
imp_long_boys2 <- mice(data_long_boys, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)
imp_long_girls2 <- mice(data_long_girls, m=nimp, maxit=10,
					method=meth, predictorMatrix=pred_mat)

save(imp_boys_sub, imp_girls_sub,
	imp_long_boys, imp_long_boys2,
	imp_long_girls, imp_long_girls2,
	file='cohort_imp_data_depression_analysis.RData')

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
# 	imp_long_boys,imp_long_boys2,
# 	imp_long_girls,imp_long_girls2,
# 	file='cohort_imp_data_depression_analysis.RData')


################################################################################
# 3. Difference-in-difference analysis (similar to approach taken by Clayton et al 
# with cardiovascular outcomes in pregnancy), for causal effect of IPVA on depression 
# (this is possible because we have numerical depression scores before and after exposure)
################################################################################

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
out <- data.frame(matrix(NA, nrow = (nmodel*3)+1, ncol = 12))
colnames(out) <- c(rep("",2),"Males",rep("",4),"Females",rep("",4))
out[1, ] <- c("Model","Variable",rep(c("N obs","Coef","LCI","UCI","p-value"),2))
out[2:((nmodel*3)+1),1] <- c(rep(model1,3),rep(model2,3),rep(model3,3),
	rep(model4,3),rep(model5,3),rep(model6,3))
out[2:((nmodel*3)+1),2] <- rep(c("Vic","Time","Vic*Time"),nmodel)
level <- 0
digits <- 2

#Read in functions
source("O:/Training/R/did_cca.R")
source("O:/Training/R/did_mi.R")

#Model 1
did_mi(out,level,"depress ~ vic_1821*time",
	imp_long_boys, imp_long_girls, digits)

#Model 2
a <- complete(imp_long_boys, action='long',include=TRUE)
a[,"log_depress"] <- log(a$depress)
imp_long_boys <- as.mids(a)

a <- complete(imp_long_girls, action='long',include=TRUE)
a[,"log_depress"] <- log(a$depress)
imp_long_girls <- as.mids(a)

did_mi(out,level,"depress ~ vic_1821*time",
	imp_long_boys, imp_long_girls, digits)

#Model 3
did_cca(out,level,"depress ~ vic_1821*time",
	data_long_boys, data_long_girls, digits)

#Model 4
data_long_boys[,"log_depress"] <- log(data_long_boys$depress)
data_long_girls[,"log_depress"] <- log(data_long_girls$depress)

did_cca(out,level,"log_depress ~ vic_1821*time",
	data_long_boys, data_long_girls, digits)

#Model 5
did_mi(out,level,"depress ~ vic_1821*time",
	imp_long_boys2, imp_long_girls2, digits)

#Model 6
a <- complete(imp_long_boys2, action='long',include=TRUE)
a[,"log_depress"] <- log(a$depress)
imp_long_boys2 <- as.mids(a)

a <- complete(imp_long_girls2, action='long',include=TRUE)
a[,"log_depress"] <- log(a$depress)
imp_long_girls2 <- as.mids(a)

did_mi(out,level,"log_depress ~ vic_1821*time",
	imp_long_boys2, imp_long_girls2, digits)

write.csv(out, "did_table.csv", row.names = FALSE, na = "")


################################################################################
# 4. Check balance in covariates before IPTW and distribution of propensity scores
################################################################################

#Create a table for standardised differences before and after weighting
out <- data.frame(matrix(NA, nrow = length(cov_list)+2, ncol = 5))
colnames(out) <- c("","Males","","Females","")
out[1, ] <- c("Variable",rep("Unweighted SD","Weighted SD"),2)
out[2:(length(cov_list)+2),1] <- cov_list
digits <- 2

#Check univarable associations and slot in standardised differences (pre-weighting)
for (c in cov_list[cov_list!="mfqtot_ccs"]){
	model <- paste0("vic_1821 ~", c)
	fit <- with(as.formula(model), data = imp_boys_sub, family = "binomial")
	print(exp(fit$coef[2]))
	print(exp(summary(fit)$coef[2,1]-1.96*summary(fit)$coef[2,2]))
	print(exp(summary(fit)$coef[2,1]+1.96*summary(fit)$coef[2,2]))

	fit <- with(as.formula(model), data = imp_girls_sub, family = "binomial")
	print(exp(fit$coef[2]))
	print(exp(summary(fit)$coef[2,1]-1.96*summary(fit)$coef[2,2]))
	print(exp(summary(fit)$coef[2,1]+1.96*summary(fit)$coef[2,2]))
	}

for (c in cov_list[cov_list!="mfqtot_ccs"]){
	model1 <- paste0(cov_list[c]," ~ vic_1821")
	model2 <- paste0("vic_1821 ~",covs)
		for (i in 1:nimp){
		#Men
		a <- complete(imp_boys_sub, i)
		fit_uw <- glm(as.formula(model1), data=a, family = binomial(link = "logit"))
		a[,"prob_uw"] <- predict(fit_uw,a,type="response")
		p_treat_uw <- mean(a[a$vic_1821==1,"prob_uw"])
		p_control_uw <- mean(a[a$vic_1821==0,"prob_uw"])
		#Standardised differences before weighting
		out[out[,1]==c,2] <- 100*(p_treat_uw-p_control_uw)/
		(sqrt((p_treat_uw*(1-p_treat_uw)+p_control_uw*(1-p_control_uw))/2))
		b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
		a[,"ps"] <- predict(b,a,type="response")
		a[,"wgt.ATE"] <- ifelse(
	 		a$vic_1821 == 1, 1/a$ps,
	 		1/(1-a$ps))
		fit_w <- glm(as.formula(model1), data=a, family = binomial(link = "logit"), weight = (wgt.ATE))
		a[,"prob_w"] <- predict(fit_w,a,type="response")
		p_treat_w <- mean(a[a$vic_1821==1,"prob_w"])
		p_control_w <- mean(a[a$vic_1821==0,"prob_w"])
		#Standardised differences after weighting
		out[out[,1]==c,3] <- 100*(p_treat_w-p_control_w)/
			(sqrt((p_treat_w*(1-p_treat_w)+p_control_w*(1-p_control_w))/2))

		#Women
		a <- complete(imp_girls_sub, i)
		fit_uw <- glm(as.formula(model1), data=a, family = binomial(link = "logit"))
		a[,"prob_uw"] <- predict(fit_uw,a,type="response")
		p_treat_uw <- mean(a[a$vic_1821==1,"prob_uw"])
		p_control_uw <- mean(a[a$vic_1821==0,"prob_uw"])
		out[out[,1]==c,4] <- 100*(p_treat_uw-p_control_uw)/
			(sqrt((p_treat_uw*(1-p_treat_uw)+p_control_uw*(1-p_control_uw))/2))
		b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
		a[,"ps"] <- predict(b,a,type="response")
		a[,"wgt.ATE"] <- ifelse(
	 		a$vic_1821 == 1, 1/a$ps,
	 		1/(1-a$ps))
		fit_w <- glm(as.formula(model1), data=a, family = binomial(link = "logit"), weight = (wgt.ATE))
		a[,"prob_w"] <- predict(fit_w,a,type="response")
		p_treat_w <- mean(a[a$vic_1821==1,"prob_w"])
		p_control_w <- mean(a[a$vic_1821==0,"prob_w"])
		out[out[,1]==c,5] <- 100*(p_treat_w-p_control_w)/
			(sqrt((p_treat_w*(1-p_treat_w)+p_control_w*(1-p_control_w))/2))

		assign(paste0("imp_boys_",i), a)
		assign(paste0("imp_girls_",i), a)
		}
	}

#Now MFQ CCS separately (as standardised difference in mean not prevalence)
for (i in 1:nimp){
	#Men
	a <- complete(imp_boys_sub, i)
	model1 <- paste0(mfqtot_ccs," ~ vic_1821")
	fit_uw <- lm(as.formula(model1), data=a)
	a[,"mean_uw"] <- predict(fit_uw,a,type="response")
	mean_treat_uw <- mean(a[a$vic_1821==1,"mean_uw"])
	mean_control_uw <- mean(a[a$vic_1821==0,"mean_uw"])
	#Standardised differences before weighting
	out[out[,1]==c,2] <- 100*(mean_treat_uw-mean_control_uw)/
			(sqrt((mean_treat_uw*(1-mean_treat_uw)+mean_control_uw*(1-mean_control_uw))/2))
	b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	fit_w <- lm(as.formula(model1), data=a, weight = (wgt.ATE))
	a[,"mean_w"] <- predict(fit_w,a,type="response")
	mean_treat_w <- mean(a[a$vic_1821==1,"mean_w"])
	mean_control_w <- mean(a[a$vic_1821==0,"mean_w"])
	#Standardised differences after weighting
	out[out[,1]==c,3] <- 100*(mean_treat_w-mean_control_w)/
			(sqrt((mean_treat_w*(1-mean_treat_w)+mean_control_w*(1-mean_control_w))/2))
	
	#Women
	a <- complete(imp_girls_sub, i)
	fit_uw <- lm(as.formula(model1), data=a)
	a[,"mean_uw"] <- predict(fit_uw,a,type="response")
	mean_treat_uw <- mean(a[a$vic_1821==1,"mean_uw"])
	mean_control_uw <- mean(a[a$vic_1821==0,"mean_uw"])
	out[out[,1]==c,2] <- 100*(mean_treat_uw-mean_control_uw)/
			(sqrt((mean_treat_uw*(1-mean_treat_uw)+mean_control_uw*(1-mean_control_uw))/2))
	b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
	a[,"ps"] <- predict(b,a,type="response")
	a[,"wgt.ATE"] <- ifelse(
 		a$vic_1821 == 1, 1/a$ps,
 		1/(1-a$ps))
	fit_w <- lm(as.formula(model1), data=a, weight = (wgt.ATE))
	a[,"prob_w"] <- predict(fit_w,a,type="response")
	mean_treat_w <- mean(a[a$vic_1821==1,"mean_w"])
	mean_control_w <- mean(a[a$vic_1821==0,"mean_w"])
	#Standardised differences after weighting
	out[out[,1]==c,5] <- 100*(mean_treat_w-mean_control_w)/
			(sqrt((mean_treat_w*(1-mean_treat_w)+mean_control_w*(1-mean_control_w))/2))
	
	assign(paste0("imp_boys_",i), a)
	assign(paste0("imp_girls_",i), a)
	}

#Finsh and save table
meanrow <- rep(NA,5)
meanrow[1] <- "Pooled"
for(i in 2:5){
	meanrow[i] <- mean(out[,i])
	}
out <- rbind(out,meanrow)
write.csv(out, paste0("standardised_differences.csv"), row.names = FALSE, na = "")


#Histogram of propensity scores for first imputation
a <- complete(imp_boys_sub, 1)
b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
a[,"ps"] <- predict(b,a,type="response")
a[,"wgt.ATE"] <- ifelse(
		a$vic_1821 == 1, 1/a$ps,
		1/(1-a$ps))
a$vic_1821 <- factor(a$vic_1821)

jpeg(file="ps_density_by_trt_boys.jpeg")
a %>% ggplot(aes(x=ps, fill=vic_1821)) +
    geom_density(color="#e9ecef", alpha=0.6, 
    	position = 'identity', binwidth=0.01)
dev.off()

a <- complete(imp_girls_sub, 1)
b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
a[,"ps"] <- predict(b,a,type="response")
a[,"wgt.ATE"] <- ifelse(
		a$vic_1821 == 1, 1/a$ps,
		1/(1-a$ps))
a$vic_1821 <- factor(a$vic_1821)

jpeg(file="ps_density_by_trt_girls.jpeg")
a %>% ggplot(aes(x=ps, fill=vic_1821)) +
    geom_density(color="#e9ecef", alpha=0.6, 
    	position = 'identity', binwidth=0.01)
dev.off()


################################################################################
# 4. Standard linear regression (crude coefficients), 
# regular adjustment (handful of covariates and all covariates), 
# IPTW (both raw MFQ and logged MFQ)
# In all cases check normality of residuals through Kolmogorov-Smirnof test and
# histogram of residuals
################################################################################

#Can make this following block more condensed
models <- c("Crude raw, MI","Adj 1 raw, MI","Adj 2 raw, MI","IPTW raw, MI",
			"Crude logMFQ, MI","Adj 1 logMFQ, MI","Adj 2 logMFQ, MI","IPTW logMFQ, MI",
			"Crude raw, CCA","Adj 1 raw, CCA","Adj 2 raw, CCA","IPTW raw, CCA",
			"Crude logMFQ, CCA","Adj 1 logMFQ, CCA","Adj 2 logMFQ, CCA","IPTW logMFQ, CCA")
nmodel <- length(models)

#Adjustment 1 uses just a subset of covariates
cov_list2 <- c("mfqtot_ccs",
			"bimd2010q5_bin","ethnicity",
			"emotional_ab","physical_abu","sexual_abuse",
			"emotional_ne")
covs2 <- cov_list2[1]
for (c in 2:length(cov_list2) ) {
covs2 <- paste0(covs," + ",cov_list2[c])
	}
#Adjustment 2 is all of cov_list

#Create a table for results
out <- data.frame(matrix(NA, nrow = nmodel+1, ncol = 5))
colnames(out) <- c("","Males",rep("",2),"Females",rep("",2))
out[1, ] <- c("Model",,rep(c("Coef","SE"),2))
out[ ,1] <- models
level <- 2
digits <- 2

#Raw MFQ, MI
model <- paste0("mfqtot_ypc ~ vic_1821")
vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(model)))
out[level,2] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,3] <- summary(pool(vic_mfq23_fit))[2,3]
vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(model)))
out[level,4] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,5] <- summary(pool(vic_mfq23_fit))[2,3]
level <- level+1

model <- paste0("mfqtot_ypc ~ vic_1821 +",covs2)
vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(model)))
out[level,2] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,3] <- summary(pool(vic_mfq23_fit))[2,3]
vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(model)))
out[level,4] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,5] <- summary(pool(vic_mfq23_fit))[2,3]
level <- level+1

model <- paste0("mfqtot_ypc ~ vic_1821 +",covs)
vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(model)))
out[level,2] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,3] <- summary(pool(vic_mfq23_fit))[2,3]
vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(model)))
out[level,4] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,5] <- summary(pool(vic_mfq23_fit))[2,3]

source("O:/Training/R/iptw_mi.R")
iptw_raw <- iptw_mi(mfqtot_ypc,vic_1821,cov_list,imp_boys_sub,imp_girls_sub)
out[level,2] <- iptw_raw[iptw_raw$imp=="Pooled",2]
out[level,3] <- iptw_raw[iptw_raw$imp=="Pooled",3]
out[level,4] <- iptw_raw[iptw_raw$imp=="Pooled",15]
out[level,5] <- iptw_raw[iptw_raw$imp=="Pooled",16]
write.csv(iptw_raw, "imputed_ps_distn_mfq23.csv", row.names = FALSE, na = "")
level <- level+1

#Log MFQ, MI
model <- paste0("log_mfqtot_ypc ~ vic_1821")
vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(model)))
out[level,2] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,3] <- summary(pool(vic_mfq23_fit))[2,3]
vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(model)))
out[level,4] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,5] <- summary(pool(vic_mfq23_fit))[2,3]
level <- level+1

model <- paste0("log_mfqtot_ypc ~ vic_1821 +",covs2)
vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(model)))
out[level,2] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,3] <- summary(pool(vic_mfq23_fit))[2,3]
vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(model)))
out[level,4] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,5] <- summary(pool(vic_mfq23_fit))[2,3]
level <- level+1

model <- paste0("log_mfqtot_ypc ~ vic_1821 +",covs)
vic_mfq23_fit <- with(imp_boys_sub, lm(as.formula(model)))
out[level,2] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,3] <- summary(pool(vic_mfq23_fit))[2,3]
vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(model)))
out[level,4] <- summary(pool(vic_mfq23_fit))[2,2]
out[level,5] <- summary(pool(vic_mfq23_fit))[2,3]

iptw_log <- iptw_mi(log_mfqtot_ypc,vic_1821,cov_list,imp_boys_sub,imp_girls_sub)
out[level,2] <- iptw_log[iptw_log$imp=="Pooled",2]
out[level,3] <- iptw_log[iptw_log$imp=="Pooled",3]
out[level,4] <- iptw_log[iptw_log$imp=="Pooled",15]
out[level,5] <- iptw_log[iptw_log$imp=="Pooled",16]
write.csv(iptw_raw, "imputed_ps_distn_logmfq23.csv", row.names = FALSE, na = "")
level <- level+1

#Raw MFQ, CCA
model <- paste0("mfqtot_ypc ~ vic_1821")
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_boys)
out[level,2] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,3] <- summary(vic_mfq23_fit)$coef[2,2]
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_girls)
out[level,4] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,5] <- summary(vic_mfq23_fit)$coef[2,2]
level <- level+1

model <- paste0("mfqtot_ypc ~ vic_1821 +", covs2)
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_boys)
out[level,2] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,3] <- summary(vic_mfq23_fit)$coef[2,2]
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_girls)
out[level,4] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,5] <- summary(vic_mfq23_fit)$coef[2,2]
level <- level+1

model <- paste0("mfqtot_ypc ~ vic_1821 +", covs)
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_boys)
out[level,2] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,3] <- summary(vic_mfq23_fit)$coef[2,2]
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_girls)
out[level,4] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,5] <- summary(vic_mfq23_fit)$coef[2,2]
level <- level+1

model1 <- paste0("vic_1821 ~ ",covs)
model2 <- paste0("mfqtot_ypc ~ vic_1821")

ps_model <- glm(as.formula(model1), data=cohort.dta_sub_boys, family = binomial(link = "logit"))
cohort.dta_sub_boys[,"ps"] <- predict(b,a,type="response")
cohort.dta_sub_boys[,"wgt.ATE"] <- ifelse(
		a$vic_1821 == 1, 1/a$ps,
		1/(1-a$ps))
fit_ipw <- lm(as.formula(model2), data=cohort.dta_sub_boys, weight = (wgt.ATE))
out[level,2] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,3] <- summary(vic_mfq23_fit)$coef[2,2]

ps_model <- glm(as.formula(model1), data=cohort.dta_sub_girls, family = binomial(link = "logit"))
cohort.dta_sub_girls[,"ps"] <- predict(b,a,type="response")
cohort.dta_sub_girls[,"wgt.ATE"] <- ifelse(
		a$vic_1821 == 1, 1/a$ps,
		1/(1-a$ps))
fit_ipw <- lm(as.formula(model2), data=cohort.dta_sub_girls, weight = (wgt.ATE))
out[level,4] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,5] <- summary(vic_mfq23_fit)$coef[2,2]
level <- level+1

#Log MFQ, CCA
model <- paste0("log_mfqtot_ypc ~ vic_1821")
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_boys)
out[level,2] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,3] <- summary(vic_mfq23_fit)$coef[2,2]
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_girls)
out[level,4] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,5] <- summary(vic_mfq23_fit)$coef[2,2]
level <- level+1

model <- paste0("log_mfqtot_ypc ~ vic_1821 +", covs2)
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_boys)
out[level,2] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,3] <- summary(vic_mfq23_fit)$coef[2,2]
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_girls)
out[level,4] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,5] <- summary(vic_mfq23_fit)$coef[2,2]
level <- level+1

model <- paste0("log_mfqtot_ypc ~ vic_1821 +", covs)
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_boys)
out[level,2] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,3] <- summary(vic_mfq23_fit)$coef[2,2]
vic_mfq23_fit <-lm(as.formula(model), data=cohort.dta_sub_girls)
out[level,4] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,5] <- summary(vic_mfq23_fit)$coef[2,2]
level <- level+1

model2 <- paste0("log_mfqtot_ypc ~ vic_1821")
fit_ipw <- lm(as.formula(model2), data=cohort.dta_sub_boys, weight = (wgt.ATE))
out[level,2] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,3] <- summary(vic_mfq23_fit)$coef[2,2]
fit_ipw <- lm(as.formula(model2), data=cohort.dta_sub_girls, weight = (wgt.ATE))
out[level,4] <- summary(vic_mfq23_fit)$coef[2,1]
out[level,5] <- summary(vic_mfq23_fit)$coef[2,2]

write.csv(out, paste0("regressions_inc_IPTW.csv"), row.names = FALSE, na = "")
