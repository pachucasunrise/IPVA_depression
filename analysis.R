## Project: MH outcomes following IPVA
## Script purpose: Taking dataset (that includes covariates (IPVA RFs 
## and IPVA at 18-21) and depression at age 23 (the outcome), 
## difference-in-difference analysis, show imbalance in covariates 
## between IPVA and no IPVA, and IPTW analysis
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
	'gridExtra','grid','ggplot2','lattice','nnet','jtools')

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
loc_inp<-'//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/046/working/data/'
loc_out<-'//ads.bris.ac.uk/filestore/BRMS/Studies/MRC_IPV/Quant/P3 - Mental health/results/'
setwd(loc_out)

#Load data
load(paste0(loc_inp,'cohort/data_for_analysis.RData'))


################################################################################
# 1. Numbers of covariates (including missing) between vic and no vic
# (on original dataset, not imputed) - Table S1
################################################################################
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
out_list <- list("sex" = c(rep(1,4),rep(2,4)),"vic" = rep(c(0,0,1,1),2))
for (i in 1:4){
out[(dim(stats)[1]+3),(i*2)+1] <- median(cohort.dta[cohort.dta$kz021_new==out_list[["sex"]][(i*2)-1] & 
	cohort.dta$vic_1821==out_list[["vic"]][(i*2)-1] & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"])
out[(dim(stats)[1]+3),(i*2)+2] <- paste0(quantile(cohort.dta[cohort.dta$kz021_new==out_list[["sex"]][(i*2)] & 
	cohort.dta$vic_1821==out_list[["vic"]][(i*2)-1] & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.25)," to ",
	quantile(cohort.dta[cohort.dta$kz021_new==out_list[["sex"]][(i*2)] & 
	cohort.dta$vic_1821==out_list[["vic"]][(i*2)-1] & is.na(cohort.dta$mfqtot_ccs)==FALSE,"mfqtot_ccs"],0.75))

out[(dim(stats)[1]+4),(i*2)+1] <- count(is.na(cohort.dta[cohort.dta$kz021_new==out_list[["sex"]][(i*2)-1] & 
	cohort.dta$vic_1821==out_list[["vic"]][(i*2)-1],"mfqtot_ccs"]))
out[(dim(stats)[1]+4),(i*2)+2] <- round(as.numeric(out[(dim(stats)[1]+4),(i*2)+1])/as.numeric(out[3,(i*2)+1])*100,digits=1)
	}
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
out[is.na(out[,2])==TRUE,2] <- "Missing"
out[out[,2]==0,2] <- "No"
out[out[,2]==1,2] <- "Yes"

#Export table
write.csv(out, "baseline_characs.csv", row.names = FALSE, na = "")
#Paste output into Excel file 'tables', sheet 'baseline characs' and Table S1 is populated


################################################################################
# 2. Average MFQ scores - Table 1
################################################################################
out <- data.frame(matrix(NA, nrow = 4, ncol = 10))
colnames(out) <- c("Sex","Vic","Median","IQR","Arithmetic mean","95% CI","SD","Geometric mean","95% CI","CV")
out[,1] <- c("Men","","Women","")
out[,2] <- c("No vic","Vic","No vic","Vic")

level <- 1
sex <- c("boys","girls")
for (k in sex){
	for (j in 0:1){
		#Both mids (x) and long versions (data) where aux vars have been corrected and data stripped to subset not exposed at 0-17
		x<- get(paste0("imp_",k,"_sub"))
		data <- complete(x,action='long',include=TRUE)
		#Same but for logged mfq (y and data2_log)
		y<- get(paste0("imp_",k,"_sub_log"))
		data_log <- complete(y,action='long',include=TRUE)

		#long versions where only vic_1821 == 0 or 1
		data2 <- data[data$vic_1821==j,]
		data2_log <- data_log[data_log$vic_1821==j,]
		#data3 <- as.mids(data2)

		#Have to fudge x and y, as relevel wont work for resetting reference categories
		x$data$vic_1821 <- as.numeric(x$data$vic_1821)
		#Making a number that can't possibly cause issues...
		x$data$vic_1821 <- x$data$vic_1821-1

		if(j==1){
			x$data$vic_1821[x$data$vic_1821==1] <- 2	
			x$data$vic_1821[x$data$vic_1821==0] <- 1
			x$data$vic_1821[x$data$vic_1821==2] <- 0
			}
		x$data$vic_1821 <- as.factor(x$data$vic_1821)

		y$data$vic_1821 <- as.numeric(y$data$vic_1821)
		y$data$vic_1821 <- y$data$vic_1821-1
		if(j==1){
			y$data$vic_1821[y$data$vic_1821==1] <- 2	
			y$data$vic_1821[y$data$vic_1821==0] <- 1
			y$data$vic_1821[y$data$vic_1821==2] <- 0
			}
		y$data$vic_1821 <- as.factor(y$data$vic_1821)

		out[level,3] <- round(mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, median))),digits=2)
		out[level,4] <- paste0(mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, quantile, probs=0.25))),
						" to ",mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, quantile, probs=0.75))))
		out[level,5] <- round(mean(with(data2[data2$.imp!=0,], tapply(mfqtot_ypc, .imp, mean))),digits=2)
		out[level,6] <- paste0(round(summary(pool(with(x, lm(mfqtot_ypc ~ vic_1821))))[1,2]-
								1.96*summary(pool(with(x, lm(mfqtot_ypc ~ vic_1821))))[1,3],digits=2),
						" to ",round(summary(pool(with(x, lm(mfqtot_ypc ~ vic_1821))))[1,2]+
								1.96*summary(pool(with(x, lm(mfqtot_ypc ~ vic_1821))))[1,3],digits=2))
		out[level,7] <- round(mean(with(data2[data2$.imp!=0,],  tapply(mfqtot_ypc, .imp, sd))),digits=2)
		out[level,8] <- round(exp(summary(pool(with(y, lm(log_mfqtot_ypc ~ vic_1821))))[1,2]),digits=2)
		out[level,9] <- paste0(round(exp(summary(pool(with(y, lm(log_mfqtot_ypc ~ vic_1821))))[1,2]-
								1.96*summary(pool(with(y, lm(log_mfqtot_ypc ~ vic_1821))))[1,3]),digits=2),
						" to ",round(exp(summary(pool(with(y, lm(log_mfqtot_ypc ~ vic_1821))))[1,2]+
								1.96*summary(pool(with(y, lm(log_mfqtot_ypc ~ vic_1821))))[1,3]),digits=2))
		#SD of logged data
		out[level,10] <- round(mean(with(data2_log[data2_log$.imp!=0,],  tapply(log_mfqtot_ypc, .imp, sd))),digits=2)
		#out[level,10] <- round(sqrt(exp(mean(with(data2[data2$.imp!=0 & !is.na(data2$log_mfqtot_ypc),],  
		#													tapply(log_mfqtot_ypc, .imp, sd)))^2)-1),digits=2)
		level <- level+1
		}
	}
write.csv(out, "mfq_stats.csv", row.names = FALSE, na = "")
#Paste output into Excel file 'tables', sheet 'mfq_stats' and Table 1 is populated


################################################################################
# 3. Balance in covariates (as standardised differences) before IPTW 
# and distribution of propensity scores
# Figures 2 and 1 in Supp Box S1
################################################################################

#Create a table for standardised differences before and after weighting
out <- data.frame(matrix(NA, nrow = length(cov_list)+1, ncol = 5))
colnames(out) <- c("var","sd_men","wgt_sd_men","sd_women","wgt_sd_women")
out[1, ] <- c("","men","men","women","women")
out[2:length(cov_list),1] <- cov_list_log[cov_list_log!="log_mfqtot_ccs"]
out[length(cov_list)+1,1] <- "log_mfqtot_ccs"
out[2:(length(cov_list)+1),2:5] <- 0

for (c in cov_list_log[cov_list_log!="log_mfqtot_ccs"]){
	model1 <- paste0(c," ~ vic_1821")
	model2 <- paste0("vic_1821 ~",covs_log)
	#Additional model for stablised weights - intercept model
	model3 <- paste0("vic_1821 ~ 1")
		for (i in 1:nimp){
		
		##At some point update code to loop over men and women

		#Men
		a <- complete(imp_boys_sub_log, i)
		fit_uw <- glm(as.formula(model1), data=a, family = binomial(link = "logit"))
		a[,"prob_uw"] <- predict(fit_uw,a,type="response")
		p_treat_uw <- mean(a[a$vic_1821==1,"prob_uw"])
		p_control_uw <- mean(a[a$vic_1821==0,"prob_uw"])
		out[out[,1]==c,2] <- as.numeric(out[out[,1]==c,2])+((p_treat_uw-p_control_uw)/
			(sqrt(((p_treat_uw*(1-p_treat_uw))+(p_control_uw*(1-p_control_uw)))/2)))
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
		a <- complete(imp_girls_sub_log, i)
		fit_uw <- glm(as.formula(model1), data=a, family = binomial(link = "logit"))
		a[,"prob_uw"] <- predict(fit_uw,a,type="response")
		p_treat_uw <- mean(a[a$vic_1821==1,"prob_uw"])
		p_control_uw <- mean(a[a$vic_1821==0,"prob_uw"])
		out[out[,1]==c,4] <- as.numeric(out[out[,1]==c,4])+((p_treat_uw-p_control_uw)/
			(sqrt(((p_treat_uw*(1-p_treat_uw))+(p_control_uw*(1-p_control_uw)))/2)))

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

##This part could go in same men/women loop mentioned above.
#Now MFQ CCS separately (as standardised difference in mean not prevalence)
model1 <- paste0("log_mfqtot_ccs ~ vic_1821")

for (i in 1:nimp){
	#Men
	a <- complete(imp_boys_sub_log, i)
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
	a <- complete(imp_girls_sub_log, i)
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

#Need to reshape data so legend appears
out2 <- reshape2::melt(out, id.var = "var")
out2$value <- as.numeric(out2$value)
#Need an additional column so that unweighted and weighted are labelled properly
out2[(out2$variable == "sd_men") | (out2$variable == "sd_women"),"weight"] <- "Un-weighted"
out2[(out2$variable == "wgt_sd_men") | (out2$variable == "wgt_sd_women"),"weight"] <- "Weighted"

#Create legend
plot_men <- ggplot(out2[(out2$variable == "sd_men") | out2$variable == "wgt_sd_men",], 
	aes(x=value, y=var, colour = weight)) +
	geom_point() +
	scale_color_brewer(palette="Dark2", name="")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(plot_men)

#Plots without legend
plot_men <- ggplot(out2[(out2$variable == "sd_men") | out2$variable == "wgt_sd_men",], 
	aes(x=value, y=var, colour = weight)) +
	geom_point() +
	scale_color_brewer(palette="Dark2") +
	xlim(xmin, xmax) +
	scale_y_discrete(limits = rev(levels(out2$var)),
		labels=rev(labels)
		) +
	ggtitle("Men") +
	#ylab("Covariate") +
	geom_vline(xintercept = c(-0.05,0.05), linetype = "dashed") +
	theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position = "none")

plot_women <- ggplot(out2[(out2$variable == "sd_women") | out2$variable == "wgt_sd_women",], 
	aes(x=value, y=var, colour = weight)) +
	geom_point() +
	scale_color_brewer(palette="Dark2") +
	xlim(xmin, xmax) +
	scale_y_discrete(limits = rev(levels(out2$var)),
		labels=rev(labels)
		) +
	ggtitle("Women") +
	#ylab("Covariate") +
	geom_vline(xintercept = c(-0.05,0.05), linetype = "dashed") +
	theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
		legend.position = "none")

#Put everything together
tiff(file="figs2.tiff", units="in", width=8, height=5, res=300)
grid.arrange(arrangeGrob(plot_men,plot_women,ncol=2,widths=c(1.4,1.1)), 
	bottom=textGrob("Standardised Difference",hjust=0.5),
    left = textGrob("Covariates",rot=90), 
	mylegend, nrow=1, widths=c(8.5,1.5))
dev.off()

#Histogram of propensity scores for first imputation (Figure S1)
a <- complete(imp_boys_sub, 1)
b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
a[,"ps"] <- predict(b,a,type="response")
a[,"wgt.ATE"] <- ifelse(
		a$vic_1821 == 1, 1/a$ps,
		1/(1-a$ps))
a$vic_1821 <- factor(a$vic_1821, labels=c("No vic","Vic"))
#jpeg(file="ps_density_by_trt_boys.jpeg")
plot_men <- a %>% ggplot(aes(x=ps, fill=vic_1821)) +
    geom_density(color="#e9ecef", alpha=0.6, 
    	position = 'identity') +
		labs(title="Men",
        x ="", y = "") +
       	theme(legend.position = "none")
#dev.off()

a <- complete(imp_girls_sub, 1)
b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
a[,"ps"] <- predict(b,a,type="response")
a[,"wgt.ATE"] <- ifelse(
		a$vic_1821 == 1, 1/a$ps,
		1/(1-a$ps))
a$vic_1821 <- factor(a$vic_1821, labels=c("No vic","Vic"))

#jpeg(file="ps_density_by_trt_girls.jpeg")
plot_women <- a %>% ggplot(aes(x=ps, fill=vic_1821)) +
    geom_density(color="#e9ecef", alpha=0.6, 
    	position = 'identity') +
    	labs(title="Women",
        x ="", y = "") +
       	theme(legend.title = element_blank())
#dev.off()

tiff(file="figs1.tiff", units="in", width=8, height=5, res=300)
grid.arrange(arrangeGrob(plot_men,plot_women,ncol=2,widths=c(1,1.1)), 
	bottom=textGrob("Propensity score",hjust=0.5),
    left = textGrob("Density",rot=90))
dev.off()


################################################################################
# 4. Standard linear regression (crude coefficients), regular 
# adjustment (handful of covariates and all covariates), IPTW (both 
# raw MFQ and logged MFQ). In all cases check normality of residuals 
# through Kolmogorov-Smirnof test and histogram of residuals
# - Forms part of Tables 2 and S2
################################################################################

models <- c("Crude raw, MI","Adj 1 raw, MI","Adj 2 raw, MI","IPTW raw, MI",
			"Crude logMFQ, MI","Adj 1 logMFQ, MI","Adj 2 logMFQ, MI","IPTW logMFQ, MI",
			"Crude raw, CCA","Adj 1 raw, CCA","Adj 2 raw, CCA","IPTW raw, CCA",
			"Crude logMFQ, CCA","Adj 1 logMFQ, CCA","Adj 2 logMFQ, CCA","IPTW logMFQ, CCA")
nmodel <- length(models)
#Adjustment 1 uses just a subset of covariates (cov_list2 for raw MFQ or cov_list3 for log MFQ)
#Adjustment 2 is all of cov_list or cov_list_log

#Create a table for results
out <- data.frame(matrix(NA, nrow = ((nmodel*2)+1), ncol = 12))
colnames(out) <- c("","","Males",rep("",3),"Females",rep("",3),"","")
out[1, ] <- c("Model","Term",rep(c("Coef","SE","Coef p-value","KS p-value"),2),"n_boys","n_girls")
out[seq(2,(nmodel*2),by=2),1] <- models
out[2:((nmodel*2)+1),2] <- rep(c("Intercept","Victimisation"),nmodel)
level <- 2
digits <- 2

##Definitely need to condense the following
##Including creating a loop over men and women
#Raw MFQ, MI
model_list <- c("mfqtot_ypc ~ vic_1821", 
				paste0("mfqtot_ypc ~ vic_1821 +",covs2),
				paste0("mfqtot_ypc ~ vic_1821 +",covs))

#Issues with K-S test so have left out for now
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
	tiff(file=paste0(models[level/2],"_boys.tiff"))
	print(ggplot(data=a, aes(x=res, fill=vic_1821)) +
	    geom_density(color="#e9ecef", alpha=0.6, 
	    	position = 'identity'))
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
	tiff(file=paste0(models[level/2],"_girls.tiff"))
	print(ggplot(data=a, aes(x=res, fill=vic_1821)) +
	    geom_density(color="#e9ecef", alpha=0.6, 
	    	position = 'identity'))
	dev.off()
	level <- level+2
	}

source("//ads.bris.ac.uk/filestore/BRMS/Studies/MRC_IPV/Quant/P3 - Mental health/code/iptw_mi.R")
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
	vic_mfq23_fit <- with(imp_boys_sub_log, lm(as.formula(m)))
	out[level,3] <- summary(pool(vic_mfq23_fit))[1,2]
	out[level,4] <- summary(pool(vic_mfq23_fit))[1,3]
	out[level,5] <- summary(pool(vic_mfq23_fit))[1,6]

	out[level+1,3] <- summary(pool(vic_mfq23_fit))[2,2]
	out[level+1,4] <- summary(pool(vic_mfq23_fit))[2,3]
	out[level+1,5] <- summary(pool(vic_mfq23_fit))[2,6]
	a <- complete(imp_boys_sub_log, 1)
	a$vic_1821 <- factor(a$vic_1821)
	b <- lm(as.formula(m), data=a)
	a$res <- resid(b)
	#out[level,6] <- ols_test_normality(b)$kolmogorv$p.value
	tiff(file=paste0(models[level/2],"_boys.tiff"))
	print(ggplot(data=a, aes(x=res, fill=vic_1821)) +
	    geom_density(color="#e9ecef", alpha=0.6, 
	    	position = 'identity'))
	dev.off()

	vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(m)))
	out[level,7] <- summary(pool(vic_mfq23_fit))[1,2]
	out[level,8] <- summary(pool(vic_mfq23_fit))[1,3]
	out[level,9] <- summary(pool(vic_mfq23_fit))[1,6]

	out[level+1,7] <- summary(pool(vic_mfq23_fit))[2,2]
	out[level+1,8] <- summary(pool(vic_mfq23_fit))[2,3]
	out[level+1,9] <- summary(pool(vic_mfq23_fit))[2,6]
	a <- complete(imp_girls_sub_log, 1)
	a$vic_1821 <- factor(a$vic_1821)
	b <- lm(as.formula(m), data=a)
	a$res <- resid(b)
	#out[level,10] <- ols_test_normality(b)$kolmogorv$p.value
	tiff(file=paste0(models[level/2],"_girls.tiff"))
	print(ggplot(data=a, aes(x=res, fill=vic_1821)) +
	    geom_density(color="#e9ecef", alpha=0.6, 
	    	position = 'identity'))
	dev.off()
	level <- level+2
	}

iptw_log <- iptw_mi("log_mfqtot_ypc","vic_1821",cov_list_log,imp_boys_sub_log,imp_girls_sub_log)
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
	vic_mfq23_fit <-lm(as.formula(m), data=cohort.dta_boys)
	out[level,3] <- summary(vic_mfq23_fit)$coef[1,1]
	out[level,4] <- summary(vic_mfq23_fit)$coef[1,2]
	out[level,5] <- summary(vic_mfq23_fit)$coef[1,4]

	out[level+1,3] <- summary(vic_mfq23_fit)$coef[2,1]
	out[level+1,4] <- summary(vic_mfq23_fit)$coef[2,2]
	out[level+1,5] <- summary(vic_mfq23_fit)$coef[2,4]
	#out[level,6] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
	vic_mfq23_fit <-lm(as.formula(m), data=cohort.dta_girls)
	out[level,7] <- summary(vic_mfq23_fit)$coef[1,1]
	out[level,8] <- summary(vic_mfq23_fit)$coef[1,2]
	out[level,9] <- summary(vic_mfq23_fit)$coef[1,4]

	out[level+1,7] <- summary(vic_mfq23_fit)$coef[2,1]
	out[level+1,8] <- summary(vic_mfq23_fit)$coef[2,2]
	out[level+1,9] <- summary(vic_mfq23_fit)$coef[2,4]
	#out[level,10] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
	
	level <- level+2
	}

#IPTW crude MFQ, CCA
model1 <- paste0("vic_1821 ~ ",covs)
model2 <- "mfqtot_ypc ~ vic_1821"
model3 <- "vic_1821 ~ 1"
b <- glm(as.formula(model1), data=cohort.dta_boys, family = binomial(link = "logit"))
cohort.dta_boys[,"ps"] <- predict(b,cohort.dta_boys,type="response")
cohort.dta_boys[,"wgt.ATE"] <- ifelse(
		cohort.dta_boys$vic_1821 == 1, 1/cohort.dta_boys$ps,
		1/(1-cohort.dta_boys$ps))
b <- glm(as.formula(model3), data=cohort.dta_boys, family = binomial(link = "logit"))
cohort.dta_boys[,"ps_int"] <- predict(b,cohort.dta_boys,type="response")
cohort.dta_boys[,"wgt.ATE_stab"] <- ifelse(
		cohort.dta_boys$vic_1821 == 1, cohort.dta_boys$ps_int/cohort.dta_boys$ps,
		(1-cohort.dta_boys$ps_int)/(1-cohort.dta_boys$ps))
vic_mfq23_fit <- lm(as.formula(model2), data=cohort.dta_boys, weight = (wgt.ATE_stab))
out[level,3] <- summary(vic_mfq23_fit)$coef[1,1]
out[level,4] <- summary(vic_mfq23_fit)$coef[1,2]
out[level,5] <- summary(vic_mfq23_fit)$coef[1,4]

out[level+1,3] <- summary(vic_mfq23_fit)$coef[2,1]
out[level+1,4] <- summary(vic_mfq23_fit)$coef[2,2]
out[level+1,5] <- summary(vic_mfq23_fit)$coef[2,4]
#out[level,6] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value

b <- glm(as.formula(model1), data=cohort.dta_girls, family = binomial(link = "logit"))
cohort.dta_girls[,"ps"] <- predict(b,cohort.dta_girls,type="response")
cohort.dta_girls[,"wgt.ATE"] <- ifelse(
		cohort.dta_girls$vic_1821 == 1, 1/cohort.dta_girls$ps,
		1/(1-cohort.dta_girls$ps))
b <- glm(as.formula(model3), data=cohort.dta_girls, family = binomial(link = "logit"))
cohort.dta_girls[,"ps_int"] <- predict(b,cohort.dta_girls,type="response")
cohort.dta_girls[,"wgt.ATE_stab"] <- ifelse(
		cohort.dta_girls$vic_1821 == 1, cohort.dta_girls$ps_int/cohort.dta_girls$ps,
		(1-cohort.dta_girls$ps_int)/(1-cohort.dta_girls$ps))
vic_mfq23_fit <- lm(as.formula(model2), data=cohort.dta_girls, weight = (wgt.ATE_stab))
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
	vic_mfq23_fit <-lm(as.formula(m), data=cohort.dta_boys)
	out[level,3] <- summary(vic_mfq23_fit)$coef[1,1]
	out[level,4] <- summary(vic_mfq23_fit)$coef[1,2]
	out[level,5] <- summary(vic_mfq23_fit)$coef[1,4]
	out[level,11] <- stats::nobs(vic_mfq23_fit)

	out[level+1,3] <- summary(vic_mfq23_fit)$coef[2,1]
	out[level+1,4] <- summary(vic_mfq23_fit)$coef[2,2]
	out[level+1,5] <- summary(vic_mfq23_fit)$coef[2,4]
	#out[level,6] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
	vic_mfq23_fit <-lm(as.formula(m), data=cohort.dta_girls)
	out[level,7] <- summary(vic_mfq23_fit)$coef[1,1]
	out[level,8] <- summary(vic_mfq23_fit)$coef[1,2]
	out[level,9] <- summary(vic_mfq23_fit)$coef[1,4]
	out[level,12] <- stats::nobs(vic_mfq23_fit)

	out[level+1,7] <- summary(vic_mfq23_fit)$coef[2,1]
	out[level+1,8] <- summary(vic_mfq23_fit)$coef[2,2]
	out[level+1,9] <- summary(vic_mfq23_fit)$coef[2,4]
	#out[level,10] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value
	level <- level+2
	}

model1 <- paste0("vic_1821 ~ ",covs_log)
model2 <- "log_mfqtot_ypc ~ vic_1821"
model3 <- "vic_1821 ~ 1"

b <- glm(as.formula(model1), data=cohort.dta_boys, family = binomial(link = "logit"))
cohort.dta_boys[,"ps"] <- predict(b,cohort.dta_boys,type="response")
cohort.dta_boys[,"wgt.ATE"] <- ifelse(
		cohort.dta_boys$vic_1821 == 1, 1/cohort.dta_boys$ps,
		1/(1-cohort.dta_boys$ps))
b <- glm(as.formula(model3), data=cohort.dta_boys, family = binomial(link = "logit"))
cohort.dta_boys[,"ps_int"] <- predict(b,cohort.dta_boys,type="response")
cohort.dta_boys[,"wgt.ATE_stab"] <- ifelse(
		cohort.dta_boys$vic_1821 == 1, cohort.dta_boys$ps_int/cohort.dta_boys$ps,
		(1-cohort.dta_boys$ps_int)/(1-cohort.dta_boys$ps))
vic_mfq23_fit <- lm(as.formula(model2), data=cohort.dta_boys, weight = (wgt.ATE_stab))
out[level,3] <- summary(vic_mfq23_fit)$coef[1,1]
out[level,4] <- summary(vic_mfq23_fit)$coef[1,2]
out[level,5] <- summary(vic_mfq23_fit)$coef[1,4]

out[level+1,3] <- summary(vic_mfq23_fit)$coef[2,1]
out[level+1,4] <- summary(vic_mfq23_fit)$coef[2,2]
out[level+1,5] <- summary(vic_mfq23_fit)$coef[2,4]
#out[level,6] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value

b <- glm(as.formula(model1), data=cohort.dta_girls, family = binomial(link = "logit"))
cohort.dta_girls[,"ps"] <- predict(b,cohort.dta_girls,type="response")
cohort.dta_girls[,"wgt.ATE"] <- ifelse(
		cohort.dta_girls$vic_1821 == 1, 1/cohort.dta_girls$ps,
		1/(1-cohort.dta_girls$ps))
b <- glm(as.formula(model3), data=cohort.dta_girls, family = binomial(link = "logit"))
cohort.dta_girls[,"ps_int"] <- predict(b,cohort.dta_girls,type="response")
cohort.dta_girls[,"wgt.ATE_stab"] <- ifelse(
		cohort.dta_girls$vic_1821 == 1, cohort.dta_girls$ps_int/cohort.dta_girls$ps,
		(1-cohort.dta_girls$ps_int)/(1-cohort.dta_girls$ps))
vic_mfq23_fit <- lm(as.formula(model2), data=cohort.dta_girls, weight = (wgt.ATE_stab))
out[level,7] <- summary(vic_mfq23_fit)$coef[1,1]
out[level,8] <- summary(vic_mfq23_fit)$coef[1,2]
out[level,9] <- summary(vic_mfq23_fit)$coef[1,4]

out[level+1,7] <- summary(vic_mfq23_fit)$coef[2,1]
out[level+1,8] <- summary(vic_mfq23_fit)$coef[2,2]
out[level+1,9] <- summary(vic_mfq23_fit)$coef[2,4]
#out[level,10] <- ols_test_normality(vic_mfq23_fit)$kolmogorv$p.value

write.csv(out, paste0("regressions_inc_iptw.csv"), row.names = FALSE, na = "")
#Paste output into Excel file 'tables', sheet 'regressions_inc_iptw' and Table 2 is populated


################################################################################
# 5. Difference-in-difference analysis (similar to approach taken by 
# Gemma Clayton et al with cardiovascular outcomes in pregnancy), for 
# causal effect of IPVA on depression (this is possible because we have 
# numerical depression scores before and after exposure)
# Forms part of Table 3 and S3, and Figure 2
################################################################################

#Check relationship between mfqtot_ccs (age 16) and mfqtot_ypc (age 23):
plot(cohort.dta$mfqtot_ccs, cohort.dta$mfqtot_ypc)
summary(lm(mfqtot_ypc ~ mfqtot_ccs, data = cohort.dta_boys))
summary(lm(mfqtot_ypc ~ mfqtot_ccs, data = cohort.dta_girls))

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
source("//ads.bris.ac.uk/filestore/BRMS/Studies/MRC_IPV/Quant/P3 - Mental health/code/did_cca.R")
source("//ads.bris.ac.uk/filestore/BRMS/Studies/MRC_IPV/Quant/P3 - Mental health/code/did_mi.R")

#Model 1
out <- did_mi(out,level,"depress ~ vic_1821*time",
	imp_long_boys, imp_long_girls, digits)

#Model 2
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
level <- level +1
out <- did_mi(out,level,"log_depress ~ vic_1821*time",
	imp_long2_boys_par, imp_long2_girls_par, digits)

write.csv(out, "did_table.csv", row.names = FALSE, na = "")
#Paste output into Excel file 'tables', sheet 'did_table' and Table 3 is populated


#Also want to plots estimates from model 2 (Figure 1)
#Free up some space, will need it for imp_long2_girls which is rather big...
gc()

group <- expand.grid(c(0,1),c(16,23))
plot_est <- data.frame(cbind(group, matrix(nrow=dim(group)[1], ncol=6)))
colnames(plot_est) <- c("ipva","age","log_mfq_boys","lci_boys","uci_boys",
										"log_mfq_girls","lci_girls","uci_girls")

#Fit model 2 again (easier than using function did_mi)
didreg_boys = pool(with(imp_long2_boys, exp = lm(log_depress ~ vic_1821*time)))
plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_boys"] <- summary(didreg_boys)[1,2]
plot_est[plot_est$ipva==0 & plot_est$age==16,"lci_boys"] <- summary(didreg_boys)[1,2]-1.96*summary(didreg_boys)[1,3]
plot_est[plot_est$ipva==0 & plot_est$age==16,"uci_boys"] <- summary(didreg_boys)[1,2]+1.96*summary(didreg_boys)[1,3]

plot_est[plot_est$ipva==0 & plot_est$age==23,"log_mfq_boys"] <- summary(didreg_boys)[1,2]+summary(didreg_boys)[3,2]
#Need covariance of the estimates
#https://github.com/amices/mice/issues/121 explains issues in getting var-cov matrix out...
# cov_est <-
# se_est <- sqrt((summary(didreg_boys)[1,3]^2)+(summary(didreg_boys)[3,3]^2)+2*cov_est)
# plot_est[plot_est$ipva==0 & plot_est$age==23,"lci_boys"] <- (summary(didreg_boys)[1,2]+summary(didreg_boys)[3,2])-1.96*se_est
# plot_est[plot_est$ipva==0 & plot_est$age==23,"uci_boys"] <- (summary(didreg_boys)[1,2]+summary(didreg_boys)[3,2])+1.96*se_est

plot_est[plot_est$ipva==1 & plot_est$age==16,"log_mfq_boys"] <- summary(didreg_boys)[1,2]+summary(didreg_boys)[2,2]

plot_est[plot_est$ipva==1 & plot_est$age==23,"log_mfq_boys"] <- summary(didreg_boys)[1,2]+summary(didreg_boys)[2,2]+
																	summary(didreg_boys)[3,2]+summary(didreg_boys)[4,2]

didreg_girls = pool(with(imp_long2_girls, exp = lm(log_depress ~ vic_1821*time)))
plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_girls"] <- summary(didreg_girls)[1,2]
plot_est[plot_est$ipva==0 & plot_est$age==23,"log_mfq_girls"] <- summary(didreg_girls)[1,2]+summary(didreg_girls)[3,2]
plot_est[plot_est$ipva==1 & plot_est$age==16,"log_mfq_girls"] <- summary(didreg_girls)[1,2]+summary(didreg_girls)[2,2]
plot_est[plot_est$ipva==1 & plot_est$age==23,"log_mfq_girls"] <- summary(didreg_girls)[1,2]+summary(didreg_girls)[2,2]+
																	summary(didreg_girls)[3,2]+summary(didreg_girls)[4,2]
plot_est$ipva <- factor(plot_est$ipva)
ymin <- min(c(min(plot_est$log_mfq_boys),min(plot_est$log_mfq_girls)))
ymax <- max(c(max(plot_est$log_mfq_boys),max(plot_est$log_mfq_girls)))

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

plot_est$ipva <- factor(plot_est$ipva, labels=c("No vic","Vic"))

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
	       	theme(legend.title = element_blank())

tiff(file="fig1.tiff", units="in", width=10, height=5, res=300)
grid.arrange(arrangeGrob(plot_men,plot_women,ncol=2,widths=c(0.9,1), 
	bottom=textGrob("Age",hjust=0)))
dev.off()


################################################################################
# 6. Sub-types of IPVA - regression results, logged outcome, MI  
# - Table 4
################################################################################

models <- c("Crude logMFQ, MI","Adj 1 logMFQ, MI","Adj 2 logMFQ, MI","IPTW logMFQ, MI")
nmodel <- length(models)

#Adjustment 1 uses subset of covariates cov_list3 (covs3)
#Adjustment 2 is all of cov_list_log (covs_log)

#Create a table for results
header <- c("Model","Term",rep(c("Coef","SE","Coef p-value","KS p-value"),2))
out <- data.frame(matrix(NA, nrow = ((nmodel*4)+1), ncol = length(header)))
colnames(out) <- c("","","Males",rep("",3),"Females",rep("",3))
out[1, ] <- header
out[seq(2,(nmodel*4),by=4),1] <- models
out[2:((nmodel*4)+1),2] <- rep(c("Intercept","Psych only","Phys (no sexual)","Sexual"),nmodel)

level <- 2
digits <- 2

model_list <- c("log_mfqtot_ypc ~ vic_sub_1821", 
				paste0("log_mfqtot_ypc ~ vic_sub_1821 +",covs3),
				paste0("log_mfqtot_ypc ~ vic_sub_1821 +",covs_log))

for (m in model_list){
	vic_mfq23_fit <- with(imp_boys_sub_log, lm(as.formula(m)))
	out[level:(level+3),3] <- summary(pool(vic_mfq23_fit))[1:4,2]
	out[level:(level+3),4] <- summary(pool(vic_mfq23_fit))[1:4,3]
	out[level:(level+3),5] <- summary(pool(vic_mfq23_fit))[1:4,6]

	vic_mfq23_fit <- with(imp_girls_sub, lm(as.formula(m)))
	out[level:(level+3),7] <- summary(pool(vic_mfq23_fit))[1:4,2]
	out[level:(level+3),8] <- summary(pool(vic_mfq23_fit))[1:4,3]
	out[level:(level+3),9] <- summary(pool(vic_mfq23_fit))[1:4,6]
	
	level <- level+4
	}

#Need to create a second table for IPTW results, that we then pull 
#bits from into first table
header <- c("imputation",rep(c("coef_int","SE_int","p_int",
				"coef_vic1","SE_vic1","p_vic1",
				"min(ps|tr=0)","25pc(ps|tr=0)",
				"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
				"min(ps|tr=1)","25pc(ps|tr=1)",
				"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)",
				"coef_vic2","SE_vic2","p_vic2",
				"min(ps|tr=2)","25pc(ps|tr=2)",
				"50pc(ps|tr=2)","75(ps|tr=2)","max(ps|tr=2)",
				"coef_vic3","SE_vic3","p_vic3",
				"min(ps|tr=3)","25pc(ps|tr=3)",
				"50pc(ps|tr=3)","75pc(ps|tr=3)","max(ps|tr=3)"),2))
out2 <- data.frame(matrix(NA, nrow = (nimp+1), ncol = length(header)))
colnames(out2) <- c("","Men",rep("",((length(header)/2)-2)),"Women",rep("",((length(header)/2)-2)))
out2[1,] <- header

model1 <- paste0("vic_sub_1821 ~ ",covs_log)
model2 <- paste0("vic_sub_1821 ~ 1")
model3 <- paste0("log_mfqtot_ypc ~ vic_sub_1821")

for (i in 1:nimp){
	a <- complete(imp_boys_sub, i)
	b <- multinom(model1, data = a)
	c <- predict(b,a,type="probs")
	a$ps0 <- c[,1]
	a$ps1 <- c[,2]
    a$ps2 <- c[,3]
    a$ps3 <- c[,4]
    #Stabilised weights
   	b <- multinom(model2, data = a)
   	d <- predict(b,a,type="probs")
    a$ps_int0 <- d[,1]
   	a$ps_int1 <- d[,2]
    a$ps_int2 <- d[,3]
    a$ps_int3 <- d[,4]
    #Now assign a ps to each person depending on what group they are in
    for (j in 0:3){
    	a[a$vic_sub_1821==j,"wgt.ATE_stab"] = a[a$vic_sub_1821==j,paste0("ps_int",j)]/
    												a[a$vic_sub_1821==j,paste0("ps",j)]
    	}
	fit_ipw <- glm(as.formula(model3), data=a, family = "gaussian", weight = (wgt.ATE_stab))
	out2[i+1,1] <- i
	out2[i+1,2] <- summary(fit_ipw)$coef[1,1]
	out2[i+1,3] <- summary(fit_ipw)$coef[1,2]
	out2[i+1,4] <- summary(fit_ipw)$coef[1,4]
	out2[i+1,5] <- summary(fit_ipw)$coef[2,1]
	out2[i+1,6] <- summary(fit_ipw)$coef[2,2]
	out2[i+1,7] <- summary(fit_ipw)$coef[2,4]
	out2[i+1,18] <- summary(fit_ipw)$coef[3,1]
	out2[i+1,19] <- summary(fit_ipw)$coef[3,2]
	out2[i+1,20] <- summary(fit_ipw)$coef[3,4]
	out2[i+1,26] <- summary(fit_ipw)$coef[4,1]
	out2[i+1,27] <- summary(fit_ipw)$coef[4,2]
	out2[i+1,28] <- summary(fit_ipw)$coef[4,4]

	out2[i+1,8:12] <- summary(a$ps_int0/a$ps0)[c(1,2,3,5,6)]
	out2[i+1,13:17] <- summary(a$ps_int1/a$ps1)[c(1,2,3,5,6)]
	out2[i+1,21:25] <- summary(a$ps_int2/a$ps2)[c(1,2,3,5,6)]
	out2[i+1,29:33] <- summary(a$ps_int3/a$ps3)[c(1,2,3,5,6)]

	a <- complete(imp_girls_sub, i)
	b <- multinom(model1, data = a)
	c <- predict(b,a,type="probs")
	a$ps0 <- c[,1]
	a$ps1 <- c[,2]
    a$ps2 <- c[,3]
    a$ps3 <- c[,4]
    b <- multinom(model2, data = a)
   	d <- predict(b,a,type="probs")

    a$ps_int0 <- d[,1]
   	a$ps_int1 <- d[,2]
    a$ps_int2 <- d[,3]
    a$ps_int3 <- d[,4]

    for (j in 0:3){
    	a[a$vic_sub_1821==j,"wgt.ATE_stab"] = a[a$vic_sub_1821==j,paste0("ps_int",j)]/
    												a[a$vic_sub_1821==j,paste0("ps",j)]
    	}
	fit_ipw <- glm(as.formula(model3), data=a, family = "gaussian", weight = (wgt.ATE_stab))
	out2[i+1,34] <- summary(fit_ipw)$coef[1,1]
	out2[i+1,35] <- summary(fit_ipw)$coef[1,2]
	out2[i+1,36] <- summary(fit_ipw)$coef[1,4]
	out2[i+1,37] <- summary(fit_ipw)$coef[2,1]
	out2[i+1,38] <- summary(fit_ipw)$coef[2,2]
	out2[i+1,39] <- summary(fit_ipw)$coef[2,4]
	out2[i+1,50] <- summary(fit_ipw)$coef[3,2]
	out2[i+1,51] <- summary(fit_ipw)$coef[3,2]
	out2[i+1,52] <- summary(fit_ipw)$coef[3,4]
	out2[i+1,58] <- summary(fit_ipw)$coef[4,1]
	out2[i+1,59] <- summary(fit_ipw)$coef[4,2]
	out2[i+1,60] <- summary(fit_ipw)$coef[4,4]

	out2[i+1,40:44] <- summary(a$ps_int0/a$ps0)[c(1,2,3,5,6)]
	out2[i+1,45:49] <- summary(a$ps_int1/a$ps1)[c(1,2,3,5,6)]
	out2[i+1,53:57] <- summary(a$ps_int2/a$ps2)[c(1,2,3,5,6)]
	out2[i+1,61:65] <- summary(a$ps_int3/a$ps3)[c(1,2,3,5,6)]
	}

meanrow <- rep(NA,length(header))
meanrow[1] <- "Pooled"
#Skipping out p-values as we want medians not means of these
for(i in c(2:3,5:6,18:19,26:27,34:35,37:38,50:51,58:59)){
	meanrow[i] <- mean(as.numeric(out2[2:(nimp+1),i]))
	}
#p-values
for(i in c(4,7,20,28,36,39,52,60)){
	meanrow[i] <- median(as.numeric(out2[2:(nimp+1),i]))
	}

#Calculating SEs
se_sq <- as.numeric(out2[2:(nimp+1),3])^2
vw <- (sum(se_sq))/nimp
a <- complete(imp_boys_sub, 1)
vb <- sqrt(sum((as.numeric(out2[2:(nimp+1),2])-mean(as.numeric(out2[2:(nimp+1),2])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[3] <- se_pooled

se_sq <- as.numeric(out2[2:(nimp+1),6])^2
vw <- (sum(se_sq))/nimp
a <- complete(imp_boys_sub, 1)
vb <- sqrt(sum((as.numeric(out2[2:(nimp+1),5])-mean(as.numeric(out2[2:(nimp+1),5])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[6] <- se_pooled

se_sq <- as.numeric(out2[2:(nimp+1),19])^2
vw <- (sum(se_sq))/nimp
a <- complete(imp_boys_sub, 1)
vb <- sqrt(sum((as.numeric(out2[2:(nimp+1),18])-mean(as.numeric(out2[2:(nimp+1),18])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[19] <- se_pooled

se_sq <- as.numeric(out2[2:(nimp+1),27])^2
vw <- (sum(se_sq))/nimp
a <- complete(imp_boys_sub, 1)
vb <- sqrt(sum((as.numeric(out2[2:(nimp+1),26])-mean(as.numeric(out2[2:(nimp+1),26])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[27] <- se_pooled

#Girls
se_sq <- as.numeric(out2[2:(nimp+1),35])^2
vw <- (sum(se_sq))/nimp
a <- complete(imp_girls_sub, 1)
vb <- sqrt(sum((as.numeric(out2[2:(nimp+1),34])-mean(as.numeric(out2[2:(nimp+1),34])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[35] <- se_pooled

se_sq <- as.numeric(out2[2:(nimp+1),38])^2
vw <- (sum(se_sq))/nimp
a <- complete(imp_girls_sub, 1)
vb <- sqrt(sum((as.numeric(out2[2:(nimp+1),37])-mean(as.numeric(out2[2:(nimp+1),37])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[38] <- se_pooled

se_sq <- as.numeric(out2[2:(nimp+1),51])^2
vw <- (sum(se_sq))/nimp
a <- complete(imp_girls_sub, 1)
vb <- sqrt(sum((as.numeric(out2[2:(nimp+1),50])-mean(as.numeric(out2[2:(nimp+1),50])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[50] <- se_pooled

se_sq <- as.numeric(out2[2:(nimp+1),59])^2
vw <- (sum(se_sq))/nimp
a <- complete(imp_girls_sub, 1)
vb <- sqrt(sum((as.numeric(out2[2:(nimp+1),58])-mean(as.numeric(out2[2:(nimp+1),58])))^2)/(dim(a)[1]-1))
se_pooled <- sqrt(vw+vb+(vb/nimp))
meanrow[59] <- se_pooled

out2 <- rbind(out2, meanrow)
out2
write.csv(out2, paste0("iptw_subtype_v3.csv"), row.names = FALSE, na = "")

#Now add IPTW results into main table
out[level,3] <- out2[nimp+1,2]
out[level,4] <- out2[nimp+1,3]
out[level,5] <- out2[nimp+1,4]

out[level+1,3] <- out2[nimp+1,5]
out[level+1,4] <- out2[nimp+1,6]
out[level+1,5] <- out2[nimp+1,7]

out[level+2,3] <- out2[nimp+1,18]
out[level+2,4] <- out2[nimp+1,19]
out[level+2,5] <- out2[nimp+1,20]

out[level+3,3] <- out2[nimp+1,26]
out[level+3,4] <- out2[nimp+1,27]
out[level+3,5] <- out2[nimp+2,28]

out[level,7] <- out2[nimp+1,34]
out[level,8] <- out2[nimp+1,35]
out[level,9] <- out2[nimp+1,36]

out[level+1,7] <- out2[nimp+1,37]
out[level+1,8] <- out2[nimp+1,38]
out[level+1,9] <- out2[nimp+1,39]

out[level+2,7] <- out2[nimp+1,50]
out[level+2,8] <- out2[nimp+1,51]
out[level+2,9] <- out2[nimp+1,52]

out[level+3,7] <- out2[nimp+1,58]
out[level+3,8] <- out2[nimp+1,59]
out[level+3,9] <- out2[nimp+1,60]

write.csv(out, paste0("regressions_subtype.csv"), row.names = FALSE, na = "")


################################################################################
# 7. Sub-types of IPVA - DID analysis
# - Table 5
################################################################################

#Will take a fair bit of memory, so remove nearly everything apart 
#from data needed: imp_long_boys, imp_long2_boys, imp_long_girls, 
#imp_long2_girls, imp_long2_boys_par, imp_long2_girls_par, and a few 
#other things

keep <- c("loc_inp", "loc_out",
	"nimp",
	"imp_long_boys", "imp_long2_boys",
	"imp_long_girls", "imp_long2_girls", 
	"imp_long_boys_par", "imp_long_girls_par",
	"imp_long2_boys_par", "imp_long2_girls_par",

	"data_long_boys", "data_long2_boys",
	"data_long_girls", "data_long2_girls", 

	"cov_list", "cov_list2", "cov_list3",
	"cov_list_log", "covs_log", "covs", "covs2", "covs3",

	"imp_long_boys", "imp_long_girls",
	"imp_long2_boys", "imp_long2_girls",

	"data_long_boys", "data_long_girls",
	"data_long2_boys", "data_long2_girls"
	)
rm(list=setdiff(ls(), keep))

#Number of models per sex
nmodel <- 6
model1 <- "MFQ, MI"
model2 <- "logMFQ, MI"
model3 <- "MFQ, CCA"
model4 <- "logMFQ, CCA"
model5 <- "Parallel, MFQ, MI"
model6 <- "Parallel, logMFQ, MI"

subtypes <- c(0:3)

#Create a table for results
out <- data.frame(matrix(NA, nrow = ((length(subtypes)-1)*nmodel*4)+1, ncol = 11))
colnames(out) <- c(rep("",2),"Males",rep("",3),"Females",rep("",3),"")
out[1, ] <- c("Model","Variable",rep(c("N obs","Coef","SE","p-value"),2),"Subtype vs. No Vic")
out[2:(((length(subtypes)-1)*nmodel*4)+1),1] <- rep(c(rep(model1,4),rep(model2,4),rep(model3,4),
													  rep(model4,4),rep(model5,4),rep(model6,4)),
													(length(subtypes)-1))
out[2:(((length(subtypes)-1)*nmodel*4)+1),2] <- rep(c("Intercept","Vic","Time","Vic*Time"),(length(subtypes)-1)*nmodel)
out[2:(((length(subtypes)-1)*nmodel*4)+1),11] <- c(rep(1,nmodel*4),rep(2,nmodel*4),rep(3,nmodel*4))
level <- 0
digits <- 4
did_model1 <- "depress ~ vic_sub_1821*time"
did_model2 <- "log_depress ~ vic_sub_1821*time"

#Need to read in did_mi and did_cca functions again
source("//ads.bris.ac.uk/filestore/BRMS/Studies/MRC_IPV/Quant/P3 - Mental health/code/did_cca.R")
source("//ads.bris.ac.uk/filestore/BRMS/Studies/MRC_IPV/Quant/P3 - Mental health/code/did_mi.R")

#Note that this following loops takes a long time, at least an hour or two
for(i in 1:(length(subtypes)-1)){
	#Subset certain datasets
	for (j in c("long","long2")){
		for (k in c("boys","girls")){
			a <- complete(get(paste0("imp_",j,"_",k)), action='long', include=TRUE)
			b <- a[(a$vic_sub_1821==0 | a$vic_sub_1821==i),]
			c <- as.mids(b)
			assign(paste0("imp_",j,"_",k,"_temp"),c)	
			}
		} 
	for (j in c("long","long2")){
		for (k in c("boys","girls")){
			a <- get(paste0("data_",j,"_",k))
			b <- a[(a$vic_sub_1821==0 | a$vic_sub_1821==i),]
			assign(paste0("data_",j,"_",k,"_temp"),b)	
			}
		} 
	for (j in c("long","long2")){
		for (k in c("boys","girls")){
			a <- complete(get(paste0("imp_",j,"_",k,"_par")), action='long', include=TRUE)
			b <- a[(a$vic_sub_1821==0 | a$vic_sub_1821==i),]
			c <- as.mids(b)
			assign(paste0("imp_",j,"_",k,"_par_temp"),c)	
			}
		} 
	#Model 1
	out <- did_mi(out,level,did_model1,
		imp_long_boys_temp, imp_long_girls_temp, digits)
	level <- level +1

	#Model 2
	out <- did_mi(out,level,did_model2,
		imp_long2_boys_temp, imp_long2_girls_temp, digits)
	level <- level +1

	#Model 3
	out<- did_cca(out,level,did_model1,
		data_long_boys_temp, data_long_girls_temp, digits)
	level <- level +1

	#Model 4
	out <- did_cca(out,level,did_model2,
		data_long2_boys_temp, data_long2_girls_temp, digits)
	level <- level +1

	#Model 5
	out <- did_mi(out,level,did_model1,
		imp_long_boys_par_temp, imp_long_girls_par_temp, digits)
	level <- level +1

	#Model 6
	out <- did_mi(out,level,did_model2,
	imp_long2_boys_par_temp, imp_long2_girls_par_temp, digits)
	level <- level +1
	}

write.csv(out, "did_table_subtypes.csv", row.names = FALSE, na = "")

#The following is to plot estimates from Model 2 (was potential Figure 
#2 but we're proabbly not going to include in the end)
#Again need to free up some space, imp_long2_girls is rather big...
keep <- c("imp_long2_boys", "imp_long2_girls","subtypes")
rm(list=setdiff(ls(), keep))
gc()

group <- expand.grid(c(0,1,2,3),c(16,23))
plot_est <- data.frame(cbind(group, matrix(nrow=dim(group)[1], ncol=2)))
colnames(plot_est) <- c("ipva_type","age","log_mfq_boys","log_mfq_girls")

for(i in 1:(length(subtypes)-1)){
	a <- complete(get(paste0("imp_long2_boys")), action='long', include=TRUE)
	b <- a[(a$vic_sub_1821==0 | a$vic_sub_1821==i),]
	c <- as.mids(b)
	assign(paste0("imp_long2_boys_temp"),c)	
	
	didreg_boys = pool(with(imp_long2_boys_temp, exp = lm(log_depress ~ vic_1821*time)))
	plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_boys"] <- summary(didreg_boys)[1,2]
	plot_est[plot_est$ipva==0 & plot_est$age==23,"log_mfq_boys"] <- summary(didreg_boys)[1,2]+summary(didreg_boys)[3,2]
	plot_est[plot_est$ipva==i & plot_est$age==16,"log_mfq_boys"] <- summary(didreg_boys)[1,2]+summary(didreg_boys)[2,2]
	plot_est[plot_est$ipva==i & plot_est$age==23,"log_mfq_boys"] <- summary(didreg_boys)[1,2]+summary(didreg_boys)[2,2]+
																	+summary(didreg_boys)[3,2]++summary(didreg_boys)[4,2]

	a <- complete(get(paste0("imp_long2_girls")), action='long', include=TRUE)
	b <- a[(a$vic_sub_1821==0 | a$vic_sub_1821==i),]
	c <- as.mids(b)
	assign(paste0("imp_long2_girls_temp"),c)	
	
	didreg_girls = pool(with(imp_long2_girls_temp, exp = lm(log_depress ~ vic_1821*time)))
	plot_est[plot_est$ipva==0 & plot_est$age==16,"log_mfq_girls"] <- summary(didreg_girls)[1,2]
	plot_est[plot_est$ipva==0 & plot_est$age==23,"log_mfq_girls"] <- summary(didreg_girls)[1,2]+summary(didreg_girls)[3,2]
	plot_est[plot_est$ipva==i & plot_est$age==16,"log_mfq_girls"] <- summary(didreg_girls)[1,2]+summary(didreg_girls)[2,2]
	plot_est[plot_est$ipva==i & plot_est$age==23,"log_mfq_girls"] <- summary(didreg_girls)[1,2]+summary(didreg_girls)[2,2]+
																	+summary(didreg_girls)[3,2]++summary(didreg_girls)[4,2]

	}

plot_est$ipva <- factor(plot_est$ipva, labels=c("No vic","Psych (no phys/sex)","Phys (no sex)","Sex"))
ymin <- min(c(min(plot_est$log_mfq_boys),min(plot_est$log_mfq_girls)))
ymax <- max(c(max(plot_est$log_mfq_boys),max(plot_est$log_mfq_girls)))

plot_men <- ggplot(plot_est, aes(x=age, y=log_mfq_boys)) + 
	geom_point(aes(colour = ipva),size=2, shape=18) +
	geom_line(aes(colour = ipva)) +
	ggtitle("Men") +
	xlab("Age") +
	ylab("Log MFQ score") +
	geom_vline(xintercept = c(18,21), linetype = "dashed") +
	scale_x_discrete(breaks=c(16,17,18,19,20,21,22,23)) +
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
	       	theme(legend.title = element_blank())

tiff(file="fig2.tiff", units="in", width=10, height=5, res=300)
grid.arrange(arrangeGrob(plot_men,plot_women,ncol=2,widths=c(0.9,1), 
	bottom=textGrob("Age",hjust=0)))
dev.off()


################################################################################
# 8. Sensitivity analyses - include those who were victimised at age 
# 0-17, and only those who had had a romantic encounter by age 17.
# Feeds into Tables S4 and S5.
################################################################################

# Already have main analyses and complete cases analyses. Want:

# 1. including all those victimised at 0-17
# (cohort_sens.dta_sub_boys_mpm2_imp and cohort_sens.dta_sub_girls_mpm2_imp for reg
# data_long2_sens_boys_mpm5_imp and data_long2_sens_girls_mpm5_imp for DID)

# 2. including all those in a romantic relationship by age 17
# (imp_boys_sub_log_rom and imp_girls_sub_log_rom for regs,
# imp_long2_boys_rom and imp_long2_girls_rom)

#Need to read full file back in (as had removed most objects earlier due to memory issues)
loc_inp<-'//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/046/working/data/'
loc_out<-'//ads.bris.ac.uk/filestore/BRMS/Studies/MRC_IPV/Quant/P3 - Mental health/results/'
setwd(loc_out)
load(paste0(loc_inp,'cohort/data_for_analysis.RData'))

keep <- c("loc_inp", "loc_out",
	"nimp",
	"cohort_sens.dta_sub_boys_mpm2_imp", "cohort_sens.dta_sub_girls_mpm2_imp",
	"data_long2_sens_boys_mpm5_imp", "data_long2_sens_girls_mpm5_imp",
	"imp_boys_sub_log_rom", "imp_girls_sub_log_rom",
	"imp_long2_boys_rom", "imp_long2_girls_rom",
	"covs3", "covs_log", "cov_list_log"
	)
rm(list=setdiff(ls(), keep))

models <- c("Crude logMFQ, MI","Adj 1 logMFQ, MI","Adj 2 logMFQ, MI","IPTW logMFQ, MI")
nmodel <- length(models)

samples <- c("vic_017","rom_017")
nsample <- length(samples)

out <- data.frame(matrix(NA, nrow = ((nsample*nmodel*2)+1), ncol = 11))
colnames(out) <- c("","","","Males",rep("",3),"Females",rep("",3))
out[1, ] <- c("Model","Term",rep(c("Coef","SE","Coef p-value","N"),2),"Sample")
out[seq(2,(nsample*nmodel*2),by=2),1] <- models
out[2:((nsample*nmodel*2)+1),2] <- rep(c("Intercept","Victimisation"),nsample*nmodel)
level <- 2
digits <- 2

model_list <- c("log_mfqtot_ypc ~ vic_1821", 
				paste0("log_mfqtot_ypc ~ vic_1821 +",covs3),
				paste0("log_mfqtot_ypc ~ vic_1821 +",covs_log))

sample_list <- c("cohort_sens.dta_sub_boys_mpm2_imp","cohort_sens.dta_sub_girls_mpm2_imp",
					"imp_boys_sub_log_rom","imp_girls_sub_log_rom")
for (i in 1:2){
	for (m in model_list){
		vic_mfq23_fit <- with(get(sample_list[(i*2)-1]), lm(as.formula(m)))
		out[level,3] <- summary(pool(vic_mfq23_fit))[1,2]
		out[level,4] <- summary(pool(vic_mfq23_fit))[1,3]
		out[level,5] <- summary(pool(vic_mfq23_fit))[1,6]
		out[level,6] <- ""

		out[level+1,3] <- summary(pool(vic_mfq23_fit))[2,2]
		out[level+1,4] <- summary(pool(vic_mfq23_fit))[2,3]
		out[level+1,5] <- summary(pool(vic_mfq23_fit))[2,6]
		out[level+1,6] <- ""

		vic_mfq23_fit <- with(get(sample_list[i*2]), lm(as.formula(m)))
		out[level,7] <- summary(pool(vic_mfq23_fit))[1,2]
		out[level,8] <- summary(pool(vic_mfq23_fit))[1,3]
		out[level,9] <- summary(pool(vic_mfq23_fit))[1,6]
		out[level,10] <- ""
		out[level,11] <- samples[i]

		out[level+1,7] <- summary(pool(vic_mfq23_fit))[2,2]
		out[level+1,8] <- summary(pool(vic_mfq23_fit))[2,3]
		out[level+1,9] <- summary(pool(vic_mfq23_fit))[2,6]
		out[level+1,10] <- ""
		out[level+1,11] <- ""
		level <- level+2
		}
	#Need to read function iptw_mi back in
	source("//ads.bris.ac.uk/filestore/BRMS/Studies/MRC_IPV/Quant/P3 - Mental health/code/iptw_mi.R")
	iptw_log <- iptw_mi("log_mfqtot_ypc","vic_1821",cov_list_log,get(sample_list[(i*2)-1]),get(sample_list[i*2]))
	
	out[level,3] <- iptw_log[nimp+2,2]
	out[level,4] <- iptw_log[nimp+2,3]
	out[level,5] <- iptw_log[nimp+2,4]
	out[level,6] <- iptw_log[nimp+2,36]
	
	out[level,7] <- iptw_log[nimp+2,19]
	out[level,8] <- iptw_log[nimp+2,20]
	out[level,9] <- iptw_log[nimp+2,21]
	out[level,10] <- iptw_log[nimp+2,37]

	out[level,11] <- samples[i]

	out[level+1,3] <- iptw_log[nimp+2,5]
	out[level+1,4] <- iptw_log[nimp+2,6]
	out[level+1,5] <- iptw_log[nimp+2,7]
	out[level+1,6] <- ""

	out[level+1,7] <- iptw_log[nimp+2,22]
	out[level+1,8] <- iptw_log[nimp+2,23]
	out[level+1,9] <- iptw_log[nimp+2,24]
	out[level+1,10] <- ""
	out[level+1,11] <- ""

	level <- level+2
	}
out
write.csv(out, paste0("sensitivity_regs.csv"), row.names = FALSE, na = "")

#Now addressing DID analysis
samples <- c("vic_017","rom_017")
nsample <- length(samples)

out <- data.frame(matrix(NA, nrow = (nsample*4)+1, ncol = 10))
colnames(out) <- c(rep("",2),"Males",rep("",3),"Females",rep("",3))
out[1, ] <- c("Sample","Variable",rep(c("N obs","Coef","SE","p-value"),2))
out[2:((nsample*4)+1),1] <- c(rep(samples[1],4),rep(samples[2],4))
out[2:((nsample*4)+1),2] <- rep(c("Intercept","Vic","Time","Vic*Time"),nsample)
level <- 2
digits <- 4

sample_list <- c("data_long2_sens_boys_mpm5_imp","data_long2_sens_girls_mpm5_imp",
					"imp_long2_boys_rom","imp_long2_girls_rom")

#Function did_mi2 works as per function did_mi, but with a greater number of categories in exposure 
source("//ads.bris.ac.uk/filestore/BRMS/Studies/MRC_IPV/Quant/P3 - Mental health/code/did_mi2.R")

for (i in 1:2){
	out2 <- did_mi2("log_depress ~ vic_1821*time",
			get(sample_list[(i*2)-1]), get(sample_list[(i*2)]), digits)
	out[level:(level+3),3:10] <- out2[1:4,2:9]
	level <- level+4
	}
write.csv(out, "sensitivity_did.csv", row.names = FALSE, na = "")

