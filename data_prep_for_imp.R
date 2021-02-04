## Project: Depression following IPVA
## Script purpose: Preparing data for imputation and analysis
## Date: 12th November 2020
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
loc_inp<-'W:/data/'
loc_out<-'C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/IPV project/p5 - MH outcomes/results'
setwd(loc_out)


################################################################################
# 1. Get and prep data  
################################################################################

Sys.time()
#20.13, finished at 20.15 

# Load data, selecting those who do not have missing sex (n=1), 
# and who did not say they were exposed to vic at 0-17 years old (n=487)

# Need to create file using objects alspacKids_ACE_data_2018,
# ACEmeasures ACEs, and classicACEs, in 
# 'W:/data/ACE data/Annie/alspacKids_ACE_0_16_imputation_AH.RData'
# and "W:/data/cohort/current_IPVA_cohort_id.dta"

cohort.dta <- read.dta13("W:/data/cohort/current_IPVA_cohort_id.dta")
dim(cohort.dta)
# N = 3280, vars = 235

#Remove Lotte's ACEs post-imputation
cohort.dta <- cohort.dta[,!names(cohort.dta) %in% 
	c("ace_c","ace_no","physical_abu","sexual_abuse",
	"emotional_ab","emotional_ne","bullying_0_1","violence_bet","substance_ho",
	"mentl_hlth_p", "parent_convi", "parental_sep", "social_class_0_16yrs",
	"financial_difficulties_0_16yrs", "neighbourhood_0_16yrs", 
	"social_support_child_0_16yrs", "social_support_parent_0_16yrs", 
	"vlnc_btwn_chld_nd_prtnr_0_16yrs", "physical_illness_child_0_16yrs", 
	"physical_illness_parent_0_16yrs", "parent_child_bond_0_16yrs", 
	"ACEscore_extended_0_16yrs", "ACEcat_extended_0_16yrs", 
	"ACEscore_classic_0_16yrs", "ACEcat_classic_0_16yrs")]

#Load Lotte's ACEs pre-imputation
load("W:/data/ACE data/Annie/alspacKids_ACE_0_16_imputation_AH.RData")

#And Lotte's description of ACE and imputation variables
adv_description <- data.frame(readxl::read_excel(
 "W:/data/ACE data/Annie/SI_data1_ACE_definitions_overtime.xlsx",
  sheet = 'ACE variables',col_names=TRUE))

#Need to remove those with levels including observations n < 50
alspacBoys_ACE_data_2018 <- alspacKids_ACE_data_2018[
  grepl('Male', alspacKids_ACE_data_2018$kz021),]

alspacGirls_ACE_data_2018 <- alspacKids_ACE_data_2018[
  grepl('Female', alspacKids_ACE_data_2018$kz021),]

#Drop unused factor levels
alspacKids_ACE_data_2018 <- droplevels(alspacKids_ACE_data_2018)
alspacBoys_ACE_data_2018 <- droplevels(alspacBoys_ACE_data_2018)
alspacGirls_ACE_data_2018 <- droplevels(alspacGirls_ACE_data_2018)

# make the variables factors if they are factors in Lotte's dataset - using factors_list.RData
load("W:/data/ACE data/Annie/factors_list.RData")
for (i in 1:length(factors[[1]])){
  if(factors[[2]][i]=="TRUE"){
    alspacKids_ACE_data_2018[,factors[[1]][i]] <- as.factor(alspacKids_ACE_data_2018[,factors[[1]][i]])
    }
  }

# 'org' indicates that variables for SES ('pa_sc_p' to 'pl_sc_p') and adversity 
# pre-birth, e.g. smoking ('c804' to 'e178')
imp_standard <- grep('org',names(alspacKids_ACE_data_2018),value=T)
table(sapply(alspacKids_ACE_data_2018, function(x) class(x)))
#106 factor vars, 109 logical vars in full alspacKids dataframe, currently
length(imp_standard)
table(sapply(imp_standard, function(x) class(x)))
#108 character variables in imp_standard

# Take out only those that are factor variables
imp_factor <- imp_standard[
  sapply(alspacKids_ACE_data_2018[,imp_standard],function(x) is.factor(x))]
# 92 are factor variables

# Identify ACE imputation variables with <50 obs in any factor level: ----
imp_ACE <- imp_factor[gsub('_org','',imp_factor)%in%adv_description
            $variable_unique]

# 84 of these factor variables make up the definition of ACE
table(sapply(imp_ACE, function(x) class(x)))

# List of ACE factor variables where there are obs <50 in any level
imp_ACE_less_n50 <-
  imp_ACE[
    sapply(alspacBoys_ACE_data_2018[,imp_ACE],
    function(x) any(table(factor(x))<50))|

    sapply(alspacGirls_ACE_data_2018[,imp_ACE],
    function(x) any(table(factor(x))<50))]
imp_ACE_less_n50
#77 vars have a level less than 50
remainder <- length(imp_standard)-length(imp_ACE_less_n50)
remainder
#31 vars are fine

# Now which ACE variables which didn't satisfy n>50 for both sexes, but
# could do in their binary form (as the column sums of the non-missing values)
imp_ACE_use_binary <- imp_ACE_less_n50[
  colSums(alspacBoys_ACE_data_2018[,gsub('_org','',imp_ACE_less_n50)],na.rm=T)>=50 & 
  colSums(alspacGirls_ACE_data_2018[,gsub('_org','',imp_ACE_less_n50)],na.rm=T)>=50]
imp_ACE_use_binary
# 5 vars

# Now remove the variables that don't qualify, even for binary--------------
rm_imp <- imp_ACE_less_n50[!imp_ACE_less_n50%in%imp_ACE_use_binary]
length(rm_imp)
#72 variables need to be removed

if(length(rm_imp)>0){
  imp_standard <- imp_standard[!imp_standard%in%rm_imp]}

length(imp_standard)
# 108 variables has dropped to 36

## ---- Get rid of the 'org' labels, but only for the ones that are in binary.  ----
if(length(imp_ACE_use_binary)>0){
  imp_standard[imp_standard%in%imp_ACE_use_binary] <- 
    gsub('_org','',imp_standard[imp_standard%in%imp_ACE_use_binary])}

sum(grepl('_org',imp_standard))
#36 vars, of which 5 binarised.


################################################################################
# 2. Create subset of only variables needed (aln, qlet, ACEs, aux variables)
################################################################################
aux.vars <- imp_standard

ace.vars <- c("aln","qlet","physical_abuse_0_16yrs",                     
	"sexual_abuse_0_16yrs", "emotional_abuse_0_16yrs", "emotional_neglect_0_16yrs",
	"bullying_0_16yrs", "violence_between_parents_0_16yrs", 
	"substance_household_0_16yrs", "mental_health_problems_or_suicide_0_16yrs", 
	"parent_convicted_offence_0_16yrs", "parental_separation_0_16yrs",aux.vars)

aces.dta <- alspacKids_ACE_data_2018[,c("aln","qlet",ace.vars)]

#Rename ACE vars to shorthand
setnames(aces.dta, 
	old = c("physical_abuse_0_16yrs", "sexual_abuse_0_16yrs",
		"emotional_abuse_0_16yrs", "emotional_neglect_0_16yrs",
		"bullying_0_16yrs", "violence_between_parents_0_16yrs", 
		"substance_household_0_16yrs", "mental_health_problems_or_suicide_0_16yrs", 
		"parent_convicted_offence_0_16yrs", "parental_separation_0_16yrs"), 
	new = c("physical_abu","sexual_abuse",
		"emotional_ab","emotional_ne",
		"bullying_0_1", "violence_bet",
		"substance_ho", "mentl_hlth_p", 
		"parent_convi", "parental_sep"))

cohort.dta <- merge(cohort.dta,aces.dta,by=c("aln","qlet"),all.x=TRUE)
dim(cohort.dta)
names(cohort.dta)

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

#When relationship != 1, ==0
cohort.dta[is.na(cohort.dta$rel_ind_017),"rel_ind_017"] <- 0
cohort.dta[is.na(cohort.dta$rel_ind_021),"rel_ind_021"] <- 0

#Making sure most of these variables are recognised as factors (all except mfqtot_ccs)
#As well as victimisation and relationship status
for (c in c("vic_017","vic_1821","rel_ind_017","rel_ind_021",cov_list[cov_list!="mfqtot_ccs"])){
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

#cohort.dta <- read.dta13("current_IPVA_cohort_id.dta")
#Reduce to those who haven't been exposed to IPVA at ages 0-17

#Also want a cohort for sensitivity analysis!
cohort_sens.dta <- cohort.dta[is.na(cohort.dta$kz021_new)==FALSE,]
dim(cohort_sens.dta)
# N = 3279

cohort.dta <- cohort.dta[is.na(cohort.dta$kz021_new)==FALSE 
					& cohort.dta$vic_017!=1
					,]
dim(cohort.dta)
# N = 2792
# List of alns and qlets that are okay for main cohort
main_cohort <- cohort.dta[,c("aln","qlet")]


################################################################################
# 3. Specific prep for imputation  
################################################################################
# Selecting only those variables that are needed
cohort_sens.dta_sub <- cohort_sens.dta[,c("aln","qlet","kz021_new",
								cov_list[cov_list!="mfqtot_ccs"],
								"vic_017","vic_1821","vic_sub_1821",
								"mfqtot_tf2","mfqtot_ccs","mfqtot_ypc",
								"log_mfqtot_tf2","log_mfqtot_ccs","log_mfqtot_ypc",
								"mfqbin_ccs","mfqbin_ypc",
								"rel_ind_017", "rel_ind_021",
								aux.vars)]

#Want aux.vars that are not marked with _org in name to be numerical, not logical 
#(important for later)
for(i in aux.vars[grepl('_org',aux.vars)==FALSE]){
	#Have to use this weird vector way because R doesn't like over-subscripting mids objects
	vector <- cohort_sens.dta_sub[,i]
	vector[vector==FALSE] <- 0
	vector[vector==TRUE] <- 1
	cohort_sens.dta_sub[,i] <- vector
	}

cohort.dta_sub <- merge(cohort_sens.dta_sub,main_cohort,by=c("aln","qlet"),all.y=TRUE)

nimp <- 50

ini <- mice(cohort.dta_sub, maxit=0)
#4 logged events
head(ini$loggedEvents, 1)
#Just a constant
pred_mat1 <- ini$pred 
meth1 <- ini$meth
exclude<-c("aln","qlet",#"kz021_new",
	"mfqbin_ccs","mfqbin_ypc",
	"log_mfqtot_tf2","log_mfqtot_ccs","log_mfqtot_ypc")
pred_mat1[exclude,] <- 0
pred_mat1[,exclude] <- 0
meth1[exclude] <- ""

#Investigating bi-variate correlations
pred_mat1
sum1 <- rowSums(pred_mat1)
sum2 <- rowSums(quickpred(cohort.dta_sub))

# Need to impute log values separately
pred_mat2 <- ini$pred 
meth2 <- ini$meth
exclude<-c("aln","qlet",#"kz021_new",
	"mfqbin_ccs","mfqbin_ypc",
	"mfqtot_tf2","mfqtot_ccs","mfqtot_ypc")
pred_mat2[,exclude] <- 0
meth2[exclude] <- ""

# Finally binary outcomes
pred_mat3 <- ini$pred 
meth3 <- ini$meth
exclude<-c("aln","qlet",#"kz021_new",
	"mfqtot_tf2","mfqtot_ccs","mfqtot_ypc",
	"log_mfqtot_tf2","log_mfqtot_ccs","log_mfqtot_ypc")
pred_mat3[,exclude] <- 0
meth3[exclude] <- ""

cohort.dta_sub_boys <- cohort.dta_sub[cohort.dta_sub$kz021_new==1,]
cohort.dta_sub_girls <- cohort.dta_sub[cohort.dta_sub$kz021_new==2,]

cohort_sens.dta_sub_boys <- cohort_sens.dta_sub[cohort.dta_sub$kz021_new==1,]
cohort_sens.dta_sub_girls <- cohort_sens.dta_sub[cohort.dta_sub$kz021_new==2,]

#Data for DID analyses - need to impute again for diff-in-diff analysis as long format
#For sensitivity analysis first
data_long_sens <- gather(cohort_sens.dta_sub, time, depress, mfqtot_ccs:mfqtot_ypc, factor_key=FALSE)
data_long_sens$time[data_long_sens$time=="mfqtot_ccs"] <- 0
data_long_sens$time[data_long_sens$time=="mfqtot_ypc"] <- 1
data_long_sens[,"vic_1821"] <- as.numeric(data_long_sens[,"vic_1821"])
data_long_sens$vic_1821 <- data_long_sens$vic_1821-1
data_long_sens[,"time"] <- as.numeric(data_long_sens[,"time"])
#Include an interaction term between vic_1821 and time as this is what we have in our analysis model
#Update: actually need to include THREE
data_long_sens[,"int_vic_time"] <- data_long_sens[,"vic_1821"]*data_long_sens[,"time"]
data_long_sens[,"int_vic_depress"] <- data_long_sens[,"vic_1821"]*data_long_sens[,"depress"]
data_long_sens[,"int_time_depress"] <- data_long_sens[,"time"]*data_long_sens[,"depress"]
data_long_sens_boys <- data_long_sens[data_long_sens$kz021_new==1 & is.na(data_long_sens$aln)==FALSE,]
data_long_sens_girls <- data_long_sens[data_long_sens$kz021_new==2 & is.na(data_long_sens$aln)==FALSE,]

#Main cohort
data_long <- merge(data_long_sens,main_cohort,by=c("aln","qlet"),all.y=TRUE)
data_long_boys <- data_long[data_long$kz021_new==1 & is.na(data_long$aln)==FALSE,]
data_long_girls <- data_long[data_long$kz021_new==2 & is.na(data_long$aln)==FALSE,]

ini <- mice(data_long_boys, maxit=0)
pred_mat4 <- ini$pred 
meth4 <- ini$meth
#Don't want these to be used in predictions
exclude<-c("aln","qlet",#kz021_new,
	"log_mfqtot_tf2", "log_mfqtot_ccs", "log_mfqtot_ypc")
pred_mat4[,exclude] <- 0
meth4[exclude] <- ""
#Also make sure interactions don't predict themselves...
#And want to impute interaction via passive imputation
#See: https://www.gerkovink.com/miceVignettes/Passive_Post_processing/Passive_imputation_post_processing.html
pred_mat4[c("vic_1821","time","int_vic_depress","int_time_depress"),"int_vic_time"] <- 0
pred_mat4[c("vic_1821","depress","int_vic_time","int_time_depress"),"int_vic_depress"] <- 0
pred_mat4[c("time","depress","int_vic_time","int_vic_depress"),"int_time_depress"] <- 0
meth4["int_vic_time"]<- "~ I(vic_1821*time)"
meth4["int_vic_depress"]<- "~ I(vic_1821*depress)"
meth4["int_time_depress"]<- "~ I(time*depress)"

# DID analyes - logged outcome
data_long2_sens <- gather(cohort_sens.dta_sub, time, log_depress, log_mfqtot_ccs:log_mfqtot_ypc, factor_key=FALSE)
data_long2_sens$time[data_long2_sens$time=="log_mfqtot_ccs"] <- 0
data_long2_sens$time[data_long2_sens$time=="log_mfqtot_ypc"] <- 1
data_long2_sens[,"vic_1821"] <- as.numeric(data_long2_sens[,"vic_1821"])
data_long2_sens$vic_1821 <- data_long2_sens$vic_1821-1
data_long2_sens[,"time"] <- as.numeric(data_long2_sens[,"time"])
data_long2_sens[,"int_vic_time"] <- data_long2_sens[,"vic_1821"]*data_long2_sens[,"time"]
data_long2_sens[,"int_vic_logdepress"] <- data_long2_sens[,"vic_1821"]*data_long2_sens[,"log_depress"]
data_long2_sens[,"int_time_logdepress"] <- data_long2_sens[,"time"]*data_long2_sens[,"log_depress"]
data_long2_sens_boys <- data_long2_sens[data_long2_sens$kz021_new==1 & is.na(data_long2_sens$aln)==FALSE,]
data_long2_sens_girls <- data_long2_sens[data_long2_sens$kz021_new==2 & is.na(data_long2_sens$aln)==FALSE,]

#Main cohort
data_long2 <- merge(data_long2_sens,main_cohort,by=c("aln","qlet"),all.y=TRUE)
data_long2_boys <- data_long2[data_long2$kz021_new==1 & is.na(data_long2$aln)==FALSE,]
data_long2_girls <- data_long2[data_long2$kz021_new==2 & is.na(data_long2$aln)==FALSE,]

ini <- mice(data_long2_boys, maxit=0)
pred_mat5 <- ini$pred 
meth5 <- ini$meth
exclude<-c("aln","qlet", #"kz021_new",
	"mfqtot_tf2", "mfqtot_ccs", "mfqtot_ypc")
pred_mat5[,exclude] <- 0
meth5[exclude] <- ""

pred_mat5[c("vic_1821","time","int_vic_logdepress","int_time_logdepress"),"int_vic_time"] <- 0
pred_mat5[c("vic_1821","log_depress","int_vic_time","int_time_logdepress"),"int_vic_logdepress"] <- 0
pred_mat5[c("time","log_depress","int_vic_time","int_vic_logdepress"),"int_time_logdepress"] <- 0
meth5["int_vic_time"]<- "~ I(vic_1821*time)"
meth5["int_vic_logdepress"]<- "~ I(vic_1821*log_depress)"
meth5["int_time_logdepress"]<- "~ I(time*log_depress)"

# For checking lines are parallel before age 16 - raw
data_long_par_sens <- gather(cohort_sens.dta_sub, time, depress, mfqtot_tf2:mfqtot_ccs, factor_key=FALSE)
data_long_par_sens$time[data_long_par_sens$time=="mfqtot_tf2"] <- 0
data_long_par_sens$time[data_long_par_sens$time=="mfqtot_ccs"] <- 1
data_long_par_sens[,"vic_1821"] <- as.numeric(data_long_par_sens[,"vic_1821"])
data_long_par_sens$vic_1821 <- data_long_par_sens$vic_1821-1
data_long_par_sens[,"time"] <- as.numeric(data_long_par_sens[,"time"])
data_long_par_sens[,"int_vic_time"] <- data_long_par_sens[,"vic_1821"]*data_long_par_sens[,"time"]
data_long_par_sens[,"int_vic_depress"] <- data_long_par_sens[,"vic_1821"]*data_long_par_sens[,"depress"]
data_long_par_sens[,"int_time_depress"] <- data_long_par_sens[,"time"]*data_long_par_sens[,"depress"]
data_long_par_sens_boys <- data_long_par_sens[data_long_par_sens$kz021_new==1 & is.na(data_long_par_sens$aln)==FALSE,]
data_long_par_sens_girls <- data_long_par_sens[data_long_par_sens$kz021_new==2 & is.na(data_long_par_sens$aln)==FALSE,]

#Main cohort
data_long_par <- merge(data_long_par_sens,main_cohort,by=c("aln","qlet"),all.y=TRUE)
data_long_par_boys <- data_long_par[data_long_par$kz021_new==1 & is.na(data_long_par$aln)==FALSE,]
data_long_par_girls <- data_long_par[data_long_par$kz021_new==2 & is.na(data_long_par$aln)==FALSE,]

ini <- mice(data_long_par_boys, maxit=0)
pred_mat6 <- ini$pred 
meth6 <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"log_mfqtot_tf2", "log_mfqtot_ccs", "log_mfqtot_ypc")
pred_mat6[,exclude] <- 0
meth6[exclude] <- ""  
pred_mat6[c("vic_1821","time","int_vic_depress","int_time_depress"),"int_vic_time"] <- 0
pred_mat6[c("vic_1821","depress","int_vic_time","int_time_depress"),"int_vic_depress"] <- 0
pred_mat6[c("time","depress","int_vic_time","int_vic_depress"),"int_time_depress"] <- 0
meth6["int_vic_time"]<- "~ I(vic_1821*time)"
meth6["int_vic_depress"]<- "~ I(vic_1821*depress)"
meth6["int_time_depress"]<- "~ I(time*depress)"

# For checking lines are parallel before age 16 - logged outcome
data_long2_par_sens <- gather(cohort_sens.dta_sub, time, log_depress, log_mfqtot_tf2:log_mfqtot_ccs, factor_key=FALSE)
data_long2_par_sens$time[data_long2_par_sens$time=="log_mfqtot_tf2"] <- 0
data_long2_par_sens$time[data_long2_par_sens$time=="log_mfqtot_ccs"] <- 1
data_long2_par_sens[,"vic_1821"] <- as.numeric(data_long2_par_sens[,"vic_1821"])
data_long2_par_sens$vic_1821 <- data_long2_par_sens$vic_1821-1
data_long2_par_sens[,"time"] <- as.numeric(data_long2_par_sens[,"time"])
data_long2_par_sens[,"int_vic_time"] <- data_long2_par_sens[,"vic_1821"]*data_long2_par_sens[,"time"]
data_long2_par_sens[,"int_vic_logdepress"] <- data_long2_par_sens[,"vic_1821"]*data_long2_par_sens[,"log_depress"]
data_long2_par_sens[,"int_time_logdepress"] <- data_long2_par_sens[,"time"]*data_long2_par_sens[,"log_depress"]
data_long2_par_sens_boys <- data_long2_par_sens[data_long2_par_sens$kz021_new==1 & is.na(data_long2_par_sens$aln)==FALSE,]
data_long2_par_sens_girls <- data_long2_par_sens[data_long2_par_sens$kz021_new==2 & is.na(data_long2_par_sens$aln)==FALSE,]

#Main cohort
data_long2_par <- merge(data_long2_par_sens,main_cohort,by=c("aln","qlet"),all.y=TRUE)
data_long2_par_boys <- data_long2_par[data_long2_par$kz021_new==1 & is.na(data_long2_par$aln)==FALSE,]
data_long2_par_girls <- data_long2_par[data_long2_par$kz021_new==2 & is.na(data_long2_par$aln)==FALSE,]

ini <- mice(data_long2_par_boys, maxit=0)
pred_mat7 <- ini$pred 
meth7 <- ini$meth
exclude<-c("aln","qlet","kz021_new",
	"mfqtot_tf2", "mfqtot_ccs", "mfqtot_ypc")
pred_mat7[,exclude] <- 0
meth7[exclude] <- ""
pred_mat7[c("vic_1821","time","int_vic_logdepress","int_time_logdepress"),"int_vic_time"] <- 0
pred_mat7[c("vic_1821","log_depress","int_vic_time","int_time_logdepress"),"int_vic_logdepress"] <- 0
pred_mat7[c("time","log_depress","int_vic_time","int_vic_logdepress"),"int_time_logdepress"] <- 0
meth7["int_vic_time"]<- "~ I(vic_1821*time)"
meth7["int_vic_logdepress"]<- "~ I(vic_1821*log_depress)"
meth7["int_time_logdepress"]<- "~ I(time*log_depress)"

dataset <- c("cohort_sens.dta_sub","cohort_sens.dta_sub","cohort_sens.dta_sub","data_long_sens","data_long2_sens","data_long_par_sens","data_long2_par_sens")
label_out <- c("imp_sub_sens","imp_sub_log_sens","imp_sub_bin_sens","imp_long_sens","imp_long2_sens","imp_long_par_sens","imp_long2_par_sens")

#Lists to work through
imp_list <- list("dataset" = 1, "maxit" = 2, 
	"meth_pred_mat" = rep(seq(from = 1, to = length(dataset), by = 1),2),
	"label_out" = 3)

imp_list[["dataset"]] <- c(paste0(dataset,"_boys"),paste0(dataset,"_girls"))
imp_list[["maxit"]][grepl("long",imp_list[["dataset"]])==TRUE] <- 30
imp_list[["maxit"]][grepl("long",imp_list[["dataset"]])==FALSE] <- 10
imp_list[["label_out"]] <- paste0(imp_list[["dataset"]],"_mpm",imp_list[["meth_pred_mat"]],"_imp")

save(nimp,
	# cohort.dta_sub_boys, cohort.dta_sub_girls,
	# data_long_boys, data_long_girls,
	# data_long2_boys, data_long2_girls,
	# data_long_boys_par, data_long_girls_par, 
	# data_long2_boys_par, data_long2_girls_par,
	cohort_sens.dta_sub_boys, cohort_sens.dta_sub_girls,
	data_long_sens_boys, data_long_sens_girls,
	data_long2_sens_boys, data_long2_sens_girls,
	data_long_par_sens_boys, data_long_par_sens_girls, 
	data_long2_par_sens_boys, data_long2_par_sens_girls,
	meth1, pred_mat1, meth2, pred_mat2, meth3, pred_mat3,
	meth4, pred_mat4, meth5, pred_mat5, meth6, pred_mat6,
	meth7, pred_mat7,
	imp_list,
	#and for analysis later:
	cov_list, cov_list2, cov_list3,
	cov_list_log, covs_log, covs, covs2, covs3,
	aux.vars,
	file='W:/data/cohort/data_for_imputation.RData')

#Check how long one imputation, one iteration, likely to take
Sys.time()
imp_boys_test<-mice(cohort_sens.dta_sub_boys, m = 1, maxit = 1,
	print = TRUE, method = meth1, predictorMatrix = pred_mat1,
	stringsAsFactor = TRUE, seed=140817)
Sys.time()
#20 seconds, 60 logged events, investigating:
head(imp_boys_test$loggedEvents, 5)
tail(imp_boys_test$loggedEvents, 5)
#   it im            dep   meth       out      
# 1  1  1 bimd2010q5_bin logreg vic_18211      
# 2  1  1      ethnicity logreg vic_18211      
# 3  1  1    sex_min_tf3 logreg vic_18211      
# 4  1  1    anxiety_tf4 logreg vic_18211      
# 5  1  1        asb_ccq logreg vic_18211      

#    it im                dep meth       out   
# 56  1  1         fa5510_org  pmm vic_18211   
# 57  1  1   matsmok_tri1_org  pmm vic_18211   
# 58  1  1   matsmok_tri2_org  pmm vic_18211   
# 59  1  1 matsmok_tri3_c_org  pmm vic_18211   
# 60  1  1 matsmok_tri3_e_org  pmm vic_18211         
#out: "a (possibly long) character vector with the names of the altered or removed predictors."
#i.e. vic_1821 not great for predicting other values

Sys.time()
imp_girls_test<-mice(cohort_sens.dta_sub_girls, m = 1, maxit = 1,
	print = TRUE, method = meth1, predictorMatrix = pred_mat1,
	stringsAsFactor = TRUE, seed=140817)
Sys.time()
#20 seconds, 60 logged events, investigating:
head(imp_girls_test$loggedEvents, 5)
tail(imp_girls_test$loggedEvents, 5)
#Same as before, but also including a525_org (mother's marital status)
#where category is Widowed (n=0).
#This was included because it's not one of the variables that makes up the ACEs
#And rules about <50 were only applied there.
#Would expect 50 imputations, 10 iterations to take around 50*10*20=3 hours

#Do a dry run with the data_long type of dataset, too
Sys.time()
imp_boys_test2<-mice(data_long_sens_boys, m = 1, maxit = 1,
	print = TRUE, method = meth4, predictorMatrix = pred_mat4,
	stringsAsFactor = TRUE, seed=140817)
Sys.time()
imp_girls_test2<-mice(data_long_sens_girls, m = 1, maxit = 1,
	print = TRUE, method = meth4, predictorMatrix = pred_mat4,
	stringsAsFactor = TRUE, seed=140817)
Sys.time()
#Similar loggedEvents and timing as for the other data (25 and 40 seconds)
#(also mention pb177 = separated since partner preg, v sparse)
#Would expect 50 imputations, 50 iterations to take up to 50*50*40=28 hours

################################################################################
# 4. Run 'imputation.R' in Blue Crystal (HPC)
################################################################################
