## Project: Depression following IPVA
## Script purpose: Taking mids objects from imputations that were run in Blue Crystal (HPC), 
## manipulating, and re-saving, along with complete case data.
## Date: 12th January 2020
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

loc_inp<-'W:/data/'
loc_out<-'C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/IPV project/p5 - MH outcomes/results'
setwd(loc_out)


## ---- Load all datasets and lists needed -------------------------------------------------------------------
#For imp_list, complete case data, covariate lists, etc.
load(paste0(loc_inp,'cohort/data_for_analysis.RData'))

#Now load all imputed datasets
for (i in imp_list$label_out){
	load(paste0(loc_inp,'cohort/',i,'.RData'))
	assign(i,temp)
	}
rm(temp)


#Main cohort before imputation is cohort_sens.dta_sub_boys + cohort_sens.dta_sub_girls
#with those exposed to victimisation at age 0-17, removed
cohort.dta <- rbind(cohort_sens.dta_sub_boys,cohort_sens.dta_sub_girls)
cohort.dta <- cohort.dta[cohort.dta$vic_017!=1,]
cohort.dta_boys <- cohort.dta[cohort.dta$kz021_new==1,]
cohort.dta_girls <- cohort.dta[cohort.dta$kz021_new==2,]

data_long_boys <- data_long_sens_boys[data_long_sens_boys$vic_017!=1,] 
data_long_girls <- data_long_sens_girls[data_long_sens_girls$vic_017!=1,] 
data_long2_boys <- data_long2_sens_boys[data_long2_sens_boys$vic_017!=1,] 
data_long2_girls <- data_long2_sens_girls[data_long2_sens_girls$vic_017!=1,] 


#Want main cohort but after imputation, both where crude mfq and log mfq
sex <- c("boys","girls")	
for (k in sex){
	dataset <- get(paste0("cohort_sens.dta_sub_",k,"_mpm1_imp"))
	dataset_log <- get(paste0("cohort_sens.dta_sub_",k,"_mpm2_imp"))
	#Now working on data reshaped for DID analyses
	did_dataset <- get(paste0("data_long_sens_",k,"_mpm4_imp"))
	did_dataset_log <- get(paste0("data_long2_sens_",k,"_mpm5_imp"))

	did_par_dataset <- get(paste0("data_long_par_sens_",k,"_mpm6_imp"))
	did_par_dataset_log <- get(paste0("data_long2_par_sens_",k,"_mpm7_imp"))

	#vars b587, pb177, t3336, t5404, t3316 (all those aux vars that weren't _org) creating probs as logical in data part of mids object, but numerical in imp part
	for(i in aux.vars[grepl('_org',aux.vars)==FALSE]){
		#Have to use this weird vector way because R doesn't like over-subscripting mids objects
		vector <- dataset$data[,i]
		vector[vector==FALSE] <- 0
		vector[vector==TRUE] <- 1
		dataset$data[,i] <- vector

		vector <- dataset_log$data[,i]
		vector[vector==FALSE] <- 0
		vector[vector==TRUE] <- 1
		dataset_log$data[,i] <- vector

		vector <- did_dataset$data[,i]
		vector[vector==FALSE] <- 0
		vector[vector==TRUE] <- 1
		did_dataset$data[,i] <- vector

		vector <- did_dataset_log$data[,i]
		vector[vector==FALSE] <- 0
		vector[vector==TRUE] <- 1
		did_dataset_log$data[,i] <- vector
		}

	#Want subset that are not exposed to vic at age 0-17
	data <- complete(dataset,action='long',include=TRUE)	
	data2 <- data[data$vic_017!=1,]
	data3 <- as.mids(data2)
	assign(paste0("imp_",k,"_sub"),data3)

	data_log <- complete(dataset_log,action='long',include=TRUE)	
	data2_log <- data_log[data_log$vic_017!=1,]
	data3_log <- as.mids(data2_log)
	assign(paste0("imp_",k,"_sub_log"),data3_log)

	did_data <- complete(did_dataset,action='long',include=TRUE)	
	did_data2 <- did_data[did_data$vic_017!=1,]
	did_data3 <- as.mids(did_data2)
	assign(paste0("imp_long_",k),did_data3)

	did_data_log <- complete(did_dataset_log,action='long',include=TRUE)	
	did_data2_log <- did_data_log[did_data_log$vic_017!=1,]
	did_data3_log <- as.mids(did_data2_log)
	assign(paste0("imp_long2_",k),did_data3_log)

	#Dataset where romantic encounter by age 17
	data2_log_rom <- data_log[data_log$rel_ind_017==1,]
	data3_log_rom <- as.mids(data2_log_rom)
	assign(paste0("imp_",k,"_sub_log_rom"),data3_log_rom)

	did_data2_log_rom <- did_data_log[did_data_log$rel_ind_017==1,]
	did_data3_log_rom <- as.mids(did_data2_log_rom)
	assign(paste0("imp_long2_",k,"_rom"),did_data3_log_rom)
	}
#Will have some warnings, and this is because for most datasets,
# vic_017 now has just one level (and the rest rom_ind_017 has just one)


par datasets need to be done separately as at this point, lots of objects using up memory!
rm(list=c("cohort_sens.dta_sub_boys_mpm1_imp",
	"cohort_sens.dta_sub_boys_mpm3_imp",
	"data_long_sens_boys_mpm4_imp",
	"cohort_sens.dta_sub_girls_mpm1_imp",
	"cohort_sens.dta_sub_girls_mpm3_imp",
	"data_long_sens_girls_mpm4_imp"))	

for (k in sex){
	did_par_dataset <- get(paste0("data_long_par_sens_",k,"_mpm6_imp"))
	did_par_dataset_log <- get(paste0("data_long2_par_sens_",k,"_mpm7_imp"))

	#vars b587, pb177, t3336, t5404, t3316 (all those aux vars that weren't _org) creating probs as logical in data part of mids object, but numerical in imp part
	for(i in aux.vars[grepl('_org',aux.vars)==FALSE]){
		vector <- did_par_dataset$data[,i]
		vector[vector==FALSE] <- 0
		vector[vector==TRUE] <- 1
		did_par_dataset$data[,i] <- vector

		vector <- did_par_dataset_log$data[,i]
		vector[vector==FALSE] <- 0
		vector[vector==TRUE] <- 1
		did_par_dataset_log$data[,i] <- vector
		}

	did_par_data <- complete(did_par_dataset,action='long',include=TRUE)	
	did_par_data2 <- did_par_data[did_par_data$vic_017!=1,]
	did_par_data3 <- as.mids(did_par_data2)
	assign(paste0("imp_long_",k,"_par"),did_par_data3)

	did_par_data_log <- complete(did_par_dataset_log,action='long',include=TRUE)	
	did_par_data2_log <- did_par_data_log[did_par_data_log$vic_017!=1,]
	did_par_data3_log <- as.mids(did_par_data2_log)
	assign(paste0("imp_long2_",k,"_par"),did_par_data3_log)
	}


#Save all datasets needed within one RData file, as section above can take ages
save(cohort_sens.dta_sub_boys, cohort_sens.dta_sub_girls,
	data_long_sens_boys, data_long_sens_girls,
	data_long2_sens_boys, data_long2_sens_girls,
	data_long_par_sens_boys, data_long_par_sens_girls, 
	data_long2_par_sens_boys, data_long2_par_sens_girls,
	
	cov_list, cov_list2, cov_list3,
	cov_list_log, covs_log, covs, covs2, covs3,
	aux.vars, 

	nimp, imp_list,

	cohort_sens.dta_sub_boys_mpm2_imp, cohort_sens.dta_sub_girls_mpm2_imp,
	data_long2_sens_boys_mpm5_imp, data_long2_sens_girls_mpm5_imp,

	cohort.dta, 
	cohort.dta_boys, cohort.dta_girls,

	data_long_boys, data_long_girls,
	data_long2_boys, data_long2_girls,
	
	imp_boys_sub, imp_girls_sub, 
	imp_boys_sub_log, imp_girls_sub_log,
	imp_long_boys, imp_long_girls,
	imp_long2_boys, imp_long2_girls,
	imp_boys_sub_log_rom, imp_girls_sub_log_rom,
	imp_long2_boys_rom, imp_long2_girls_rom,

	imp_long_boys_par, imp_long_girls_par,
	imp_long2_boys_par, imp_long2_girls_par,

	file=paste0(loc_inp,'cohort/data_for_analysis.RData'))