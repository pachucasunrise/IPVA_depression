## Project: Depression following IPVA
## Script purpose: Multiple imputation of missing data to run in Blue Crystal (HPC)
## Date: 12th November 2020
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

Sys.time()
#Started at 21.30

require(mice)
require(parallel)

#Loading all prepared datasets, nimp (number of iterations for all imputations),
#and imp_list which includes details of what dataset to use,
#the specific prediction matrix, maximum iterations, and methods to use 
#(these latter three are different per dataset), 
#and what the call the new imputed dataset
#load('//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/046/working/data/cohort/data_for_imputation.RData')
load('data_for_imputation.RData')

# Just try the first elements of imp_list to start
# for (i in 1:length(imp_list[["dataset"]])){
# 	imp_list[i] <- imp_list[i][1]
# 	}

# Just work from the 5th element (1-4 have already been done, which took 50 hours!  So this time have asked for 200...)
for (i in 1:length(imp_list[["dataset"]])){
	# Change to generic names
	assign("dataset",get(imp_list[["dataset"]][i]))
	assign("maxit",imp_list[["maxit"]][i])
	assign("pred_mat",get(paste0("pred_mat",imp_list[["meth_pred_mat"]][i])))
	assign("meth",get(paste0("meth",imp_list[["meth_pred_mat"]][i])))

	#specify nr nodes used
	cores_2_use <- 15

	#Do not change next four lines
	cl <- makeCluster(cores_2_use)
	clusterSetRNGStream(cl, 9956)
	clusterExport(cl,list('nimp',
		'dataset',
		'maxit',
		'pred_mat',
		'meth'
		))
	clusterEvalQ(cl, library(mice))

	Sys.time()
	#Imp pars runs imputations in parallel
	imp_pars <-
	  parLapply(cl = cl, X = 1:cores_2_use, fun = function(no){
	    mice(dataset, m = nimp, maxit=maxit, 
	    	printFlag = TRUE,method=meth,predictorMatrix=pred_mat,seed=136454,nnet.MaxNWts=2000)
	  })
	stopCluster(cl)
	Sys.time()

	#Put imputations back together in same dataset
	.Random.seed=136454
	assign(as.character(imp_list[["label_out"]][i]), imp_pars[[1]])
	for (n in 2:length(imp_pars)){
	    assign(as.character(imp_list[["label_out"]][i]),
	    ibind(get(as.character(imp_list[["label_out"]][i])),
	          imp_pars[[n]]))
		}

	#Plots to asses convergence (cannot do this after creating object using as.mids)
	assign("mids",get(as.character(imp_list[["label_out"]][i])))
	pdf(file=paste0("check_",imp_list[["label_out"]][i],".pdf"))
	print(plot(mids))
	dev.off()

	#Save individual file as session may not last long enough to do all 14 (7 per sex)
    assign("temp",get(as.character(imp_list[["label_out"]][i])))
    save(temp,file=paste0(imp_list[["label_out"]][i],".RData"))
	}

save(get(imp_list[["label_out"]]),file='data_for_analysis.RData')

Sys.time()