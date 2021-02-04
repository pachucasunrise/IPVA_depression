################################################################################
## Project: MH outcomes following IPVA
## Script purpose: Function to estimate and tabulate propensity scores, 
## IPTW coefficients and SEs (when outcome numerical or binary), 
## Kolmogorov-Smirnoff test for Normality of residuals,
## as well as plot histogram of residuals
## Date: 27th July 2020
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################

iptw_mi <- function(outcome,exposure,covariates,imp_boys_sub,imp_girls_sub){
	header <- c("imputation",rep(c("intercept","SE","intercept p-value","coeff for vic","SE","Coef p-value","KS p-value",
					"min(ps|tr=0)","25pc(ps|tr=0)",
					"50pc(ps|tr=0)","75pc(ps|tr=0)","max(ps|tr=0)",
					"min(ps|tr=1)","25pc(ps|tr=1)",
					"50pc(ps|tr=1)","75(ps|tr=1)","max(ps|tr=1)"),2),"n_boys","n_girls")
	out <- data.frame(matrix(NA, nrow = (nimp+1), ncol = length(header)))
	colnames(out) <- c("","Men",rep("",13),"Women",rep("",13),"","")
	out[1,] <- header
	covs <- covariates[1]
	for (c in 2:length(covariates) ) {
	covs <- paste0(covs," + ",covariates[c])
		}
	model1 <- paste0(exposure," ~ ",covs)
	model2 <- paste0(exposure," ~ 1")
	model3 <- paste0(outcome," ~ ",exposure)
	#Make sure K-S columns are 0's to start with
	out[2:(nimp+1),c(8,25)] <- 0

	for (i in 1:nimp){
		a <- complete(imp_boys_sub, i)
		b <- glm(as.formula(model1), data=a, family = binomial(link = "logit"))
		a[,"ps"] <- predict(b,a,type="response")
		a[,"wgt.ATE"] <- ifelse(
	 		a[,exposure] == 1, 1/a$ps,
	 		1/(1-a$ps))
		b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
		a[,"ps_int"] <- predict(b,a,type="response")
		a[,"wgt.ATE_stab"] <- ifelse(
			a[,exposure] == 1, a$ps_int/a$ps,
	 		(1-a$ps_int)/(1-a$ps))
		ifelse(class(imp_boys_sub$data[,outcome])=="numeric",
			fit_ipw <- glm(as.formula(model3), data=a, family = "gaussian", weight = (wgt.ATE_stab)),
			fit_ipw <- glm(as.formula(model3), data=a, family = binomial(link = "logit"), weight = (wgt.ATE_stab))
			)
		out[i+1,1] <- i
		out[i+1,2] <- fit_ipw$coef[1]
		out[i+1,3] <- summary(fit_ipw)$coef[1,2]
		out[i+1,4] <- summary(fit_ipw)$coef[1,4]
		out[i+1,5] <- fit_ipw$coef[2]
		out[i+1,6] <- summary(fit_ipw)$coef[2,2]
		out[i+1,7] <- summary(fit_ipw)$coef[2,4]
		if(class(imp_boys_sub$data[,outcome])=="numeric"){
			fit_ipw <- lm(as.formula(model3), data=a, weight = (wgt.ATE_stab))
			#out[i+1,8] <- ols_test_normality(fit_ipw)$kolmogorv$p.value
			a$res <- resid(fit_ipw)
			tiff(file=paste0(outcome,"_iptw_mi_res_density_by_trt_boys.tiff"))
			# a %>% ggplot(aes(x=res, fill=vic_1821)) +
		 	#    geom_density(color="#e9ecef", alpha=0.6, 
		 	#    position = 'identity')
		 	hist(a$res)
			dev.off()
		}else{
			out[i+1,8] <- NA
			}
		out[i+1,9:13] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
		out[i+1,14:18] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
		out[i+1,36] <- stats::nobs(fit_ipw)
		assign(paste0("imp_boys_",i), a)

		a <- complete(imp_girls_sub, i)
		b <- glm(as.formula(model1), data=a, family = binomial(link = "logit"))
		a[,"ps"] <- predict(b,a,type="response")
		a[,"wgt.ATE"] <- ifelse(
	 		a[,exposure] == 1, 1/a$ps,
	 		1/(1-a$ps))
		b <- glm(as.formula(model2), data=a, family = binomial(link = "logit"))
		a[,"ps_int"] <- predict(b,a,type="response")
		a[,"wgt.ATE_stab"] <- ifelse(
			a[,exposure] == 1, a$ps_int/a$ps,
	 		(1-a$ps_int)/(1-a$ps))
		ifelse(class(imp_girls_sub$data[,outcome])=="numeric",
			fit_ipw <- glm(as.formula(model3), data=a, family = "gaussian", weight = (wgt.ATE_stab)),
			fit_ipw <- glm(as.formula(model3), data=a, family = binomial(link = "logit"), weight = (wgt.ATE_stab))
			)
		out[i+1,19] <- fit_ipw$coef[1]
		out[i+1,20] <- summary(fit_ipw)$coef[1,2]
		out[i+1,21] <- summary(fit_ipw)$coef[1,4]
		out[i+1,22] <- fit_ipw$coef[2]
		out[i+1,23] <- summary(fit_ipw)$coef[2,2]
		out[i+1,24] <- summary(fit_ipw)$coef[2,4]
		if(class(imp_girls_sub$data[,outcome])=="numeric"){
			fit_ipw <- lm(as.formula(model3), data=a, weight = (wgt.ATE_stab))
			#out[i+1,25] <- ols_test_normality(fit_ipw)$kolmogorv$p.value
			a$res <- resid(fit_ipw)
			tiff(file=paste0(outcome,"_iptw_mi_res_density_by_trt_girls.tiff"))
			# a %>% ggplot(aes(x=res, fill=vic_1821)) +
		 	#    geom_density(color="#e9ecef", alpha=0.6, 
		 	#    position = 'identity')
		 	hist(a$res)
			dev.off()
		}else{
			out[i+1,25] <- NA
			}
		out[i+1,26:30] <- summary(a$ps[a$vic_1821 == 0])[c(1,2,3,5,6)]
		out[i+1,31:35] <- summary(a$ps[a$vic_1821 == 1])[c(1,2,3,5,6)]
		out[i+1,37] <- stats::nobs(fit_ipw)

		assign(paste0("imp_girls_",i), a)
		}

	meanrow <- rep(NA,length(header))
	meanrow[1] <- "Pooled"
	#Skipping out p-values as we want medians of these
	for(i in c(2:3,5:6,9:length(header))){
		meanrow[i] <- mean(as.numeric(out[2:(nimp+1),i]))
		}
	#p-values
	for(i in c(4,7,8:length(header))){
		meanrow[i] <- median(as.numeric(out[2:(nimp+1),i]))
		}
	
	#Need to replace SE values
	#Boys
	se_sq <- as.numeric(out[2:(nimp+1),3])^2
	vw <- (sum(se_sq))/nimp
	a <- complete(imp_boys_sub, 1)
	vb <- sqrt(sum((as.numeric(out[2:(nimp+1),2])-mean(as.numeric(out[2:(nimp+1),3])))^2)/(dim(a)[1]-1))
	se_pooled <- sqrt(vw+vb+(vb/nimp))
	meanrow[3] <- se_pooled

	se_sq <- as.numeric(out[2:(nimp+1),6])^2
	vw <- (sum(se_sq))/nimp
	a <- complete(imp_boys_sub, 1)
	vb <- sqrt(sum((as.numeric(out[2:(nimp+1),5])-mean(as.numeric(out[2:(nimp+1),5])))^2)/(dim(a)[1]-1))
	se_pooled <- sqrt(vw+vb+(vb/nimp))
	meanrow[6] <- se_pooled

	#Girls
	se_sq <- as.numeric(out[2:(nimp+1),20])^2
	vw <- (sum(se_sq))/nimp
	a <- complete(imp_girls_sub, 1)
	vb <- sqrt(sum((as.numeric(out[2:(nimp+1),19])-mean(as.numeric(out[2:(nimp+1),19])))^2)/(dim(a)[1]-1))
	se_pooled <- sqrt(vw+vb+(vb/nimp))
	meanrow[17] <- se_pooled

	se_sq <- as.numeric(out[2:(nimp+1),23])^2
	vw <- (sum(se_sq))/nimp
	a <- complete(imp_girls_sub, 1)
	vb <- sqrt(sum((as.numeric(out[2:(nimp+1),22])-mean(as.numeric(out[2:(nimp+1),22])))^2)/(dim(a)[1]-1))
	se_pooled <- sqrt(vw+vb+(vb/nimp))
	meanrow[23] <- se_pooled

	out <- rbind(out, meanrow)
	out
	}