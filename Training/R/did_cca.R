################################################################################
## Project: MH outcomes following IPVA
## Script purpose: Function to run difference-in-differences analysis on complete 
## cases and put output in table
## Date: 27th July 2020
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################


#source("./useful-code-r/code/functions/match-lab.r")
did_cca <- function(out,level,model,data_long_boys,data_long_girls,digits){
	didreg_boys = lm(as.formula(model), data = data_long_boys)
	summary(didreg_boys)
	out[2+(level*3),3] <- nobs(didreg_boys)/2
	out[2+(level*3),4] <- round(summary(didreg_boys)$coef[2,1],digits)
	out[2+(level*3),5] <- round(summary(didreg_boys)$coef[2,1]-1.96*summary(didreg_boys)$coef[2,2],digits)
	out[2+(level*3),6] <- round(summary(didreg_boys)$coef[2,1]+1.96*summary(didreg_boys)$coef[2,2],digits)
	out[2+(level*3),7] <- round(summary(didreg_boys)$coef[2,4],5)

	out[3+(level*3),4] <- round(summary(didreg_boys)$coef[3,1],digits)
	out[3+(level*3),5] <- round(summary(didreg_boys)$coef[3,1]-1.96*summary(didreg_boys)$coef[3,2],digits)
	out[3+(level*3),6] <- round(summary(didreg_boys)$coef[3,1]+1.96*summary(didreg_boys)$coef[3,2],digits)
	out[3+(level*3),7] <- round(summary(didreg_boys)$coef[3,4],5)

	out[4+(level*3),4] <- round(summary(didreg_boys)$coef[4,1],digits)
	out[4+(level*3),5] <- round(summary(didreg_boys)$coef[4,1]-1.96*summary(didreg_boys)$coef[4,2],digits)
	out[4+(level*3),6] <- round(summary(didreg_boys)$coef[4,1]+1.96*summary(didreg_boys)$coef[4,2],digits)
	out[4+(level*3),7] <- round(summary(didreg_boys)$coef[4,4],digits)

	didreg_girls = lm(as.formula(model), data = data_long_girls)
	summary(didreg_girls)
	out[2+(level*3),8] <- nobs(didreg_girls)/2
	out[2+(level*3),9] <- round(summary(didreg_girls)$coef[2,1],digits)
	out[2+(level*3),10] <- round(summary(didreg_girls)$coef[2,1]-1.96*summary(didreg_girls)$coef[2,2],digits)
	out[2+(level*3),11] <- round(summary(didreg_girls)$coef[2,1]+1.96*summary(didreg_girls)$coef[2,2],digits)
	out[2+(level*3),12] <- round(summary(didreg_girls)$coef[2,4],digits)

	out[3+(level*3),9] <- round(summary(didreg_girls)$coef[3,1],digits)
	out[3+(level*3),10] <- round(summary(didreg_girls)$coef[3,1]-1.96*summary(didreg_girls)$coef[3,2],digits)
	out[3+(level*3),11] <- round(summary(didreg_girls)$coef[3,1]+1.96*summary(didreg_girls)$coef[3,2],digits)
	out[3+(level*3),12] <- round(summary(didreg_girls)$coef[3,4],5)

	out[4+(level*3),9] <- round(summary(didreg_girls)$coef[4,1],digits)
	out[4+(level*3),10] <- round(summary(didreg_girls)$coef[4,1]-1.96*summary(didreg_girls)$coef[4,2],digits)
	out[4+(level*3),11] <- round(summary(didreg_girls)$coef[4,1]+1.96*summary(didreg_girls)$coef[4,2],digits)
	out[4+(level*3),12] <- round(summary(didreg_girls)$coef[4,4],5)
	level <- level+1
	out
	}