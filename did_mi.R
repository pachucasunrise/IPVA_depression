################################################################################
## Project: MH outcomes following IPVA
## Script purpose: Function to run difference-in-differences analysis on complete 
## cases and put output in table
## Date: 27th July 2020
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################


#source("./useful-code-r/code/functions/match-lab.r")
did_mi <- function(out,level,model,imp_long_boys,imp_long_girls,digits){
	didreg_boys = pool(with(imp_long_boys, exp = lm(as.formula(model))))
	out[2+(level*4),3] <- (dim(imp_long_boys$data)[1])/2
	out[2+(level*4),4] <- round(summary(didreg_boys)[1,2],digits)
	out[2+(level*4),5] <- round(summary(didreg_boys)[1,3],digits)
	out[2+(level*4),6] <- round(summary(didreg_boys)[1,6],5)

	out[3+(level*4),4] <- round(summary(didreg_boys)[2,2],digits)
	out[3+(level*4),5] <- round(summary(didreg_boys)[2,3],digits)
	out[3+(level*4),6] <- round(summary(didreg_boys)[2,6],5)

	out[4+(level*4),4] <- round(summary(didreg_boys)[3,2],2)
	out[4+(level*4),5] <- round(summary(didreg_boys)[3,3],digits)
	out[4+(level*4),6] <- round(summary(didreg_boys)[3,6],5)

	out[5+(level*4),4] <- round(summary(didreg_boys)[4,2],digits)
	out[5+(level*4),5] <- round(summary(didreg_boys)[4,3],digits)
	out[5+(level*4),6] <- round(summary(didreg_boys)[4,6],5)

	didreg_girls = pool(with(imp_long_girls, exp = lm(as.formula(model))))
	out[2+(level*4),7] <- (dim(imp_long_girls$data)[1])/2
	out[2+(level*4),8] <- round(summary(didreg_girls)[1,2],digits)
	out[2+(level*4),9] <- round(summary(didreg_girls)[1,3],digits)
	out[2+(level*4),10] <- round(summary(didreg_girls)[1,6],5)

	out[3+(level*4),8] <- round(summary(didreg_girls)[2,2],digits)
	out[3+(level*4),9] <- round(summary(didreg_girls)[2,3],digits)
	out[3+(level*4),10] <- round(summary(didreg_girls)[2,6],5)

	out[4+(level*4),8] <- round(summary(didreg_girls)[3,2],digits)
	out[4+(level*4),9] <- round(summary(didreg_girls)[3,3],digits)
	out[4+(level*4),10] <- round(summary(didreg_girls)[3,6],5)

	out[5+(level*4),8] <- round(summary(didreg_girls)[4,2],digits)
	out[5+(level*4),9] <- round(summary(didreg_girls)[4,3],digits)
	out[5+(level*4),10] <- round(summary(didreg_girls)[4,6],5)
	out
	}