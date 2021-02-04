################################################################################
## Project: MH outcomes following IPVA
## Script purpose: Function to run difference-in-differences analysis on complete 
## cases and put output in table
## Date: 27th July 2020
## Author: Annie Herbert
## Email: annie.herbert@bristol.ac.uk
################################################################################


#source("./useful-code-r/code/functions/match-lab.r")
did_mi2 <- function(model,data_boys,data_girls,digits){
	table <- data.frame(matrix(NA, nrow = 4, ncol = 9))
	colnames(table) <- c("Variable","n_boys","coef_boys","SE_boys","p_boys",
										"n_girls","coef_girls","SE_girls","p_girls")
	table[,1] <- c("Intercept","Vic","Time","Vic*Time")
	didreg_boys = pool(with(data_boys, exp = lm(as.formula(model))))
	table[1,2] <- (dim(data_boys$data)[1])/2
	table[1,3] <- round(summary(didreg_boys)[1,2],digits)
	table[1,4] <- round(summary(didreg_boys)[1,3],digits)
	table[1,5] <- round(summary(didreg_boys)[1,6],5)

	table[2,3] <- round(summary(didreg_boys)[2,2],digits)
	table[2,4] <- round(summary(didreg_boys)[2,3],digits)
	table[2,5] <- round(summary(didreg_boys)[2,6],5)

	table[3,3] <- round(summary(didreg_boys)[3,2],2)
	table[3,4] <- round(summary(didreg_boys)[3,3],digits)
	table[3,5] <- round(summary(didreg_boys)[3,6],5)

	table[4,3] <- round(summary(didreg_boys)[4,2],digits)
	table[4,4] <- round(summary(didreg_boys)[4,3],digits)
	table[4,5] <- round(summary(didreg_boys)[4,6],5)

	didreg_girls = pool(with(data_girls, exp = lm(as.formula(model))))
	table[1,6] <- (dim(data_girls$data)[1])/2
	table[1,7] <- round(summary(didreg_girls)[1,2],digits)
	table[1,8] <- round(summary(didreg_girls)[1,3],digits)
	table[1,9] <- round(summary(didreg_girls)[1,6],5)

	table[2,7] <- round(summary(didreg_girls)[2,2],digits)
	table[2,8] <- round(summary(didreg_girls)[2,3],digits)
	table[2,9] <- round(summary(didreg_girls)[2,6],5)

	table[3,7] <- round(summary(didreg_girls)[3,2],digits)
	table[3,8] <- round(summary(didreg_girls)[3,3],digits)
	table[3,9] <- round(summary(didreg_girls)[3,6],5)

	table[4,7] <- round(summary(didreg_girls)[4,2],digits)
	table[4,8] <- round(summary(didreg_girls)[4,3],digits)
	table[4,9] <- round(summary(didreg_girls)[4,6],5)
	table
	}