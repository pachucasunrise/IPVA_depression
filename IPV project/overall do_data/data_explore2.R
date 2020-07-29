#28.05.19
#Working with the ALSPAC package that Gib et al have developed (https://github.com/explodecomputer/alspac)

install.packages("devtools")
install.packages("Rcpp")

#Looks like we need the library before install_github will work
library(devtools)
install_github("explodecomputer/alspac")

library(Rcpp)
library(alspac)
data(current)
data(useful)

#Pull variables I want (where WoC has occurred implicitly)
varnames <- c(#Core variables
              #Mother-based
              "aln","mz001","mz010a", "mz013", "mz014", "mz028b", 
              "a006", "a525", "b032", "b650", "b663", "b667",
              "c645a", "c755", "c765", "c800", "c804", "bestgest",
              
              #Child-based
              "qlet", "kz011b", "kz021", "kz030", "in_core", "in_alsp", "in_phase2", "in_phase3", "in_phase4", "tripquad",
              
              #Age at completion of YPA
              "YPA9020", 
              
              #Romantic relations in TF1
              "ff5787", "ff5791", "ff5809", "ff5813", "ff5829", "ff5833", 
              "ff5849", "ff5853", "ff5868", "ff5872", "ff5889", "ff5893", 
              "ff5889", "ff5893", "ff5914", "ff5918",
              
              #Direct romantic relationship question in TF2
              "fg4190",
              
              #Romantic relations in TF3
              "fh8931", "fh8932", "fh8952", "fh8953", "fh8973", "fh8974", 
              "fh9013", "fh9014", "fh9075", "fh9076", "fh9116", "fh9118",
              
              #Friendship questions in CCXB
              "ccxb171", "ccxb271", "ccxb371", "ccxb471", "ccxb571",
              
              #Help from who when hurting themselves in CCS
              "ccs6582c",
              
              #Romantic relationship questions (including direct Q) in TF4
              "FJPC1000", "FJPC1100", "FJLE160", "FJLE162",
              
              #Who viewing current social media in CCXC
              "CCXC326", "CCXC338",
              
              #Who do you live with in CCU
              "CCU4032", "CCU4033", "CCU4035", "CCU2085i",
              
              #Reason for having sex in YPA
              "YPA3042", "YPA3041",
              
              #Sexuality question in TF3
              "fh9140",
              
              #Depression at 16 in CCS
              "ccs4500", "ccs4502", "ccs4503", "ccs4504", "ccs4505", "ccs4506", "ccs4508", "ccs4509", "ccs4511", "ccs4512", "ccs4513", "ccs4514", "ccs4515",
              
              #Depression at 18 in CCT
              "cct2700", "cct2701", "cct2702", "cct2703", "cct2704", "cct2705", "cct2706", "cct2707", "cct2708", "cct2709", "cct2710", "cct2711", "cct2712",
              
              #Anxiety at 15 in TF3
              "fh6893", "fh6894",
              
              #ASB at 15 in TF3
              #These are wrong and need replacing
              "fh9573", "fh9577", "fh9579", "fh9581", "fh9583", "fh9587",

              #ASB at 18 in CCT
              "cct6000", "cct6001", "cct6002", "cct6003", "cct6004", "cct6005", "cct6006", "cct6008", "cct6009", "cct6010", "cct6011",
              
              #RSB in TF1, TF2, TF3, and TF4
              "ff5900", "ff5907", "ff5908", "ff5909", "ff5910", "ff5911", "ff5912",
              "fg5320", "fg5328", "fg5329", "fg5330", "fg5331", "fg5332",
              "fh9100", "fh9109", "fh9110",
              "FJCH700", "FJCH707")

#Certain IPVA variables from YPA, including impact
varnames2 <- findVars("YPA5", logic="any", whole.word=FALSE, ignore.case=TRUE)
varnames <- c(varnames, varnames2)

#ACE variables
varnames2 <- findVars("_0_16yrs", logic="any", whole.word=FALSE, ignore.case=TRUE)
varnames <- c(varnames, varnames2)

#Copying from online...
#Not sure why they don't go straight the subset
vars <- findVars(varnames)
vars <- subset(vars, subset=tolower(name) %in% varnames)

#"For example, searching for variables "kz021", "kz011b" and "c645a" will return multiple variables with the same name."
#I then require that the "kz021" variable come from a STATA file name starting with "kz" ("obj" column in vars), 
#"kz011b" comes from a file name starting with "cp" and the description of the variable ("lab" column in vars) include the word "Participant", 
#and "c645a" comes from a questionnaire ("cat2" column in vars)."
varnames2 <- filterVars(varnames,
                        kz021=c(obj="^kz"),
                        kz011b=c(obj="^cp", lab="Participant"),
                        c645a=c(cat2="Quest")) 

#Extract these variables
results <- extractVars(vars)