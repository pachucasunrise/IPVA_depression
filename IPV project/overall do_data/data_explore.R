#Annie Herbert, UoB
#14.05.19
#Re-creating Stata code used to derive IPV cohort, 
#and produce some preliminary tables re: risk factors for relationships and IPV

#Clear
install.packages("mise")
library(mise)
mise()

#Set working directory
#Note '/'s instead of '\'s
setwd("//ads.bris.ac.uk/folders/Health Sciences/SSCM ALSPAC/Data")

#Global variables
temp_path <- "C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/IPV project/p1 - risk factors/data/temp/"
data_path <- "C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/IPV project/p1 - risk factors/data/"
logs_path <- "C:/Users/is19262/OneDrive - University of Bristol/MyFiles-Migrated/IPV project/p1 - risk factors/logs/"

childc_path <- "Current/Clinic/Child/"
childq_path <- "Current/Quest/Child Completed/"
extra_path <- "Useful_data/adverse childhood experiences/"

#Libraries
#For calling Stata data
install.packages("readstata13")
library(readstata13)
library(foreign)

install.packages("tidyverse")
library(haven)

#For running Stata code in RMarkdown
install.packages("rmarkdown")

install.packages("devtools")
library(devtools)
install_github("hemken/Statamarkdown")

install.packages("assertthat")



####################################################################################
#Locate where Stata is
```{r, echo=FALSE, message=FALSE}
library(Statamarkdown)
###ISSUE IS HERE!!!###
```

```{stata}

-- Stata code here --
  
```

####################################################################################
#LocaLs (since file names are subject to change)
local_data_a <- "a_3e"
local_data_b <- "b_4f"
local_data_c <- "c_8a"
local_data_mz <- "mz_5a"
local_data_kz <- "kz_5c"
local_data_cp <- "cp_2b"


####################################################################################
#Mother questionnaire files - in this section the following files need to be placed:
#Mother completed Qs about herself
#Maternal grandparents social class
#Partner_proxy social class

#ALWAYS KEEP THIS SECTION EVEN IF ONLY MOTHER CLINIC REQUESTED
library(haven)
#data_mz will be 'atomic', i.e., a collection of vectors that, within the vector, all have the same mode
data_mz <- read_dta(file=paste('Current/Other/Sample Definition/',local_data_mz,'.dta',sep=""))

#The warnings are to do with variable formats
#Just to check:
View(data_mz)
data_mz$in_mz <- 1

#Other data that need to be merged on:
data_a <- read_dta(file=paste('Current/Quest/Mother/',local_data_a,'.dta',sep = ""))
data_b <- read_dta(file=paste('Current/Quest/Mother/',local_data_b,'.dta',sep = ""))
data_c <- read_dta(file=paste('Current/Quest/Mother/',local_data_c,'.dta',sep = ""))
data_bestgest <- read_dta("Useful_data/bestgest/bestgest.dta")

mydata <- merge(data_mz, data_a, by="aln")
mydata <- merge(mydata, data_b, by="aln")
mydata <- merge(mydata, data_c, by="aln")
mydata <- merge(mydata, data_bestgest, by="aln")

#Can change the order here (want aln and mz001 first so we can ignore them)
myvars <- c("aln", "mz001", "mz010a", "mz013", "mz014", "mz028b", "a006", "a525", "b032", "b650", "b663", "b665", "b667",
            "c645a", "c755", "c765", "c800", "c801", "c802", "c803", "c804", "bestgest")
mydata <- mydata[myvars]

#Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before bestgest, so replace the *** line above with additional variables. 
#If none are required remember to delete the *** line.
#An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file

#Script to set values, for mothers who have withdrawn consent, to missing
###Would be best to run THE Stata file here, as will often be updated to include extra IDs (alns), as will the withdrawal of consent of participants
#run "Withdrawal of consent\mother_quest_WoC.R"

#IDs of mothers
mother_ex <- c(31075, 32812, 42568, 35366, 51798, 38666, 30484, 53371, 37355, 40755, 52237, 36828, 46659, 46312, 52177, 50491)
#Variables to set to missing
myvars_ex <- myvars[3:length(myvars)]

mydata[mydata$aln %in% mother_ex, myvars_ex]  <- NA

#Check withdrawal of consent frequencies mum quest=12
#Frequencies of pregnancy size
table(mydata$mz010a, useNA = "always")
#11813 single pregnancies, 146 twin pregnancies, 8 NA.  We're after 11 withdrawals...

#Now save
write.table(mydata, file=paste(temp_path,'motherQ.txt'), sep="\t")


####################################################################################
#Child BASED files - in this section the following files need to be placed:
#Mother completed Qs about YP
#Obstetrics file OA

#ALWAYS KEEP THIS SECTION EVEN IF ONLY CHILD COMPLETED REQUESTED, although you will need to remove the *****
library(haven)
data_kz <- read_dta(file=paste('Current/Other/Sample Definition/',local_data_kz,'.dta',sep=""))
in_kz<-1
data_cp <- read_dta(file=paste('Current/Other/cohort profile/',local_data_cp,'.dta',sep = ""))
#Suffixes stops .x from appearing as column name in merged matrix
mydata <- merge(data_kz, data_cp, by="aln", suffixes = c("", ".y"))

#Again will just change order here (putting aln, qlet, in_alsp, and tripquad first)
myvars <- c("aln", "qlet", "in_alsp", "tripquad", "kz021", "kz011b", "kz030", "in_core", "in_phase2", "in_phase3", 
            "in_phase4")
mydata <- mydata[myvars]

#Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before in_core, so replace the ***** line with additional variables.
#If none are required remember to delete the ***** line. ---> Done
#An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file

#do "Syntax\Withdrawal of consent\child_based_WoC.do"

#IDs of children
child_ex <- c(31075, 32812, 42568, 35366, 51798, 38666, 30484, 53371, 37355, 40755, 52237, 36828, 46659, 46312, 52177, 50491)
#Variables to set to missing
myvars_ex <- myvars[5:length(myvars)]

mydata[mydata$aln %in% child_ex, myvars_ex]  <- NA

table(mydata$kz021, useNA = "always")
#10166 single pregnancies, 9474 twin pregnancies, 22 NA.

#Now save
write.table(mydata, file=paste(temp_path,'childB.txt'), sep="\t")


####################################################################################
#Child COMPLETED files - in this section the following files need to be placed:
#YP completed Qs
#Puberty Qs
#Child clinic data
#Child biosamples data
#School Qs
#Obstetrics file OC

#If there are no child completed files, this section can be starred out.
#NOTE: having to keep kz021 tripquad just to make the withdrawal of consent work - these are dropped for this file as the ones in the child BASED file are the important ones and should take priority
mydata <- merge(data_kz, data_cp, by = "aln", suffixes = c("", ".y"))
myvars <- c("aln", "qlet", "kz021", "tripquad")
mydata <- mydata[myvars]

#Dealing with withdrawal of consent: For this to work additional variables required have to be inserted before in_core, so replace the ***** line with additional variables.
#If none are required remember to delete the ***** line. ---> Done
#An additional do file is called in to set those withdrawing consent to missing so that this is always up to date whenever you run this do file

#do "Syntax\Withdrawal of consent\child_completed_WoC.do"

#IDs of children
child_ex <- c(32778, 32803, 39444, 53228, 54074, 34657, 32230, 47080, 36194, 34688, 30006, 47430, 38685, 47501,
              46814, 38666, 39400, 53032, 50035, 30484, 38831, 36828, 46659)

#Variables to set to missing
myvars_ex <- myvars[3:length(myvars)]

mydata[mydata$aln %in% child_ex, myvars_ex]  <- NA

#Check withdrawal of consent frequencies child completed=21
table(mydata$kz021, useNA = "always")

#drop kz021 tripquad (but I'm not sure why?)
#mydata <- mydata[,-c("kz021","tripquad")]

#Now save
write.table(mydata, file=paste(temp_path,'childC.txt'), sep ="\t")


####################################################################################
##Matching all data together and saving out the final file*.
#NOTE: any linkage data should be added here.

childB <- read.table(file=paste(temp_path,'childB.txt'))
childC <- read.table(file=paste(temp_path,'childC.txt'))
motherQ <- read.table(file=paste(temp_path,'motherQ.txt'))

mydata <- merge(childB, childC, by = c("aln","qlet"), suffixes = c("", ".y"))
mydata <- merge(mydata, motherQ, by = c("aln"), suffixes = c("", ".y"))

#Checking where the NAs are
View(mydata[which(is.na(mydata$kz021.y==TRUE)),])

#IF mother clinic data is required please unstar the following line
#merge m:1 aln using "YOUR PATHWAY\motherC.dta", nogen */
#IF partner data is required please unstar the following line
#merge m:1 aln using "YOUR PATHWAY\partner.dta", nogen */
  
#Remove non-alspac children.
mydata <- mydata[which(mydata$in_alsp==1),]
#Remove trips and quads.
mydata <- mydata[which(mydata$tripquad!=1),]
#Appears no tripquads dropped.

#Now save
write.table(mydata, file=paste(data_path,'starting_cohort.txt'), sep ="\t")


############################################################################
#Check that there are 15645 records.
nrow(mydata)
#13003 :-/
nrow(mydata[which(is.na(mydata$kz021)==TRUE),])
#27, but n should be 13...


############################################################################
#Re-set wd before exiting, so history, etc. save in local files
setwd(logs_path)










