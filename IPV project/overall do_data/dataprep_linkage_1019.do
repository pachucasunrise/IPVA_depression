********************************************************************************
** Project: RFs for IPVA
** Script purpose: Prepping dataset for ALSPAC team so can link on education and deprivation
** Date: 22.08.19
** Author: Annie Herbert
** Email: a.herbert@bristol.ac.uk
********************************************************************************

********************************************************************************
* Set locals, etc.  
********************************************************************************
clear
clear matrix
clear mata
set maxvar 32767

cd "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data"
local temp_path "O:\IPV project\P1 - risk factors\Data\Temp"
local data_path "O:\IPV project\P1 - risk factors\Data"
local childc_path "Current\Clinic\Child"
local childq_path "Current\Quest\Child Completed"
local ace_path "W:\data\ACE data\Annie"

*As versions of datasets change...
*Core sets
local data_mz "mz_5a"
local data_a "a_3e"
local data_b "b_4f"
local data_c "c_8a"
local data_kz "kz_5b"
local data_cp "cp_2b"

*Postal questionnaires
local data_ccq "`childq_path'\ccq_r1c"
local data_ccs "`childq_path'\ccs_r1b"
local data_cct "`childq_path'\cct_1b"
local data_ccxb "`childq_path'\ccxb_r1a"
local data_ccxc "`childq_path'\ccxc_r2a"
local data_ccu "`childq_path'\ccu_2b"
local data_YPA "`childq_path'\YPA_r1a"
local data_YPC "`childq_path'\YPC_2a"

*Clinics
local data_tf1 "`childc_path'\tf1_3b"
local data_tf2 "`childc_path'\tf2_4a"
local data_tf3 "`childc_path'\tf3_4c"
local data_tf4 "`childc_path'\tf4_4b"


********************************************************************************
* Now put all the core files together  
********************************************************************************
* Mother questionnaire files - in this section the following files need to be placed:
* Mother completed Qs about herself
* Maternal grandparents social class
* Partner_proxy social class

* ALWAYS KEEP THIS SECTION EVEN IF ONLY MOTHER CLINIC REQUESTED
	
use "Current\Other\Sample Definition/`data_mz'.dta", clear
sort aln
gen in_mz=1
merge 1:1 aln using "Current\Quest\Mother/`data_a'.dta", nogen
merge 1:1 aln using "Current\Quest\Mother/`data_b'.dta", nogen
merge 1:1 aln using "Current\Quest\Mother/`data_c'.dta", nogen
merge 1:1 aln using "Useful_data\bestgest\bestgest.dta", nogen

keep aln mz001 mz010a mz013 mz014 mz028b ///
a006 a525 ///
b032 b650 b663 - b667 ///
c645a c755 c765 c800 - c804 ///
bestgest

* Dealing with withdrawal of consent: For this to work additional variables required have 
* to be inserted before bestgest, so replace the *** line above with additional variables. 
* If none are required remember to delete the *** line.
* An additional do file is called in to set those withdrawing consent to missing so that 
* this is always up to date whenever you run this do file

order aln mz010a, first
order bestgest, last

do "Syntax\Withdrawal of consent\mother_quest_WoC.do"

* Check withdrawal of consent frequencies mum quest=11
tab1 mz010a, mis

save "`temp_path'\motherQ.dta", replace


* Child BASED files - in this section the following files need to be placed:
* Mother completed Qs about YP
* Obstetrics file OA

* ALWAYS KEEP THIS SECTION EVEN IF ONLY CHILD COMPLETED REQUESTED, although you will need 
* to remove the *****

use "Current\Other\Sample Definition/`data_kz'.dta", clear
sort aln qlet
gen in_kz=1
merge 1:1 aln qlet using "Current\Other\cohort profile/`data_cp'.dta", nogen

keep aln qlet kz011b kz021 kz030 ///
in_core in_alsp in_phase2 in_phase3 in_phase4 tripquad

* Dealing with withdrawal of consent: For this to work additional variables required have 
* to be inserted before in_core, so replace the ***** line with additional variables.
* If none are required remember to delete the ***** line. ---> Done
* An additional do file is called in to set those withdrawing consent to missing so that this 
* is always up to date whenever you run this do file

order aln qlet kz021, first
order in_alsp tripquad, last

do "Syntax\Withdrawal of consent\child_based_WoC.do"

* Check withdrawal of consent frequencies child based=13 (two mums of twins have withdrawn consent)
tab1 kz021, mis

save "`temp_path'\childB.dta", replace


* Child COMPLETED files - in this section the following files need to be placed:
* YP completed Qs
* Puberty Qs
* Child clinic data
* Child biosamples data
* School Qs
* Obstetrics file OC

* If there are no child completed files, this section can be starred out.
* NOTE: having to keep kz021 tripquad just to make the withdrawal of consent work - 
* these are dropped for this file as the ones in the child BASED file are the important ones 
* and should take priority

use "Current\Other\Sample Definition/`data_kz'.dta", clear
sort aln qlet
merge 1:1 aln qlet using "Current\Other\cohort profile/`data_cp'.dta", nogen

keep aln qlet kz021 ///
tripquad

* Dealing with withdrawal of consent: For this to work additional variables required have 
* to be inserted before in_core, so replace the ***** line with additional variables.
* If none are required remember to delete the ***** line. ---> Done
* An additional do file is called in to set those withdrawing consent to missing so that this 
* is always up to date whenever you run this do file

order aln qlet kz021, first
order tripquad, last

do "Syntax\Withdrawal of consent\child_completed_WoC.do"

* Check withdrawal of consent frequencies child completed=21
tab1 kz021, mis

drop kz021 tripquad
save "`temp_path'\childC.dta", replace


** Matching all data together and saving out the final file*.
* NOTE: any linkage data should be added here*.

use "`temp_path'\childB.dta", clear
merge 1:1 aln qlet using "`temp_path'\childC.dta", nogen
merge m:1 aln using "`temp_path'\motherQ.dta", nogen
* IF mother clinic data is required please unstar the following line
/* merge m:1 aln using "YOUR PATHWAY\motherC.dta", nogen */
* IF partner data is required please unstar the following line
/* merge m:1 aln using "YOUR PATHWAY\partner.dta", nogen */

* Remove non-alspac children.
drop if in_alsp!=1.

* Remove trips and quads.
drop if tripquad==1

drop in_alsp tripquad
save "`data_path'\starting_cohort.dta", replace


********************************************************************************
* Pull in IPVA variables, including impact  
********************************************************************************

local var_vic_emo_co "YPA5000"
local var_vic_emo_co_age "YPA5001"
local var_vic_emo_sh "YPA5002"
local var_vic_emo_sh_age "YPA5003"
local var_vic_emo "`var_vic_emo_co' `var_vic_emo_sh'"

local var_vic_phys1 "YPA5004" 
local var_vic_phys1_age "YPA5005" 
local var_vic_phys2 "YPA5006"
local var_vic_phys2_age "YPA5007"
local var_vic_phys "`var_vic_phys1' `var_vic_phys2'" 

local var_vic_sex_co1 "YPA5008"
local var_vic_sex_co1_age "YPA5009"
local var_vic_sex_co2 "YPA5012"
local var_vic_sex_co2_age "YPA5013"

local var_vic_sex_fo1 "YPA5010"
local var_vic_sex_fo1_age "YPA5011"
local var_vic_sex_fo2 "YPA5014"
local var_vic_sex_fo2_age "YPA5015"
local var_vic_sex "`var_vic_sex_co1' `var_vic_sex_co2' `var_vic_sex_fo1' `var_vic_sex_fo2'"

local var_vic "`var_vic_emo' `var_vic_phys' `var_vic_sex'"
local var_vic_age "`var_vic_emo_co_age' `var_vic_emo_sh_age' `var_vic_phys1_age' `var_vic_phys2_age' `var_vic_sex_co1_age' `var_vic_sex_co2_age' `var_vic_sex_fo1_age' `var_vic_sex_fo2_age'"

local var_per_emo_co "YPA5030"
local var_per_emo_co_age "YPA5031" 
local var_per_emo_sh "YPA5032"
local var_per_emo_sh_age "YPA5033"
local var_per_phys "YPA5034"
local var_per_phys_age "YPA5035"
local var_per_sex "YPA5036"
local var_per_sex_age "YPA5037"
local var_per "`var_per_emo_co' `var_per_emo_sh' `var_per_phys' `var_per_sex'"
local var_per_age "`var_per_emo_co_age' `var_per_emo_sh_age' `var_per_phys_age' `var_per_sex_age'"

local impact "neg neu pos"
local impact_neg "YPA5020 YPA5021 YPA5022 YPA5024 YPA5025 YPA5028 YPA5029"
local impact_neu "YPA5023"
local impact_pos "YPA5026 YPA5027"

local age "021 017 1821"

#delimit ;
merge 1:1 aln qlet using "`data_YPA'.dta", keep(match) keepusing(YPA9020 /*age at completion*/ `var_vic' `var_per' 
	`var_vic_age' `var_per_age' `impact_neg' `impact_neu' `impact_pos') nogen;
#delimit cr
*In the cohort if returned questionnaire
gen cohort = .
foreach v of local var_vic{
	replace cohort = 1 if `v' > 0 & `v' != .
	}
foreach v of local var_per{
	replace cohort = 1 if `v' > 0 & `v' != .
	}	
keep if cohort == 1

numlabel, add force
* 1 = Never, 2 = Once, 3 = A few times, 4 = Often 
local cat "vic_emo_co vic_emo_sh vic_phys1 vic_phys2 vic_sex_co1 vic_sex_co2 vic_sex_fo1 vic_sex_fo2 per_emo_co per_emo_sh per_phys per_sex"
local age "021 017 1821"
foreach c of local cat{
	foreach a of local age{
		cap gen `c'_`a' = 0
		foreach v of local var_`c'{
			*Different thresholds
			local crit_021_lo `v' >= 2 & `v' <= 4
			local crit_017_lo `v' >= 2 & `v' <= 4 & (`var_`c'_age' == 1  | `var_`c'_age' == 3)
			local crit_1821_lo `v' >= 2 & `v' <= 4 & (`var_`c'_age' == 2  | `var_`c'_age' == 3)
			
			/*
			local crit_021_lo `v' >= 3 & `v' <= 4
			local crit_017_lo `v' >= 3 & `v' <= 4 & (`var_`c'_age' == 1  | `var_`c'_age' == 3)
			local crit_1821_lo `v' >= 3 & `v' <= 4 & (`var_`c'_age' == 2  | `var_`c'_age' == 3)
			*/

			*Using the low threshold for all atm
			cap replace `c'_`a' = 1 if `crit_`a'_lo'
			}
		}
	}

*For those that have two variables representing the concept...
local cat "vic_phys vic_sex_co vic_sex_fo"
local age "021 017 1821"
foreach c of local cat{
	foreach a of local age{
		cap gen `c'_`a' = 0
		forvalues i = 1/2{
			replace `c'_`a' = 1 if `c'`i'_`a' == 1
			}
		}
	}

*Or more global vic or per
local age "021 017 1821"
local type "vic per"
foreach a of local age{
	*No need for per, already sorted
	cap gen vic_sex_`a' = 0
	replace vic_sex_`a' = 1 if vic_sex_co_`a' == 1 | vic_sex_fo_`a' == 1
	
	foreach t of local type{
		cap gen `t'_emo_`a' = 0
		replace `t'_emo_`a' = 1 if `t'_emo_co_`a' == 1 | `t'_emo_sh_`a' == 1
		
		cap gen `t'_`a' = 0
		replace `t'_`a' = 1 if `t'_emo_`a' == 1 | `t'_phys_`a' == 1 | `t'_sex_`a' == 1
		}
	}


*Impact
foreach i of local impact{
	gen impact_`i' = 0
		foreach j of local impact_`i'{
		replace impact_`i' = 1 if `j' == 1
		}
	}

*Start with any to start with (not age-dependent)
putexcel set "O:\IPV project\P1 - risk factors\impact", sheet("Numbers") modify

local quest "YPA5000 YPA5002 YPA5004 YPA5006 YPA5008 YPA5010 YPA5012 YPA5014"
local YPA5000_row = 3
local YPA5002_row = 8 
local YPA5004_row = 14 
local YPA5006_row = 19 
local YPA5008_row = 25 
local YPA5010_row = 30 
local YPA5012_row = 35
local YPA5014_row = 40

foreach q of local quest{
	tab `q' impact_neg if `q' >= 1 & `q' <= 4, mis matcell(`q'_neg)
	tab `q' impact_neu if `q' >= 1 & `q' <= 4, mis matcell(`q'_neu)
	tab `q' impact_pos if `q' >= 1 & `q' <= 4, mis matcell(`q'_pos)
	#delimit ;
	putexcel A``q'_row' = matrix(`q'_neg) 
	C``q'_row' = matrix(`q'_neu) 
	D``q'_row' = matrix(`q'_pos);
	#delimit cr
	}

*Now split by sex
forvalues i = 1/2{
	putexcel set "O:\IPV project\P1 - risk factors\impact", sheet("Numbers `i'") modify

	local quest "YPA5000 YPA5002 YPA5004 YPA5006 YPA5008 YPA5010 YPA5012 YPA5014"

	local YPA5000_row = 3
	local YPA5002_row = 8 
	local YPA5004_row = 14 
	local YPA5006_row = 19 
	local YPA5008_row = 25 
	local YPA5010_row = 30 
	local YPA5012_row = 35
	local YPA5014_row = 40

	foreach q of local quest{
		tab `q' impact_neg if `q' >= 1 & `q' <= 4 & kz021 == `i', mis matcell(`q'_neg)
		tab `q' impact_neu if `q' >= 1 & `q' <= 4 & kz021 == `i', mis matcell(`q'_neu)
		tab `q' impact_pos if `q' >= 1 & `q' <= 4 & kz021 == `i', mis matcell(`q'_pos)
		#delimit ;
		putexcel A``q'_row' = matrix(`q'_neg) 
		C``q'_row' = matrix(`q'_neu) 
		D``q'_row' = matrix(`q'_pos);
		#delimit cr
		}
	}


********************************************************************************
* Pulling in all other risk factor variables  
********************************************************************************

** ---- TF1 (12.5y): relationships and RSB -------------------------------------------------------------------
local tf1_rel "ff5787 ff5791 ff5809 ff5813 ff5829 ff5833 ff5849 ff5853 ff5868 ff5872 ff5889 ff5893 ff5889 ff5893 ff5914 ff5918"
local tf1_sex_ever "ff5900" 
local tf1_sex_cond "ff5907 ff5908 ff5909 ff5910 ff5911 ff5912"
merge 1:1 aln qlet using "`data_tf1'.dta", keep(master match) keepusing(`tf1_rel' `tf1_sex_ever' `tf1_sex_cond') nogen

** ---- CCQ (13y): ASB -------------------------------------------------------------------
*No ASB at age 13y as too sparse a variable?
local ccq_asb "ccq651 ccq653 ccq655 ccq656 ccq657 ccq658 ccq659 ccq660 ccq661 ccq663"
merge 1:1 aln qlet using "`data_ccq'.dta", keep(master match) keepusing(`ccq_asb') nogen


** ---- TF2 (13.5y): relationships and RSB -------------------------------------------------------------------
local tf2_rel "fg4190"
local tf2_sex_ever "fg5320" 
local tf2_sex_cond "fg5328 fg5329 fg5330 fg5331 fg5332"
merge 1:1 aln qlet using "`data_tf2'.dta", keep(master match) keepusing(`tf2_rel' `tf2_sex_ever' `tf2_sex_cond') nogen

** ---- TF3 (15.5y): relationships, sexual minority, anxiety, RSB, parental monitoring, and hospitalisations -------------------------------------------------------------------
local tf3_rel "fh8931 fh8932 fh8952 fh8953 fh8973 fh8974 fh9013 fh9014 fh9075 fh9076 fh9116 fh9118"
local tf3_sexmin "fh9140"
local tf3_anx "fh6893 fh6894"
local tf3_sh "fh6481"
local tf3_sex_ever "fh9100" 
local tf3_sex_cond "fh9109 fh9110"
local tf3_patmon "fh8200 fh8201 fh8202 fh8203"
local ts3_hosp "fh4563 fh4663"
merge 1:1 aln qlet using "`data_tf3'.dta", keep(master match) keepusing(`tf3_rel' `tf3_sexmin' `tf3_anx' `tf3_sh' `tf3_sex_ever' `tf3_sex_cond' `tf3_patmon' `tf3_hosp') nogen

** ---- CCXB (15.5y): relationships -------------------------------------------------------------------
local ccxb_rel "ccxb171 ccxb271 ccxb371 ccxb471 ccxb571"
merge 1:1 aln qlet using "`data_ccxb'.dta", keep(master match) keepusing(`ccxb_rel') nogen

** ---- CCS (16y): relationships, depression (MFQ scores), self-harm, smoking, cannabis, illicit non-cannabis drugs, and hospitalisations  -------------------------------------------------------------------
local ccs_rel "ccs6582c"
local ccs_dep "ccs4500 ccs4502 ccs4503 ccs4504 ccs4505 ccs4506 ccs4508 ccs4509 ccs4511 ccs4512 ccs4513 ccs4514 ccs4515"
local ccs_sh "ccs6530"
local ccs_smok "ccs4005"
local ccs_can "ccs4065"
local ccs_drug "ccs4150 ccs4151 ccs4152 ccs4153 ccs4154 ccs4160 ccs4161 ccs4162 ccs4163 ccs4164 ccs4165 ccs4166 ccs4167 ccs4168 ccs4169 ccs4170"
local ccs_hosp "ccs7330 ccs7340 ccs6547" 
merge 1:1 aln qlet using "`data_ccs'.dta", keep(master match) keepusing(`ccs_rel' `ccs_dep' `ccs_sh' `ccs_smok' `ccs_can' `ccs_drug' `ccs_hosp') nogen

** ---- TF4 (17.5y): relationships, anxiety, self-harm, hazardous alcohol use, and RSB -------------------------------------------------------------------
local tf4_rel "FJPC1000 FJPC1100 FJLE160 FJLE162"
local tf4_anx "FJCI600 FJCI601"
local tf4_sh "FJCI369"
local tf4_alc "FJAL4000"
local tf4_sex_ever "FJCH700" 
local tf4_sex_cond "FJCH707"
local tf4_sex_mult "FJCH705"
local tf4_hosp "FJLE134"
merge 1:1 aln qlet using "`data_tf4'.dta", keep(master match) keepusing(`tf4_rel' `tf4_anx' `tf4_sh' `tf4_alc' `tf4_sex_ever' `tf4_sex_cond' `tf4_sex_mult' `tf4_hosp') nogen

** ---- CCT (18y): depression (MFQ scores), ASB, smoking, cannabis, illicit non-cannabis drugs, and hospitalisations -------------------------------------------------------------------
*Removed drug use at 18y as 93% missing
local cct_dep "cct2700 cct2701 cct2702 cct2703 cct2704 cct2705 cct2706 cct2707 cct2708 cct2709 cct2710 cct2711 cct2712"
local cct_asb "cct6001 cct6003 cct6004 cct6005 cct6006 cct6008 cct6009 cct6010 cct6011"
local cct_smok "cct5014"
local cct_can "cct5055"
local cct_drug "cct5102 cct5112 cct5122 cct5132 cct5152 cct5162 cct5172"
local cct_hosp "cct4163 cct4164 cct7032 cct7033"
merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(`cct_dep' `cct_asb' `cct_smok' `cct_can' `cct_drug' `cct_hosp') nogen

** ---- CCXC (18y): relationships -------------------------------------------------------------------
local ccxc_rel "CCXC326 CCXC338"
merge 1:1 aln qlet using "`data_ccxc'.dta", keep(master match) keepusing(`ccxc_rel') nogen

** ---- CCU (20+y): relationships -------------------------------------------------------------------
local ccu_rel "CCU4032 CCU4033 CCU4035 CCU2085i"
local ccu_hosp "CCU2072"
merge 1:1 aln qlet using "`data_ccu'.dta", keep(master match) keepusing(`ccu_rel' `ccu_hosp') nogen

** ---- YPA (21y): relationships and RSB -------------------------------------------------------------------
local ypa_rel "YPA3042 YPA3041"
local ypa_sex "YPA1011"
merge 1:1 aln qlet using "`data_YPA'.dta", keep(match) keepusing(`ypa_rel' `ypa_sex') nogen

** ---- Other: ethnicity from schools' data -------------------------------------------------------------------
merge 1:1 aln qlet using "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\other data\child_ethnicity.dta", keep(master match) keepusing(ethnicity) nogen
*One contradicting value
replace ethnicity = . if ethnicity == -1 | ethnicity == 3

*Extra smoking and cannabis variables
merge 1:1 aln qlet using "`data_ccs'.dta", keep(master match) keepusing(ccs4000) nogen
merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(cct5000) nogen
merge 1:1 aln qlet using "`data_ccs'.dta", keep(master match) keepusing(ccs4060) nogen
merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(cct5050) nogen

*Merge on all ccu and cct variables (for NEET)
merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) nogen
merge 1:1 aln qlet using "`data_ccu'.dta", keep(master match) nogen

*Variables needed to derive ACES using Lotte's R scripts...
merge 1:1 aln qlet using "W:\data\ACE data\useful_data_ACE\01Recode_the_original_variables_to_binary\alspac.table_ACE.dta", keep(master match) nogen 

*Can leave out all the derived vic, per, impact variables
drop cohort vic* per* impact*

*Save
save "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\cohort\cohort_forlinkage_1019", replace
export excel using "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\cohort\cohort_forlinkage_1019.xlsx", sheetreplace firstrow(variables) missing(".")
