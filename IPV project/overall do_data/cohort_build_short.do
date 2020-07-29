********************************************************************************
** Project: RFs for IPVA
** Script purpose: 
** Date: 16.04.19
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
local temp_path "W:data\tempfiles"
local data_path "W:data\cohort"
local motherq_path "Current\Quest\Mother"
local childc_path "Current\Clinic\Child"
local childq_path "Current\Quest\Child Completed"
local ace_path "W:\data\ACE data\Annie"

*As versions of datasets change...
*Core sets
local data_mz "mz_5a"
local data_a "a_3e"
local data_b "b_4f"
local data_c "c_8a"
local data_kz "kz_5c"
local data_cp "cp_2b"

*Mothers
local data_e "`motherq_path'\e_4f"
local data_f "`motherq_path'\f_2b"
local data_g "`motherq_path'\g_5c"
local data_h "`motherq_path'\h_6d"
local data_j "`motherq_path'\j_5a"
local data_k "`motherq_path'\k_r1b"
local data_l "`motherq_path'\l_r1b"
local data_p "`motherq_path'\p_r1b"
local data_r "`motherq_path'\r_r1b"
local data_t "`motherq_path'\t_2a"

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

*merge 1:1 aln qlet using "`data_YPA'.dta", keep(match)
#delimit ;
merge 1:1 aln qlet using "`data_YPA'.dta", keep(match) keepusing(YPA* /*age at completion*/ `var_vic' `var_per' 
	`var_vic_age' `var_per_age' `impact_neg' `impact_neu' `impact_pos') nogen;
#delimit cr

gen answered_any_IPVA = 0
local var "YPA5000 YPA5002 YPA5004 YPA5006 YPA5008 YPA5010 YPA5012 YPA5014 YPA5030 YPA5032 YPA5034 YPA5036"
foreach v of local var{
	replace answered_any_IPVA = 1 if `v' >= 1 & `v' <= 4
	}
tab answered_any_IPVA if YPA0002 == 1, m

gen answered_any_hwb = 0
cap replace answered_any_hwb = 1 if YPA4000>=1 & YPA4000 !=.
forvalues v = 10/99{
	cap replace answered_any_hwb = 1 if YPA40`v'>=1 & YPA40`v'!=.
	}
forvalues v = 100/120{
	cap replace answered_any_hwb = 1 if YPA4`v'>=1 & YPA4`v'!=.
	}
tab answered_any_hwb if YPA0002 == 1, m

gen answered_any_tob = 0
cap replace answered_any_tob = 1 if YPA6000>=1 & YPA6000 !=.
forvalues v = 10/99{
	cap replace answered_any_tob = 1 if YPA60`v'>=1 & YPA60`v'!=.
	}
forvalues v = 100/120{
	cap replace answered_any_tob = 1 if YPA6`v'>=1 & YPA6`v'!=.
	}
tab answered_any_tob if YPA0002 == 1, m

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
*No need for qlet on mother questionnaires

** ---- B (18w gest): Parental IPVA -------------------------------------------------------------------
local b_dv "b592 b607"
merge m:1 aln using "`motherq_path'/`data_b'.dta", keep(master match) keepusing(`b_dv') nogen

** ---- C (32w gest): Parental IPVA -------------------------------------------------------------------
#delimit ;
local c_dv "c830 c831 c832 c833 c834 c850 c851 c852 c853 c854 c870 c871 c872 c873 c874 
c891 c892 c893 c894 c910 c911 c912 c913 c914 c930 c931 c932 c933 c934 c951 c952";
#delimit cr
merge m:1 aln using "`motherq_path'/`data_c'.dta", keep(master match) keepusing(`c_dv') nogen

** ---- E (8w): Parental IPVA -------------------------------------------------------------------
local e_dv "e422 e437"
merge m:1 aln using "`data_e'.dta", keep(master match) keepusing(`e_dv') nogen

** ---- F (8m): Parental IPVA -------------------------------------------------------------------
local f_dv "f242a f256a"
merge m:1 aln using "`data_f'.dta", keep(master match) keepusing(`f_dv') nogen

** ---- G (1.75y): Parental IPVA -------------------------------------------------------------------
local g_dv "g322a g336a"
merge m:1 aln using "`data_g'.dta", keep(master match) keepusing(`g_dv') nogen

** ---- H (2.75y): Parental IPVA -------------------------------------------------------------------
local h_dv "h232a h246a"
merge m:1 aln using "`data_h'.dta", keep(master match) keepusing(`h_dv') nogen

** ---- J (4y): Parental IPVA -------------------------------------------------------------------
local j_dv "j322a j336a"
merge m:1 aln using "`data_j'.dta", keep(master match) keepusing(`j_dv') nogen

** ---- K (5y): Parental IPVA -------------------------------------------------------------------
local k_dv "k4022 k4036"
merge m:1 aln using "`data_k'.dta", keep(master match) keepusing(`k_dv') nogen

** ---- L (6y): Parental IPVA -------------------------------------------------------------------
local l_dv "l4022 l4036"
merge m:1 aln using "`data_l'.dta", keep(master match) keepusing(`l_dv') nogen

** ---- P (9y): Parental IPVA -------------------------------------------------------------------
local p_dv "p2022 p2036"
merge m:1 aln using "`data_p'.dta", keep(master match) keepusing(`p_dv') nogen

** ---- R (11y): Parental IPVA -------------------------------------------------------------------
local r_dv "r5022 r5036"
merge m:1 aln using "`data_r'.dta", keep(master match) keepusing(`r_dv') nogen

** ---- T (18y): Parental IPVA -------------------------------------------------------------------
local t_dv "t3321 t3335"
merge m:1 aln using "`data_t'.dta", keep(master match) keepusing(`t_dv') nogen

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

** ---- CCT (18y): depression (MFQ scores), ASB, smoking, cannabis, illicit non-cannabis drugs, hospitalisations, and NEET -------------------------------------------------------------------
*Removed drug use at 18y as 93% missing
local cct_dep "cct2700 cct2701 cct2702 cct2703 cct2704 cct2705 cct2706 cct2707 cct2708 cct2709 cct2710 cct2711 cct2712"
local cct_asb "cct6001 cct6003 cct6004 cct6005 cct6006 cct6008 cct6009 cct6010 cct6011"
local cct_smok "cct5014"
local cct_can "cct5055"
local cct_drug "cct5102 cct5112 cct5122 cct5132 cct5152 cct5162 cct5172"
local cct_hosp "cct4163 cct4164 cct7032 cct7033"
local cct_neet "cct2950 cct3100 cct3180"
merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(`cct_dep' `cct_asb' `cct_smok' `cct_can' `cct_drug' `cct_hosp' `cct_neet') nogen

** ---- CCXC (18y): relationships -------------------------------------------------------------------
local ccxc_rel "CCXC326 CCXC338"
merge 1:1 aln qlet using "`data_ccxc'.dta", keep(master match) keepusing(`ccxc_rel') nogen

** ---- CCU (20+y): relationships -------------------------------------------------------------------
local ccu_rel "CCU4032 CCU4033 CCU4035 CCU2085i"
local ccu_hosp "CCU2072"
local ccu_neet "CCU4039 CCU4050 CCU4051 CCU4055 CCU4060 CCU4101 CCU4115"
merge 1:1 aln qlet using "`data_ccu'.dta", keep(master match) keepusing(`ccu_rel' `ccu_hosp' `ccu_neet') nogen

** ---- YPA (21y): parental IPVA, relationships and RSB -------------------------------------------------------------------
local ypa_dv "YPA5040 YPA5041 YPA5042 YPA5043 YPA5044 YPA5045 YPA5046 YPA5047 YPA5048 YPA5049 YPA5050 YPA5051 YPA5052 YPA5053 YPA5054 YPA5055 YPA5056 YPA5057 YPA5058 YPA5059"
local ypa_rel "YPA3042 YPA3041"
local ypa_sex "YPA1011"
merge 1:1 aln qlet using "`data_YPA'.dta", keep(match) keepusing(`ypa_dv' `ypa_rel' `ypa_sex') nogen

** ---- Other: ethnicity from schools' data -------------------------------------------------------------------
merge 1:1 aln qlet using "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\other data\child_ethnicity.dta", keep(master match) keepusing(ethnicity) nogen
*One contradicting value
replace ethnicity = . if ethnicity == -1 | ethnicity == 3

** ---- ACEs to be pulled in after running Script1 and Script 2 in R -------------------------------------------------------------------
merge 1:1 aln qlet using "`ace_path'\alspacKids_ACE_0_16_AH.dta", keep(master match) keepusing(*0_16yrs*) nogen


********************************************************************************
* Now tidy up these RF variables  
********************************************************************************

** ---- Local for recoding, this is the most common system needed: -------------------------------------------------------------------
local recode_reg "(2 = 0) (1 = 1) (-9999 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 . = 2)"


** ---- Relationships from 12.5y (TF1), 13.5y (TF2), 15.5y (CCXB), 15.5y (TF3), 16y (CCS), 17.5y (TF4), 18y (CCXC), 20+y (CCU), 21y (YPA) -------------------------------------------------------------------
gen rel_tf1 = .
foreach i of local tf1_rel{
	replace rel_tf1 = 1 if `i' == 1
	}
tab rel_tf1, mis matcell(rel_tf1)
putexcel set "O:\IPV project\P1 - risk factors\tables_080819", sheet("relationships") modify
putexcel E2 = rel_tf1[1,1]

gen rel_tf2 = 1 if fg4190 == 1
replace rel_tf2 = 0 if fg4190 == 0
tab rel_tf2, mis matcell(rel_tf2)
putexcel E3 = rel_tf2[1,1]

gen rel_tf3 = .
foreach i of local tf3_rel{
	replace rel_tf3 = 1 if `i' == 1
	}
tab rel_tf3, mis matcell(rel_tf3)
putexcel E4 = rel_tf3[1,1]

gen rel_ccs = 1 if ccs6582c == 1
tab rel_ccs, mis matcell(rel_ccs)
putexcel E8 = rel_ccs[1,1]

gen rel_ccxb = .
foreach i of local ccxb_rel{
	replace rel_ccxb = 1 if `i' == 1
	}
tab rel_ccxb, mis matcell(rel_ccxb)
putexcel E5 = rel_ccxb[1,1]

gen rel_tf4 = .
replace rel_tf4 = 1 if FJPC1000 == 1
tab rel_tf4, mis matcell(rel_tf4)
putexcel E9 = rel_tf4[1,1]

*Questions about length of relationship
replace rel_tf4 = 1 if FJPC1100 >= 1 & FJPC1100 <= 4 | FJPC1100 == 9
gen rel_tf4_2 = 1 if FJPC1100 >= 1 & FJPC1100 <= 4 | FJPC1100 == 9
tab rel_tf4_2, mis matcell(rel_tf4_2)
putexcel E10 = rel_tf4_2[1,1]
*Assuming the YP wouldn't say they don't know how long if they've never had a relationship, same for FJPC1150
replace rel_tf4 = 0 if FJPC1100 == 5

*Now questions about what had happened in last year
replace rel_tf4 = 1 if FJLE160 == 1 | FJLE162 == 1
drop rel_tf4_2
gen rel_tf4_2 = 1 if FJLE160 == 1 | FJLE162 == 1
tab rel_tf4_2, mis matcell(rel_tf4_2)
putexcel E15 = rel_tf4_2[1,1]
drop rel_tf4_2

gen rel_ccxc = 1 if CCXC326 == 1 | CCXC338 == 1
tab rel_ccxc, mis matcell(rel_ccxc)
putexcel E16 = rel_ccxc[1,1]

gen rel_ccu = 1 if CCU4032 == 1 | CCU4033 == 1 | CCU4035== 1
tab rel_ccu, mis matcell(rel_ccu)
putexcel E21 = rel_ccu[1,1]

replace rel_ccu = 1 if CCU2085i == 1
gen rel_ccu_2 = 1 if CCU2085i == 1
tab rel_ccu_2, mis matcell(rel_ccu_2)
putexcel E25 = rel_ccu_2[1,1]
drop rel_ccu_2

gen rel_ypa = 1 if YPA3042 == 1 | YPA3041 == 1
tab rel_ypa, mis matcell(rel_ypa)
putexcel E29 = rel_ypa[1,1]

*Now let's see who's been in a relationship by the time they're 17, or ever
gen rel_def_017 = 1 if fg4190 == 1 | FJPC1000 == 1

gen rel_ind_017 = .
local dataset "tf1 tf2 tf3 ccxb ccs tf4"
foreach i of local dataset{
	replace rel_ind_017 = 1 if rel_`i' == 1
	}

gen rel_ind_021 = 1 if rel_ind_017 == 1
replace rel_ind_021 = 1 if rel_ccu == 1
replace rel_ind_021 = 1 if rel_ypa == 1


** ---- Ethnicity from baseline records (or Jon's school's ethnicity) -------------------------------------------------------------------
recode c800 (-9999 -1 99 = .) (2 3 4 5 6 7 8 9 = 0), gen(white)
replace white = 2 if white == .
label define white 1 "White" 0 "Non-White" 2 "Missing"
label values white white


** ---- Sexuality from 16y (TF3) -------------------------------------------------------------------
recode fh9140 (-9999 -11 -10 -6 -5 -3 -2 -1 6 9 = .) (1 = 0) (2 3 4 5 = 1), gen(sex_min)
replace sex_min = 2 if sex_min == .
label variable sex_min "Sexual minority"
label define sex_min 0 "100% hetero" 1 "not 100% hetero" 2 "Not reported"
label values sex_min sex_min


** ---- ACEs (derived from several waves already by L Houttepen, Wellcome data note) -------------------------------------------------------------------
gen ace_c = 0
gen ace_ce = 0
#delimit ;
local var_ace_c "physical_abuse_0_16yrs sexual_abuse_0_16yrs emotional_abuse_0_16yrs 
emotional_neglect_0_16yrs bullying_0_16yrs violence_between_parnts_0_16yrs 
substance_household_0_16yrs mentl_hlth_prblms_r_scd_0_16yrs parent_convicted_offenc_0_16yrs 
parental_separation_0_16yrs";
#delimit cr
foreach i of local var_ace{
	replace ace_c = 1 if `i' == 1
	replace ace_ce = 1 if `i' == 1
	}
#delimit ;
local var_ace_ce "social_class_0_16yrs financial_difficulties_0_16yrs neighbourhood_0_16yrs 
social_support_child_0_16yrs social_support_parent_0_16yrs vlnc_btwn_chld_nd_prtnr_0_16yrs 
physical_illness_child_0_16yrs physical_illness_parent_0_16yrs parent_child_bond_0_16yrs";
#delimit cr
foreach i of local var_ace_ce{
	replace ace_ce = 1 if `i' == 1
	}
label define ace_c 0 "No reported ACEs" 1 "ACEs reported"
label values ace_c ace_c
label values ace_ce ace_c

*Numbers of classic ACEs
gen ace_no = 0
foreach i of local var_ace_c{
	replace ace_no = ace_no+1 if `i' == 1
	}
replace ace_no = 3 if ace_no >= 3 & ace_no != .


** ---- Depression - pulling in clinic MFQ scores from 16.5 (ccs) and 18.5 (cct) years old -------------------------------------------------------------------
local var "ccs cct"
foreach v of local var{
	foreach i of local `v'_dep{
		recode `i' (3 = 0) (2 = 1) (1 = 3) (-9999 -11 -10 -7 -6 -5 -2 -1 = .), gen(mfqscore_`i')
		}
	egen mfq_total_`v' = rowtotal(mfqscore*), mis
	gen depress_`v' = 1 if mfq_total_`v' > 11 & mfq_total_`v' != .
	replace depress_`v' = 0 if depress_`v' == . & mfq_total_`v' != .
	drop mfqscore_*
	tab depress_`v', mis
	replace depress_`v' = 2 if depress_`v' == .
	}

*Depression at ages 16-18?
gen depress_16to18 = 0 if depress_ccs == 0 | depress_cct == 0
replace depress_16to18 = 1 if depress_ccs == 1 | depress_cct == 1
replace depress_16to18 = 2 if depress_ccs == 2 & depress_cct == 2


** ---- Anxiety - pull in (teen-reported) DAWBA categorisation of anxiety from 15.5y (TF3) CIS-R at 17.5y (TF4) -------------------------------------------------------------------
recode fh6893 `recode_reg', gen(anxiety_tf3)
gen anxiety_tf4 = 2
replace anxiety_tf4 = 1 if FJCI600 == 2 | FJCI600 == 4 | FJCI600 == 8
replace anxiety_tf4 = 1 if FJCI601 == 2 | FJCI601 == 4 | FJCI601 == 8
replace anxiety_tf4 = 0 if anxiety_tf4 == 2 & (FJCI600 == 0 & FJCI601 == 0)


** ---- Self-harm at 15.5y (TF3), 16y (CCS), and 17.5y (TF4) -------------------------------------------------------------------
local var "tf3 ccs tf4"
local recode_tf3 "`recode_reg'"
local recode_ccs "`recode_reg'"
local recode_tf4 "(1 = 0) (2 = 1) (-999 -11 -10 -4 -1 = 2)"
foreach v of local var{
	gen sh_`v' = 2
	foreach i of local `v'_sh{
		recode `i' `recode_`v'', gen(`v'2)
		replace sh_`v' = 0 if `v'2 == 0  & sh_`v' == 2
		replace sh_`v' = 1 if `v'2 == 1
		}
	drop `v'2
	tab sh_`v'
	}


** ---- Anti-social behaviour at 13y (CCQ) and 18y (CCT) -------------------------------------------------------------------
*TF3 was also possible but in the end left out as picked up so little

local var "ccq cct"
*local var_tf3 "fh9573 fh9577 fh9579 fh9581 fh9583 fh9587"
*local crit_tf3 "`i' == 1"
*local crit_tf3_no "`i' == 2"
		
foreach v of local var{
	cap gen asb_`v' = .
	foreach i of local `v'_asb{
		replace asb_`v' = 1 if `i' == 3 | `i' == 4
		replace asb_`v' = 0 if (`i' == 1 | `i' == 2) & (asb_`v' == .)
		}
	}
*ASB is a bit of a special case so we put no/not reported category rather than no and missing separately 
* (can't guarantee a no, as with risky sexual behaviour)



** ---- Smoking at 16y (CCS) and 18y (CCT) -------------------------------------------------------------------
local var "ccs cct"
local recode_ccs "(1 2 3 = 0) (4 5 6 = 1) (-9999 -11 -10 -1 . = 2)"
local recode_cct "`recode_reg'"

foreach v of local var{
	merge 1:1 aln qlet using "`data_ccs'.dta", keep(master match) keepusing(ccs4000) nogen
	merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(cct5000) nogen
	gen smok_`v' = 2
	cap replace smok_ccs = 0 if ccs4000 == 2
	cap replace smok_cct = 0 if cct5000 == 2
	foreach i of local `v'_smok{
		recode `i' `recode_`v'', gen(`i'2)
		replace smok_`v' = 0 if `i'2 == 0 & smok_`v' == 2
		replace smok_`v' = 1 if `i'2 == 1
		drop `i'2
		}
	}


** ---- *Alcohol 17.5y (TF4) -------------------------------------------------------------------
*Could have used CCS (i.e. 16y) - Audit score of 8 or more (Kipping 2014), 
*but not readily available in ALSPAC directly, just says so in paper and Jon's powerpoint re: measures 
*local var_ccs "CCU3095"
*local recode_ccs "(1 2 3 = 0) (4 5 6 = 1) (-9999 -11 -10 -1 = 2)"

gen hazalc_tf4 = 2
replace hazalc_tf4 = 0 if FJAL4000 >=0 & FJAL4000 <= 7 & hazalc_tf4 == 2
replace hazalc_tf4 = 1 if FJAL4000 >=8 & FJAL4000 != .


** ---- Cannabis at 16y (CCS), 18y (CCT) -------------------------------------------------------------------
local var "ccs cct"
local recode_ccs "(1 2 3 = 0) (4 5 6 = 1) (-9999 -11 -10 -1 = 2)"
local recode_cct "(1 2 3 = 0) (4 5 = 1) (-9999 -11 -10 -2 -1 = 2)"

foreach v of local var{
	*Merge on these indicators of no cannabis
	merge 1:1 aln qlet using "`data_ccs'.dta", keep(master match) keepusing(ccs4060) nogen
	merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(cct5050) nogen
	gen cann_`v' = 2
	cap replace cann_ccs = 0 if ccs4060 == 2
	cap replace cann_cct = 0 if cct5050 == 2
	foreach i of local `v'_can{
		recode `i' `recode_`v'', gen(`i'2)
		replace cann_`v' = 0 if `i'2 == 0 & cann_`v' == 2
		replace cann_`v' = 1 if `i'2 == 1
		drop `i'2
		}
	tab cann_`v'
	}


** ---- Ilicit drugs at 16y (CCS) and 18y (CCT) -------------------------------------------------------------------
local var "ccs cct"
local recode_ccs "(1 2 = 0) (3 = 1) (-9999 -11 -10 -1 = 2)"
local recode_cct "`recode_reg'"

foreach v of local var{
	gen drug_`v' = 2
	foreach i of local `v'_drug{
		recode `i' `recode_`v'', gen(`i'2)
		replace drug_`v' = 0 if `i'2 == 0 & drug_`v' == 2
		replace drug_`v' = 1 if `i'2 == 1
		drop `i'2
		}
	tab drug_`v'
	}


** ---- Risky sexual behaviour at 12.5y (TF1), 13.5y (TF2), 15.5y (TF3) and 17.5y (TF4)  -------------------------------------------------------------------
* Could also include pregnant or fathered a pregnancy by age 18 (TF4: FJLE148), but do not for now.
* We do not include STDs as data on these are too late (21 years old).
* 24.07.19 - maybe also young pregnancy (Gemma Clayton will share work she’s done on this).
* YPA (21y) also includes a question on if ever been pregnant and if so, what year that pregnancy/birth was

*Sex before 16 (ie. TF1, TF2, TF3)
gen sex_before16 = 0
local var "tf1 tf2 tf3"
foreach i of local var{
	foreach j of local `i'_sex_ever{
		replace sex_before16 = 1 if `j' == 1
		}
	}
*Gave birth before ~17? I.e. date of birth of first child was before (inc) ~2009 (this also means that participant could have had sex at up 2009-1990=18 years old)
replace sex_before16 = 1 if YPA1011 <= 2009 & YPA1011 > 0 & YPA1011 != .

*Sex for 16 onwards - did they use protection? I.e. TF4 info
gen sex_unp_from16 = 0
replace sex_unp_from16 = 1 if FJCH700 == 1 & FJCH707 != 1

*Multiple partners (3+ in past year)
gen sex_mult = 0
replace sex_mult  = 1 if FJCH705 >= 3 & FJCH705 != .

*Composite outcome of RSB
gen rsb = .
replace rsb = 1 if sex_before16 == 1 | sex_unp_from16 == 1 | sex_mult == 1


** ---- Parental monitoring at 15.5y (TF3) -------------------------------------------------------------------
*numlabel, add force
for var fh8200-fh8203: recode X min/-1=.

egen fh_pawareness_miss = rmiss(fh8200 - fh8203)
egen patmon_score = rowtotal(fh8200 - fh8203)
replace patmon_score = . if fh_pawareness_miss>0
gen ext_patmon = 2 if patmon_score == . | patmon_score < 0
replace ext_patmon = 0 if patmon_score <= 13
replace ext_patmon = 1 if patmon_score > 13 & patmon_score != .


** ---- Hospitalisations at 15.5y (TF3), 16y (CCS), 17.5y (TF4), 18y (CCT), 20+y (CCU) -------------------------------------------------------------------
local var "tf3 ccs tf4 cct ccu"
gen hosp = .
foreach v of local var{
	foreach i of local `v'_hosp{
		replace hosp = 1 if `i' == 1
		}
	}

** ---- NEET status at 18y (CCT) and 20+y (CCU) -------------------------------------------------------------------	
*CCT
*Have checked cct3180: gives no extra info
gen noteduc_cct=cct2950
recode noteduc_cct -10/-1=. 1/6=0 7=1 .=.
replace noteduc_cct=0 if cct3100==7

gen notemp_cct=cct3100
recode notemp_cct -9999/-1=. 1/3=0 4=1 5/8=0 .=.

gen neet_cct=notemp_cct
replace neet_cct=0 if noteduc_cct==0
replace neet_cct=. if noteduc_cct==. & notemp_cct==1
replace neet_cct=. if noteduc_cct==1 & notemp_cct==.

*CCU
*Have checked CCU4100, CCU4102, CCU4135, CCU4155, CCU4175, and CCU4310, against CCU4060: give no extra info
gen noteduc_ccu=CCU4055
recode noteduc_ccu -9999/-1=. 1/6=0 7=1
*Additional education questions that negate this...
replace noteduc_ccu=0 if (CCU4039==1 | CCU4051==1 | CCU4060==5) & (noteduc_ccu==. | noteduc_ccu==1)

gen notemp_ccu=CCU4060
recode notemp_ccu -9999/-1=. 1/3=0 4=1 5/6=0
*Additional employment questions that negate this...
replace notemp_ccu=0 if ((CCU4101>=1 & CCU4101<=4) | (CCU4115>=1 & CCU4115<=8)) & (notemp_ccu==. | notemp_ccu==1)

gen neet_ccu=CCU4050
recode neet_ccu -9999/-1=. 1=0 2=1
replace neet_ccu=0 if noteduc_ccu==0 | notemp_ccu==0
replace neet_ccu=. if noteduc_ccu==. & notemp_ccu==1
replace neet_ccu=. if noteduc_ccu==1 & notemp_ccu==.

** ---- Parental IPVA at 18 & 32 weeks gestations (B and C), 8 weeks (E), 8 months (F), ----------------------------------------------------------	
** ---- 1.75y (G), 2.75y (H), 4y (J), 5y (K), 6y (L), 9y (P), 11y (R), 18y (T), 21y (YPA) -------------------------------------------------------------------	
gen parent_dv = .
local var "b592 b607 e422 e437 k4022 k4036 l4022 l4036"
foreach v of local var{
	replace parent_dv=1 if `v'>=1 & `v'<=4
	}
*Leave out code for questionnaire C atm
local var "f242a f256a g322a g336a h232a h246a j322a j336a t3321 t3335"
foreach v of local var{
	replace parent_dv=1 if `v'==1
	}
local var "p2022 p2036 r5022 r5036"
foreach v of local var{
	replace parent_dv=1 if `v'>=1 & `v'<=3
	}
local var "YPA5040 YPA5045 YPA5050 YPA5055"
foreach v of local var{
	replace parent_dv=1 if `v'>=2 & `v'<=4
	}
tab parent_dv, m

	
*parent_pdv (physical) 
gen parent_pdv = .
local var "b592 e422 k4022 l4022"
foreach v of local var{
	replace parent_pdv=1 if `v'>=1 & `v'<=4
	}
*Leave out code for questionnaire C atm
local var "f242a g322a h232a j322a t3321"
foreach v of local var{
	replace parent_pdv=1 if `v'==1
	}
local var "p2022 r5022"
foreach v of local var{
	replace parent_pdv=1 if `v'>=1 & `v'<=3
	}
replace parent_pdv=1 if YPA5050>=2 & YPA5050<=4
tab parent_pdv, m

*parent_edv (emotional)
gen parent_edv = .
local var "b607 e437 k4022 l4036"
foreach v of local var{
	replace parent_edv=1 if `v'>=1 & `v'<=4
	}
*Leave out code for questionnaire C atm
local var "f256a g322a h232a j336a t3335"
foreach v of local var{
	replace parent_edv=1 if `v'==1
	}
local var "p2036 r5036"
foreach v of local var{
	replace parent_edv=1 if `v'>=1 & `v'<=3
	}
local var "YPA5040 YPA5045 YPA5055"
foreach v of local var{
	replace parent_edv=1 if `v'>=2 & `v'<=4
	}
tab parent_edv, m

tab parent_pdv parent_edv, m
*So far about 43% parental dv, 12% physical, 40% emotional, 11% both

********************************************************************************
* Save data as 'IPVA_cohort' in our shared directory so I can run analyses directly.  
********************************************************************************
keep if cohort == 1
#delimit ;
keep aln qlet kz021 
vic* per* impact* 
YPA5020 YPA5021 YPA5022 YPA5024 YPA5025 YPA5028 YPA5029 YPA5023 YPA5026 YPA5027
`var_vic' `var_vic_age' `var_per' `var_per_age'
rel_* ethnic sex_min 
`var_ace_c' ace_c ace_no *patmon* 
depress_ccs depress_cct mfq_total_ccs mfq_total_cct anxiety_tf3 anxiety_tf4 sh_ccs sh_tf4 asb_ccq
asb_cct smok_ccs smok_cct FJAL4000 hazalc_tf4 cann_ccs cann_cct drug_ccs drug_cct
rsb hosp neet_cct neet_ccu 
parent*;
#delimit cr
*Note that these ACES were derived using Script1 and Script2

*Rename ACE vars
#delimit ;
local var "physical_abuse_0_16yrs sexual_abuse_0_16yrs emotional_abuse_0_16yrs 
emotional_neglect_0_16yrs bullying_0_16yrs violence_between_parnts_0_16yrs 
substance_household_0_16yrs mentl_hlth_prblms_r_scd_0_16yrs parent_convicted_offenc_0_16yrs 
parental_separation_0_16yrs";
#delimit cr

foreach v of local var{
	local name = substr("`v'",1,12)
	rename `v' `name'
	}

save "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\cohort\current_IPVA_cohort_id", replace
desc, det


********************************************************************************
* Exporting out RF data so that can multiply impute (along with ACEs) in R  
********************************************************************************
*Make sure '.', not '2'. denotes 'missing'
local var "sex_min anxiety_tf3 anxiety_tf4 sh_ccs sh_tf4 smok_ccs smok_cct cann_ccs cann_cct drug_ccs drug_cct neet_cct neet_ccu"
foreach i of local var{
	gen `i'_fi = `i'
	replace `i'_fi = . if `i' == 2
	}

*Variables where originally 'no' and 'missing' combined (now not until here)
local var "asb_ccq asb_cct rsb hosp"
foreach i of local var{
	cap gen `i'_fi = `i'
	*replace `i'_fi = 0 if `i' == .
	}

*Hazardous alcohol score
recode FJAL4000 (-9999 -11 -10 -1 = .), gen(FJAL4000_fi)


*Export out everything we'll need for imputation and after...
#delimit ;
keep aln qlet 
kz021 
*YPA*
vic* per* impact*
ethnic sex_min patmon_score 
mfq_total_ccs mfq_total_cct anxiety_tf3_fi anxiety_tf4_fi sh_ccs_fi sh_tf4_fi 
asb_ccq_fi asb_cct_fi smok_ccs_fi smok_cct_fi FJAL4000_fi cann_ccs_fi cann_cct_fi 
drug_ccs_fi drug_cct_fi rsb_fi hosp_fi neet_cct_fi neet_ccu_fi;
#delimit cr
save "`ace_path'\ipva_cohort_for_imp_id.dta", replace
export excel using "`ace_path'\ipva_cohort_for_imp_id.xlsx", replace firstrow(varlabels)


/*
********************************************************************************
* Tidying up imputed datasets
********************************************************************************
use "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\ACE data\Annie\imp_all_ACE_0_16.dta", replace

*Need to merge back in
#delimit ;
merge m:1 aln qlet using "W:\data\cohort\current_IPVA_cohort.dta", 
keepusing(YPA* vic* per* impact*) keep(match) nogen;
/*ethnicity sex_min hosp*/
#delimit cr

*Rename ACE vars to be shorter
#delimit ;
local var_ace_c "physical_abuse sexual_abuse emotional_abuse 
emotional_neglect bullying violence_between
substance_household mental_health parent_convicted_offenc 
parental_separation";
#delimit cr

label define yesno 0 "No" 1 "Yes"
foreach v of local var_ace_c{
	local name = substr("`v'",1,12)
	rename `v' `name'
	replace `name' = 0 if `name' == 1
	replace `name' = 1 if `name' == 2
	label values `name' yesno
	}
	
*Regen ace_no and hazalc
gen ace_c_fi = 0

#delimit ;
local var_ace_c "physical_abu sexual_abuse emotional_ab
emotional_ne bullying violence_bet
substance_ho mental_healt parent_convi
parental_sep";
#delimit cr
foreach i of local var_ace_c{
	replace ace_c_fi = 1 if `i' == 1
	}
label values ace_c_fi ace_c
gen ace_no_fi = 0
foreach i of local var_ace_c{
	replace ace_no_fi = ace_no_fi+1 if `i' == 1
	}
replace ace_no_fi = 3 if ace_no_fi >= 3 & ace_no_fi != .

*New ext_patmon, depress_ccs, and depress_cct
gen ext_patmon = .
replace ext_patmon = 2 if (patmon_score == . | patmon_score < 0)
replace ext_patmon = 0 if patmon_score <= 13
replace ext_patmon = 1 if patmon_score > 13 & patmon_score != .
rename ext_patmon ext_patmon_fi

local var "ccs cct"
foreach v of local var{
	gen depress_`v'_fi = 1 if mfq_total_`v' > 11 & mfq_total_`v' != .
	replace depress_`v'_fi = 0 if depress_`v' == . & mfq_total_`v' != .
	replace depress_`v'_fi = 2 if depress_`v' == .
	}
	
*Regen hazalc
gen hazalc_tf4_fi = 2 if _imp != 0
replace hazalc_tf4_fi = 0 if alc_score >=0 & alc_score <= 7 & alc_score == 2
replace hazalc_tf4 = 1 if alc_score >=8 & alc_score != .

save "W:\data\cohort\current_IPVA_cohort_inc_imp.dta", replace
*/
