********************************************************************************
** Project: EU trafficking project
** Script purpose: Pull variables from ALSPAC (to load into R for analysis
** Date: 04.11.19
** Author: Annie Herbert
** Email: annie.herbert@bristol.ac.uk
********************************************************************************

********************************************************************************
* Set locals, etc.  
********************************************************************************
clear
clear matrix
clear mata
set maxvar 32767

cd "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data"
local temp_path "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\other data\Trafficking project\Temp"
local data_path "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\other data\Trafficking project\Data"
local motherc_path "Current\Clinic\Adult"
local motherq_path "Current\Quest\Mother"
local fatherq_path "Current\Quest\Father"
local partnerq_path "Current\Quest\Partner"
local childc_path "Current\Clinic\Child"
local childq_path "Current\Quest\Child Completed"
*local ace_path "W:\data\ACE data\Annie"

*As versions of datasets change...
*Core sets
local data_mz "mz_5a"
local data_a "a_3e"
local data_b "b_4f"
local data_c "c_8a"
local data_kz "kz_5c"
local data_cp "cp_2b"

*Mothers
local data_d "`motherq_path'\d_4b"
local data_e "`motherq_path'\e_4f"
local data_f "`motherq_path'\f_2b"
local data_g "`motherq_path'\g_5c"
local data_h "`motherq_path'\h_6d"
local data_j "`motherq_path'\j_5a"
local data_k "`motherq_path'\k_r1b"
local data_l "`motherq_path'\l_r1b"
local data_p "`motherq_path'\p_r1b"
local data_q "`motherq_path'\q_r1b"
local data_r "`motherq_path'\r_r1b"
local data_t "`motherq_path'\t_2a"
local data_v "`motherq_path'\v_1a"

local data_fom1 "`motherc_path'\fom1_3a"

*Fathers
local data_fa "`fatherq_path'\FA_1b"

*Partners
local data_pn "`partnerq_path'\pn_r1b"

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

*Other
local data_income "Useful_data\Income\income3347corrected_aln"


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
* Pulling in DV at 8wks gest and 18wks postpartum, and all other risk factor variables  
********************************************************************************
*No need for qlet on mother questionnaires

** ---- A (8w gest): smoking in preg -------------------------------------------------------------------
local a_smok "a200"
merge m:1 aln using "`motherq_path'/`data_a'.dta", keep(master match) keepusing(`a_smok') nogen

** ---- B (18w gest): Parental IPVA, smoking in preg, meds for depression -------------------------------------------------------------------
local b_dv "b592 b607"
local b_smok "b665 b667"
local b_epds "b370"
local b_med_dep "b122" 
local b_med_anx "b106"
*Also available: Crown Crisp - Depression (some other sort of score)/ depression subscale of CCEI in HAB, 
*Crown Crisp - Anxiety/ anxiety subscale of CCEI in HAB,
*and Unnecessary anxiety or worry in past WK
merge m:1 aln using "`motherq_path'/`data_b'.dta", keep(master match) keepusing(`b_dv' `b_smok' `b_epds' `b_med_dep' `b_med_anx') nogen

** ---- C (32w gest): smoking in preg, education, employment, social class, meds for depression -------------------------------------------------------------------
*Blocking this one out as includes all those questions about flashing, etc.
/*
#delimit ;
local c_dv "c830 c831 c832 c833 c834 c850 c851 c852 c853 c854 c870 c871 c872 c873 c874 
c891 c892 c893 c894 c910 c911 c912 c913 c914 c930 c931 c932 c933 c934 c951 c952";
#delimit cr
*/
local c_smok "c482"
local c_educ "c645 c645a"
local c_employ "c710 c711 c753"
*Social class based on employment status:
local c_social "c755 c765" 
local c_med_dep "c101" 
local c_med_anx "c093"
*Also available: depression subscale of CCEI in YP and 'depression with missing values' (again some sort of score)
*Same variables also available in terms of anxiety, and Unnecessary anxiety or worry in past week
merge m:1 aln using "`motherq_path'/`data_c'.dta", keep(master match) keepusing(`c_smok' `c_educ' `c_employ' `c_social' `c_med_dep' `c_med_anx') nogen

** ---- D (12w gest): depression -------------------------------------------------------------------
local d_wgt "dw002" 
local d_hgt "dw021"
local d_dep "d171" 
merge m:1 aln using "`data_d'.dta", keep(master match) keepusing(`d_wgt' `d_hgt' `d_dep') nogen

** ---- E (8w): Parental IPVA, smoking in preg -------------------------------------------------------------------
*Various depression measures also available
local e_dv "e422 e437"
local e_smok "e170 e172 e174 e176 e178"
merge m:1 aln using "`data_e'.dta", keep(master match) keepusing(`e_dv' `e_smok') nogen

** ---- H (2y, 9m): income -------------------------------------------------------------------
local h_income "h470"
merge m:1 aln using "`data_h'.dta", keep(master match) keepusing(`h_income') nogen

** ---- P (9y, 2m): depression -------------------------------------------------------------------
local p_wgt "p1290"
local p_hgt "p1291"
local p_dep "p1011 p1054 p1210"
local p_anx "p1010"
merge m:1 aln using "`data_p'.dta", keep(master match) keepusing(`p_wgt' `p_hgt' `p_dep' `p_anx') nogen

** ---- Q (10y, 2m): employment, depression -------------------------------------------------------------------
local q_employ "q5007 q5016"
local q_med_dep "q4100"
local q_med_anx "q4110"
merge m:1 aln using "`data_q'.dta", keep(master match) keepusing(`q_employ' `q_med_dep' `q_med_anx') nogen

** ---- PN (Partners, 10y, 2m): employment -------------------------------------------------------------------
local pn_employ "pn5007"
merge m:1 aln using "`data_pn'.dta", keep(master match) keepusing(`pn_employ') nogen

** ---- R (11y, 2m): smoking, income, and depression -------------------------------------------------------------------
local r_smok "r6018 r6020"
local r_income "r9020"
local r_dep "r2021 r2160"
*Also available: E12: Frequency respondent has been anxious/worried for no good reason in past week
merge m:1 aln using "`data_r'.dta", keep(master match) keepusing(`r_smok' `r_income' `r_dep') nogen

** ---- FOM1 (Dec 2008 - July 2011, i.e. ~16-20y): BMI -------------------------------------------------------------------
local fom1_bmi "fm1ms111"
merge m:1 aln using "`data_fom1'.dta", keep(master match) keepusing(`fom1_bmi') nogen

** ---- T (May 2010 - Sept 2010, i.e. ~17-18y): BMI, meds for depression -------------------------------------------------------------------
local t_bmi "t6099"
local t_med_dep "t5404"
merge m:1 aln using "`data_t'.dta", keep(master match) keepusing(`t_bmi' `t_med_dep') nogen

** ---- FA (Fathers, Sept 2011 - Feb 2013, i.e. ~18-21y): employment -------------------------------------------------------------------
local fa_employ "fa1150"
merge m:1 aln using "`data_fa'.dta", keep(master match) keepusing(`fa_employ') nogen

** ---- V (2013, i.e. i.e. ~20-22y): smoking, income -------------------------------------------------------------------
local v_wgt "V4410"
local v_hgt "V4400"
local v_smok "V5520"
local v_employ "V1150 V1152 V1154 V1156 V1158"

local v_income "V1300"
local v_anx "V5151"
merge m:1 aln using "`data_v'.dta", keep(master match) keepusing(`v_wgt' `v_hgt' `v_smok' `v_employ' `v_income' `v_anx') nogen

** ---- Equivalised income measure -------------------------------------------------------------------
local equiv_income "logavinceq"
merge m:1 aln using "`data_income'.dta", keep(master match) keepusing(`equiv_income') nogen

rename _all, lower
save "`data_path'\ipva_mums.dta", replace
export excel using "`data_path'\ipva_mums.xlsx", replace firstrow(var)
