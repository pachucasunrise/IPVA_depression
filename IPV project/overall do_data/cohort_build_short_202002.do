********************************************************************************
** Project: Mental Health outcomes of IPVA
** Script purpose: Define cohort and pull out IPVA, RFs, MH variables from data 
*at different ALSPAC waves
** Date: 16.04.19 (updated in Feb 2020 to incorporate MH outcomes)
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
local temp_path "W:data\tempfiles"
local data_path "W:data\cohort"
local motherq_path "Current\Quest\Mother"
local partnerq_path "Current\Quest\Partner"
local childb_path "Current\Quest\Child Based"
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
local data_j "`motherq_path'\j_5b"
local data_k "`motherq_path'\k_r1b"
local data_n "`motherq_path'\n_3a"
local data_l "`motherq_path'\l_r1b"
local data_p "`motherq_path'\p_r1b"
local data_r "`motherq_path'\r_r1b"
local data_s "`motherq_path'\s_r1a"
local data_t "`motherq_path'\t_2a"

*Partners
local data_pb "`partnerq_path'\pb_4b"
local data_pc "`partnerq_path'\pc_3a"
local data_pd "`partnerq_path'\pd_7b"
local data_pe "`partnerq_path'\pe_4a"
local data_pf "`partnerq_path'\pf_r1a"
local data_pg "`partnerq_path'\pg_2a"
local data_ph "`partnerq_path'\ph_1c"
local data_pj "`partnerq_path'\pj_r1a"
local data_pl "`partnerq_path'\pl_r1b"
local data_pm "`partnerq_path'\pm_r1b"
local data_pp "`partnerq_path'\pp_r1b"
local data_pq "`partnerq_path'\pq_r1a"

*Focus on Fathers (2018... so unlikely to use)
local data_fa "`partnerq_path'\FA_1b"

*Child-based
local data_tc "`childb_path'\tc_2a"

*Postal questionnaires
local data_ccq "`childq_path'\ccq_r1c"
local data_ccs "`childq_path'\ccs_r1b"
local data_cct "`childq_path'\cct_1b"
local data_ccxb "`childq_path'\ccxb_r1a"
local data_ccxc "`childq_path'\ccxc_r2a"
local data_ccxd "`childq_path'\ccxd_2a"
local data_ccu "`childq_path'\ccu_2b"
local data_YPA "`childq_path'\YPA_r1a"
local data_YPB "`childq_path'\YPB_r1b"
local data_YPC "`childq_path'\YPC_2a"
local data_YPD "`childq_path'\YPD_1a"
local data_YPE "`childq_path'\YPE_4a"
local data_YPF "`childq_path'\YPF_1"

*Clinics
local data_f10 "`childc_path'\f10_6b"
local data_tf1 "`childc_path'\tf1_3b"
local data_tf2 "`childc_path'\tf2_4a"
local data_tf3 "`childc_path'\tf3_4c"
local data_tf4 "`childc_path'\tf4_5a"
local data_F24 "`childc_path'\F24_4a"


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

*At this point, we have 202 who are in same mother pairs (i.e. 101), among the 15.6k


********************************************************************************
* Update sex (as one value missing)
********************************************************************************
tab kz021, m
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccj_r1b.dta", keep(master match) keepusing(ccj900) nogenerate
*tab kz021 ccj900, m
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccl_r1b.dta", keep(master match) keepusing(ccl900) nogenerate
*tab kz021 ccl900, m
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccm_r1c.dta", keep(master match) keepusing(ccm900) nogenerate
*tab kz021 ccm900, m
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccn_r1c.dta", keep(master match) keepusing(ccn900) nogenerate
*tab kz021 ccn900, m
gen kz021_new = kz021
replace kz021_new = 1 if kz021 == . & (ccj900 == 1 | ccl900 == 1 | ccm900 == 1 | ccn900 == 1) & (ccj900 != 2 & ccl900 != 2 & ccm900 != 2 & ccn900 != 2)
replace kz021_new = 2 if kz021 == . & (ccj900 == 2 | ccl900 == 2 | ccm900 == 2 | ccn900 == 2) & (ccj900 != 1 & ccl900 != 1 & ccm900 != 1 & ccn900 != 1)
tab kz021 kz021_new
label var kz021_new "sex whereby missing baseline values updated at later waves"

**Also available at waves ccc, ccd, cce, ccf, ccg...

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
merge 1:1 aln qlet using "`data_YPA'.dta", keep(master match) keepusing(YPA* /*age at completion*/ `var_vic' `var_per' 
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
*keep if cohort == 1

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

*17th Feb 2020: No need to tabulate impact at this stage, we have already done this 
*during analysis of paper 1.
/*
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
		tab `q' impact_neg if `q' >= 1 & `q' <= 4 & kz021_new == `i', mis matcell(`q'_neg)
		tab `q' impact_neu if `q' >= 1 & `q' <= 4 & kz021_new == `i', mis matcell(`q'_neu)
		tab `q' impact_pos if `q' >= 1 & `q' <= 4 & kz021_new == `i', mis matcell(`q'_pos)
		#delimit ;
		putexcel A``q'_row' = matrix(`q'_neg) 
		C``q'_row' = matrix(`q'_neu) 
		D``q'_row' = matrix(`q'_pos);
		#delimit cr
		}
	}
*/

*At this point, there are 23 pairs of individuals who have the same mum.

********************************************************************************
* Pulling in all other risk factor/DV/MH variables  
********************************************************************************
*No need for qlet on mother questionnaires

** ---- B and PB (18w gest): Parental IPVA (maternal and paternal -reported), partner's highest educational qualification -------------------------------------------------------------------
local b_dv "b592 b607"
merge m:1 aln using "`motherq_path'/`data_b'.dta", keep(master match) keepusing(`b_dv') nogen
local pb_pareduc "pb325 pb325a"
local pb_dv "pb182 pb182a pb196 pb196a"
merge m:1 aln using "`data_pb'.dta", keep(master match) keepusing(`pb_pareduc' `pb_dv') nogen

** ---- C and PC (32w gest): Parental IPVA (maternal and paternal -reported), Mother's highest educational qualification -------------------------------------------------------------------
#delimit ;
local c_dv "c830 c831 c832 c833 c834 c850 c851 c852 c853 c854 c870 c871 c872 c873 c874 
c891 c892 c893 c894 c910 c911 c912 c913 c914 c930 c931 c932 c933 c934 c951 c952";
#delimit cr
local c_pareduc "c645 c645a"
local pc_dv "pc222 pc222a pc235 pc235a"
merge m:1 aln using "`motherq_path'/`data_c'.dta", keep(master match) keepusing(`c_dv' `c_pareduc') nogen
merge m:1 aln using "`data_pc'.dta", keep(master match) keepusing(`pc_dv') nogen

** ---- E (8w): Maternal-reported IPVA -------------------------------------------------------------------
local e_dv "e422 e437"
merge m:1 aln using "`data_e'.dta", keep(master match) keepusing(`e_dv') nogen

** ---- F and PD (8m): Parental IPVA (maternal and paternal -reported) -------------------------------------------------------------------
local f_dv "f242a f256a"
merge m:1 aln using "`data_f'.dta", keep(master match) keepusing(`f_dv') nogen
local pd_dv "pd242 pd242a pd256 pd256a"
merge m:1 aln using "`data_pd'.dta", keep(master match) keepusing(`pd_dv') nogen

** ---- G and PE (1.75y): Parental IPVA (maternal and paternal -reported) -------------------------------------------------------------------
local g_dv "g322a g336a"
merge m:1 aln using "`data_g'.dta", keep(master match) keepusing(`g_dv' `g_liveswith') nogen
local pe_dv "pe322 pe322a pe336 pe336a"
merge m:1 aln using "`data_pe'.dta", keep(master match) keepusing(`pe_dv') nogen

** ---- H and PF (2.75y): Parental IPVA (maternal and paternal -reported) -------------------------------------------------------------------
local h_dv "h232a h246a"
merge m:1 aln using "`data_h'.dta", keep(master match) keepusing(`h_dv') nogen
local pf_dv "pf5022 pf5035"
merge m:1 aln using "`data_pf'.dta", keep(master match) keepusing(`pf_dv') nogen

** ---- J and PG (4y): Parental IPVA (maternal and paternal -reported) -------------------------------------------------------------------
local j_dv "j322a j336a"
merge m:1 aln using "`data_j'.dta", keep(master match) keepusing(`j_dv') nogen
local pg_dv "pg3022 pg3035"
merge m:1 aln using "`data_pg'.dta", keep(master match) keepusing(`pg_dv') nogen

** ---- K and PH (5y): Parental IPVA (paternal and maternal -reported) -------------------------------------------------------------------
local k_dv "k4022 k4036"
merge m:1 aln using "`data_k'.dta", keep(master match) keepusing(`k_dv') nogen
local ph_dv "ph4022 ph4036"
merge m:1 aln using "`data_ph'.dta", keep(master match) keepusing(`ph_dv') nogen

** ---- L and PJ (7y): Parental IPVA (paternal and maternal -reported) -------------------------------------------------------------------
local l_dv "l4022 l4036"
merge m:1 aln using "`data_l'.dta", keep(master match) keepusing(`l_dv') nogen
local pj_dv "pj4022 pj4036"
merge m:1 aln using "`data_pj'.dta", keep(master match) keepusing(`pj_dv') nogen

** ---- N and PL (8y): Parental IPVA (paternal and maternal -reported) -------------------------------------------------------------------
*A lot of variables - report on whether they were victimised and whether perpetrated
merge m:1 aln using "`data_n'.dta", keep(master match) keepusing(n3030-n3059) nogen
merge m:1 aln using "`data_pl'.dta", keep(master match) keepusing(pl3030-pl3059) nogen

** ---- P and PM (9y): IPVA (maternal and paternal -reported) -------------------------------------------------------------------
local p_dv "p2022 p2036"
merge m:1 aln using "`data_p'.dta", keep(master match) keepusing(`p_dv') nogen
local pm_dv "pm2022 pm2036"
merge m:1 aln using "`data_pm'.dta", keep(master match) keepusing(`pm_dv') nogen

** ---- F10 (10.5y): Friends score-------------------------------------------------------------------
local f10_friend "fdfs120"
merge m:1 aln qlet using "`data_f10'.dta", keep(master match) keepusing(`f10_friend') nogen

** ---- F11 (11y clinic) -------------------------------------------------------------------
*Friendship questions

** ---- R and PP (11y): Parental IPVA (maternal and paternal -reported) -------------------------------------------------------------------
local r_dv "r5022 r5036"
merge m:1 aln using "`data_r'.dta", keep(master match) keepusing(`r_dv') nogen
local pp_dv "pp5022 pp5036"
merge m:1 aln using "`data_pp'.dta", keep(master match) keepusing(`pp_dv') nogen

** ---- S and PQ (12y): Parental IPVA (maternal and paternal -reported) -------------------------------------------------------------------
local s_dv "s3153 s3154 s3202 s3211 s3216 s3219"
merge m:1 aln using "`data_s'.dta", keep(master match) keepusing(`s_dv') nogen
local pq_dv "pq3153 pq3154 pq3202 pq3211 pq3216 pq3219"
merge m:1 aln using "`data_pq'.dta", keep(master match) keepusing(`pq_dv') nogen

** ---- TF1 clinic (12.5y): relationships and RSB -------------------------------------------------------------------
*Friendship questions
local tf1_rel "ff5787 ff5791 ff5809 ff5813 ff5829 ff5833 ff5849 ff5853 ff5868 ff5872 ff5889 ff5893 ff5889 ff5893 ff5914 ff5918"
local tf1_sex_ever "ff5900" 
local tf1_sex_cond "ff5907 ff5908 ff5909 ff5910 ff5911 ff5912"
merge 1:1 aln qlet using "`data_tf1'.dta", keep(master match) keepusing(`tf1_rel' `tf1_sex_ever' `tf1_sex_cond') nogen

** ---- CCQ (13y): ASB -------------------------------------------------------------------
*No ASB at age 13y as too sparse a variable?
local ccq_asb "ccq651 ccq653 ccq655 ccq656 ccq657 ccq658 ccq659 ccq660 ccq661 ccq663"
merge 1:1 aln qlet using "`data_ccq'.dta", keep(master match) keepusing(`ccq_asb') nogen

** ---- TF2 clinic (13.5y): relationships, RSB and BMI (to deriving eating disorders at 16y) -------------------------------------------------------------------
*Friendship questions
local tf2_rel "fg4190"
local tf2_sex_ever "fg5320" 
local tf2_sex_cond "fg5328 fg5329 fg5330 fg5331 fg5332"
local tf2_bmi "fg3139"
merge 1:1 aln qlet using "`data_tf2'.dta", keep(master match) keepusing(`tf2_rel' `tf2_sex_ever' `tf2_sex_cond' `tf2_bmi') nogen

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

** ---- CCS (16y): relationships, depression (MFQ scores), self-harm, smoking, cannabis, illicit non-cannabis drugs, hospitalisations, eating disorder  -------------------------------------------------------------------
local ccs_rel "ccs6582c"
local ccs_dep "ccs4500 ccs4502 ccs4503 ccs4504 ccs4505 ccs4506 ccs4508 ccs4509 ccs4511 ccs4512 ccs4513 ccs4514 ccs4515"
local ccs_sh "ccs6530"
local ccs_smok "ccs4005"
local ccs_can "ccs4060 ccs4065"
local ccs_drug "ccs4150 ccs4151 ccs4152 ccs4153 ccs4154 ccs4160 ccs4161 ccs4162 ccs4163 ccs4164 ccs4165 ccs4166 ccs4167 ccs4168 ccs4169 ccs4170"
local ccs_hosp "ccs7330 ccs7340 ccs6547" 
local ccs_eat "ccs5500 ccs5501 ccs5502 ccs5503 ccs5510 ccs5511 ccs5512 ccs5513 ccs5520 ccs5530 ccs5540 ccs5541 ccs5550 ccs5560 ccs5561 ccs5562 ccs5563 ccs5564 ccs5565 ccs5570 ccs5571 ccs5572 ccs5573 ccs5580 ccs5581 ccs5582 ccs5583 ccs5590 ccs5600"
merge 1:1 aln qlet using "`data_ccs'.dta", keep(master match) keepusing(`ccs_rel' `ccs_dep' `ccs_sh' `ccs_smok' `ccs_can' `ccs_drug' `ccs_hosp' `ccs_eat') nogen

** ---- TC (16y, parent report): Eating disorder  -------------------------------------------------------------------
local tc_eat "tc5000 tc5001 tc5050 tc5070"
merge 1:1 aln qlet using "`data_tc'.dta", keep(master match) keepusing(`tc_eat') nogen

** ---- TF4 clinic (17.5y): relationships, anxiety, self-harm, hazardous alcohol use, RSB, overweight -------------------------------------------------------------------
*Friendship questions
local tf4_rel "FJPC1000 FJPC1100 FJLE160 FJLE162"
local tf4_anx "FJCI600 FJCI601"
local tf4_sh "FJCI369"
local tf4_alc "FJAL4000"
local tf4_sex_ever "FJCH700" 
local tf4_sex_cond "FJCH707"
local tf4_sex_mult "FJCH705"
local tf4_hosp "FJLE134"
local tf4_ow "FJMR022a"
local tf4_sleep "FJCI250 FJCI251 FJCI252 FJCI254"
merge 1:1 aln qlet using "`data_tf4'.dta", keep(master match) keepusing(`tf4_rel' `tf4_anx' `tf4_sh' `tf4_alc' `tf4_sex_ever' `tf4_sex_cond' `tf4_sex_mult' `tf4_hosp' `tf4_ow' `tf4_sleep') nogen

** ---- T (18y): Parental IPVA -------------------------------------------------------------------
local t_dv "t3321 t3335"
merge m:1 aln using "`data_t'.dta", keep(master match) keepusing(`t_dv') nogen

** ---- CCT (18y): depression (MFQ scores), ASB, smoking, cannabis, illicit non-cannabis drugs, hospitalisations, and NEET -------------------------------------------------------------------
*Removed drug use at 18y as 93% missing
local cct_dep "cct2700 cct2701 cct2702 cct2703 cct2704 cct2705 cct2706 cct2707 cct2708 cct2709 cct2710 cct2711 cct2712"
local cct_asb "cct6001 cct6003 cct6004 cct6005 cct6006 cct6008 cct6009 cct6010 cct6011"
local cct_smok "cct5014"
local cct_can "cct5050 cct5055"
local cct_drug "cct5102 cct5112 cct5122 cct5132 cct5152 cct5162 cct5172"
local cct_hosp "cct4163 cct4164 cct7032 cct7033"
local cct_neet "cct2950 cct3100 cct3180"
local cct_friend "cct2501"
merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(`cct_dep' `cct_asb' `cct_smok' `cct_can' `cct_drug' `cct_hosp' `cct_neet' `cct_friend') nogen

** ---- CCXC (18y): relationships -------------------------------------------------------------------
local ccxc_rel "CCXC326 CCXC338"
merge 1:1 aln qlet using "`data_ccxc'.dta", keep(master match) keepusing(`ccxc_rel') nogen

** ---- CCXD (17.5y): self-esteem -------------------------------------------------------------------
local ccxc_se "CCXD860a"
merge 1:1 aln qlet using "`data_ccxd'.dta", keep(master match) keepusing(`ccxd_se') nogen

** ---- CCU (20+y): relationships, hospitalisation, NEET, friendships -------------------------------------------------------------------
local ccu_rel "CCU4032 CCU4033 CCU4035 CCU2085i"
local ccu_hosp "CCU2072"
local ccu_neet "CCU4039 CCU4050 CCU4051 CCU4055 CCU4060 CCU4101 CCU4115"
local ccu_friend "CCU3200"
merge 1:1 aln qlet using "`data_ccu'.dta", keep(master match) keepusing(`ccu_rel' `ccu_hosp' `ccu_neet' `ccu_friend') nogen

** ---- YPA (21y): parental IPVA, relationships, RSB, NEET -------------------------------------------------------------------
local ypa_dv "YPA5040 YPA5041 YPA5042 YPA5043 YPA5044 YPA5045 YPA5046 YPA5047 YPA5048 YPA5049 YPA5050 YPA5051 YPA5052 YPA5053 YPA5054 YPA5055 YPA5056 YPA5057 YPA5058 YPA5059"
local ypa_rel "YPA3042 YPA3041"
local ypa_sexmin "YPA3000"
local ypa_neet "YPA8000"
merge 1:1 aln qlet using "`data_YPA'.dta", keep(master match) keepusing(`ypa_dv' `ypa_rel' `ypa_sexmin' `ypa_neet') nogen


** ---- YPB (22y): hazardous alcohol use -------------------------------------------------------------------
local ypb_alc "YPB4388a YPB4388b"
local ypb_can "YPB4390 YPB4400"
local ypb_drug "YPB4440 YPB4441 YPB4443 YPB4444 YPB4446 YPB4447 YPB4449 YPB4450 YPB4452 YPB4453 YPB4458 YPB4459 YPB4461 YPB4462 YPB4464 YPB4465 YPB4467"
merge 1:1 aln qlet using "`data_YPB'.dta", keep(master match) keepusing(`ypb_alc' `ypb_can' `ypb_drug') nogen


** ---- YPC (23y): PTSD, depression (MFQ scores), suicide attempt, NEET -------------------------------------------------------------------
*Friendship questions
local ypc_ptsd "YPC1850 YPC1851 YPC1852 YPC1853 YPC1854 YPC1855 YPC1856 YPC1857 YPC1858 YPC1859 YPC1860 YPC1861 YPC1862 YPC1863 YPC1864 YPC1865 YPC1866 YPC1867 YPC1868 YPC1869"

*Note that some vars, e.g. YPC1652 have been removed as they were tester questions and not part of the Short MFQ
local ypc_dep "YPC1650 YPC1651 YPC1653 YPC1654 YPC1655 YPC1656 YPC1658 YPC1659 YPC1660 YPC1662 YPC1663 YPC1665 YPC1667"

*Non-MFQ vars that may still be useful for defining depression
local ypc_dep2 "YPC1680 YPC1681 YPC1682 YPC1683 YPC1684 YPC1685 YPC1686 YPC1687 YPC1688 YPC1689"

local ypc_suic "YPC2370"

local ypc_neet "YPC2450 YPC2451 YPC2452 YPC2453 YPC2454 YPC2455 YPC2456 YPC2457 YPC2458 YPC2459 YPC2460 YPC2461"

merge 1:1 aln qlet using "`data_YPC'.dta", keep(master match) keepusing(`ypc_ptsd' `ypc_dep' `ypc_dep2' `ypc_suic' `ypc_neet') nogen


** ---- YPD (24y): suicide attempt, eating disorder -------------------------------------------------------------------
local ypd_suic "YPD1220"
local ypd_eat "YPD8010 YPD8011 YPD8012 YPD8020 YPD8030 YPD8040 YPD8060 YPD8070 YPD8071"
merge 1:1 aln qlet using "`data_YPD'.dta", keep(master match) keepusing(`ypd_suic' `ypd_eat') nogen


** ---- F24 (24y clinic): Anxiety, depression, suicidal thoughts, psychotic experiences, BMI (to help define eating disorders) -------------------------------------------------------------------
*These vars also incudes realted conditions such as social phobia
local f24_anx "FKDQ1030 FKDQ1050 FKDQ1060 FKDQ1070"
local f24_dep "FKDQ1000 FKDQ1010 FKDQ1020"
local f24_suic "FKSH1070 FKSH1080 FKSH1130 FKSH1140"
local f24_psych "FKPL2410"
local f24_bmi "FKMS1040"
local f24_sleep "FKDQ4000 FKDQ4010 FKDQ4020 FKDQ4030 FKDQ4040 FKDQ4050 FKDQ4100 FKDQ4110 FKDQ4120 FKDQ4130 FKDQ4140"
local f24_alc "FKAL1400 FKAL1410 FKAL1420 FKAL1430 FKAL1440 FKAL1450 FKAL1500"
local f24_neet "FKFR1300 FKFR1301 FKFR1302 FKFR1303 FKFR1304 FKFR1305"

*Noting that in YPB it's AUDIT, but in F24 it's AUDIT-C?
merge 1:1 aln qlet using "`data_F24'.dta", keep(master match) keepusing(`f24_anx' `f24_dep' `f24_suic' `f24_psych' `f24_bmi' `f24_sleep' `f24_alc' `f24_neet') nogen

** ---- YPE (25y): depression (MFQ scores) -------------------------------------------------------------------
local ype_dep "YPE4080 YPE4082 YPE4083 YPE4084 YPE4085 YPE4086 YPE4088 YPE4089 YPE4091 YPE4092 YPE4093 YPE4094 YPE4095"
merge 1:1 aln qlet using "`data_YPE'.dta", keep(master match) keepusing(`ype_dep') nogen


** ---- YPF (26y): suicide attempt -------------------------------------------------------------------
local ypf_suic "YPF6230"
merge 1:1 aln qlet using "`data_YPF'.dta", keep(master match) keepusing(`ypf_suic') nogen

** ---- Other: ethnicity from schools' data -------------------------------------------------------------------
merge 1:1 aln qlet using "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\other data\child_ethnicity.dta", keep(master match) keepusing(ethnicity) nogen
*One contradicting value
replace ethnicity = . if ethnicity == -1 | ethnicity == 3

** ---- ACEs to be pulled in after running Script1 and Script 2 in R -------------------------------------------------------------------
merge 1:1 aln qlet using "`ace_path'\alspacKids_ACE_0_16_AH.dta", keep(master match) keepusing(*0_16yrs*) nogen

** ---- Already-derived MFQs from Jon -------------------------------------------------------------------
merge 1:1 aln qlet using "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\other data\nine_mfqs.dta", keep(master match) keepusing(mfqtot*) nogen
*Derive MFQ at 25y:
gen mfqtot_ype = 0
foreach i of local ype_dep{
	replace mfqtot_ype = mfqtot_ype+`i' if `i'>=0 & `i'!=.
	}

** ---- SES data -------------------------------------------------------------------
*18w gest (all versions of IMD)
local imd_G0 "bimd2000q5 bimd2004q5 bimd2007q5 bimd2010q5"
merge m:1 aln using "Current\Other\Geodata\G0_IMD_1b.dta", keep(master match) keepusing(`imd_G0') nogen

*14y, 16y, 18y, ~20-22y, ~22-24y (all versions of IMD)
#delimit ;
local imd_G1 "ccrimd2000q5 ccrimd2004q5 ccrimd2007q5 ccrimd2010q5
ccsimd2000q5 ccsimd2004q5 ccsimd2007q5 ccsimd2010q5
cctimd2000q5 cctimd2004q5 cctimd2007q5 cctimd2010q5
jan2011imd2000q5_YP jan2011imd2004q5_YP jan2011imd2007_incomeq5_YP jan2011imd2010q5_YP 
jan2014imd2000q5_YP jan2014imd2004q5_YP jan2014imd2007_incomeq5_YP jan2014imd2010q5_YP";
#delimit cr
merge 1:1 aln qlet using "Current\Other\Geodata\G1_IMD_1b.dta", keep(master match) keepusing(`imd_G1') nogen


********************************************************************************
* RF variables  
********************************************************************************

** ---- Local for recoding, this is the most common system needed: -------------------------------------------------------------------
*local recode_reg "(2 = 0) (1 = 1) (-9999 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 . = 2)"
local recode_reg "(2 = 0) (1 = 1) (-9999 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 . = .)"

** ---- Relationships from 12.5y (TF1), 13.5y (TF2), 15.5y (CCXB), 15.5y (TF3), 16y (CCS), 17.5y (TF4), 18y (CCXC), 20+y (CCU), 21y (YPA) -------------------------------------------------------------------
gen rel_tf1 = .
foreach i of local tf1_rel{
	replace rel_tf1 = 1 if `i' == 1
	}
tab rel_tf1, mis matcell(rel_tf1)
*17th Feb 2020: No need to record relationship stats here, as already covered in paper 1
*putexcel set "O:\IPV project\P1 - risk factors\tables_080819", sheet("relationships") modify
*putexcel E2 = rel_tf1[1,1]

gen rel_tf2 = 1 if fg4190 == 1
replace rel_tf2 = 0 if fg4190 == 0
*tab rel_tf2, mis matcell(rel_tf2)
*putexcel E3 = rel_tf2[1,1]

gen rel_tf3 = .
foreach i of local tf3_rel{
	replace rel_tf3 = 1 if `i' == 1
	}
*tab rel_tf3, mis matcell(rel_tf3)
*putexcel E4 = rel_tf3[1,1]

gen rel_ccs = 1 if ccs6582c == 1
*tab rel_ccs, mis matcell(rel_ccs)
*putexcel E8 = rel_ccs[1,1]

gen rel_ccxb = .
foreach i of local ccxb_rel{
	replace rel_ccxb = 1 if `i' == 1
	}
*tab rel_ccxb, mis matcell(rel_ccxb)
*putexcel E5 = rel_ccxb[1,1]

gen rel_tf4 = .
replace rel_tf4 = 1 if FJPC1000 == 1
*tab rel_tf4, mis matcell(rel_tf4)
*putexcel E9 = rel_tf4[1,1]

*Questions about length of relationship
replace rel_tf4 = 1 if FJPC1100 >= 1 & FJPC1100 <= 4 | FJPC1100 == 9
gen rel_tf4_2 = 1 if FJPC1100 >= 1 & FJPC1100 <= 4 | FJPC1100 == 9
*tab rel_tf4_2, mis matcell(rel_tf4_2)
*putexcel E10 = rel_tf4_2[1,1]
*Assuming the YP wouldn't say they don't know how long if they've never had a relationship, same for FJPC1150
replace rel_tf4 = 0 if FJPC1100 == 5

*Now questions about what had happened in last year
replace rel_tf4 = 1 if FJLE160 == 1 | FJLE162 == 1
drop rel_tf4_2
gen rel_tf4_2 = 1 if FJLE160 == 1 | FJLE162 == 1
*tab rel_tf4_2, mis matcell(rel_tf4_2)
*putexcel E15 = rel_tf4_2[1,1]
drop rel_tf4_2

gen rel_ccxc = 1 if CCXC326 == 1 | CCXC338 == 1
*tab rel_ccxc, mis matcell(rel_ccxc)
*putexcel E16 = rel_ccxc[1,1]

gen rel_ccu = 1 if CCU4032 == 1 | CCU4033 == 1 | CCU4035== 1
*tab rel_ccu, mis matcell(rel_ccu)
*putexcel E21 = rel_ccu[1,1]

replace rel_ccu = 1 if CCU2085i == 1
gen rel_ccu_2 = 1 if CCU2085i == 1
*tab rel_ccu_2, mis matcell(rel_ccu_2)
*putexcel E25 = rel_ccu_2[1,1]
drop rel_ccu_2

gen rel_ypa = 1 if YPA3042 == 1 | YPA3041 == 1
*tab rel_ypa, mis matcell(rel_ypa)
*putexcel E29 = rel_ypa[1,1]

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
*replace white = 2 if white == .
label define white 1 "White" 0 "Non-White" 2 "Missing"
label values white white


** ---- Sexuality from 16y (TF3) -------------------------------------------------------------------
*17th Feb 2020: and now also from 22y (YPA) - 
recode fh9140 (-9999 -11 -10 -6 -5 -3 -2 -1 6 9 = .) (1 = 0) (2 3 4 5 = 1), gen(sex_min_tf3)
*replace sex_min_tf3 = 2 if sex_min_tf3 == .
label variable sex_min_tf3 "Sexual minority"
label define sex_min_tf3 0 "100% hetero" 1 "not 100% hetero" 2 "Not reported"
label values sex_min_tf3 sex_min_tf3

recode YPA3000 (-1 6 7 = .) (1 = 0) (2 3 4 5 = 1), gen(sex_min_ypa)
*replace sex_min_ypa = 2 if sex_min_ypa == .
label variable sex_min_ypa "Sexual minority"
label values sex_min_ypa sex_min_tf3


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
	*replace depress_`v' = 2 if depress_`v' == .
	}

/*
*Depression at ages 16-18?
gen depress_16to18 = 0 if depress_ccs == 0 | depress_cct == 0
replace depress_16to18 = 1 if depress_ccs == 1 | depress_cct == 1
*replace depress_16to18 = 2 if depress_ccs == 2 & depress_cct == 2
*/


** ---- Anxiety - pull in (teen-reported) DAWBA categorisation of anxiety from 15.5y (TF3) CIS-R at 17.5y (TF4) -------------------------------------------------------------------
recode fh6893 `recode_reg', gen(anxiety_tf3)
*gen anxiety_tf4 = 2
gen anxiety_tf4 = .
replace anxiety_tf4 = 1 if FJCI600 == 2 | FJCI600 == 4 | FJCI600 == 8
replace anxiety_tf4 = 1 if FJCI601 == 2 | FJCI601 == 4 | FJCI601 == 8
*replace anxiety_tf4 = 0 if anxiety_tf4 == 2 & (FJCI600 == 0 & FJCI601 == 0)
replace anxiety_tf4 = 0 if anxiety_tf4 == . & (FJCI600 == 0 & FJCI601 == 0)


** ---- Self-harm at 15.5y (TF3), 16y (CCS), and 17.5y (TF4) -------------------------------------------------------------------
local var "tf3 ccs tf4"
local recode_tf3 "`recode_reg'"
local recode_ccs "`recode_reg'"
local recode_tf4 "(1 = 0) (2 = 1) (-999 -11 -10 -4 -1 = 2)"
foreach v of local var{
	*gen sh_`v' = 2
	gen sh_`v' = .
	foreach i of local `v'_sh{
		recode `i' `recode_`v'', gen(`v'2)
		*replace sh_`v' = 0 if `v'2 == 0  & sh_`v' == 2
		replace sh_`v' = 0 if `v'2 == 0  & sh_`v' == .
		replace sh_`v' = 1 if `v'2 == 1
		}
	drop `v'2
	tab sh_`v'
	}

	
** ---- Friendships at 10.5y (F10), 18y (CCT) -------------------------------------------------------------------
** ---- 20y (CCU - but Q actually about 18-21y) -------------------------------------------------------------------

*10.5y
*0 denotes most positive friend score, 15 the least
label list fdfs120
summ fdfs120 if fdfs120 >= 0 & fdfs120 != ., det
/*
                   Friends score: F&S: F10
-------------------------------------------------------------
      Percentiles      Smallest
 1%            0              0
 5%            0              0
10%            1              0       Obs               2,709
25%            2              0       Sum of Wgt.       2,709

50%            3                      Mean           3.032484
                        Largest       Std. Dev.        2.0872
75%            4             13
90%            6             14       Variance       4.356404
95%            7             14       Skewness       1.103085
99%           10             15       Kurtosis        5.10188
*/

recode fdfs120 (-9/-1 = .) (0/3 = 1) (4/15 = 0), gen(pos_friend_f10)

*18y
label list cct2501
*Note that numbers don't correspond, e.g. 1 = None, 5 = 6-9
tab cct2501 if cct2501 >= 0 & cct2501 <= 6

recode cct2501 (-10/-1 7 8 = .) (1/2 = 0) (3/6 = 1), gen(friends_cct)

*20y
label list CCU3200
summ CCU3200 if CCU3200 >= 0 & CCU3200 != ., det
*Still haven't decided what to do with this, do more friends mean better?
/*Also:
CCU4030	E2a: Lives with father/stepfather (including mother's partner)
CCU4031	E2b: Lives with mother/stepmother (including father's partner)
CCU4032	E2c: Lives with partner's mother
CCU4033	E2d: Lives with partner's father
CCU4034	E2e: Lives with brothers or sisters
CCU4035	E2f: Lives with husband, wife or partner
CCU4036	E2g: Lives with their own child/children
CCU4037	E2h: Lives with any other relatives
CCU4038	E2i: Lives with friends/housemates
CCU4039	E2j: Lives in halls of residence
CCU4040	E2k: Lives with anyone else they have not told us about already
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccu_2b.dta", keepusing(CCU4030-CCU4040) keep(master match) nogenerate
About 30 odd live with partner's mother, 30 with partner's dad, 270 with husband wife or partner, 75 on their own/with children
~1/3 with friends/housemates, 1/3 with parent(s)
*/

/*YPC - 23y:
YPC0550	c10a: Degree to which respondent feels they are free to decide for themselves how to live their life
YPC0551	c10b: Degree to which respondent really likes the people they interact with
YPC0554	c10e: Degree to which people I know tell me I am good at what I do
YPC0555	c10f: Degree to which respondent gets along with people they come into contact with
YPC0556	c10g: Degree to which respondent pretty much keeps to themselves and doesn't have a lot of social contacts
YPC0558	c10i: Degree to which respondent considers the people they regularly interact with to be their friends
YPC0561	c10l: Degree to which people in respondent's life care about them
YPC0563	c10n: Degree to which people respondent interacts with on a daily basis tend to take their feelings into consideration
YPC0565	c10p: Degree to which there are not many people that respondent is close to
YPC0567	c10r: Degree to which the people respondent interacts with regularly do not seem to like them much
YPC0570	c10u: Degree to which people are generally pretty friendly towards respondent
YPC0594	c12e: Respondent enjoys their friends a lot
*/

/*YPD - 24y
YPD3040	C5: YP has difficulty making friends, even when trying their best
*/

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
	*gen smok_`v' = 2
	gen smok_`v' = .
	cap replace smok_ccs = 0 if ccs4000 == 2
	cap replace smok_cct = 0 if cct5000 == 2
	foreach i of local `v'_smok{
		recode `i' `recode_`v'', gen(`i'2)
		*replace smok_`v' = 0 if `i'2 == 0 & smok_`v' == 2
		replace smok_`v' = 0 if `i'2 == 0 & smok_`v' == .
		replace smok_`v' = 1 if `i'2 == 1
		drop `i'2
		}
	}


** ---- *Alcohol 17.5y (TF4) -------------------------------------------------------------------
*Could have used CCS (i.e. 16y) - Audit score of 8 or more (Kipping 2014), 
*but not readily available in ALSPAC directly, just says so in paper and Jon's powerpoint re: measures 
*local var_ccs "CCU3095"
*local recode_ccs "(1 2 3 = 0) (4 5 6 = 1) (-9999 -11 -10 -1 = 2)"

*gen hazalc_tf4 = 2
gen hazalc_tf4 = .
*replace hazalc_tf4 = 0 if FJAL4000 >=0 & FJAL4000 <= 7 & hazalc_tf4 == 2
replace hazalc_tf4 = 0 if FJAL4000 >=0 & FJAL4000 <= 7 & hazalc_tf4 == .
replace hazalc_tf4 = 1 if FJAL4000 >=8 & FJAL4000 != .


** ---- Cannabis at 16y (CCS), 18y (CCT) -------------------------------------------------------------------
gen cann_ccs = .
replace cann_ccs = 0 if ccs4060 == 2
replace cann_ccs = 0 if ccs4065 >= 1 & ccs4065 <= 3
replace cann_ccs = 1 if ccs4065 >= 4 & ccs4065 <= 6

gen cann_cct = .
replace cann_cct = 0 if cct5050 == 2
replace cann_cct = 0 if cct5055 >= 1 & cct5055 <= 3
replace cann_cct = 1 if cct5055 >= 4 & cct5055 <= 6


** ---- Ilicit drugs at 16y (CCS) and 18y (CCT) -------------------------------------------------------------------
local var "ccs cct"
local recode_ccs "(1 2 = 0) (3 = 1) (-9999 -11 -10 -1 = 2)"
local recode_cct "`recode_reg'"

foreach v of local var{
	*gen drug_`v' = 2
	gen drug_`v' = .
	foreach i of local `v'_drug{
		recode `i' `recode_`v'', gen(`i'2)
		*replace drug_`v' = 0 if `i'2 == 0 & drug_`v' == 2
		replace drug_`v' = 0 if `i'2 == 0 & drug_`v' == .
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
*gen ext_patmon = 2 if patmon_score == . | patmon_score < 0
gen ext_patmon = . if patmon_score == . | patmon_score < 0
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

** ---- NEET status at 18y (CCT), 20+y (CCU), 21y (YPA), 23+y (YPC), F24 (24y) -------------------------------------------------------------------	
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

*YPA
recode YPA8000 (-1 = .) (1 = 0) (2 = 1), gen(neet_ypa)

*YPC
gen neet_ypc = .
#delimit  ;
replace neet_ypc = 0 if YPC2450 == 1 | YPC2451 == 1 | YPC2452 == 1 | YPC2453 == 1 | 
YPC2456 == 1 | YPC2458 == 1 | 
(YPC2460 == 1 & (YPC2461 == 3 | YPC2461 == 4 | YPC2461 == 6 | YPC2461 == 7 | YPC2461 == 10 | YPC2461 == 15));

replace neet_ypc = 1 if YPC2450 == 0 & YPC2451 == 0 & YPC2451 == 0 & YPC2451 == 0 & 
YPC2456 == 0 & YPC2458 == 0;
#delimit cr

*F24
recode FKFR1305 (-9999/-1 = .) (0 = 0) (1 = 1), gen(neet_f24)

#delimit  ;
replace neet_f24 = 0 if (FKFR1300 == 1 | FKFR1301 == 1 | FKFR1302 == 1 | FKFR1303 == 1 | 
FKFR1304 == 1 | YPC2458 == 1);

replace neet_f24 = 1 if FKFR1300 == 0 & FKFR1301 == 0 & FKFR1302 == 0 & FKFR1303 == 0 & 
FKFR1304 == 0 & YPC2458 == 0 & neet_f24 == .;
#delimit cr

********************************************************************************
* RFs for depression
********************************************************************************

** ---- Overweight at 17.5y (TF4) ----------------------------------------------------------	
gen ow_tf4 = .
replace ow_tf4 = 0 if FJMR022a >= 0 & FJMR022a <= 24
replace ow_tf4 = 1 if FJMR022a >= 25 & FJMR022a != .

** ---- Self-esteem at 17.5y (CCXD) ----------------------------------------------------------	
gen lowselfest_ccxd = 1
replace lowselfest_ccxd = . if CCXD860a < 0
replace lowselfest_ccxd = 0 if CCXD860a > 29 & CCXD860a != .
*Khambati et al, 2018
*"Positive adaptation was demonstrated by scores greater than the 
*median of the non-maltreated ALSPAC cohort (29 for the RSE-B)."


** ---- Eating disorder at 16y (ccs) ----------------------------------------------------------	
*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4515576/

/*Anorexia nervosa
(1.Underweight (BMI<18.5) at age 14 AND (at age 16:)
2.engaged in fasting for weight loss or to avoid weight gain at least monthly
OR engaged in excessive exercise)*/
gen an_ccs = .
#delimit ;
*Child report
replace an_ccs = 1 if (fg3139 >= 0 & fg3139 <= 18.5) & 
((ccs5520 >= 3 & ccs5520 <= 5) | 
((ccs5512 >= 1 & ccs5512 <= 2) & 
((ccs5511 >= 1 & ccs5511 <= 2) | (ccs5513 >= 1 & ccs5513 <= 2))));
/*Parental report
(1.Underweight (BMI<18.5) AND
2.Presence of fear of weight gain AND 
fat avoidance in the 3 months prior to assessment*/
gen bmi_tc = tc5001/((tc5000/100)^2) if (tc5001 > 0 & tc5000 > 0);
replace an_ccs = 1 if (bmi_tc >= 0 & bmi_tc <= 18.5) & 
(tc5050 >= 2 & tc5050 <= 4) &
(tc5070 >= 2 & tc5070 <= 4);

replace an_ccs = 0 if ((fg3139 > 18.5 & fg3139 != .) | 
((ccs5520 >= 0 & ccs5520 <= 1) & 
(ccs5512 == 3 & ccs5511 == 3 & ccs5513 == 3))) &
((bmi_tc > 18.5 & bmi_tc != .) |
(tc5050 == 1) |
(tc5070 == 1));
#delimit cr

/*Binge eating
(1.Weekly binge eating
AND
2.At least 3 cognitive symptoms (eating fast or faster than normal; 
eating until stomach hurt or they felt sick, 
eating large amounts when not hungry, eating alone, 
feeling guilty about amount eaten)
AND
3.Absence of purging)*/
gen be_ccs = .
gen besum_ccs = 0
local cog "ccs5561 ccs5562 ccs5563 ccs5564 ccs5565"
foreach i of local cog{
	replace besum_ccs = besum_ccs+1 if `i' >= 1 & `i' <= 2
	}
#delimit ;
replace be_ccs = 1 if (ccs5550 >= 3 & ccs5550 <= 4) &
(besum_ccs >= 3 & besum_ccs != .) &
(ccs5530 <= 1 | ccs5530 == .) & (ccs5540 != 1 & ccs5540 != 2);
replace be_ccs = 0 if (ccs5550 == 1 | ccs5550 == 2 | ccs5550 == 5) |
(besum_ccs <= 2 | besum_ccs == .) |
(ccs5530 >= 2 & ccs5530 <= 6) | (ccs5540 == 1 | ccs5540 == 2);
#delimit cr

/*Sub-BE
(1.monthly binge eating
AND
2.At least 3 cognitive symptoms
AND
3.Absence of purging)*/
gen subbe_ccs = .
#delimit ;
replace subbe_ccs = 1 if ((ccs5550 >= 2 & ccs5550 <= 4) &
(besum_ccs >= 3 & besum_ccs != .) &
(ccs5530 <= 1 | ccs5530 == .) & (ccs5540 != 1 & ccs5540 != 2)) &
be_ccs != 1;
replace subbe_ccs = 0 if ((ccs5550 == 1 | ccs5550 == 5) |
(besum_ccs <= 2 | besum_ccs == .) |
(ccs5530 >= 2 & ccs5530 <= 6) | (ccs5540 == 1 | ccs5540 == 2)) |
be_ccs == 1;
#delimit cr

/*Bulimia nervosa
(1.Weekly binge eating
AND
2.weekly purging)*/
gen bn_ccs = .
#delimit ;
replace bn_ccs = 1 if (ccs5550 >= 3 & ccs5550 <= 4) &
((ccs5530 >= 4 & ccs5530 <= 6) | (ccs5541 >= 4 & ccs5541 <= 6));
replace bn_ccs = 0 if (ccs5550 == 1 | ccs5550 == 2 | ccs5550 == 5) |
((ccs5530 >= 1 & ccs5530 <= 3) & (ccs5541 >= 1 & ccs5541 <= 3));
#delimit cr

/*Sub-BN
(1.monthly binge eating
AND
2.monthly purging)*/
gen subbn_ccs = .
#delimit ;
replace subbn_ccs = 1 if ((ccs5550 >= 2 & ccs5550 <= 4) &
((ccs5530 >= 3 & ccs5530 <= 6) | (ccs5541 >= 3 & ccs5541 <= 6))) &
bn_ccs != 1;
replace subbn_ccs = 0 if ((ccs5550 == 1 | ccs5550 == 5) |
((ccs5530 >= 1 & ccs5530 <= 2) & (ccs5541 >= 1 & ccs5541 <= 2))) |
bn_ccs == 1;
#delimit cr

/*Purging disorder
(1.Purging at least weekly
AND
2.Binge eating absent or < monthly)*/
gen purge_ccs = .
#delimit ;
replace purge_ccs = 1 if ((ccs5530 >= 4 & ccs5530 <= 6) | (ccs5541 >= 4 & ccs5541 <= 6)) &
(ccs5550 != 2 & ccs5550 != 3 & ccs5550 != 4);
replace purge_ccs = 0 if ((ccs5530 >= 1 & ccs5530 <= 3) & (ccs5541 >= 1 & ccs5541 <= 3)) |
(ccs5550 >= 2 & ccs5550 <= 4);
#delimit cr

*OFSED2 - < monthly bingeing, purging, excessive exercise, fasting, respectively
*OFSED1 is as above, but at least monthly
gen ofsed2_ccs = .
#delimit ;
replace ofsed2_ccs = 1 if 
(ccs5550 >= 1 & ccs5550 <= 4) |
(ccs5530 >= 2 & ccs5550 <= 6) |
(ccs5541 >= 2 & ccs5541 <= 6) |
((ccs5512 >= 1 & ccs5512 <= 2) & 
((ccs5511 >= 1 & ccs5511 <= 2) | (ccs5513 >= 1 & ccs5513 <= 2))) |
(ccs5520 >= 2 & ccs5520 <= 5);

replace ofsed2_ccs = 0 if 
(ccs5550 == 5) &
(ccs5530 == 1) &
(ccs5541 == 1) &
((ccs5512 == 3) | 
((ccs5511 == 3) & (ccs5513 != 1 & ccs5513 != 2))) &
(ccs5520 == 1);
#delimit cr

*Global any eating disorder
gen eatd_ccs = 1 if an_ccs == 1 | be_ccs == 1 | subbe_ccs == 1 | bn_ccs == 1 | subbn_ccs == 1 | purge_ccs == 1 | ofsed2_ccs == 1
replace eatd_ccs = 0 if an_ccs == 0 & be_ccs == 0 & subbe_ccs == 0 & bn_ccs == 0 & subbn_ccs == 0 & purge_ccs == 0 & ofsed2_ccs == 0

** ---- Sleep problems at 17y (TF4) ----------------------------------------------------------	
gen sleep_tf4 = 1 if FJCI250 >= 2 & FJCI250 <= 4
replace sleep_tf4 = 0 if FJCI250 >= 0 & FJCI250 <= 1

** ---- Parental education at baseline' ----------------------------------------------------------	
*Alexa: At least one parent with O-level qualification
gen pareduc = .
replace pareduc = 1 if (c645 == 3 | c645 == 4 | c645 == 5 | pb325 == 3 | pb325 == 4 | pb325 == 5)
replace pareduc = 0 if (c645 == 1 & c645 == 2) & (pb325 == 1 | pb325 == 2)

** ---- Parental separation (already have as part of ACEs, also covers parents living in household) ----------------------------------------------------------	


********************************************************************************
* Mental Health outcomes (P5)
********************************************************************************

** ---- PTSD at 23y (YPC) ----------------------------------------------------------	
*Looking at the questionnaire build, this has been paid for by Stan Zammit.  At the moment, these questions don't appear
*to have been operationalised, as until now all ALSPAC PTSD studies have used the DAWBA, but these questions don't match up with that...


** ---- Anxiety at 24y (F24) ----------------------------------------------------------	
gen anxiety_f24 = .
replace anxiety_f24 = 1 if FKDQ1030 == 1 | FKDQ1050 == 1 | FKDQ1060 == 1 | FKDQ1070 == 1
replace anxiety_f24 = 0 if FKDQ1030 == 0 & FKDQ1050 == 0 & FKDQ1060 == 0 & FKDQ1070 == 0


** ---- Depression at 23y (YPC) and 24y (F24) ----------------------------------------------------------	
foreach i of local ypc_dep{
	recode `i' (1 = 0) (2 = 1) (3 = 2) (-9999 -11 -10 -8 -7 -6 -5 -2 -1 = .), gen(mfqscore_`i')
	}
egen mfq_total_ypc = rowtotal(mfqscore*), mis
gen depress_ypc = 1 if mfq_total_ypc > 11 & mfq_total_ypc != .
replace depress_ypc = 0 if depress_ypc == . & mfq_total_ypc != .
drop mfqscore*
tab depress_ypc, mis
	
gen depress_f24 = .
replace depress_f24 = 1 if FKDQ1000 == 1 | FKDQ1010 == 1 | FKDQ1020 == 1
replace depress_f24 = 0 if FKDQ1000 == 0 & FKDQ1010 == 0 & FKDQ1020 == 0


** ---- Attempted suicide and suicidal thoughts by 26y ----------------------------------------------------------	
local suic "YPC2370 YPD1220 YPF6230"
foreach i of local suic{
	recode `i' (-9999 -11 -10 -9 -8 -1 = .) (0 = 0) (1 2 3 4 = 1), gen(suic_`i')
	}
gen suic_FKSH1070 = 1 if FKSH1070 == 2 & FKSH1080 == 1
replace suic_FKSH1070 = 0 if (FKSH1070 == 2 & FKSH1080 == 3) | (FKSH1070 == 0 | FKSH1070 == 1)

gen suicatt_26 = 1 if suic_YPC2370 == 1 | suic_YPD1220 == 1 | suic_FKSH1070 == 1 | suic_YPF6230 == 1
replace suicatt_26 = 0 if suic_YPC2370 == 0 & suic_YPD1220 == 0 & suic_FKSH1070 == 0 & suic_YPF6230 == 0

*Suicidal thoughts
gen suic_FKSH1130 = 1 if (FKSH1130 == 1 | FKSH1130 == 2) & (FKSH1140 == 1 | FKSH1140 == 2)
replace suic_FKSH1130 = 0 if ((FKSH1130 == 1 | FKSH1130 == 2) & FKSH1140 == 3) | (FKSH1130 == 0)

gen suicth_26 = suicatt_26
replace suicth_26 = 1 if suic_FKSH1130 == 1


** ---- Psychosis at 24y (F24) ----------------------------------------------------------	
recode FKPL2410 (-9999 -99 -11 -10 -9 -7 -2 -1 = .) (0 = 0) (1 = 1), gen(psych_f24)


** ---- Eating disorder at 24y (YPD; BMI from F24) ----------------------------------------------------------	
*Doing this quite a long way around, but helps me understand all the defintions being used... (Micali et al 2015)
*Anorexia nervosa
gen an_ypd = 1 if (FKMS1040 >= 0 & FKMS1040 <= 18.5) & ((YPD8020 >= 2 & YPD8020 <= 4) | ((YPD8010 >= 2 & YPD8010 <= 4) & ((YPD8011 >= 1 & YPD8011 <= 2) | (YPD8012 >= 1 & YPD8012 <= 2))))
replace an_ypd = 0 if (FKMS1040 > 18.5 & FKMS1040 != .) | (YPD8010 >= 0 & YPD8010 <= 1) | ((YPD8011 == 0) & (YPD8012 == 0))

*Bulimia nervosa
gen bn_ypd = 1 if ((YPD8070 >= 3 & YPD8070 <= 4) & (YPD8060 >= 1 & YPD8060 <= 2)) & ((YPD8030 >= 3 & YPD8030 <= 4) | (YPD8040 >= 3 & YPD8040 <= 4))
replace bn_ypd = 0 if ((YPD8070 >= 0 & YPD8070 <= 2) | (YPD8060 == 0)) | ((YPD8030 >= 0 & YPD8030 <= 2) & (YPD8040 >= 0 & YPD8040 <= 2))

gen subbn_ypd = 1 if ((YPD8070 >= 2 & YPD8070 <= 4) & (YPD8060 >= 1 & YPD8060 <= 2)) & ((YPD8030 >= 2 & YPD8030 <= 4) | (YPD8040 >= 2 & YPD8040 <= 4))
replace subbn_ypd = 0 if ((YPD8070 >= 0 & YPD8070 <= 1) | (YPD8060 == 0)) | ((YPD8030 >= 0 & YPD8030 <= 1) & (YPD8040 >= 0 & YPD8040 <= 1))

*Purging disorder
gen purge_ypd = 1 if ((YPD8030 >= 3 & YPD8030 <= 4) | (YPD8040 >= 3 & YPD8040 <= 4)) & ((YPD8070 >= 0 & YPD8070 <= 1) | (YPD8060 == 0))
replace purge_ypd = 0 if ((YPD8030 >= 0 & YPD8030 <= 2) & (YPD8040 >= 0 & YPD8040 <= 2)) | ((YPD8070 >= 2 & YPD8070 <= 4) & (YPD8060 >= 1 & YPD8060 <= 2))

*OFSED - monthly bingeing, purging, excessive exercise, fasting, respectively
#delimit ;
gen ofsed_ypd = 1 if 
((YPD8070 >= 2 & YPD8070 <= 4) & (YPD8060 >= 1 & YPD8060 <= 2)) | 
((YPD8030 >= 2 & YPD8030 <= 4) | (YPD8040 >= 2 & YPD8040 <= 4)) |
((YPD8010 >= 2 & YPD8010 <= 4) & ((YPD8011 >= 1 & YPD8011 <= 2) | (YPD8012 >= 1 & YPD8012 <= 2))) |
(YPD8020 >= 2 & YPD8020 <= 4);

replace ofsed_ypd = 0 if 
((YPD8070 >= 0 & YPD8070 <= 1) | (YPD8060 == 0)) &
((YPD8030 >= 0 & YPD8030 <= 1) & (YPD8040 >= 0 & YPD8040 <= 1)) &
((YPD8010 >= 0 & YPD8010 <= 1) | ((YPD8011 == 0) & (YPD8012 == 0))) &
(YPD8020 >= 0 & YPD8020 <= 1);
#delimit cr

*Global any eating disorder
gen eatd_ypd = 1 if an_ypd == 1 | bn_ypd == 1 | subbn_ypd == 1 | purge_ypd == 1 | ofsed_ypd == 1
replace eatd_ypd = 0 if an_ypd == 0 & bn_ypd == 0 & subbn_ypd == 0 & purge_ypd == 0 & ofsed_ypd == 0


** ---- Insomnia at 24y (F24) ----------------------------------------------------------	
local f24_sleep "FKDQ4000 FKDQ4010 FKDQ4020 FKDQ4030 FKDQ4040 FKDQ4050 FKDQ4100 FKDQ4110 FKDQ4120 FKDQ4130 FKDQ4140"


** ---- Alcohol misuse at 22y (YPB) and 24y (F24) ----------------------------------------------------------	
recode YPB4388b (-9999 -11 -10 -9 -2 = .) (1 = 0) (2 3 4 = 1), gen(hazalc_ypb)

gen hazalc_f24 = 1 if FKAL1410 == 1 | FKAL1430 == 1 | (FKAL1450 >= 1 & FKAL1450 <= 4) | (FKAL1500 >= 4 & FKAL1500 <= 12)
replace hazalc_f24 = 0 if FKAL1410 == 0 & FKAL1430 == 0 & FKAL1450 == 0 & (FKAL1500 >= 0 & FKAL1500 <= 3)


** ---- Cannabis at 22y (YPB) -------------------------------------------------------------------
gen cann_ypb = .
replace cann_ypb = 0 if YPB4390 == 2
replace cann_ypb = 0 if YPB4400 == 1 | YPB4400 == 2 | YPB4400 == 3
replace cann_ypb = 1 if YPB4400 == 4 | YPB4400 == 5 | YPB4400 == 6


** ---- Ilicit drugs at 22y (YPB) -------------------------------------------------------------------
gen drug_ypb = .
replace drug_ypb = 0 if YPB4440 == 2 & YPB4443 == 2 & YPB4446 == 2 & YPB4449 == 2 & YPB4452 == 2 & YPB4458 == 2 & YPB4461 == 2 & YPB4464 == 2
replace drug_ypb = 1 if YPB4441 == 1 | YPB4444 == 1 | YPB4447 == 1 | YPB4450 == 1 | YPB4453 == 1 | YPB4459 == 1 | YPB4462 == 1 | YPB4465 == 1


********************************************************************************
* Parental DV
********************************************************************************

** ---- Parental IPVA at 18 & 32 weeks gestations (B, PB, C and PC), 
*8 weeks (E), 8 months (F and PD), 1.75y (G and PE), 2.75y (H and PF), 
*4y (J and PG), 5y (K and PH), 6y (L and PJ), 8y (N and PL), 
*9y (P and PM), 11y (R and PP), 12y (S and PQ), 18y (T), 21y (YPA) ---
gen mum_dv = .
gen mum_edv = .
gen mum_pdv = .
gen part_dv = .
gen part_edv = .
gen part_pdv = .
local var "b607 e437 k4036 l4036"
foreach v of local var{
	replace mum_edv=1 if `v'>=1 & `v'<=4
	}
local var "b592 e422 k4022 l4022"
foreach v of local var{
	replace mum_pdv=1 if `v'>=1 & `v'<=4
	}
local var "pf5035 pg3035 ph4036"
foreach v of local var{
	replace part_edv=1 if `v'>=1 & `v'<=4
	}
local var "pf5022 pg3022 ph4022"
foreach v of local var{
	replace part_pdv=1 if `v'>=1 & `v'<=4
	}
*Leave out code for questionnaire C atm - things that happened 
*sexually to mum before they were 16

*N and PL at 8y and S and PQ at 12y are interesting ones... 
*will share this with the wider group

*Note that in PM is given as 'Father's wife/partner was...'

local var "f256a g336a h246a j336a"
foreach v of local var{
	replace mum_edv=1 if `v'==1
	}
local var "f242a g322a h232a j322a"
foreach v of local var{
	replace mum_pdv=1 if `v'==1
	}
local var "pb196a pc235a pd256a pe336a"
foreach v of local var{
	replace part_edv=1 if `v'==1
	}
local var "pb182a pc222a pd242a pe322a"
foreach v of local var{
	replace part_pdv=1 if `v'==1
	}
	
local var "p2036 r5036"
foreach v of local var{
	replace mum_edv=1 if `v'>=1 & `v'<=3
	}
local var "p2022 r5022"
foreach v of local var{
	replace mum_pdv=1 if `v'>=1 & `v'<=3
	}
local var "pm2036 pp5036"
foreach v of local var{
	replace part_edv=1 if `v'>=1 & `v'<=3
	}
local var "pm2022 pp5022"
foreach v of local var{
	replace part_pdv=1 if `v'>=1 & `v'<=3
	}	

*Leave out code for questionnaire T and YPA (and FoF) atm 
*- things that happened to either mum or partner after G1 turned 18
/*
*t3321 t3335
local var "YPA5040 YPA5045 YPA5050 YPA5055"
foreach v of local var{
	replace mum_dv=1 if `v'>=2 & `v'<=4
	}
*/
replace mum_dv = 1 if mum_edv == 1 | mum_pdv == 1
replace part_dv = 1 if part_edv == 1 | part_pdv == 1
local var "mum_dv mum_edv mum_pdv part_dv part_edv part_pdv"
foreach v of local var{
	tab `v', m
	}
tab mum_edv mum_pdv
tab part_edv part_pdv
*So before 18, about 25% mum dv, 24% emotional, 8% physical, 7% (227/3280) both
*20% partner dv, 17% emotional, 7% physical, 5% (152/3280) both


********************************************************************************
* Save data as 'IPVA_cohort' in our shared directory so I can run analyses directly.  
********************************************************************************

*Education only available via anonymous data
#delimit ;
keep aln qlet kz021_new
YPA0001
cohort
vic* per* impact* 
YPA5020 YPA5021 YPA5022 YPA5024 YPA5025 YPA5028 YPA5029 YPA5023 YPA5026 YPA5027
`var_vic' `var_vic_age' `var_per' `var_per_age'
rel_* ethnic sex_min* 
`var_ace_c' ace_c ace_no *patmon* 
depress* mfq* anxiety* sh* 
pos_friend* asb*
smok* FJAL4000 hazalc* cann* drug*
rsb hosp neet*
lowselfest* ow* eatd* sleep* pareduc*
suic* *_ypd psych*
YPB4388b
parent* *imd* *0_16*;
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

save "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\cohort\ALSPAC_age21.dta", replace	
keep if cohort == 1
drop cohort
save "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\cohort\current_IPVA_cohort_id.dta", replace
export excel using "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\cohort\current_IPVA_cohort_id.xls", replace firstrow(varlabels)

desc, det
