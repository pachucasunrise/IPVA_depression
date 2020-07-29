********************************************************************************
** Project: RFs for IPVA
** Script purpose: Tidying up dataset received from ALSPAC (anonymous, includes 
** linked IMD and education data
** Date: 22.10.19
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

*The data we sent for linkage was already for those who answered at least one of the IPVA questions (n = 3280 at this point)
use "W:\data\cohort\anonymous_linked_18Oct19.dta", clear

*One individual where sex missing (n = 3279)
keep if kz021 != .

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

/*
#delimit ;
merge 1:1 aln qlet using "`data_YPA'.dta", keep(match) keepusing(YPA9020 /*age at completion*/ `var_vic' `var_per' 
	`var_vic_age' `var_per_age' `impact_neg' `impact_neu' `impact_pos') nogen;
#delimit cr
*/

*numlabel, add force
* 1 = Never, 2 = Once, 3 = A few times, 4 = Often 
local cat "vic_emo_co vic_emo_sh vic_phys1 vic_phys2 vic_sex_co1 vic_sex_co2 vic_sex_fo1 vic_sex_fo2 per_emo_co per_emo_sh per_phys per_sex"
local age "021 017 1821"
foreach c of local cat{
	foreach a of local age{
		cap gen `c'_`a' = 0
		cap gen `c'_`a'_th1 = 0
		cap gen `c'_`a'_th2 = 0
		foreach v of local var_`c'{
			*Different thresholds
			local crit_021_lo `v' >= 2 & `v' <= 4
			local crit_017_lo `v' >= 2 & `v' <= 4 & (`var_`c'_age' == 1  | `var_`c'_age' == 3)
			local crit_1821_lo `v' >= 2 & `v' <= 4 & (`var_`c'_age' == 2  | `var_`c'_age' == 3)
			
			local crit_021_th1 `v' >= 3 & `v' <= 4
			local crit_017_th1 `v' >= 3 & `v' <= 4 & (`var_`c'_age' == 1  | `var_`c'_age' == 3)
			local crit_1821_th1 `v' >= 3 & `v' <= 4 & (`var_`c'_age' == 2  | `var_`c'_age' == 3)

			local crit_021_th2 `v' == 4
			local crit_017_th2 `v' == 4 & (`var_`c'_age' == 1  | `var_`c'_age' == 3)
			local crit_1821_th2 `v' == 4 & (`var_`c'_age' == 2  | `var_`c'_age' == 3)
			
			cap replace `c'_`a' = 1 if `crit_`a'_lo'
			cap replace `c'_`a'_th1 = 1 if `crit_`a'_th1'
			cap replace `c'_`a'_th2 = 1 if `crit_`a'_th2'
			}
		}
	}

*For those that have two variables representing the concept...
local cat "vic_phys vic_sex_co vic_sex_fo"
local age "021 017 1821"
foreach c of local cat{
	foreach a of local age{
		cap gen `c'_`a' = 0
		cap gen `c'_`a'_th1 = 0
		cap gen `c'_`a'_th2 = 0

		forvalues i = 1/2{
			replace `c'_`a' = 1 if `c'`i'_`a' == 1
			replace `c'_`a'_th1 = 1 if `c'`i'_`a'_th1 == 1
			replace `c'_`a'_th2 = 1 if `c'`i'_`a'_th2 == 1
			}
		}
	}

*Or more global vic or per
local age "021 017 1821"
local type "vic per"
foreach a of local age{
	*No need for per, already sorted
	cap gen vic_sex_`a' = 0
	cap gen vic_sex_`a'_th1 = 0
	cap gen vic_sex_`a'_th2 = 0
		
	replace vic_sex_`a' = 1 if vic_sex_co_`a' == 1 | vic_sex_fo_`a' == 1
	replace vic_sex_`a' = 1 if vic_sex_co_`a'_th1 == 1 | vic_sex_fo_`a'_th1 == 1
	replace vic_sex_`a' = 1 if vic_sex_co_`a'_th2 == 1 | vic_sex_fo_`a'_th2 == 1
	
	foreach t of local type{
		cap gen `t'_emo_`a' = 0
		cap gen `t'_emo_`a'_th1 = 0
		cap gen `t'_emo_`a'_th2 = 0

		replace `t'_emo_`a' = 1 if `t'_emo_co_`a' == 1 | `t'_emo_sh_`a' == 1
		replace `t'_emo_`a'_th1 = 1 if `t'_emo_co_`a'_th1 == 1 | `t'_emo_sh_`a'_th1 == 1
		replace `t'_emo_`a'_th2 = 1 if `t'_emo_co_`a'_th2 == 1 | `t'_emo_sh_`a'_th2 == 1
		
		cap gen `t'_`a' = 0
		cap gen `t'_`a'_th1 = 0
		cap gen `t'_`a'_th2 = 0

		replace `t'_`a' = 1 if `t'_emo_`a' == 1 | `t'_phys_`a' == 1 | `t'_sex_`a' == 1
		replace `t'_`a'_th1 = 1 if `t'_emo_`a'_th1 == 1 | `t'_phys_`a'_th1 == 1 | `t'_sex_`a'_th1 == 1
		replace `t'_`a'_th2 = 1 if `t'_emo_`a'_th2 == 1 | `t'_phys_`a'_th2 == 1 | `t'_sex_`a'_th2 == 1
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
	putexcel set "O:\IPV project\P1 - risk factors\results\impact", sheet("Numbers `i'") modify

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
*No need now anon ids, but keeping relevant locals

** ---- TF1 (12.5y): relationships and RSB -------------------------------------------------------------------
local tf1_rel "ff5787 ff5791 ff5809 ff5813 ff5829 ff5833 ff5849 ff5853 ff5868 ff5872 ff5889 ff5893 ff5889 ff5893 ff5914 ff5918"
local tf1_sex_ever "ff5900" 
local tf1_sex_cond "ff5907 ff5908 ff5909 ff5910 ff5911 ff5912"
*merge 1:1 aln qlet using "`data_tf1'.dta", keep(master match) keepusing(`tf1_rel' `tf1_sex_ever' `tf1_sex_cond') nogen

** ---- CCQ (13y): ASB -------------------------------------------------------------------
*No ASB at age 13y as too sparse a variable?
local ccq_asb "ccq651 ccq653 ccq655 ccq656 ccq657 ccq658 ccq659 ccq660 ccq661 ccq663"
*merge 1:1 aln qlet using "`data_ccq'.dta", keep(master match) keepusing(`ccq_asb') nogen

** ---- TF2 (13.5y): relationships and RSB -------------------------------------------------------------------
local tf2_rel "fg4190"
local tf2_sex_ever "fg5320" 
local tf2_sex_cond "fg5328 fg5329 fg5330 fg5331 fg5332"
*merge 1:1 aln qlet using "`data_tf2'.dta", keep(master match) keepusing(`tf2_rel' `tf2_sex_ever' `tf2_sex_cond') nogen

** ---- TF3 (15.5y): relationships, sexual minority, anxiety, RSB, parental monitoring, and hospitalisations -------------------------------------------------------------------
local tf3_rel "fh8931 fh8932 fh8952 fh8953 fh8973 fh8974 fh9013 fh9014 fh9075 fh9076 fh9116 fh9118"
local tf3_sexmin "fh9140"
local tf3_anx "fh6893 fh6894"
local tf3_sh "fh6481"
local tf3_sex_ever "fh9100" 
local tf3_sex_cond "fh9109 fh9110"
local tf3_patmon "fh8200 fh8201 fh8202 fh8203"
local ts3_hosp "fh4563 fh4663"
*merge 1:1 aln qlet using "`data_tf3'.dta", keep(master match) keepusing(`tf3_rel' `tf3_sexmin' `tf3_anx' `tf3_sh' `tf3_sex_ever' `tf3_sex_cond' `tf3_patmon' `tf3_hosp') nogen

** ---- CCXB (15.5y): relationships -------------------------------------------------------------------
local ccxb_rel "ccxb171 ccxb271 ccxb371 ccxb471 ccxb571"
*merge 1:1 aln qlet using "`data_ccxb'.dta", keep(master match) keepusing(`ccxb_rel') nogen

** ---- CCS (16y): relationships, depression (MFQ scores), self-harm, smoking, cannabis, illicit non-cannabis drugs, and hospitalisations  -------------------------------------------------------------------
local ccs_rel "ccs6582c"
local ccs_dep "ccs4500 ccs4502 ccs4503 ccs4504 ccs4505 ccs4506 ccs4508 ccs4509 ccs4511 ccs4512 ccs4513 ccs4514 ccs4515"
local ccs_sh "ccs6530"
local ccs_smok "ccs4005"
local ccs_can "ccs4065"
local ccs_drug "ccs4150 ccs4151 ccs4152 ccs4153 ccs4154 ccs4160 ccs4161 ccs4162 ccs4163 ccs4164 ccs4165 ccs4166 ccs4167 ccs4168 ccs4169 ccs4170"
local ccs_hosp "ccs7330 ccs7340 ccs6547" 
*merge 1:1 aln qlet using "`data_ccs'.dta", keep(master match) keepusing(`ccs_rel' `ccs_dep' `ccs_sh' `ccs_smok' `ccs_can' `ccs_drug' `ccs_hosp') nogen

** ---- TF4 (17.5y): relationships, anxiety, self-harm, hazardous alcohol use, and RSB -------------------------------------------------------------------
local tf4_rel "FJPC1000 FJPC1100 FJLE160 FJLE162"
local tf4_anx "FJCI600 FJCI601"
local tf4_sh "FJCI369"
local tf4_alc "FJAL4000"
local tf4_sex_ever "FJCH700" 
local tf4_sex_cond "FJCH707"
local tf4_sex_mult "FJCH705"
local tf4_hosp "FJLE134"
*merge 1:1 aln qlet using "`data_tf4'.dta", keep(master match) keepusing(`tf4_rel' `tf4_anx' `tf4_sh' `tf4_alc' `tf4_sex_ever' `tf4_sex_cond' `tf4_sex_mult' `tf4_hosp') nogen

** ---- CCT (18y): depression (MFQ scores), ASB, smoking, cannabis, illicit non-cannabis drugs, hospitalisations, and NEET -------------------------------------------------------------------
*Removed drug use at 18y as 93% missing
local cct_dep "cct2700 cct2701 cct2702 cct2703 cct2704 cct2705 cct2706 cct2707 cct2708 cct2709 cct2710 cct2711 cct2712"
local cct_asb "cct6001 cct6003 cct6004 cct6005 cct6006 cct6008 cct6009 cct6010 cct6011"
local cct_smok "cct5014"
local cct_can "cct5055"
local cct_drug "cct5100 cct5102 cct5110 cct5112 cct5120 cct5122 cct5130 cct5132 cct5150 cct5152 cct5160 cct5162 cct5170 cct5172"
local cct_hosp "cct4163 cct4164 cct7032 cct7033"
local cct_neet "cct2950 cct3100 cct3180"
*merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(`cct_dep' `cct_asb' `cct_smok' `cct_can' `cct_drug' `cct_hosp' `cct_neet') nogen

** ---- CCXC (18y): relationships -------------------------------------------------------------------
local ccxc_rel "CCXC326 CCXC338"
*merge 1:1 aln qlet using "`data_ccxc'.dta", keep(master match) keepusing(`ccxc_rel') nogen

** ---- CCU (20+y): relationships -------------------------------------------------------------------
local ccu_rel "CCU4032 CCU4033 CCU4035 CCU2085i"
local ccu_hosp "CCU2072"
local ccu_neet "CCU4039 CCU4050 CCU4051 CCU4055 CCU4060 CCU4101 CCU4115"
*merge 1:1 aln qlet using "`data_ccu'.dta", keep(master match) keepusing(`ccu_rel' `ccu_hosp' `ccu_neet') nogen

** ---- YPA (21y): parental IPVA, relationships and RSB -------------------------------------------------------------------
local ypa_dv "YPA5040 YPA5041 YPA5042 YPA5043 YPA5044 YPA5045 YPA5046 YPA5047 YPA5048 YPA5049 YPA5050 YPA5051 YPA5052 YPA5053 YPA5054 YPA5055 YPA5056 YPA5057 YPA5058 YPA5059"
local ypa_rel "YPA3042 YPA3041"
local ypa_sex "YPA1011"
*merge 1:1 aln qlet using "`data_YPA'.dta", keep(match) keepusing(`ypa_dv' `ypa_rel' `ypa_sex') nogen

** ---- Other: ethnicity from schools' data -------------------------------------------------------------------
*merge 1:1 aln qlet using "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\other data\child_ethnicity.dta", keep(master match) keepusing(ethnicity) nogen
*One contradicting value
replace ethnicity = . if ethnicity == -1 | ethnicity == 3

** ---- ACEs to be pulled in by Amanda Hill -------------------------------------------------------------------
*merge 1:1 aln qlet using "`ace_path'\alspacKids_ACE_0_16.dta", keep(master match) keepusing(*0_16yrs*) nogen
*/

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
gen rel_tf2 = 1 if fg4190 == 1
replace rel_tf2 = 0 if fg4190 == 0
gen rel_tf3 = .
foreach i of local tf3_rel{
	replace rel_tf3 = 1 if `i' == 1
	}
gen rel_ccs = 1 if ccs6582c == 1
gen rel_ccxb = .
foreach i of local ccxb_rel{
	replace rel_ccxb = 1 if `i' == 1
	}
gen rel_tf4 = .
replace rel_tf4 = 1 if FJPC1000 == 1
*Questions about length of relationship
replace rel_tf4 = 1 if FJPC1100 >= 1 & FJPC1100 <= 4 | FJPC1100 == 9
gen rel_tf4_2 = 1 if FJPC1100 >= 1 & FJPC1100 <= 4 | FJPC1100 == 9
*Assuming the YP wouldn't say they don't know how long if they've never had a relationship, same for FJPC1150
replace rel_tf4 = 0 if FJPC1100 == 5
*Now questions about what had happened in last year
replace rel_tf4 = 1 if FJLE160 == 1 | FJLE162 == 1
drop rel_tf4_2
gen rel_tf4_2 = 1 if FJLE160 == 1 | FJLE162 == 1
drop rel_tf4_2
gen rel_ccxc = 1 if CCXC326 == 1 | CCXC338 == 1
gen rel_ccu = 1 if CCU4032 == 1 | CCU4033 == 1 | CCU4035== 1
replace rel_ccu = 1 if CCU2085i == 1
gen rel_ccu_2 = 1 if CCU2085i == 1
drop rel_ccu_2
gen rel_ypa = 1 if YPA3042 == 1 | YPA3041 == 1

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

/*
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
*/

** ---- Depression - pulling in clinic MFQ scores from 16.5 (ccs) and 18.5 (cct) years old -------------------------------------------------------------------
local var "ccs cct"
foreach v of local var{
	foreach i of local `v'_dep{
		recode `i' (3 = 0) (2 = 1) (1 = 3) (-9999 -11 -10 -7 -6 -5 -2 -1 = .), gen(mfqscore_`i')
		}
	egen mfq_total_`v' = rowtotal(mfqscore*), mis
	drop mfqscore_*
	}
*We then derive depression later after imputing scores


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
	*merge 1:1 aln qlet using "`data_ccs'.dta", keep(master match) keepusing(ccs4000) nogen
	*merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(cct5000) nogen
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
	*merge 1:1 aln qlet using "`data_ccs'.dta", keep(master match) keepusing(ccs4060) nogen
	*merge 1:1 aln qlet using "`data_cct'.dta", keep(master match) keepusing(cct5050) nogen
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
gen drug_ccs = 2
foreach i of local ccs_drug{
	recode `i' (1 2 = 0) (3 = 1) (-9999 -11 -10 -1 = 2), gen(`i'2)
	replace drug_ccs = 0 if `i'2 == 0 & drug_ccs == 2
	replace drug_ccs = 1 if `i'2 == 1
	drop `i'2
	}
tab drug_ccs, m


local cct_drug "cct510 cct511 cct512 cct513 cct515 cct516 cct517"
*cct_drug elements+0 = drug ever, cct_drug elements+2 = drug in past 3 months
gen drug_cct = 2
foreach i of local cct_drug{
	recode `i'2 `recode_reg', gen(`i'2_v2)
	replace drug_cct = 0 if `i'0 == 2 & drug_cct == 2
	replace drug_cct = 0 if `i'2_v2 == 0 & drug_cct == 2
	replace drug_cct = 1 if `i'2_v2 == 1
	drop `i'2
	}
tab drug_cct, m


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


** ---- Education variables - KS3 score (13-14y) and KS4 5x A*-C grades (age 16-ish) --------------------------
recode k3_totps (-10 = .), gen(score_13)
recode ks4_fiveac (-10 = .), gen(gcse_16)


** ---- IMD variables - 2010 quintile (21y) -------------------------------------------------------------------	
recode ypaimd2010q5 (-9999 -10 = .), gen(imd_2010)

/*
** ---- Parental IPVA at 18 & 32 weeks gestations (B and C), 8 weeks (E), 8 months (F), -----------------------
** ---- 1.75y (G), 2.75y (H), 4y (J), 5y (K), 6y (L), 9y (P), 11y (R), 18y (T), 21y (YPA) ---------------------
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
*/

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
local var "asb_ccq asb_cct score_13 gcse_16 rsb hosp imd_2010"
foreach i of local var{
	cap gen `i'_fi = `i'
	*replace `i'_fi = 0 if `i' == .
	}

*Hazardous alcohol score
recode FJAL4000 (-9999 -11 -10 -1 = .), gen(FJAL4000_fi)

*Replace rsb_fi and hosp_fi with 0 if missing so isn't dropped from imputation later.
replace rsb_fi = 0 if rsb_fi == .
replace hosp_fi = 0 if hosp_fi == .

*Export out everything we'll need for imputation and after...
/*
#delimit ;
keep /*aln*/ qlet 
kz021 
YPA*
vic* per* impact*
rel*
imd_2010_fi ethnic sex_min patmon_score 
mfq_total_ccs mfq_total_cct anxiety_tf3_fi anxiety_tf4_fi sh_ccs_fi sh_tf4_fi 
asb_ccq_fi asb_cct_fi smok_ccs_fi smok_cct_fi FJAL4000_fi cann_ccs_fi cann_cct_fi 
drug_ccs_fi drug_cct_fi rsb_fi hosp_fi score_13_fi gcse_16_fi neet_cct_fi neet_ccu_fi;
#delimit cr
*/

*Need to rename a few variables because they're not recognised in Lotte's ACE R scripts:
local var "FJAL4000 YPA5*"
foreach v of local var{
	cap rename `v'_A `v'
	}

*numlabel, remove
	
save "`data_path'\ipva_cohort_for_imp_anon.dta", replace
export excel using "`data_path'\ipva_cohort_for_imp_anon.xlsx", replace firstrow(var)
