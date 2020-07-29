*26.03.20
*Checking age of completion

use "W:\data\cohort\current_IPVA_cohort_id.dta"

*IPV q
merge 1:1 aln qlet using "R:\Data\Current\Quest\Child Completed\YPA_r1a.dta", keep(master match) nogenerate
gen YPA9020_y = trunc(YPA9020/12)
summ YPA9020_y if YPA9020 >= 0, det

*YPC (23+)
merge 1:1 aln qlet using "R:\Data\Current\Quest\Child Completed\YPC_2a.dta", keep(master match) nogen
gen YPC2650_y = trunc(YPC2650/12)
summ YPC2650_y if YPC2650 >= 0, det

*F24 (24y clinic)
merge 1:1 aln qlet using "R:\Data\Current\Clinic\Child\F24_4a.dta", keep(master match) nogen
summ FKAR0011 if FKAR0011 >= 0, det
