*17.12.19
*Checking gender vs. sex in ALSPAC (IPVA cohort)

*'Year 11' (do they mean 11 years old?)
use "\\rdsfcifs.acrc.bris.ac.uk\MRC-IEU-research\projects\ieu2\p6\046\working\data\cohort\cohort_forlinkage_1019", replace
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccj_r1b.dta", keepusing(ccj900) keep(master match) nogenerate
tab kz021 ccj900, m

/*
                      |                  C1: Gender of child
                  Sex | Not compl  No respon        Boy       Girl          . |     Total
----------------------+-------------------------------------------------------+----------
              1. Male |       125          1      1,021          0          1 |     1,148 
            2. Female |       232          1          0      1,890          6 |     2,129 
                    . |         1          0          1          1          0 |         3 
----------------------+-------------------------------------------------------+----------
                Total |       358          2      1,022      1,891          7 |     3,280 
*/

*12 years old
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccl_r1b.dta", keep(master match) keepusing(ccl900) nogenerate
tab kz021 ccl900, m

/*
                      |                   D1: Child's gender
                  Sex | Not compl  Not state        Boy       Girl          . |     Total
----------------------+-------------------------------------------------------+----------
              1. Male |       116          0      1,031          1          0 |     1,148 
            2. Female |       266          1          0      1,858          4 |     2,129 
                    . |         1          0          1          1          0 |         3 
----------------------+-------------------------------------------------------+----------
                Total |       383          1      1,032      1,860          4 |     3,280
*/

*13 years old
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccm_r1c.dta", keep(master match) keepusing(ccm900) nogenerate
tab kz021 ccm900, m
/*
                      |                   D1: Child's gender
                  Sex | Not compl  Not state        Boy       Girl          . |     Total
----------------------+-------------------------------------------------------+----------
              1. Male |       126          1      1,021          0          0 |     1,148 
            2. Female |       232          2          0      1,891          4 |     2,129 
                    . |         1          0          1          1          0 |         3 
----------------------+-------------------------------------------------------+----------
                Total |       359          3      1,022      1,892          4 |     3,280
*/

*CCN - also 13 years old
merge 1:1 aln qlet using "\\ads.bris.ac.uk\folders\Health Sciences\SSCM ALSPAC\Data\Current\Quest\Child Completed\ccn_r1c.dta", keep(master match) keepusing(ccn900) nogenerate
tab kz021 ccn900, m

/*
                      |             D1: Child's gender
                  Sex | Not compl       Male     Female          . |     Total
----------------------+--------------------------------------------+----------
              1. Male |       126      1,022          0          0 |     1,148 
            2. Female |       240          0      1,885          4 |     2,129 
                    . |         1          1          1          0 |         3 
----------------------+--------------------------------------------+----------
                Total |       367      1,023      1,886          4 |     3,280 

*/
