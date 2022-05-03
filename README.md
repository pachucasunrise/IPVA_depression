*28.01.20

*annie.herbert@bristol.ac.uk

These are R scripts to prepare and analyse data for the manuscript 'Establishing the causal role of intimate partner violence and abuse on depressive symptoms in young adults: a population-based cohort study'

Running everything for depression paper from beginning to end
- data_prep_for_imp takes 'current_IPVA_cohort_id.dta' and updates the 
list of auxilliary vars so none used to derive ACES that have sparse levels 
(n<50) are used and where possible binary versions are used instead (as per 
Lotte's ACE scripts). Also recodes some vars where needed, and creates lists
of covariates (e.g. cov_list) that will be useful both in imputation and 
analysis. Creates specific pred_mat and mice objects to go with each of the
different datasets (by girls and boys: those where MFQ is kept raw, MFQ is 
logged, normal long format, DID format, etc.). This is done on everyone, as
datasets will be restricted by vic at 0-17 (one of the conditions of the 
main analysis) and relationship status by age 17 (one of our sensitivity/
generalisability analyses), later. At the end just tests how long one 
imptuation is likely to take. Produces R object 'data_for_imputation.RData'.

- data_prep_for_analysis.R takes the original data, and the mids objects 
resulting from running imputation.R in Blue Crystal (HPC), 
manipulates certain vars, and re-saves. E.g. main analysis datasets (so 
removes those who have been exposed to IPVA by age 17 and specific datasets 
for sensitivity/generalisability analyses such as taking those who have 
definitely been in a romantic relationship by age 17.

- analysis.R then runs all analyses needed for the paper, producing excel 
files that get pasted into tables.xls to produce Tables 1-5 and Supp Tables 
S1-S5, as well as figures 1, 2, S1, and S2 (all of these outputs are in the 
'P3 - Mental health/results' folder).
Requires R functions saved as:
	- 'iptw_mi.R' (runs inverse probability treatment weighting on MI 
	data, and outputs estimates and average propensity scores to a 
	table)
	- 'did_mi.R' (runs DID analysis on MI data when exposure is binary
	(e.g. vic vs. no vic), and outputs estimates to a table)
	- 'did_cca.R' (runs DID analysis on normal data frame, and outputs 
	estimates to a table)
	- 'did_mi2.R' (runs DID analysis on MI data when exposure is 
	categorical (not binary; e.g. vic subtypes), and outputs estimates 
	to a table)

May 2022:
This study has now been published in BMC Medicine: https://bmcmedicine.biomedcentral.com/articles/10.1186/s12916-021-02182-3


