set more off 	
pause off
set logtype text
set mem 500M

set scheme s1color , permanently

*************** DESCRIPTION ***************************************************
* Generates results for section V.D: investment behavior of highly rated firms
*
*	Inputs:  3.Final_data/main_dataset_firm_BEA
*	Outputs: Figures and Tables included in paper, numbered as in the final 
*			 paper and stored in corresponding folders
* 
*******************************************************************************   


******************** DRILL DOWN: SCARCITY OF SAFE ASSETS ********************
 
*** Data preparation
use "Data/Final/main_dataset_firm_BEA", clear

*** Data preparation ***
destring gvkey, replace
drop if gvkey==.
xtset gvkey year
drop temp

g temp = AAtoAAA if year == 2006
egen AAtoAAA0 = min(temp), by(gvkey)
drop temp
label define lab4 0 "A and below" 1 "AA to AAA" 
label values AAtoAAA0 lab4

* TABLE 13: High ratings and MV, PPE and Assets
egen indgroup = group(indcode)
reg logmv AAtoAAA0 logage l8.logmv l8.logat if year == 2014
est store t1
reg logmv AAtoAAA0 logage l8.logmv l8.logat i.indgroup if year == 2014
est store t2

reg logppe AAtoAAA0 logage l8.logmv l8.logat if year == 2014
est store t3
reg logppe AAtoAAA0 logage l8.logmv l8.logat i.indgroup if year == 2014
est store t4

reg logat AAtoAAA0 logage l8.logmv l8.logat if year == 2014
est store t5
reg logat AAtoAAA0 logage l8.logmv l8.logat i.indgroup if year == 2014
est store t6


esttab t1 t2 t3 t4 t5 t6 using Tables/13_Tab_AAA_reg.tex, stats(N r2) t(%8.2f) b(%9.3f) br nogap star(+ 0.10 * 0.05 ** .01) drop(*indgr*)


g nikrd = (capx + xrd - dp_used1)/l.at

* Compute mean and median quantities, by rating
foreach X in q ik1 nik1 nik2 nikrd osk bba bbos paya payos dlogemp dlogppe cfat nblev txtoi dlogsale txdba defat diat eiat dfpct efpct cdat invdefat dwcat dincfat{
egen a1rmed_`X' = median(`X'),by(AAtoAAA0 year)
egen a1rm_`X' = mean(`X'),by(AAtoAAA0 year)

label variable a1rmed_`X' "Median `X' - by industry"
label variable a1rm_`X' "Mean `X' - by industry"
}

* Assetss, PPE and investment
g dlogat = logat-l.logat
egen a1rm_dlogat = mean(dlogat),by(AAtoAAA0 year) 
egen a1r_at = sum(at),by(AAtoAAA0 year) missing
egen a1r_ppe = sum(ppent),by(AAtoAAA0 year) missing
egen a1r_capx = sum(capx),by(AAtoAAA0 year) missing
egen a1r_xrd = sum(capx),by(AAtoAAA0 year) missing
egen a1r_dp1 = sum(dp_used1),by(AAtoAAA0 year) missing
g a1r_logat = log(a1r_at)
g a1r_logppe = log(a1r_ppe)

* Use of financing
egen a1r_ndebtiss = sum(ndebtiss),by(AAtoAAA0 year) missing
egen a1r_neqiss = sum(neqiss),by(AAtoAAA0 year) missing
egen a1r_dv = sum(dv),by(AAtoAAA0 year) missing
egen a1r_findef = sum(findef) , by(AAtoAAA0 year) missing
egen a1r_inv_def = sum(inv_def) , by(AAtoAAA0 year) missing
egen a1r_dnwc_def = sum(dnwc_def) , by(AAtoAAA0 year) missing
egen a1r_incf_def = sum(incf_def),by(AAtoAAA0 year) missing

sort year AAtoAAA0
bys year AAtoAAA0: keep if _n == 1
xtset AAtoAAA0 year

g a1r_nik = (a1r_capx-a1r_dp1)/l.a1r_ppe if a1_count > 10
g a1r_nikrd = (a1r_capx+a1r_xrd-a1r_dp1)/l.a1r_ppe if a1_count > 10
g a1r_dlogat = d.a1r_logat
g a1r_dlogppe = d.a1r_logppe
g a1r_defat = a1r_findef/a1r_at
g a1r_diat = a1r_ndebtiss/a1r_at
g a1r_eiat = a1r_neqiss/a1r_at
g a1r_divat = a1r_dv/a1r_at
g a1r_invdefat = a1r_inv_def/a1r_at
g a1r_dwcat = a1r_dnwc_def/a1r_at
g a1r_cdat = a1r_dv/a1r_at
g a1r_dincfat = a1r_incf_def/a1r_at

*** Analyses

* FIGURE 14: Assets and Investment
xtline a1rm_dlogat if year >= 1970 & year <= 2015, xlab(1970(5)2015) overlay yti("") ti("Average Log-change in Total Assets")  plot1(lp(shortdash))
graph export Figures/14a_dLogat_ByRating.eps, as(eps) mag(150) replace 

xtline a1rm_nikrd if year >= 1970 & year <= 2015, xlab(1970(5)2015) overlay yti("") ti("Average NI/K, inc R&D") plot1(lp(shortdash))
graph export Figures/14b_NIK_ByRating.eps, as(eps) mag(150) replace

sort AAtoAAA0 year

g a1rma_defat = (a1r_findef+l.a1r_findef)/(a1r_at+l.a1r_at)
g a1rma_diat = (a1r_ndebtiss+l.a1r_ndebtiss)/(a1r_at+l.a1r_at)
g a1rma_eiat = (a1r_neqiss+l.a1r_neqiss)/(a1r_at+l.a1r_at)

label variable a1rma_defat "Financing deficit"
label variable a1rma_diat "Debt Issuance"
label variable a1rma_eiat "Equity Issuance"

xtline a1rma_defat a1rma_diat a1rma_eiat if year >= 1970 & year <= 2015, lpattern(solid shortdash dash_dot) lw(medium medium medthick) xlab(1970(10)2015) xline(1982) byopts(note(""))
graph export Figures/15a_AggIssByRating.eps, as(eps) mag(150) replace

* Financing source 
g a1r_extfin = a1r_diat + a1r_eiat

label variable a1r_cdat "Dividends"
label variable a1r_invdefat "Investment"
label variable a1r_dwcat "dWC"
label variable a1r_dincfat "Internal Cash Flow"
label variable a1r_extfin "Net external financing"

* TABLE 14: Financing uses table
capture log close
log using Tables/14_Tab_AAA_funding, replace 
tabstat a1r_cdat a1r_invdefat a1r_dwcat a1r_dincfat a1r_defat a1r_diat a1r_eiat a1r_extfin if year >=1971 & AAtoAAA0 == 1, s(mean) f("%8.3f") by(yeargroups) not
tabstat a1r_cdat a1r_invdefat a1r_dwcat a1r_dincfat a1r_defat a1r_diat a1r_eiat a1r_extfin if year >=1971 & AAtoAAA0 == 0, s(mean) f("%8.3f") by(yeargroups) not
log close
