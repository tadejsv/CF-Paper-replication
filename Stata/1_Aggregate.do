set more off 	
pause off
set logtype text
set mem 500M

set scheme s1color , permanently


*************** DESCRIPTION ***************************************************
* Generates aggregate plots and regression results. 
*
*	Inputs:  3.Final_data/main_dataset_ind_BEA
*	Outputs: Figures and Tables included in paper, numbered as in the final 
*			 paper and stored in corresponding folders
*
*******************************************************************************   

use "Data/Final/main_dataset_ind_BEA", clear
drop if year >= 2016
egen indgroup = group(indcode)
xtset indgroup year
sort indgr year


/* ---------------------------- */
/* 	  Agg Investment Figures 	*/
/* ---------------------------- */

*Capital and depr
g krc_nfcb = a_equip_nfcb + a_intell_nfcb + krc_res_nfcb + krc_nonres_nfcb 
g krc_nfncb = a_equip_nfncb + a_intell_nfncb + krc_res_nfncb + krc_nonres_nfncb
g krc_nfb = krc_nfcb + krc_nfncb

* value added
g y_nfb = y_nfcb + y_nfncb
g dep_nfb = dep_nfcb + dep_nfncb
g i_nfb = i_nfcb + i_nfncb
g wn_nfb = wn_nfcb + wn_nfncb
g taxprod_nfb = taxprod_nfcb + taxprod_nfncb
g ni_nfb = i_nfb-dep_nfb
		
g depk_nfcb = dep_nfcb/l.krc_nfcb
g depk_nfncb = dep_nfncb/l.krc_nfncb
g depk_nfb = dep_nfb/l.krc_nfb
g depk = dep_nonres_priv/l.krc_nonres_priv

* Profitability
g nos_nfb = nos_nfcb+nos_nfncb

* OS/K
g osk_nfcb = nos_nfcb/l.krc_nfcb
g osk_nfncb = nos_nfncb/l.krc_nfncb
g osk_nfb = nos_nfb/l.krc_nfb

* INVESTMENT
g ik = i_nonres/l.krc_nonres_priv
g ik_nfcb  = i_nfcb/l.krc_nfcb
g ik_nfncb  = i_nfncb/l.krc_nfncb
g ik_nfb  = i_nfb/l.krc_nfb

g nik = ik-depk
g nik_nfcb = ik_nfcb-depk_nfcb
g nik_nfncb = ik_nfncb-depk_nfncb
g nik_nfb = ik_nfb-depk_nfb

label variable osk_nfcb "Non Financial Corporate"
label variable osk_nfncb "Non Financial Non Corporate"
label variable osk_nfb "Non Financial Business"

label variable ik_nfcb "Gross I/K"
label variable ik_nfncb "Gross I/K"
label variable ik_nfb "Gross I/K"

label variable nik_nfcb "Net I/K"
label variable nik_nfncb "Net I/K"
label variable nik_nfb "Net I/K"

label variable depk_nfcb  "Depreciation/K"
label variable depk_nfncb  "Depreciation/K"
label variable depk_nfb  "Depreciation/K"


/* ---------------------*/
/* 		PROFITABILITY	*/
/* ---------------------*/

*** MAIN BODY ***
g igos = i_nfb/(nos_nfb + dep_nfb)
g nios = (i_nfb-dep_nfb)/nos_nfb
label variable nios "Net Investment over Operating Surplus"

g nios_histo = 0.2 if year<2001
replace nios_histo = 0.1 if year>2001

* Figure 1: Net Investment Relative to Net Operating Surplus
line nios nios_histo year if year>=1960 & indgroup == 1, xlab(1960(10)2010) legend(on order(1 "NI/NOS" 2 "Mean") c(2) size(small)) 
graph export Figures/1_nios.eps, as(eps) mag(150) replace

*** APPENDIX ***
* Table 18: Current Account of Non financial Sector
capture log close
log using Tables/18_Tab_curracc, replace t
foreach X in nfcb nfncb nfb  {	
	g os_temp = y_`X' - dep_`X' - wn_`X' - taxprod_`X'
	g ni_temp = i_`X' - dep_`X' 
	tabstat y_`X' krc_`X' dep_`X' os_temp i_`X' ni_temp if year == 2014 & indgroup == 1 , c(s) f(%9.1f)
	drop os_temp ni_temp	
}
log close

* Figure 21: Net Operating Return, by Sector
scatter osk_nfcb osk_nfncb osk_nfb year if year>=1970 & indgroup ==1, xlab(1970(10)2010) leg(size(small)) c(l l l) ms(i Oh o) 
graph export Figures/21_OSK.eps, as(eps) mag(150) replace 


***


/* ---------------------*/
/* 	Stock and bond Q 	*/
/* ---------------------*/

* Comparison charts
g kmv_nfcb = a_nonfin_nfcb - a_inventories_nfcb

* Q 
g mvo_nfcb = mve_nfcb + l_tot_nfcb - a_inventories_nfcb - a_fin_nfcb 
g mvomisc_nfcb = mvo_nfcb + a_misc_nfcb - l_misc_nfcb

g Q_nfcb = mvo_nfcb/krc_nfcb
g Qmisc = mvomisc_nfcb/krc_nfcb
g niv_nfcb = (i_nfcb-dep_nfcb)/mvo_nfcb

label variable nik_nfb "Net I/K - Nonfin Business"
label variable nik_nfcb "Net I/K - Nonfin Corp"
label variable Qmisc "Stock Q (misc) - Nonfin Corp"
label variable Q_nfcb "Stock Q - Nonfin Corp"
label variable a_q "Compustat Q"
label variable amed_q "Median Compustat Q"
label variable a_nik_all "BEA NI/K"

line Q_nfcb year if year>1960 & indgroup ==1, lpattern(solid shortdash) ytitle("Stock Q", axis(1)) xlab(1960(10)2010) leg(size(small)) 
graph export Figures/2_Q.eps, as(eps) mag(150) replace


***


/* --------- */
/* 	NI vs Q  */
/* --------- */


*** FIGURES ***
sort indgroup year

* Cumulative gap chart
reg nik_nfb l.Q_nfcb if indgroup == 1 & year >= 1990 & year <= 2001
predict pred_Q
predict res_Q, res

sort indgroup year
by indgroup: gen cum_gap=sum(res_Q) if year >= 1990

label variable nik_nfb "Net Investment"
label variable res_Q "Residual"
label variable cum_gap "Cumulative gap"

line nik_nfb pred_Q year if year>=1990 & indgroup ==1, lpattern(solid shortdash) xlab(1990(5)2015) yti("NI/K") ti("Net investment (actual and predicted with Q)") leg(size(med))  
graph export Figures/3a_NIQ.eps, as(eps) mag(150) replace

line cum_gap res_Q year if year>=1990 & indgroup ==1, yline(0.0, lpattern(shortdash)) lpattern(solid shortdash)  xlab(1990(5)2015) leg(size(med)) yti("Regression residuals") ti("Prediction residuals (by period and cumulative)")  c(l l l) ms(i Oh o)
graph export Figures/3b_NIQ_res.eps, as(eps) mag(150) replace

drop pred_Q res_Q cum_gap


*** REGRESSION TABLES ***


* Use BEA and Compustat data for consistency with rest of results
egen amherf  = mean(mherf), by(year)
egen aherf  = mean(herf_s), by(year)
egen amedmherf  = median(mherf), by(year)
egen amedherf  = median(herf_s), by(year)
egen aCon  = mean(a1_cpcon8_sale), by(year)

qui{
reg nik_nfb l.a_q if indgroup == 1 & year >= 1980
quiet est store Q1
reg nik_nfb l.a_q l.amedherf  if indgroup == 1 & year >= 1980
quiet est store Q2
reg nik_nfb l.a_q l.amedherf l.am_owntotQIX if indgroup == 1 & year >= 1980
quiet est store Q3

reg nik_nfb l.a_q if indgroup == 1 & year >= 1990
quiet est store Q4
reg nik_nfb l.a_q l.amedherf  if indgroup == 1 & year >= 1990
quiet est store Q5
reg nik_nfb l.a_q l.amedherf l.am_owntotQIX if indgroup == 1 & year >= 1990 
quiet est store Q6
}

* Table 3: Non Financial Business: Net Investment Regressions
esttab Q1 Q2 Q3 Q4 Q5 Q6 using Tables/3_Tab_agg_nik.tex, replace label stats(N r2) t(%8.2f) b(%9.3f) br nocons nogap star(+ 0.10 * 0.05 ** .01)  ///
mtitles(">1980 nik" ">1980 nik" ">1980 nik" ">1990 nik" ">1990 nik" ">1990 nik" )
pause

qui{
* Moving Average
arima nik_nfb l.a_q if indgroup == 1 & year >= 1980, ma(1 2)
quiet est store Q1_ma
arima nik_nfb l.a_q l.amedherf if indgroup == 1 & year >= 1980, ma(1 2)
quiet est store Q2_ma
arima nik_nfb l.a_q l.amedherf l.am_owntotQIX if indgroup == 1 & year >= 1980, ma(1 2)
quiet est store Q3_ma

arima nik_nfb l.a_q if indgroup == 1 & year >= 1990, ma(1 2)
quiet est store Q4_ma
arima nik_nfb l.a_q l.amedherf if indgroup == 1 & year >= 1990, ma(1 2)
quiet est store Q5_ma
arima nik_nfb l.a_q l.amedherf l.am_owntotQIX if indgroup == 1 & year >= 1990, ma(1 2)
quiet est store Q6_ma
}

* Table 18: Moving Avg. Regressions 

esttab Q1_ma Q2_ma Q3_ma Q4_ma Q5_ma Q6_ma using Tables/20_Tab_agg_nik_MA.tex, replace label stats(N ll) t(%8.2f) b(%9.3f) br nocons nogap star(+ 0.10 * 0.05 ** .01)  ///
mtitles(">1980 nik" ">1980 nik" ">1980 nik" ">1990 nik"  ">1990 nik" ">1990 nik" )
pause


***

/* --------- */
/* 	  ENTRY  */
/* --------- */

label variable a_entry_bds "Entry rate (Census)"
label variable a_exit_bds "Exit rate (Census)"
label variable a_entryr "Entry rate (CPSTAT)"
label variable a_exitr "Exit rate (CPSTAT)"

scatter a_entry_bds a_exit_bds year if year>=1980 & indgroup ==1, xlab(1980(5)2015)  c(l l l) ms(i Oh o)
graph export Figures/4_CensusEntry.eps, mag(150) as(eps) replace



***

/* ------------------------- */
/* 	  PAYOUTS AND OWNERSHIP  */
/* ------------------------- */

label variable a_paya "Payouts/Assets"
label variable a_bba "Buybacks/Assets"

scatter a_paya a_bba year if indgroup==1 & year >=1980, xlab(1980(5)2015 ) t1("Share Buybacks and Payouts") yti("") xline(1982)  c(l l l) ms(i Oh o) 
graph export Figures/5a_paya.eps, as(eps) mag(150) replace

label variable am_pctinsown "All institutions"
label variable am_owntotQIX "Quasi-Indexer"
label variable am_owntotDED "Dedicated"
label variable am_owntotTRA "Transient"

scatter am_pctinsown am_owntotQIX am_owntotDED am_owntotTRA year if indgroup==1 & year >=1981, xlab(1980(5)2015) t1("Average share of institutional ownership, by type") yti("") c(l l l l) ms(i Oh o Sh)
graph export Figures/5b_pctins.eps, as(eps) mag(150) replace


***


/* -----------------------------------------*/
/* 	 	REPRESENTATIVENESS OF COMPUSTAT 	*/
/* -----------------------------------------*/

* NET INVESTMENT RATE
g nominv_nik1 = a1_nik1 * l.a1_kp_all
g nominv_nik2 = a1_nik2 * l.a1_kp_all
egen totinv_nik1  = sum(nominv_nik1), by(year)
egen totinv_nik2  = sum(nominv_nik2), by(year)
g agg_nik1 = totinv_nik1 / l.a_kp_all
g agg_nik2 = totinv_nik2 / l.a_kp_all

label variable agg_nik1 "Compustat NI/K"
label variable agg_nik2 "SCF LT Inv / (FA - Intan)"
label variable am_q "Mean Q"

* Figure 6: Comparison of Compustat and BEA net investment rates
twoway (line a_nik_all year) (line agg_nik1 year, lpattern(shortdash) yaxis(2)) if year>1970 & indgroup ==1, xlab(1970(10)2010) leg(size(small))
graph export Figures/6_cpvsbea_nik1def_agg.eps, as(eps) mag(150) replace


* Flow of Funds vs compustat data
sort indgroup year
g a_capx_all_preExb = a_capx_all_preEx/1000
g a_capx_US_preExb = a_capx_US_preEx/1000
g a_capx_all_wExb = a_capx_all_wEx/1000
g a_capx_US_wExb = a_capx_US_wEx/1000

g ac_capx_all_preExb = a_capx_all_preExb/i_nfb
g ac_capx_US_preExb = a_capx_US_preExb/i_nfb
g ac_capx_all_wExb = a_capx_all_wExb/i_nfb
g ac_capx_US_wExb = a_capx_US_wExb/i_nfb

egen a_ip_total = sum(a1_ip_all_bea), by(year)

label variable i_nfb "Non Financial Business Investment"
label variable a_capx_all_preExb "Total CAPX (all Compustat firms)"
label variable a_capx_US_preExb "Total CAPX (US incorporated firms)"
label variable a_capx_all_wExb "Total CAPX (all Compustat firms, with Ex)"
label variable a_capx_US_wExb "Total CAPX (US incorporated firms, with Ex)"

* Figure 7: Net Investment predictions
scatter i_nfb a_capx_all_preExb a_capx_US_preExb year if indgroup == 1 & year >= 1970, xlab(1970(10)2015) leg(size(med) rows(3)) lpattern(solid shortdash shortdash)  c(l l l) ms(i i Oh o)
graph export Figures/19_inv_nonres.eps, as(eps) mag(150) replace

save tempindustry, replace


* Investment shares table
use tempindustry, clear
drop if indcode == ""
egen a1_niq = sum(a1_niq_all_bea) if year >= 2000, by(indcode)
egen a_niq_total = sum(a1_niq_all_bea) if year >= 2000
g a1_niq_share = a1_niq / a_niq_total 

drop if a1_niq_share == .
keep if year == 2014
g na1_niq_share = -a1_niq_share
sort na1_niq_share
gen rank = _n 

order rank indcode a1_kp_all a1_niq a1_niq_share avga1c_ppek avga1c_inv
keep rank indcode a1_kp_all a1_niq a1_niq_share avga1c_ppek avga1c_inv

* Table 3: Total net investment since 2000, by industry 
export excel using Tables/16_niq_table.xls, firstrow(variables) replace


****** Q *****
use tempindustry, clear

label variable Q_nfcb "NFCB Q"
label variable am_q "Mean Compustat Q"

* Figure 13: Stock Q and Net investment
scatter Q_nfcb a_q am_q amed_q year if year>1970 & indgroup ==1, lpattern(s s shortdash shortdash) c(l l l l) ms(i Th oh o)  xlab(1970(10)2010) leg(size(med))
graph export Figures/7_FRBvsCPQ.eps, as(eps) mag(150) replace



* Figure 22: Investment and Depreciation Rate for Non financial Business Sector
scatter nik_nfcb ik_nfcb depk_nfcb year if year>=1960 & indgroup ==1, xlab(1960(10)2010) title(Non financial Corporate)  leg(rows(2)) c(l l l) ms(i Oh o) msize(sm sm sm)
graph export Figures/22a_IK_and_Dep_nfcb.eps, as(eps) mag(150) replace

scatter nik_nfncb ik_nfncb depk_nfncb year if year>=1960 & indgroup ==1, xlab(1960(10)2010) ylab(0(0.05)0.15) lpattern(solid solid shortdash) title(Non financial Non Corporate) leg(rows(2)) c(l l l) ms(i Oh o)
graph export Figures/22b_IK_and_Dep_nfncb.eps, as(eps) mag(150) replace

* Figure 23: Relative price of investment goods 
g relp_inv = p_i_nonres / p_pce
g relp_eq = p_i_equip / p_pce

label variable relp_inv "Relative price: Nonresidential"
label variable relp_eq "Relative price: Equipment"

scatter relp_inv year if indgroup ==1, xlab(1960(10)2010) lpattern(solid solid shortdash) c(l l l) ms(i Oh o)
graph export Figures/23_RelP_i.eps, as(eps) mag(150) replace


***


/* -----------------------------*/
/* 	COMPOSITION OF INVESTMENT 	*/
/* -----------------------------*/

sort indcode year
label variable am_intanat "Mean Intangibles/Assets"
label variable am_intanexgwat "Mean Intangibles ex. Goodwill/Assets"

scatter am_intanat am_intanex year if indgroup==1 & year >=1980, xlab(1980(5)2015 ) t1("Intangibles/Assets (Compustat)") yti("")   leg(rows(2)) c(l l l) ms(i Oh o)
graph export Figures/10a_intan.eps, as(eps) mag(150) replace

g ipshare_k = a_kp_ip/a_kp_all
g ipshare_i = a_ip_ip/a_ip_all
g ipshare_ni = a_nip_ip/a_nip_all

label variable ipshare_k  "IP Capital/Total Capital"
label variable ipshare_i  "IP Investment/Total Investment"
label variable ipshare_ni  "IP Net I/total Net I"

scatter ipshare_i  ipshare_k  year if indgroup==1 & year >=1980, xlab(1980(5)2015 ) t1("Intellectual Property share of I and K (BEA)") yti("")  c(l l l) ms(i Oh o)
graph export Figures/10b_ip.eps, as(eps) mag(150) replace

window manage close graph 
erase tempindustry.dta
