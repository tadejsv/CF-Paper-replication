set more off 	
pause off
set logtype text
set mem 500M

set scheme s1color , permanently

*************** DESCRIPTION ***************************************************
* Analyzes INDUSTRY-level investment behavior over time. 
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
drop temp

replace a1m_owntotQIX = . if year == 1980
replace a1med_owntotQIX = . if year == 1980

g a1_ipshare_k = a1_kp_ip/a1_kp_all
g a1_ipshare_i = a1_ip_ip/a1_ip_all
g a1_ipshare_ni_PT = a1_inv2/(a1_inv5-a1_dp5)

/*---------------------*/
/*	 DATA PREPARATION  */
/*---------------------*/

* Generate de-meaned dependent variable for measurement-error corrected regressions
foreach X in a1_nik_all a1_nik_exip a1_nik_ip   {
	
	* For time effects, go to 2015
	egen temp = mean(`X') if year >= 1980 & year <= 2015, by(indgr) 
	g dmfe_`X' = `X' - temp
	g fdmfe_`X'  = f.dmfe_`X' 
	drop dmfe_`X' temp
	
	* For regressions, start in 1981 and end in 2015 as QIX ends then
	egen temp = mean(`X') if year >= 1982 & year <= 2015, by(indgr)
	g dm80_`X' = `X' - temp
	g fdm80_`X'  = f.dm80_`X' 
	drop dm80_`X' temp
	
	egen temp = mean(`X') if year >= 1990 & year <= 2015, by(indgr)
	g dm_`X' = `X' - temp
	g fdm_`X'  = f.dm_`X' 
	drop dm_`X' temp
	
	egen temp = mean(`X') if year >= 2000 & year <= 2015, by(indgr)
	g dm00_`X' = `X' - temp
	g fdm00_`X'  = f.dm00_`X' 
	drop dm00_`X' temp
	
	* Census: 1997-2012
	egen temp = mean(`X') if year >= 1998 & year <= 2013, by(indgr)
	g dmC_`X' = `X' - temp
	g fdmC_`X'  = f.dmC_`X' 
	drop dmC_`X' temp

}

* interpolate census concentration 
replace a1_cenconc50 = a1_cenconc50/100
replace a1_cenconc20 = a1_cenconc20/100
replace a1_cenconc8 = a1_cenconc8/100

ipolate a1_cenconc8  year , g(a1_cenconc8_ipo) by(indcode)
ipolate a1_cenconc20  year, g(a1_cenconc20_ipo) by(indcode)
ipolate a1_cenconc50  year, g(a1_cenconc50_ipo) by(indcode)

g a1med_logq_tot = log(a1med_q_tot)
* Generate de-meaned independent variables for measurement-error corrected regressions
foreach X in a1_q a1m_q a1med_q a1m_logq a1med_logq a1med_q_tot a1med_logq_tot a1_ipshare_i a1med_owntotQIX a1m_owntotQIX mherf ///
			herf_s herf_adj  a1m_logage a1m_logat a1m_intanexgw a1m_pifoadj_sh  a1m_spread ///
			a1m_xrdat a1_stocksig a1m_licensed a1_s3logN a1_li a1m_li a1med_li a1_cpcon8_sale a1_cpcon8_mv ///
			a1_cenconc8_ipo a1_cenconc20_ipo a1_cenconc50_ipo a1m_logreg ///
			a1m_pctinsown a1m_owntotDED a1m_owntotTRA a1_ipshare_k a1m_intanat {
		
	* For time effects, go to 2015
	egen temp = mean(`X') if year >= 1979 & year <= 2014, by(indgr)
	g dmfe_`X' = `X' - temp
	drop  temp
	
	* For regressions, start in 1981 and end in 2015 as QIX ends then
	egen temp = mean(`X') if year >= 1981 & year <= 2014, by(indgr)
	g dm80_`X' = `X' - temp
	drop  temp
	
	egen temp = mean(`X') if year >= 1989 & year <= 2014, by(indgr)
	g dm_`X' = `X' - temp
	drop temp
	
	egen temp = mean(`X') if year >= 1999 & year <= 2014, by(indgr)
	g dm00_`X' = `X' - temp
	drop  temp
	
	* Census: 1997-2012
	egen temp = mean(`X') if year >= 1997 & year <= 2012, by(indgr)
	g dmC_`X' = `X' - temp
	drop temp

}

* 1990 
* Compute change variables
foreach X in a1m_bba a1m_pctinsown a1m_owntotQIX a1med_extfindep_rz a1m_extfindep_rz a1m_exteqfindep_rz a1m_extdebtfindep_rz ///
				a1m_spread a1m_AAtoAAA a1m_xrdat a1m_bankdep a1m_sig_g5 a1_stocksig ///
				a1_cpcon8_sale a1_cpcon8_mv a1m_logreg a1m_logat mherf{	
		sort indgr year
		egen `X'0 = mean(`X') if year > 1995 & year < 2000, by(indgr)
		by indgr (year), sort: replace `X'0 = `X'0[_n-1] if `X'0 == . 
}

foreach X in a1_cenconc4 a1_cenconc8 a1_cenconc20 a1_cenconc50 {	
		sort indgr year
		g t`X'97 = `X' if year == 1997
		g t`X'12 = `X' if year == 2012
		
		egen `X'97 = min(t`X'97), by(indgr)
		egen `X'12 = min(t`X'12), by(indgr)
		g d`X'7 = `X'12 - `X'97 
		drop t`X'*
}


* compute change variables
g a1m_dbba = a1m_bba - a1m_bba0
g a1m_dpctinsown = a1m_pctinsown - a1m_pctinsown0
g a1m_downtotQIX = a1m_owntotQIX - a1m_owntotQIX0
g a1m_dxrdat = a1m_xrdat - a1m_xrdat0
g da1_cpcon8_sale = a1_cpcon8_sale - a1_cpcon8_sale0
g da1_cpcon8_mv = a1_cpcon8_mv - a1_cpcon8_mv0
g fa1_nik_all = f.a1_nik_all
g dmherf = mherf-mherf0

xi i.year 


***


/*------------------------*/
/*	 TIME EFFECT PLOTS    */
/*------------------------*/


* Base measurement-error corrected
xtewreg fdmfe_a1_nik_all  dmfe_a1med_logq dmfe_a1m_logage _Iyear_1980-_Iyear_2014 if year>=1979 & year <= 2014, maxdeg(4) mis(1)
coefplot , keep(*year*) xline(-0.043317523) name(g1_fe, replace) ti("Industry-level time effects (BEA)", size(med)) nodraw

xtewreg fdmfe_a1_nik_ip  dmfe_a1med_logq dmfe_a1m_logage _Iyear_1980-_Iyear_2014 if year>=1979 & year <= 2014, maxdeg(4) mis(1)
coefplot , keep(*year*) xline(-0.139846966) name(g1_fe_rd, replace) ti("Industry-level time effects (BEA)", size(med))  nodraw


* Effect of intangibles 
matrix outM = J(35,3,.)
matrix colnames outM = Year FE FE_int 
matrix rownames outM = 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 ///
1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014

xtewreg fdmfe_a1_nik_all dmfe_a1med_logq dmfe_a1m_logage _Iyear_1980-_Iyear_2014 if year>=1979 & year <= 2015, maxdeg(4) mis(1) nocons
forvalues i = 1980(1)2014{
	matrix outM[`i'- 1979,1] = `i'
	matrix outM[`i'- 1979,2] = _b[_Iyear_`i']
}	
xtewreg fdmfe_a1_nik_all dmfe_a1med_logq dmfe_a1_ipshare_i dmfe_a1m_logage _Iyear_1980-_Iyear_2014 if year>=1979 & year <= 2015, maxdeg(4) mis(1) 
forvalues i = 1980(1)2014{
	matrix outM[`i'- 1979,3] = _b[_Iyear_`i']
}	

matrix outM[1,1] = 1980
matrix list outM

forvalues i = 1980(1)2014{
	matrix outM[`i'- 1979,2] = outM[`i'- 1979,2] + 0.043317526	
	matrix outM[`i'- 1979,3] = outM[`i'- 1979,3] + 0.039198435
}	

coefplot (matrix(outM[,2]),label(Only Q)) (matrix(outM[,3]),label(Q + Intan)), offset(0) ti("Industry-level FE with and w/o intangibles") c(l l)  vert yline(0, lp(dash) ) ///
coeflabel( 1980="1980" 1981=" " 1982=" " 1983=" " 1984=" " 1985="1985" ///
1986=" " 1987=" " 1988=" " 1989=" " 1990="1990" 1991=" " ///
1992=" " 1993=" " 1994=" " 1995="1995" ///
1996=" " 1997=" " 1998=" " 1999=" " 2000="2000" 2001=" " ///
2002=" " 2003=" " 2004=" " 2005= "2005" 2006=" " 2007=" " 2008=" " 2009=" " ///
2010= "2010" 2011=" " 2012=" " 2013=" " 2014="2014" ) 
graph export Figures/12a_NIQ_FixedEffects_Ind_wIntan.eps, as(eps) mag(150) replace


* Sequential controls 
matrix outM = J(33,4,.)
matrix colnames outM = Year FE FE_int 
matrix rownames outM = 1982 1983 1984 1985 1986 1987 1988 1989 ///
1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014

xtewreg fdm80_a1_nik_all dm80_a1med_logq dm80_a1m_logage _Iyear_1982-_Iyear_2014 if year>=1981 & year <=  2014, maxdeg(4) mis(1)
forvalues i = 1982(1)2014{
	matrix outM[`i'- 1981,1] = `i'
	matrix outM[`i'- 1981,2] = _b[_Iyear_`i']
}	
xtewreg fdm80_a1_nik_all dm80_a1med_logq dm80_a1_ipshare_i dm80_a1m_logage _Iyear_1982-_Iyear_2014 if year>=1981 & year <=  2014, maxdeg(4) mis(1)
forvalues i = 1982(1)2014{
	matrix outM[`i'- 1981,3] = _b[_Iyear_`i']
}	

xtewreg fdm80_a1_nik_all dm80_a1med_logq dm80_a1_ipshare_i dm80_a1m_owntotQIX dm80_mherf dm80_a1m_logage _Iyear_1982-_Iyear_2014 if year>=1981 & year <=  2014, maxdeg(4) mis(1)
forvalues i = 1982(1)2014{
	matrix outM[`i'- 1981,4] = _b[_Iyear_`i']
}	

matrix list outM

forvalues i = 1982(1)2014{
	matrix outM[`i'- 1981,2] = outM[`i'- 1981,2] + 0.041358324	
	matrix outM[`i'- 1981,3] = outM[`i'- 1981,3] + 0.02933386
	matrix outM[`i'- 1981,4] = outM[`i'- 1981,4] + 0.011136506
}	

coefplot (matrix(outM[,2]),label(Only Q)) (matrix(outM[,3]),label(Q + Intan))  (matrix(outM[,4]),label(All)), ///
offset(0) leg(c(3)) c(l l)  vert yline(0, lp(dash) ) ///
coeflabel( 1982=" " 1983=" " 1984=" " 1985="1985" ///
1986=" " 1987=" " 1988=" " 1989=" " 1990="1990" 1991=" " ///
1992=" " 1993=" " 1994=" " 1995="1995" ///
1996=" " 1997=" " 1998=" " 1999=" " 2000="2000" 2001=" " ///
2002=" " 2003=" " 2004=" " 2005= "2005" 2006=" " 2007=" " 2008=" " 2009=" " ///
2010= "2010" 2011=" " 2012=" " 2013=" " 2014="2014" ) 
graph export Figures/9_NIQ_FixedEffects_Ind_All.eps, as(eps) mag(150) replace



***


/*--------------------- */
/*	 	CORE RESULTS    */
/*--------------------- */


capture erase Tables/5_ind_core.tex
capture erase Tables/9_ind_base_intan.tex
capture erase Tables/21_ind_allhyp.tex
capture erase Tables/12_foreign_test.tex

* Col 1
xtewreg fdm80_a1_nik_all  dm80_a1med_logq dm80_a1m_owntotQIX dm80_mherf dm80_a1m_logage _Iyear_1982-_Iyear_2014 if year>=1981 & year <=  2014, maxdeg(4) mis(1)
outreg2 using Tables/5_ind_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) e(rho)  drop(*logat *logage *year*) ctitle(>1981) sortvar(*logq* *QIX* *mherf* *herf*) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
outreg2 using Tables/21_ind_allhyp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*logq* *QIX* *mherf* *herf*) e(rho)  drop(*logat* *logage* *year*) ctitle(>1980) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* Col 2
xtewreg fdm_a1_nik_all  dm_a1med_logq dm_a1m_owntotQIX dm_mherf dm_a1m_logage _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2014, maxdeg(4) mis(1)
outreg2 using Tables/5_ind_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) e(rho)  drop(*logat *logage *year*) ctitle(>1990) sortvar(*logq* *QIX* *mherf* *herf*) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/9_ind_base_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/12_foreign_test.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/21_ind_allhyp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*logq* *QIX* *mherf* *herf*) e(rho)  drop(*logat *logage *year*) ctitle(>1980) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* Col 3
xtewreg fdm80_a1_nik_all dm80_a1med_logq dm80_a1m_owntotQIX dm80_herf_s dm80_herf_a dm80_a1m_logage _Iyear_1982-_Iyear_2014 if year>=1981 & year <= 2014, maxdeg(4) mis(1)
outreg2 using Tables/5_ind_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) e(rho)  drop(*logat *logage *year*) ctitle(>1981) sortvar(*logq* *QIX* *mherf* *herf*) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
*outreg2  using Tables/IK_Own/ind_inter.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq a1m_owntotQIX mherf *herf*) e(rho)  drop(*logat *logage *year*) ctitle(>1981) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* Col 4
xtewreg fdm_a1_nik_all dm_a1med_logq dm_a1m_owntotQIX dm_herf_s dm_herf_a dm_a1m_logage _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2014, maxdeg(3) mis(1)
outreg2 using Tables/5_ind_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) e(rho)  drop(*logat *logage *year*) ctitle(>1990) sortvar(*logq* *QIX* *mherf* *herf*) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)



***


/*-------------------------- */
/*	 INTANGIBLE INVESTMENT	 */
/*-------------------------- */

xtewreg fdm_a1_nik_all  dm_a1med_logq  dm_mherf dm_a1m_owntotQIX dm_a1_ipshare_i dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/9_ind_base_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

xtewreg fdm_a1_nik_all  dm_a1med_logq_tot  dm_mherf dm_a1m_owntotQIX dm_a1_ipshare_i dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(5) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/9_ind_base_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)


***


/*---------------------------- */
/*	 INVESTMENT BY ASSET TYPE  */
/*---------------------------- */


*XTEWREG
capture erase Tables/8_ind_bytype.tex

* Col 1
xtewreg fdm_a1_nik_all  dm_a1med_logq dm_a1m_owntotQIX dm_mherf dm_a1m_logage _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2014, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/8_ind_bytype.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf* *herf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(All) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
* Col 2
xtewreg fdm_a1_nik_exip dm_a1med_logq dm_a1m_owntotQIX dm_mherf dm_a1m_logage _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2014, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/8_ind_bytype.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf* *herf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(Ex IP) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
* Col 3
xtewreg fdm_a1_nik_ip  dm_a1med_logq dm_a1m_owntotQIX dm_mherf dm_a1m_logage _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2014, maxdeg(5) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/8_ind_bytype.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf* *herf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(IP) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)




/*-------------------- */
/*	 APPENDIX RESULTS  */
/*-------------------- */


*** ALL HYPOTHESES *** 

* Ext fin dep 
xtewreg fa1_nik_all   a1med_logq  dmherf a1m_downtotQIX  a1med_extfindep_rz0 a1m_logage _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/21_ind_allhyp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO)

*Bank dep
xtewreg fa1_nik_all   a1med_logq  dmherf a1m_downtotQIX  a1m_bankdep0 a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/21_ind_allhyp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO)

*safe assets
xtewreg fa1_nik_all   a1med_logq  dmherf a1m_downtotQIX a1m_AAtoAAA0 a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/21_ind_allhyp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO)

* intangibles
xtewreg fdm_a1_nik_all  dm_a1med_logq  dm_mherf dm_a1m_owntotQIX dm_a1_ipshare_i dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/21_ind_allhyp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* globalization
xtewreg fdm_a1_nik_all  dm_a1med_logq  dm_mherf dm_a1m_owntotQIX dm_a1m_pifoadj_sh dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/21_ind_allhyp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

outreg2 a1med_logq mherf a1m_owntotQIX using Tables/12_foreign_test.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)


* final result
xtewreg fdm_a1_nik_all  dm_a1med_logq  dm_mherf dm_a1m_owntotQIX dm_a1_ipshare_i dm_a1m_pifoadj_sh dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/21_ind_allhyp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)



*** POST 2000 ***
capture erase Tables/26_ind_allhyp_2000.tex
* base
xtewreg fdm00_a1_nik_all  dm00_a1med_logq dm00_mherf dm00_a1m_owntotQIX dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/26_ind_allhyp_2000.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* Ext fin dep 
xtewreg fa1_nik_all   a1med_logq  dmherf a1m_downtotQIX  a1med_extfindep_rz0 a1m_logage _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/26_ind_allhyp_2000.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO)

*Bank dep
xtewreg fa1_nik_all   a1med_logq  dmherf a1m_downtotQIX  a1m_bankdep0 a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/26_ind_allhyp_2000.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO)

* safe assets
xtewreg fa1_nik_all   a1med_logq  dmherf a1m_downtotQIX a1m_AAtoAAA0 a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/26_ind_allhyp_2000.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO)

* intangibles
xtewreg fdm00_a1_nik_all  dm00_a1med_logq  dm00_mherf dm00_a1m_owntotQIX dm_a1_ipshare_i dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/26_ind_allhyp_2000.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* globalization
xtewreg fdm00_a1_nik_all  dm00_a1med_logq  dm00_mherf dm00_a1m_owntotQIX dm00_a1m_pifoadj_sh dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/26_ind_allhyp_2000.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* final result
xtewreg fdm00_a1_nik_all  dm00_a1med_logq  dm00_mherf dm00_a1m_owntotQIX dm_a1_ipshare_i dm00_a1m_pifoadj_sh dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/26_ind_allhyp_2000.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)


*** COMPETITION ***

* Measurement-error corrected
capture erase Tables/22_ind_comp.tex

*entry
xtewreg fdm_a1_nik_all  dm_a1med_logq dm_a1_s3logN dm_a1m_owntotQIX dm_a1m_logage _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2014, maxdeg(4) mis(1)
outreg2 using Tables/22_ind_comp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* herfindahls
xtewreg fdm_a1_nik_all  dm_a1med_logq dm_mherf dm_a1m_owntotQIX dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/22_ind_comp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

xtewreg fdm_a1_nik_all dm_a1med_logq dm_a1m_owntotQIX dm_herf_s dm_herf_a dm_a1m_logage _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2014, maxdeg(3) mis(1)
outreg2 using Tables/22_ind_comp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

*Lerner
xtewreg fdm_a1_nik_all  dm_a1med_logq dm_a1med_li dm_a1m_owntotQIX dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(3) mis(1)
outreg2 using Tables/22_ind_comp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

*Concentration
xtewreg fdm_a1_nik_all  dm_a1med_logq dm_a1_cpcon8_sale dm_a1m_owntotQIX dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/22_ind_comp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

xtewreg fdm_a1_nik_all  dm_a1med_logq dm_a1_cpcon8_mv dm_a1m_owntotQIX  dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/22_ind_comp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

*Census
xtewreg fdmC_a1_nik_all  dmC_a1med_logq dmC_a1_cenconc50_ip dmC_a1m_owntotQIX dmC_a1m_logage _Iyear_1998-_Iyear_2012 if year>=1997 & year <= 2012, maxdeg(5) mis(1)
outreg2 using Tables/22_ind_comp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(97-12) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* regulation 
xtewreg fdm_a1_nik_all  dm_a1med_logq dm_a1m_logreg dm_a1m_owntotQIX dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/22_ind_comp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
*occ licensing
xtewreg fdm00_a1_nik_all  dm_a1med_logq a1m_licensed dm_a1m_owntotQIX dm_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/22_ind_comp.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)


* >2000: competition
capture erase Tables/27_ind_comp_00.tex

*entry
xtewreg fdm00_a1_nik_all dm00_a1med_logq dm00_a1_s3logN dm00_a1m_owntotQIX dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2014, maxdeg(4) mis(1)
outreg2 using Tables/27_ind_comp_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

* herfindahls
xtewreg fdm00_a1_nik_all  dm00_a1med_logq dm00_mherf dm00_a1m_owntotQIX dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/27_ind_comp_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

xtewreg fdm00_a1_nik_all  dm00_a1med_logq dm00_herf_s dm00_herf_a dm00_a1m_owntotQIX dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/27_ind_comp_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

*Lerner
xtewreg fdm00_a1_nik_all  dm00_a1med_logq dm00_a1med_li dm00_a1m_owntotQIX dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/27_ind_comp_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
*Concentration
xtewreg fdm00_a1_nik_all  dm00_a1med_logq dm00_a1_cpcon8_sale dm00_a1m_owntotQIX dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/27_ind_comp_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)

xtewreg fdm00_a1_nik_all  dm00_a1med_logq dm00_a1_cpcon8_mv dm00_a1m_owntotQIX  dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/27_ind_comp_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
*Census
xtewreg fdmC_a1_nik_all  dmC_a1med_logq dmC_a1_cenconc50_ip dmC_a1m_owntotQIX dmC_a1m_logage _Iyear_1998-_Iyear_2012 if year>=1997 & year <= 2012, maxdeg(5) mis(1)
outreg2 using Tables/27_ind_comp_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
* regulation 
xtewreg fdm00_a1_nik_all  dm00_a1med_logq dm00_a1m_logreg dm00_a1m_owntotQIX dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/27_ind_comp_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
*occ licensing
xtewreg fdm00_a1_nik_all  dm00_a1med_logq a1m_licensed dm00_a1m_owntotQIX dm00_a1m_logage  _Iyear_2000-_Iyear_2014 if year>=1999 & year <= 2015, maxdeg(4) mis(1)
outreg2 using Tables/27_ind_comp_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(a1med_logq *QIX* ) ///
e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)


*** OWNERSHIP ***

capture erase Tables/23_ind_own.tex

xtewreg fdm_a1_nik_all  dm_a1med_logq  dm_mherf dm_a1m_owntotQIX dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/23_ind_own.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
xtewreg fdm_a1_nik_all  dm_a1med_logq  dm_mherf dm_a1m_pctinsown dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/23_ind_own.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
xtewreg fdm_a1_nik_all  dm_a1med_logq  dm_mherf dm_a1m_owntotTRA dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/23_ind_own.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)
xtewreg fdm_a1_nik_all  dm_a1med_logq  dm_mherf dm_a1m_owntotDED dm_a1m_logage  _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2015, maxdeg(4) mis(1)
outreg2 a1med_logq mherf a1m_owntotQIX using Tables/23_ind_own.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) sortvar(*a1med_logq* *QIX* *mherf*) ///
e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES)



