
set more off 	
pause off
set logtype text
set mem 500M

set scheme s1color , permanently

*************** DESCRIPTION ***************************************************
* Analyzes firm-level investment behavior over time. 
*
*	Inputs:  3.Final_data/main_dataset_firm_BEA
*	Outputs: Figures and Tables included in paper, numbered as in the final 
*			 paper and stored in corresponding folders
* 
*******************************************************************************   

 
***************************************************************
********************** FIRM ANALYSES  **************************
****************************************************************

use "Data/Final/main_dataset_firm_BEA", clear

*** Data preparation ***
destring gvkey, replace
drop if gvkey==.
xtset gvkey year
egen indgroup = group(indcode)
drop if year == 2017
drop temp

g a1_ipshare_i = a1_ip_ip/a1_ip_all
g a1_ipshare_k = a1_kp_ip/a1_kp_all

g logik1 = log(ik1)
g logik3 = log(ik3)
g logik4 = log(ik4)
g dxrdat = d.xrdat

g q_ma2 = (f.q+q)/2
g logq_ma2 = log(q_ma2)
g logq_tot = log(q_tot)

g owntotQIX_ma2 = (owntotQIX + l.owntotQIX)/2
g owntotDED_ma2 = (owntotDED + l.owntotDED)/2
g owntotTRA_ma2 = (owntotTRA + l.owntotTRA)/2
g pctinsown_ma2 = (pctinsown + l.pctinsown)/2

g nik_pt = s1.k_int/l.k_int
winsor2 nik_pt, replace cuts(2 97) by(year)

foreach X in  nik1 ik1 logik1 ik3 logik3 ik4 logik4 paya bba nik_pt {

	egen temp = mean(`X') if year >= 1980 & year <= 2016, by(gvkey)
	g dm80_`X' = `X' - temp
	g fdm80_`X'  = f.dm80_`X' 
	drop dm80_`X' temp
	
	egen temp = mean(`X') if year >= 1990 & year <= 2016, by(gvkey)
	g dm_`X' = `X' - temp
	g fdm_`X'  = f.dm_`X' 
	drop dm_`X' temp
	
	egen temp = mean(`X') if year >= 2000 & year <= 2016, by(gvkey)
	g dm00_`X' = `X' - temp
	g fdm00_`X'  = f.dm00_`X' 
	drop dm00_`X' temp

	egen temp = mean(`X') if year >= 1980 & year <= 2016, by(indgroup)
	g dmi80_`X' = `X' - temp
	g fdmi80_`X'  = f.dmi80_`X' 
	drop temp dmi80_`X'
	
	egen temp = mean(`X') if year >= 1990 & year <= 2016, by(indgroup)
	g dmi_`X' = `X' - temp
	g fdmi_`X'  = f.dmi_`X' 
	drop temp dmi_`X'
	
	egen temp = mean(`X') if year >= 2000 & year <= 2016, by(indgroup)
	g dmi00_`X' = `X' - temp
	g fdmi00_`X'  = f.dmi00_`X' 
	drop dmi00_`X' temp

	egen temp = mean(`X' ) if year >= 1990 & year <= 2016, by(indgroup year)
	g dmiy_`X'  = `X' - temp
	g fdmiy_`X'  = f.dmiy_`X' 
	drop temp dmiy_`X' 
	
}

g logintanat = log(intanat)
g logblev = log(blev)
foreach X in q q_ma2 logq logq_ma2 q2 logq2 q_tot logq_tot mherf herf_s herf_a owntotQIX owntotQIX_ma2 logage logat osk dlogsale ///
			blev logmv cfat logblev a1_ipshare_i intanat intanexgwat a1_ipshare_k pifoadj_sh xrdat a1m_logreg dxrdat ///
			pctinsown  owntotDED_ma2 owntotTRA_ma2 sig_g5 a1_s3logN a1_cpcon8_sale a1_li logintanat  { 

	egen temp = mean(`X') if year >= 1979 & year <= 2015, by(gvkey)
	g dm80_`X' = `X' - temp
	drop temp
	
	egen temp = mean(`X') if year >= 1989 & year <= 2015, by(gvkey)
	g dm_`X' = `X' - temp
	drop temp
	
	egen temp = mean(`X') if year >= 1999 & year <= 2015, by(gvkey)
	g dm00_`X' = `X' - temp
	drop temp

	egen temp = mean(`X') if year >= 1979 & year <= 2015, by(indgroup)
	g dmi80_`X' = `X' - temp
	drop temp 

	egen temp = mean(`X') if year >= 1989 & year <= 2015, by(indgroup)
	g dmi_`X' = `X' - temp
	drop temp 
	
	egen temp = mean(`X') if year >= 1999 & year <= 2015, by(indgroup)
	g dmi00_`X' = `X' - temp
	drop temp

	egen temp = mean(`X' ) if year >= 1989 & year <= 2015, by(indgroup year)
	g dmiy_`X'  = `X' - temp
	drop temp 	
}

xi i.year 

save tempfirm,replace


***


/*--------------------------*/
/*	 TIME EFFECT CHARTS 	*/
/*--------------------------*/

use tempfirm,clear

* CAPX+R&D 
xtewreg fdm80_logik4 dm80_logq_m dm80_logage _Iyear_1980-_Iyear_2015 if year>=1979 & year <= 2015 , maxdeg(5) mis(1)
coefplot , keep(*year*) xline(-0.313104) name(g2_fe, replace) ti("Firm-level time effects (Compustat)", size(med)) nodraw
graph combine g1_fe g2_fe
graph export Figures/8_NIQ_FixedEffects.eps, as(eps) mag(200) replace 

* R&D  
xtewreg fdm80_logik3 dm80_logq_m dm80_logage _Iyear_1980-_Iyear_2015 if year>=1979 & year <= 2015 , maxdeg(5) mis(1)
coefplot , xline(-0.145066) keep(*year*) name(g2_fe_rd, replace) ti("Firm-level time effects (Compustat)", size(med)) nodraw
graph combine g1_fe_rd g2_fe_rd
graph export Figures/11_NIQ_FixedEffects_RD.eps, as(eps) mag(200) replace 

window manage close graph 

* Effect of intangibles 
matrix outM = J(36,3,.)
matrix colnames outM = Year FE FE_int 
matrix rownames outM = 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 ///
1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015

xtewreg fdmi80_ik4 dmi80_q_m dmi80_logage _Iyear_1980-_Iyear_2015 if year>=1979 & year <= 2015, maxdeg(5) mis(1)
forvalues i = 1980(1)2015{
	matrix outM[`i'- 1979,1] = `i'
	matrix outM[`i'- 1979,2] = _b[_Iyear_`i']
}	

xtewreg fdmi80_ik4 dmi80_q_m dmi80_intanat dmi80_logage _Iyear_1980-_Iyear_2015 if year>=1979 & year <= 2015, maxdeg(5) mis(1)
forvalues i = 1980(1)2015{
	matrix outM[`i'- 1979,3] = _b[_Iyear_`i']
}	

matrix outM[1,1] = 1980
matrix list outM

forvalues i = 1980(1)2015{
	matrix outM[`i'- 1979,2] = outM[`i'- 1979,2] +0.03824
	matrix outM[`i'- 1979,3] = outM[`i'- 1979,3] +0.03041
}	

coefplot (matrix(outM[,2]),label(Only Q)) (matrix(outM[,3]),label(Q + Intan)) , offset(0) ti("Firm-level FE with and w/o intangibles") c(l l) yline(0,lp(dash)) vert ///
coeflabel( 1980="1980" 1981=" " 1982=" " 1983=" " 1984=" " 1985="1985" ///
1986=" " 1987=" " 1988=" " 1989=" " 1990="1990" 1991=" " ///
1992=" " 1993=" " 1994=" " 1995="1995" ///
1996=" " 1997=" " 1998=" " 1999=" " 2000="2000" 2001=" " ///
2002=" " 2003=" " 2004=" " 2005= "2005" 2006=" " 2007=" " 2008=" " 2009=" " ///
2010= "2010" 2011=" " 2012=" " 2013=" " 2014=" " 2015="2015" ) 
graph export Figures/12b_NIQ_FixedEffects_Firm_wIntan.eps, as(eps) mag(150) replace



***



/*------------------------------*/
/*	 QNIK: CORE CAPX + R&D      */
/*------------------------------*/


*** CORRECTED FOR MEASUREMENT ERROR ***
capture erase Tables/6_firm_core.tex

* Col 1: Investment with Ind FE, year FE
xtewreg fdm_nik1 dm_q dm_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015 & dm_mherf ~= .  & dm_owntotQIX_ma2 ~= . , maxdeg(6) mis(1)
outreg2 using Tables/6_firm_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho)  drop(*logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO, Firm de-meaned, YES, Industry-Year de-meaned, NO)

xtewreg fdmi_nik1 dmi_q dmi_mherf dmi_owntotQIX_ma2 dmi_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/6_firm_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho)  drop(*logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES, Firm de-meaned, NO, Industry-Year de-meaned, NO)
* Col 2: Investment with Firm de-meaned, year FE
xtewreg fdm_nik1 dm_q dm_mherf dm_owntotQIX_ma2 dm_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/6_firm_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho)  drop(*logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO, Firm de-meaned, YES, Industry-Year de-meaned, NO)
* Col 3: Investment with Industry-Year de-meaned 
xtewreg fdmiy_nik1 dmiy_q dmiy_owntotQIX_ma2 dmiy_logage  _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/6_firm_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho)  drop( *logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, NO, Industry de-meaned, NO, Firm de-meaned, NO, Industry-Year de-meaned, YES)

* Col 4: R&D/AT with Ind FE, year FE
xtewreg fdmi_logik3 dmi_logq dmi_mherf dmi_owntotQIX_ma2 dmi_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/6_firm_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho)  drop(*logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES, Firm de-meaned, NO, Industry-Year de-meaned, NO)
* Col 5: R&D/AT with Firm de-meaned, year FE
xtewreg fdm_logik3 dm_logq dm_mherf dm_owntotQIX_ma2 dm_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/6_firm_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho)  drop(*logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO, Firm de-meaned, YES, Industry-Year de-meaned, NO)
* Col 6: R&D/AT with Ind-Year FE
xtewreg fdmiy_logik3 dmiy_logq dmiy_owntotQIX_ma2 dmiy_logage  _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/6_firm_core.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho)  drop( *logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, NO, Industry de-meaned, NO, Firm de-meaned, NO, Industry-Year de-meaned, YES)

* --> Higher MHERF and higher QIX leads to less investment. 

/*------------------------------*/
/*	 QNIK: Buyback results      */
/*------------------------------*/

capture erase Tables/7_firm_bba.tex

* Col 1: Buybacks with Industry de-meaned, year FE 
xtewreg fdmi_bba dmi_logq dmi_owntotQIX_ma2 dmi_logage dmi_logat dmi_osk dmi_dlogsale dmi_logblev dmi_logmv dmi_cfat _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/7_firm_bba.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho)  drop(*logage *logat *year* *osk* *sale* *blev* *logmv* *cfat*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, YES, Firm de-meaned, NO, Industry-Year de-meaned, NO)
* Col 2: Buybacks with Firm de-meaned, year FE 
xtewreg fdm_bba dm_logq dm_owntotQIX_ma2 dm_logage dm_logat dm_osk dm_dlogsale dm_logblev dm_logmv dm_cfat _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/7_firm_bba.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho) drop(*logage *logat *year* *osk* *sale* *blev* *logmv* *cfat*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO, Firm de-meaned, YES, Industry-Year de-meaned, NO)
* Col 3: Buybacks with Industry-Year de-meaned 
xtewreg fdmiy_bba dmiy_logq dmiy_owntotQIX_ma2 dmiy_logage dmiy_logat  dmiy_osk dmiy_dlogsale dmiy_logblev dmiy_logmv dmiy_cfat _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/7_firm_bba.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho) drop(*logage *logat *year* *osk* *sale* *blev* *logmv* *cfat*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, NO, Industry de-meaned, NO, Firm de-meaned, NO, Industry-Year de-meaned, YES)
* Col 4: Buybacks with Industry de-meaned, year FE 
xtewreg fdmi_bba dmi_logq dmi_owntotQIX_ma2 dmi_logage dmi_logat dmi_osk dmi_dlogsale dmi_logblev dmi_logmv dmi_cfat _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(7) mis(1)
outreg2 using Tables/7_firm_bba.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho) drop(*logage *logat *year* *osk* *sale* *blev* *logmv* *cfat*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, NO, Industry de-meaned, YES, Firm de-meaned, NO, Industry-Year de-meaned, NO)
* Col 5: Payouts with Firm de-meaned, year FE 
xtewreg fdm_bba dm_logq dm_owntotQIX_ma2 dm_logage dm_logat dm_osk dm_dlogsale dm_logblev dm_logmv dm_cfat _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(7) mis(1)
outreg2 using Tables/7_firm_bba.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho) drop(*logage *logat *year* *osk* *sale* *blev* *logmv* *cfat*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO, Firm de-meaned, YES, Industry-Year de-meaned, NO)
* Col 6: Payouts with Industry-Year de-meaned 
xtewreg fdmiy_paya dmiy_logq dmiy_owntotQIX_ma2 dmiy_logage dmiy_logat  dmiy_osk dmiy_dlogsale dmiy_logblev dmiy_logmv dmiy_cfat _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(7) mis(1)
outreg2 using Tables/7_firm_bba.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*logq* *QIX* mherf *herf* ) e(rho) drop(*logage *logat *year* *osk* *sale* *blev* *logmv* *cfat*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Industry de-meaned, NO, Firm de-meaned, NO, Industry-Year de-meaned, YES)




***


/*------------------------------*/
/*	 QNIK: APPENDIX RESULTS     */
/*------------------------------*/

use tempfirm,clear

******* DATA PREPARATION ******* 

** COMPUTE 2000 VALUES 
foreach X in prstkc at {
	egen cum_`X' = sum(`X') if year > 1995 & year < 2000, by(gvkey)
}

g bba0 = cum_prstkc / cum_at
g extfindep0 = extfindep_rz if year == 1999
g AAtoAAA0 = AAtoAAA if year == 1999
g bankdep0 = bankdep if year == 1999
g sprating0 = sprating if year == 1999
g sig_g50  = sig_g5  if year == 1999

egen q0  = mean(q)  if year >1995 & year < 2000, by(gvkey)
egen logat0  = mean(logat)  if year >1995 & year < 2000, by(gvkey)
egen osk0  = mean(osk)  if year >1995 & year < 2000, by(gvkey)
egen cdat0  = mean(cdat)  if year >1995 & year < 2000, by(gvkey)
egen xrdat0  = mean(xrdat)  if year >1995 & year < 2000, by(gvkey)
egen dlogsale0  = mean(dlogsale)  if year >1995 & year < 2000, by(gvkey)
egen blev0  = mean(blev)  if year >1995 & year < 2000, by(gvkey)

egen ctyrs= count(gvkey)  if year >1995 & year < 2000, by(gvkey)
egen tt1 = mean(mherf)  if ctyrs == 4, by(gvkey)
egen mherf0 = max(tt1), by(indcode)
drop ctyrs tt1

g logme0  = log(me)  if year == 1999 
g gwa0  = gwa if year == 1999 

egen pctinsown0  = mean(pctinsown)  if year >1995 & year < 2000, by(gvkey)
egen owntotQIX0  = mean(owntotQIX)  if year >1995 & year < 2000, by(gvkey)
egen owntotTRA0  = mean(owntotTRA)  if year >1995 & year < 2000, by(gvkey)
egen owntotDED0  = mean(owntotDED)  if year >1995 & year < 2000, by(gvkey)

replace bba0=0.1 if bba0>0.1 & bba0 ~= .

* copy values down
foreach X in bba0 extfindep0 AAtoAAA0 bankdep0 bankdep0 sprating0 sig_g50 q0 logat0 osk0 cdat0 xrdat0 dlogsale0 blev0 logme0 pctinsown0 owntotQIX0 {
	by gvkey (year), sort: replace `X' = `X'[_n-1] if `X' == . 
}

* compute change variables
g dbba = bba - bba0 
g dgwa = gwa - gwa0 
g dpctinsown = pctinsown - pctinsown0
g downtotQIX = owntotQIX - owntotQIX0
g fnik1 = f.nik1
g dsig_g5 = sig_g5 - sig_g50
g dmherf = mherf-mherf0



***



******* ALTERNATE EXPLANATIONS ******* 

*With measurement error correction
capture erase Tables/24_firm_alt.tex

*base
xtewreg fdm_nik1 dm_q dm_mherf dm_owntotQIX_ma2 dm_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/24_firm_alt.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)
*Ext fin dependence
xtewreg fdmi00_nik1 dmi00_q dmi00_mherf dmi00_owntotQIX extfindep0 dmi00_logage _Iyear_2001-_Iyear_2015 if year>=2000 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/24_firm_alt.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, NO, Industry de-meaned, YES)
* bank dependence
xtewreg fdmi00_nik1 dmi00_q dmi00_mherf dmi00_owntotQIX  bankdep0 dmi00_logage  _Iyear_2000-_Iyear_2015 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/24_firm_alt.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, NO, Industry de-meaned, YES)
* safe assets
xtewreg fdmi00_nik1 dmi00_q dmi00_mherf dmi00_owntotQIX  AAtoAAA0 dmi00_logage  _Iyear_2000-_Iyear_2015 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/24_firm_alt.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, NO, Industry de-meaned, YES)
*intangibles
xtewreg fdm_nik1 dm_q   dm_mherf dm_owntotQIX_ma2 dm_intanexgwat dm_logage   _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/24_firm_alt.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)
*globalization
xtewreg fdm_nik1 dm_q   dm_mherf dm_owntotQIX_ma2 dm_pifoadj_sh dm_logage   _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/24_firm_alt.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)
outreg2 q mherf owntotQIX* using Tables/12_foreign_test.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)
*regulation
xtewreg fdm_nik1 dm_q   dm_mherf dm_owntotQIX_ma2 dm_a1m_logreg dm_logage   _Iyear_1990-_Iyear_2014 if year>=1989 & year <= 2014, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/24_firm_alt.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>1990) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)


******* DEALING WITH INTANGIBLES ******* 

capture erase Tables/11_firm_intan.tex

**** CAPX NIK ***

* OLS Q
xtreg nik1 l.q l.intanat l.logage i.year if year >= 1980, fe
outreg2 using Tables/11_firm_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*q* *intan* ) e(rho) keep(L.q L.intanat) ctitle(>1980) addtext(Method, OLS, Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Industry de-meaned, NO)
* OLS Q2
xtreg nik1 l.q2 l.intanat l.logage i.year if year >= 1980, fe
outreg2 using Tables/11_firm_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*q* *intan* ) e(rho) keep(L.q2 L.intanat) ctitle(>1980) addtext(Method, OLS, Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Industry de-meaned, NO)
*OLS Qtot
xtreg nik1 l.q_tot l.intanat l.logage i.year if year >= 1980, fe
outreg2 using Tables/11_firm_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*q* *intan* ) e(rho)  keep(L.q_tot L.intanat) ctitle(>1980) addtext(Method, OLS, Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Industry de-meaned, NO)
*GP Q
xtewreg fdm80_nik1 dm80_q dm80_intanat dm80_logage  _Iyear_1980-_Iyear_2015 if year>=1979 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/11_firm_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*q* *intan* ) e(rho)  drop(*logat *logage *year*) ctitle(>1980) addtext(Method, EW, Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Industry de-meaned, NO)
*AE Q
xtewreg fdm80_nik1 dm80_q2 dm80_intanat dm80_logage  _Iyear_1980-_Iyear_2015 if year>=1979 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/11_firm_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*q* *intan* ) e(rho)  drop(*logat *logage *year*) ctitle(>1980) addtext(Method, EW, Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Industry de-meaned, NO)
*PT Q
xtewreg fdm80_nik1 dm80_q_tot dm80_intanat dm80_logage  _Iyear_1980-_Iyear_2015 if year>=1979 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/11_firm_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*q* *intan* ) e(rho)  drop(*logat *logage *year*) ctitle(>1980) addtext(Method, EW, Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Industry de-meaned, NO)

*Industry FE

* GP Q
xtewreg fdmi80_nik1 dmi80_q dmi80_intanat dmi80_logage _Iyear_1980-_Iyear_2015 if year>=1979 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/11_firm_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*q* *intan* ) e(rho)  drop(*logat *logage *year*) ctitle(>1980) addtext(Method, EW, Age Controls, YES, Year FE, YES, Firm de-meaned, NO, Industry de-meaned, YES)
*PT Q
xtewreg fdmi80_nik1 dmi80_q_tot dmi80_intanat dmi80_logage _Iyear_1980-_Iyear_2015 if year>=1979 & year <= 2015, maxdeg(6) mis(1)
outreg2 using Tables/11_firm_intan.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(*q* *intan* ) e(rho)  drop(*logat *logage *year*) ctitle(>1980) addtext(Method, EW, Age Controls, YES, Year FE, YES, Firm de-meaned, NO, Industry de-meaned, YES)


***



******* OWNERSHIP EXPLANATIONS ******* 

capture erase Tables/25_firm_own.tex
* QIX
xtewreg fdm_nik1  dm_q  dm_mherf dm_owntotQIX_ma2 dm_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/25_firm_own.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q mherf ) e(rho)  drop(*logat *logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)

* INS OWN %
xtewreg fdm_nik1  dm_q  dm_mherf dm_pctinsown  dm_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/25_firm_own.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q mherf ) e(rho)  drop(*logat *logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)

*TRA
xtewreg fdm_nik1  dm_q  dm_mherf dm_owntotTRA_ma2 dm_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/25_firm_own.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q mherf ) e(rho)  drop(*logat *logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)

* DED
xtewreg fdm_nik1  dm_q  dm_mherf dm_owntotDED_ma2 dm_logage _Iyear_1990-_Iyear_2015 if year>=1989 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/25_firm_own.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q mherf ) e(rho)  drop(*logat *logage *year*) ctitle(>1990) ///
addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)



***



******* >2000: ALTERNATE EXPLANATIONS ******* 

capture erase Tables/28_firm_alt_00.tex
*base
xtewreg fdm00_nik1 dm00_q dm00_mherf dm00_owntotQIX_ma2 dm00_logage   _Iyear_2000-_Iyear_2015 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/28_firm_alt_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)
*Ext fin dependence
xtewreg fdmi00_nik1 dmi00_q dmi00_mherf dmi00_owntotQIX extfindep0 dmi00_logage _Iyear_2001-_Iyear_2015 if year>=2000 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/28_firm_alt_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, NO, Industry de-meaned, YES)
* bank dependence
xtewreg fdmi00_nik1 dmi00_q dmi00_mherf dmi00_owntotQIX  bankdep0 dmi00_logage  _Iyear_2000-_Iyear_2015 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/28_firm_alt_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, NO, Industry de-meaned, YES)
* safe assets
xtewreg fdmi00_nik1 dmi00_q dmi00_mherf dmi00_owntotQIX  AAtoAAA0 dmi00_logage  _Iyear_2000-_Iyear_2015 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/28_firm_alt_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, NO, Industry de-meaned, YES)
*intangibles
xtewreg fdm00_nik1 dm00_q   dm00_mherf dm00_owntotQIX_ma2 dm00_intanexgwat dm00_logage   _Iyear_2000-_Iyear_2015 if year>=1999  & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/28_firm_alt_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)
*globalization
xtewreg fdm00_nik1 dm00_q   dm00_mherf dm00_owntotQIX_ma2 dm00_pifoadj_sh dm00_logage   _Iyear_2000-_Iyear_2015 if year>=1999  & year <= 2015, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/28_firm_alt_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)
*regulation
xtewreg fdm00_nik1 dm00_q   dm00_mherf dm00_owntotQIX_ma2 dm00_a1m_logreg dm00_logage   _Iyear_2000-_Iyear_2014 if year>=1999  & year <= 2014, maxdeg(5) mis(1)
outreg2 q mherf owntotQIX* using Tables/28_firm_alt_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q *QIX* mherf *herf* ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES)



***



******* >2000: OWNERSHIP ******* 

* OWNERSHIP 
capture erase Tables/29_firm_own_00.tex
* QIX
xtewreg fdm00_nik1  dm00_q  dm00_mherf dm00_owntotQIX_ma2 dm00_logage _Iyear_2000-_Iyear_2015 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/29_firm_own_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q mherf ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) ///
addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Mismeasured, Q)

* INS OWN %
xtewreg fdm00_nik1  dm00_q  dm00_mherf dm00_pctinsown  dm00_logage _Iyear_2000-_Iyear_2015 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/29_firm_own_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q mherf ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) ///
addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Mismeasured, Q)

*TRA
xtewreg fdm00_nik1  dm00_q  dm00_mherf dm00_owntotTRA dm00_logage _Iyear_2000-_Iyear_2015 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/29_firm_own_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q mherf ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) ///
addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Mismeasured, Q)

* DED
xtewreg fdm00_nik1  dm00_q  dm00_mherf dm00_owntotDED dm00_logage _Iyear_2000-_Iyear_2015 if year>=1999 & year <= 2015, maxdeg(5) mis(1)
outreg2 using Tables/29_firm_own_00.tex, append symbol(**, *, +) tstat nonote nocons br dec(3) ///
sortvar(q mherf ) e(rho)  drop(*logat *logage *year*) ctitle(>2000) ///
addtext(Age Controls, YES, Year FE, YES, Firm de-meaned, YES, Mismeasured, Q)


erase tempfirm.dta
