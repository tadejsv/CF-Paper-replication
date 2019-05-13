
set more off 	
pause off
set logtype text
set mem 500M

set scheme s1color , permanently

/*************** DESCRIPTION **************************************************
* Creates Table 19, studying the evolution of concentration and TFP 
*
*	Inputs:  NBER-CES Database and Census Concentration
*	Outputs: Table 19

*******************************************************************************/   

*** TFP from NBER-CES database 
import delimited "0.raw_inputs\naics5811.csv", clear
keep naics year vship tfp4 tfp5 prode emp pay prodw cap vadd
save temp, replace

use 2.intermediate\CenCon_naics, clear
g len = strlen(naicsid)
keep if len == 6
rename naicsid naics
destring naics, replace

merge 1:1 naics year using temp,nogen 
erase temp.dta

* Compute productivity ratios 
g vshipk = vship/cap
g logvshipk = log(vshipk)
g vaddk = vadd/cap
g logvaddk = log(vaddk)
g vshipemp = vship/emp
g logvshipemp = log(vshipemp)
g vaddemp = vadd/emp
g logvaddemp = log(vaddemp)
g prodpct = (emp-prode)/emp
g logprodpct = log(prodpct)
g avgwage = pay/emp
g logavgwage = log(avgwage)
g avgprodwage = prodw/prode
g logavgprodwage = log(avgprodwage)

rename naics indcode
egen indgroup = group(indcode)
xtset indgroup year

* compute changes in concentration (end in 2012 due to Census availability)
foreach X in a1_cenconc4 a1_cenconc8 a1_cenconc20 a1_cenconc50  {	
		sort indgr year
		g t`X'97 = `X' if year == 1997
		g t`X'02 = `X' if year == 2002
		g t`X'07 = `X' if year == 2007
		g t`X'12 = `X' if year == 2012
		
		egen `X'_97 = min(t`X'97), by(indgr)
		egen `X'_02 = min(t`X'02), by(indgr)
		egen `X'_07 = min(t`X'07), by(indgr)
		egen `X'_12 = min(t`X'12), by(indgr)
		g `X'_d9702 = `X'_02 - `X'_97 
		g `X'_d0207 = `X'_07 - `X'_02 
		g `X'_d0712 = `X'_12 - `X'_07 
		g `X'_d9712 = `X'_12 - `X'_97 
		g `X'_d0212 = `X'_12 - `X'_02 
		drop t`X'*
}

replace tfp5 = 100*tfp5
replace tfp4 = 100*tfp4

* compute changes in productivity measures (end in 2011 due to data availability)
foreach X in tfp5 tfp4 vshipk vaddk vshipemp vaddemp prodpct avgwage avgprodwage {	
		sort indgr year
		g t`X'97 = `X' if year == 1997
		g t`X'02 = `X' if year == 2002
		g t`X'07 = `X' if year == 2007
		g t`X'11 = `X' if year == 2011
		
		egen `X'_97 = min(t`X'97), by(indgr)
		egen `X'_02 = min(t`X'02), by(indgr)
		egen `X'_07 = min(t`X'07), by(indgr)
		egen `X'_11 = min(t`X'11), by(indgr)
		g `X'_d9702 = `X'_02 - `X'_97
		g `X'_d0207 = `X'_07 - `X'_02
		g `X'_d0711 = `X'_11 - `X'_07
		g `X'_d9711 = `X'_11 - `X'_97
		g `X'_d0211 = `X'_11 - `X'_02
		drop t`X'*
}


* Study correlations 
capture erase ../Tables/19_conctfp.txt
reg tfp5_d9702 a1_cenconc4_d9702 if year == 1997 // & indcode ~= 334111
outreg2 using ../Tables/19_conctfp.txt, append symbol(**, *, +) nonote nocons br dec(3) e(r2) ctitle("97-02")  
reg tfp5_d0211 a1_cenconc4_d0212 if year == 1997 & indcode ~= 334111
outreg2 using ../Tables/19_conctfp.txt, append symbol(**, *, +) nonote nocons br dec(3) e(r2) ctitle("02-12") 
