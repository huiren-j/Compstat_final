clear
**************************************************
** This do file uses several user written files
** ttable2.ado for comparison of means and T-stats
** outreg2.ado for table construction
** twopm.ado for Hurdle Model Estimation
**************************************************
set more off
use "BoganFernandezAER.dta", clear
local controlVars specialneeds2 specialneeds4 grad onechild twochild threechild fourormorechild conditions healthins married divorce young AGEOFHEAD EDUCHD_ black female own1 unemphd manager finance lninc IMPIRA2 benefitpension contributionpension WTRINHERITANCE regionM regionW regionN

**** summary statistics ******
logout, save(Tab2_corr) excel replace: ttable2 IMPSTOCK2 WTRBOND IMPCHKSAV2_ own *pension IMPIRA2_ IMPSTOCK_ TOT* pctstock1 IMPWEALTHEQ_, by(specialneeds2)  f(%8.2g)
log using summarytables.txt,replace text
mean IMPSTOCK2 WTRBOND IMPCHKSAV2_ own *pension IMPIRA2_ IMPSTOCK_ TOT* pctstock1 IMPWEALTHEQ_

**** Safe Asset Results ****

** Randome Effects Logit Model
qui xtlogit WTRBONDS `controlVars' risk* _Iyear_1999-_Iyear_2009 , re i(hhid) vce(r)
margins ,dydx(*) predict(pu0) post
outreg2 using probholdbonds.xls, replace label title("Prob of Holding Bonds") ctitle(Logit 1) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, No, Cluster Error, Yes)
qui xtlogit WTRBONDS  `controlVars' risk* WTRCOMPUTERHD _Iyear_1999-_Iyear_2009 , re i(hhid) vce(r)
margins ,dydx(*) predict(pu0) post
outreg2 using probholdbonds.xls, append label title("Prob of Holding Bonds") ctitle(Logit 2) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, Yes, Cluster Error, Yes)

** Random Effects Linear Probability model without weights
qui xtreg WTRBONDS  `controlVars' risk*  _Iyear_1999-_Iyear_2009 if regnum!=0 , re i(hhid) vce(r)
outreg2 using probholdbonds.xls, append label title("Prob of Holding Bonds") ctitle(LPM 1) drop(_Iy* regionMW regionN regionW) addtext(HH Weights, No, HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, No, Health Insurance, No, Computer Controls, No, Cluster Error, Yes)
qui xtreg WTRBONDS  `controlVars' risk* WTRCOMPUTERHD  _Iyear_1999-_Iyear_2009 if regnum!=0, re i(hhid) vce(r)
outreg2 using probholdbonds.xls, append label title("Prob of Holding Bonds") ctitle(LPM 2) drop(_Iy* regionMW regionN regionW) addtext(HH Weights, No, HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, Yes, Cluster Error, Yes)

**** Risky Asset Results ******

qui xtlogit IMPSTOCK2 `controlVars' risk* _Iyear_1999-_Iyear_2009 , re i(hhid) vce(r)
margins ,dydx(*) predict(pu0) post
outreg2 using probholdstock.xls, replace label title("Prob of Holding Stock") ctitle(Logit 1) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, No, Health Insurance, No, Computer Controls, No, Cluster Error, Yes)
qui xtlogit IMPSTOCK2  `controlVars' risk* WTRCOMPUTERHD _Iyear_1999-_Iyear_2009 , re i(hhid) vce(r)
margins ,dydx(*) predict(pu0) post
outreg2 using probholdstock.xls, append label title("Prob of Holding Stock") ctitle(Logit 2) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, Yes, Cluster Error, Yes)

** Random Effects Linear Probability model without weights
qui xtreg IMPSTOCK2  `controlVars' risk*  _Iyear_1999-_Iyear_2009 if regnum!=0 , re i(hhid) vce(r)
outreg2 using probholdstock.xls, append label title("Prob of Holding Stock") ctitle(LPM 1) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, No, Health Insurance, No, Computer Controls, No, Cluster Error, Yes)
qui xtreg IMPSTOCK2  `controlVars' risk* WTRCOMPUTERHD  _Iyear_1999-_Iyear_2009 if regnum!=0, re i(hhid) vce(r)
outreg2 using probholdstock.xls, append label title("Prob of Holding Stock") ctitle(LPM 2) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, Yes, Cluster Error, Yes)

***************************************************************************************************************************************************


*********************************************************************
*** Least Squares estimates of pct bond
*********************************************************************
** Random Effects Linear Probability model without weights
qui xtreg pctbonds `controlVars' risk*  _Iyear_1999-_Iyear_2009 if regnum!=0 , re i(hhid) vce(r)
outreg2 using xtpctbonds.xls, replace label title("PCT Bonds") ctitle(LPM 1) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, No, Health Insurance, No, Computer Controls, No, Cluster Error, Yes)
qui xtreg pctbonds `controlVars' risk* WTRCOMPUTERHD  _Iyear_1999-_Iyear_2009 if regnum!=0, re i(hhid) vce(r)
outreg2 using xtpctbonds.xls, append label title("PCT Bonds") ctitle(LPM 2) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, Yes, Cluster Error, Yes)

** Panel Tobit model with random effect
xttobit pctbonds `controlVars' risk*  _Iyear_1999-_Iyear_2009 if regnum!=0, ul(1) ll(0)
outreg2 using xtpctbonds.xls, append label title("PCT Bonds") ctitle(Tobit RE 1) drop(_Iy* regionMW regionN regionW) addtext(HH Weights, No, Year FE, Yes, Region Dummies, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, No, Cluster Error, No) 
xttobit pctbonds `controlVars'  risk* WTRCOMPUTERHD _Iyear_1999-_Iyear_2009 if regnum!=0, ul(1) ll(0)
outreg2 using xtpctbonds.xls, append label title("PCT Bonds") ctitle(Tobit RE 2) drop(_Iy* regionMW regionN regionW) addtext(HH Weights, No, Year FE, Yes, Region Dummies, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, No, Cluster Error, Yes)

** Two part hurdle model without weights
twopm pctbonds `controlVars'  risk* _Iyear_1999-_Iyear_2009 if regnum!=0, f(logit) s(regress) robust cluster(hhid)
outreg2 using xtpctbonds.xls, append label nocons title("Hurdle Model of PCT Bonds") drop(_Iy* regionMW regionN regionW) addtext(HH Weights, No, Year FE, Yes, Region Dummies, Yes, Risk Controls, No, Health Controls, No, Health Insurance, No, Computer Controls, No, Cluster Error, No)

*********************************************************************
*** Least Squares estimates of pct stock
*********************************************************************
** Random Effects Linear Probability model without weights
qui xtreg pctstock1 `controlVars' risk*  _Iyear_1999-_Iyear_2009 if regnum!=0 , re i(hhid) vce(r)
outreg2 using xtpctstock.xls, replace label title("PCT STOCK") ctitle(RE 2) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, No, Health Insurance, No, Computer Controls, No, Cluster Error, Yes)
qui xtreg pctstock1 `controlVars' risk* WTRCOMPUTERHD  _Iyear_1999-_Iyear_2009 if regnum!=0, re i(hhid) vce(r)
outreg2 using xtpctstock.xls, append label title("PCT STOCK") ctitle(RE 3) drop(_Iy* regionMW regionN regionW) addtext(HH RE, Yes, Year FE, Yes, Region Dummies, Yes, Risk Controls, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, Yes, Cluster Error, Yes)

** Panel Tobit model with random effect
xttobit pctstock1 `controlVars' risk* _Iyear_1999-_Iyear_2009 if regnum!=0, ul(1) ll(0)
outreg2 using xtpctstock.xls, append label title("PCT STOCK") ctitle(Tobit RE 1) drop(_Iy* regionMW regionN regionW) addtext(HH Weights, No, Year FE, Yes, Region Dummies, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, No, Cluster Error, No) 
xttobit pctstock1 `controlVars'  risk* WTRCOMPUTERHD _Iyear_1999-_Iyear_2009 if regnum!=0, ul(1) ll(0)
outreg2 using xtpctstock.xls, append label title("PCT STOCK") ctitle(Tobit RE 2) drop(_Iy* regionMW regionN regionW) addtext(HH Weights, No, Year FE, Yes, Region Dummies, Yes, Health Controls, Yes, Health Insurance, Yes, Computer Controls, No, Cluster Error, Yes)

** Two part hurdle model without weights
twopm pctstock1 `controlVars'  risk* _Iyear_1999-_Iyear_2009 if regnum!=0, f(logit) s(regress) robust cluster(hhid)
outreg2 using xtpctstock.xls, append label nocons title("PCT STOCK") ctitle(Hurdle) drop(_Iy* regionMW regionN regionW) addtext(HH Weights, No, Year FE, Yes, Region Dummies, Yes, Risk Controls, No, Health Controls, No, Health Insurance, No, Computer Controls, No, Cluster Error, No)

log close
