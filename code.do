**#import data
clear 
import excel "merged_data38.xlsx", sheet("Sheet1") firstrow
xtset CO_ID YEAR
gen t = YEAR - 2014    
 foreach var in Economic_Development_Level	Per_Capita_Disposable_Income	Human_Capital_Level	Population_Size_2	Fixed_Asset_Investment	Industrial_Structure_2	Resident_Consumption_Level		Welfare_Level_1
   removed_time  {
 g `var'_1=`var'*t
 g `var'_2=`var'*t*t
 g `var'_3=`var'*t*t*t
}

global cv " Economic_Development_Level	Per_Capita_Disposable_Income	Human_Capital_Level	Population_Size_2	Fixed_Asset_Investment	Industrial_Structure_2	Resident_Consumption_Level		Welfare_Level_1
   removed_time    " 

global cv_t "Economic_Development_Level_* Per_Capita_Disposable_Income_* Human_Capital_Level_* Population_Size_2_*  Fixed_Asset_Investment_* Industrial_Structure2_* Resident_Consumption_Level_*   Welfare_Level_1_*  removed_time_*"
*********************************************
**#Baseline regression
*********************************************
xtset CO_ID YEAR
qui: reghdfe Employment_Scale did ,absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "Baseline regression.xls",replace  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  addtext(County characteristics × time polynomial, "No", Pilot selection variable × time polynomial, "No", County fixed effect, "Yes", Year fixed effect, "Yes", Pilot selection variable × year fixed effect, "No", County characteristics × year fixed effect, "No") 

qui: reghdfe Employment_Scale did $cv_t , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "Baseline regression.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  addtext(County characteristics × time polynomial, "Yes", Pilot selection variable × time polynomial, "Yes", County fixed effect, "Yes", Year fixed effect, "Yes", Pilot selection variable × year fixed effect, "No", County characteristics × year fixed effect, "No") 

qui: reghdfe Employment_Scale did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "Baseline regression.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  addtext(County characteristics × time polynomial, "No", Pilot selection variable × time polynomial, "No", County fixed effect, "Yes", Year fixed effect, "Yes", Pilot selection variable × year fixed effect, "Yes", County characteristics × year fixed effect, "Yes") 

*********************************************
**#Parallel trend test
*********************************************
gen policy_time = YEAR - 2021
tab policy

forvalues i = 6(-1)1 {
    gen pre_`i' = (policy_time == -`i' & treat == 1)
}

forvalues j = 1(1)1 {
    gen post_`j' = (policy_time == `j' & treat == 1)
}

gen current = (policy_time == 0 & treat == 1)

replace pre_1 = 0

reghdfe Employment_Scale pre_6 pre_5 pre_4 pre_3 pre_2 current post_1 pre_1 c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID) 
est store m1

coefplot, baselevels vertical keep(pre_* current post_*) ///
    level(95) yline(0, lcolor(edkblue*0.8)) ///
    xline(6, lwidth(vthin) lpattern(dash) lcolor(teal)) ///
    ylabel(, labsize(*0.75)) xlabel(, labsize(*0.75)) ///
    ytitle("Policy Dynamic Effects", size(small)) xtitle("Policy Timing", size(small)) ///
	addplot(line @b @at) ciopts(lpattern(dash) recast(rcap) msize(medium)) ///
    msymbol(circle_hollow) scheme(s1mono)
	#delimit ;

coefplot m1, keep(pre_6 pre_5 pre_4 pre_3 pre_2 pre_1 current post_1)

    transform(pre_1=0)

	coeflabels(pre_5="-5"  ///
	           pre_4="-4"  ///
	           pre_3="-3"  ///
	           pre_2="-2"  ///
	           pre_1="-1"  ///
	           current="1"  ///
	           post_1="2")  ///
	
    vertical omitted 

    ciopt(recast(rarea) color(gs14)) 

    msize(*0.5) c(l) color(gs0) 

    xline(6, lp(dash)) yline(0, lp(dash)) legend(off) 
    ytitle("Policy Dynamic Effects", size(small)) xtitle("Policy Timing", size(small)) ///
    graphregion(fcolor(gs16) lcolor(gs16)) 

    plotregion(lcolor("white") lwidth(*0.9));

#delimit cr
*********************************************
**#robustness test
*********************************************

**# Placebo test

*- (1) Time placebo test
xtset CO_ID YEAR
reghdfe Employment_Scale did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
estimates store did_results
didplacebo did_results, treatvar(did) pbotime(1(1)5)

*- (2) Spatial placebo test
didplacebo did_results, treatvar(did) pbounit rep(500) seed(1)

*- (3) Mixed placebo test
didplacebo did_results, treatvar(did) pbomix(1) seed(1)

**#PSM-DID
set  seed 0000
gen  norvar_1 = rnormal()
sort norvar_1

psmatch2 treat $cv, outcome(Employment_Scale) logit neighbor(2) ties common    ///
                          ate caliper(0.05)

asdoc pstest $cv, treated(treat) both graph

psgraph, title("Common Support Domain") saving(common_support, replace)

sum _pscore if treat == 1, detail


sum _pscore if treat == 0, detail

twoway (kdensity _pscore if treat == 1 & _pscore <= 0.2, lpattern(solid) lcolor(black) lwidth(thin) scheme(s1mono) ///
    xline(.0515571, lpattern(solid) lcolor(black)) ///
    xline(`r(mean)', lpattern(dash) lcolor(black)) ///
    saving(Cross-section _ Kernel density _ Beforematching, replace)) ///
    (kdensity _pscore if treat == 0 & _pscore <= 0.2, lpattern(dash)) , ///
    xlabel(  , labsize(medlarge) format(%02.1f)) ///
    ylabel(0(10)40, labsize(medlarge)) ///
    legend(label(1 "{treatment group}") label(2 "{control group}") size(medlarge) position(1) symxsize(10)) ///
	xscale(range(0 0.2))


sum _pscore if treat == 0 & _weight != ., detail

twoway(kdensity _pscore if treat == 1 & _pscore <= 0.2, lpattern(solid) lcolor(black) lwidth(thin) scheme(s1mono)  ///
      xline(.0515571, lpattern(solid) lcolor(black)) ///
      xline(`r(mean)', lpattern(dash)  lcolor(black))  ///
      saving(Cross-section _ Kernel density _ Aftermatching, replace))  ///
      (kdensity _pscore if treat == 0 & _weight != . & _pscore <= 0.2, lpattern(dash)),  ///
      xlabel(     , labsize(medlarge) format(%02.1f))  ///
      ylabel(0(10)40, labsize(medlarge))  ///
      legend(label(1 "{treatment group}") label(2 "{control group}") size(medlarge) position(1) symxsize(10))  ///
	  xscale(range(0 0.2))



xtset CO_ID YEAR


qui: reghdfe Employment_Scale did c.($cv)#i.YEAR if _weight != ., absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "PSM-did.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3)


qui: reghdfe Employment_Scale did c.($cv)#i.YEAR if _support == 1, absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "PSM-did.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)

	 
forvalue i = 2015/2022{
      preserve
          capture {
              keep if YEAR == `i'
              set seed 0000
              gen  norvar_2 = rnormal()
              sort norvar_2

              psmatch2 treat $cv , outcome(Employment_Scale) logit neighbor(2) ties common ate caliper(0.05)

              save `i'.dta, replace
              }
      restore
      }
	  
clear all

use  2015.dta, clear

forvalue k =2016/2022 {
      capture {
          append using `k'.dta
          }
      }

save psmdata.dta, replace


sum _pscore if treat == 1, detail  


sum _pscore if treat == 0, detail

twoway(kdensity _pscore if treat == 1 & _pscore <= 0.2, lpattern(solid)                     ///
              lcolor(black)                                                  ///
              lwidth(thin)                                                   ///
              scheme(s1mono)                                              ///
                     size(medlarge))                                         ///
              xline(.0591602   , lpattern(solid) lcolor(black))                ///
              xline(`r(mean)', lpattern(dash)  lcolor(black))                ///
              saving(Before_annual_kernel_densitymatching, replace))                           ///
      (kdensity _pscore if treat == 0 & _pscore <= 0.2, lpattern(dash)),                    ///
      xlabel(     , labsize(medlarge) format(%02.1f))                        ///
      ylabel(0(10)30, labsize(medlarge))                                       ///
      legend(label(1 "{treatment group}")                                      ///
             label(2 "{control group}")                                      ///
             size(medlarge) position(1) symxsize(10))



sum _pscore if treat == 0 & _weight != ., detail

twoway(kdensity _pscore if treat == 1 & _pscore <= 0.2, lpattern(solid)                     ///
              lcolor(black)                                                  ///
              lwidth(thin)                                                   ///
              scheme(s1mono)                                              ///
                     size(medlarge))                                         ///
              xline(.0591602   , lpattern(solid) lcolor(black))                ///
              xline(`r(mean)', lpattern(dash)  lcolor(black))                ///
              saving(after_annual_kernel_densitymatching, replace))                            ///
      (kdensity _pscore if treat == 0 & _weight != . & _pscore <= 0.2, lpattern(dash)),     ///
      xlabel(     , labsize(medlarge) format(%02.1f))                        ///
      ylabel(0(10)30, labsize(medlarge))                                       ///
      legend(label(1 "{treatment group}")                                      ///
             label(2 "{control group}")                                      ///
             size(medlarge) position(1) symxsize(10))


forvalue i = 2015/2022 {
          capture {
              qui: logit treat $cv if YEAR == `i',  vce(cluster CO_ID)
              est store ybyb`i'
              }
          }

local ybyblist ybyb2015 ybyb2016 ybyb2017 ybyb2018 ybyb2019 ybyb2020 ybyb2021 ybyb2022 
esttab `ybyblist' using Yearlymatching_Beforematching_Truncation.rtf, b(%6.3f) se(%6.3f) nogap compress star(* 0.1 ** 0.05 *** 0.01)  ar2 scalar(N) replace  mtitles( "2015b" "2016b" "2017b" "2018b"  "2019b" "2020b" "2021b" "2022b")  


forvalue i = 2015/2022 {
          capture {
              qui: logit treat $cv if YEAR == `i' & _weight != .,  vce(cluster CO_ID)
              est store ybya`i'
              }
          }

local ybyalist ybya2015 ybya2016 ybya2017 ybya2018 ybya2019 ybya2020 ybya2021 ybya2022 
esttab `ybyalist' using Yearlymatching_Aftermatching_Truncation.rtf, b(%6.3f) se(%6.3f)   nogap compress star(* 0.1 ** 0.05 *** 0.01)  ar2 scalar(N) replace  mtitles( "2015a" "2016a" "2017a" "2018a"  "2019a" "2020a" "2021a" "2022a")   


xtset CO_ID YEAR

qui: reghdfe Employment_Scale did c.($cv)#i.YEAR if _weight != ., absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "PSM-did.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)


qui: reghdfe Employment_Scale did c.($cv)#i.YEAR if _support == 1, absorb(CO_ID YEAR) vce(cluster CO_ID)

outreg2  using  "PSM-did.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)

clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow

global cv " Economic_Development_Level	Per_Capita_Disposable_Income	Human_Capital_Level	Population_Size_2	Fixed_Asset_Investment	Industrial_Structure_2	Resident_Consumption_Level		Welfare_Level_1
   removed_time    " 

preserve 
winsor2 Employment_Scale $cv, cuts(1,99)
reghdfe Employment_Scale_w did c.(Economic_Development_Level_w Per_Capita_Disposable_Income_w   Human_Capital_Level_w Population_Size_2_w  Fixed_Asset_Investment_w Industrial_Structure_2_w  Resident_Consumption_Level_w  Welfare_Level_1_w  removed_time_w)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "1%reduction_in_size.xls",replace  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  addtext(Control variables: "Yes", Control variable × time fixed effect: "Yes", County fixed effect: "Yes", Year fixed effect: "Yes") 

**# Exclude the influence of special samples
clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow

global cv " Economic_Development_Level	Per_Capita_Disposable_Income	Human_Capital_Level	Population_Size_2	Fixed_Asset_Investment	Industrial_Structure_2	Resident_Consumption_Level		Welfare_Level_1
   removed_time    " 

drop if Municipality_Directly_under_the_Central_Government == 1

drop if County_level_City == 1

qui: reghdfe Employment_Scale did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "Robustness_test_Re_regression.xls", replace  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  ctitle("delete county") addtext(Control variables: "Yes", Control variable × time fixed effect: "Yes", County fixed effect: "Yes", Year fixed effect: "Yes") 

**# Exclude the influence of the concurrent policies

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow clear

global cv " Economic_Development_Level	Per_Capita_Disposable_Income	Human_Capital_Level	Population_Size_2	Fixed_Asset_Investment	Industrial_Structure_2	Resident_Consumption_Level		Welfare_Level_1
   removed_time    " 

clear
set obs 7 
gen model = _n  
gen policy = "" 
gen coef = .    
gen lower = .  
gen upper = .    
gen se = .
gen pvalue = .

forval i = 1/7 {
    preserve  
    import excel "merged_data38.xlsx", sheet("Sheet1") firstrow clear
    
    if `i' == 1 {
        local policy "policyforcomprehensivedemonst"
        local policy_name "policyforcomprehensivedemonst"
    }
    else if `i' == 2 {
        local policy "pilotpoliciesforreturningto"
        local policy_name "pilotpoliciesforreturningto"
    }
    else if `i' == 3 {
        local policy "BroadbandChinademonstration"
        local policy_name "BroadbandChinademonstration"
    }
    else if `i' == 4 {
        local policy "universaltelecommunicationsser"
        local policy_name "universaltelecommunicationsser"
    }
    else if `i' == 5 {
        local policy "pilotpolicyfortheInternetP"
        local policy_name "pilotpolicyfortheInternetP"
    }
    else if `i' == 6 {
        local policy "nationalpolicyonkeysupportf"
        local policy_name "nationalpolicyonkeysupportf"
    }
    else {
        local policy "policyforcomprehensivedemonst pilotpoliciesforreturningto BroadbandChinademonstration universaltelecommunicationsser pilotpolicyfortheInternetP nationalpolicyonkeysupportf "
        local policy_name "all policies"
    }
    

    qui: reghdfe Employment_Scale did c.($cv)#i.YEAR `policy', absorb(CO_ID YEAR) vce(cluster CO_ID)
	local b = _b[did]
    local se = _se[did]
    local p = 2 * (1 - normal(abs(`b') / `se'))  
    
    restore
    replace policy = "`policy_name'" in `i'
    replace coef = `b' in `i' 
	replace pvalue = `p' in `i'
	replace se = `se' in `i'   
    replace lower = `b' - 1.96 * `se' in `i'  
	replace upper = `b' + 1.96 * `se' in `i' 
}
gen significant = (lower > 0 | upper < 0)  
gen stars = ""
replace stars = "***" if pvalue < 0.01
replace stars = "**"  if pvalue >= 0.01 & pvalue < 0.05
replace stars = "*"   if pvalue >= 0.05 & pvalue < 0.1

gen coef_label = string(coef, "%9.3f") + stars  

gen se_label = "(" + string(0.07, "%9.2f") + ")"  
gen label_full = coef_label + char(10) + se_label

twoway ///

    (rcap upper lower model, horizontal color(edkblue) lwidth(medthin)) ///
    ///

    (scatter model coef if significant==1, ///
        color(red) msymbol(d) msize(medium) ///
        mlabel(label_full) mlabcolor(black) mlabsize(2.2) mlabp(6)) ///
    ///

    (scatter model coef if significant==0, ///
        color(edkblue) msymbol(dh) msize(medium) ///
        mlabel(label_full) mlabcolor(black) mlabsize(2.2) mlabp(6)), ///
    ///

    ylabel(1 "1) Policy1" ///
           2 "2) Policy2" ///
           3 "3) Policy3" ///
           4 "4) Policy4" ///
           5 "5) Policy5" ///
           6 "6) Policy6" ///
           7 "7) all policies", ///
           angle(0) labsize(small) nogrid) ///
    ///

    xline(0, lcolor(gs10) lpattern(dash) lwidth(medium)) ///
    xtitle("Did coefficient (95% confidence interval)", size(medsmall)) ///
    xlabel(, grid gmax) ///
    ///

    ytitle("") ///
    legend(off) ///
    yscale(range(0 8)) ///
	yscale(reverse) /// 
    graphregion(color(white)) ///
    plotregion(margin(tiny))

graph export "Stability_test_Concurrent_Policy.png", replace width(1400) height(900)
*********************************************
**#instrumental variable
*********************************************
clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow
   
global cv " Economic_Development_Level	Per_Capita_Disposable_Income	Human_Capital_Level	Population_Size_2	Fixed_Asset_Investment	Industrial_Structure_2	Resident_Consumption_Level		Welfare_Level_1
   removed_time    " 

gen Minimum_Distance_to_Optical_Fiber_Backbone_City_p=Minimum_Distance_to_Optical_Fiber_Backbone_City_km*post

ivreghdfe Employment_Scale  (did =Minimum_Distance_to_Optical_Fiber_Backbone_City_p) $cv,absorb(CO_ID YEAR)  vce(cluster CO_ID)  first


outreg2 using Instrumental_variable_test.doc,replace tstat bdec(4) tdec(4) adjr2 addtext(Control variables: "Yes", Control variable × time fixed effect: "Yes", County fixed effect: "Yes", Year fixed effect: "Yes") 
*********************************************
**#mechanism analysis
*********************************************
clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow


global cv " Economic_Development_Level	Per_Capita_Disposable_Income	Human_Capital_Level	Population_Size_2	Fixed_Asset_Investment	Industrial_Structure_2	Resident_Consumption_Level		Welfare_Level_1
   removed_time    " 
   
qui: reghdfe Government_Intervention did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "Government_Intervention.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(Control variables: "Yes", Control variable × time fixed effect: "Yes", County fixed effect: "Yes", Year fixed effect: "Yes") 


qui: reghdfe AgriculturalTechnologyLevels  did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "AgriculturalTechnologyLevels.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(Control variables: "Yes", Control variable × time fixed effect: "Yes", County fixed effect: "Yes", Year fixed effect: "Yes") 


qui: reghdfe Degree_of_Financing_Constraints  did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "Degree_of_Financing_Constraints.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(Control variables: "Yes", Control variable × time fixed effect: "Yes", County fixed effect: "Yes", Year fixed effect: "Yes") 
*********************************************
**#heterogeneity test
*********************************************
gen dw=0


replace dw = 1 if Municipal_District==1 | County_level_City==1
gen mountain1=0
replace mountain1 = 1 if Terrain_Relief_Degree > 0.5
reghdfe Employment_Scale c.npoor#c.did#c.dw   c.npoor#did  c.dw#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a1
esttab a1  using location.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f)  nolabel r2 replace
reghdfe Employment_Scale c.npoor#c.did#c.ethnic   c.npoor#did  c.ethnic#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a2
esttab a2  using ethnic.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f)  nolabel r2 replace
reghdfe Employment_Scale c.npoor#c.did#c.hu   c.npoor#did  c.hu#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a3
esttab a3  using hu.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f) nolabel r2 replace
reghdfe Employment_Scale c.npoor#c.did#c.mountain1   c.npoor#did  c.mountain1#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a4
esttab a4  using mountain1.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f)  nolabel r2 replace
reghdfe Employment_Scale c.npoor#c.did#c.Labor_Code   c.npoor#did  c.Labor_Code#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a5
esttab a5  using Labor_Code.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f)  nolabel r2 replace

*********************************************
**#Further analysis
*********************************************


clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow


global cv " Economic_Development_Level	Per_Capita_Disposable_Income	Human_Capital_Level	Population_Size_2	Fixed_Asset_Investment	Industrial_Structure_2	Resident_Consumption_Level		Welfare_Level_1
   removed_time    " 
   
xtset CO_ID YEAR
gen cld=class*did
gen clp=class*post
gen clt=class*treat
reghdfe Employment_Scale cld clp clt c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "Regional_spillover_effect.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(Control variables: "Yes", Control variable × time fixed effect: "Yes", County fixed effect: "Yes", Year fixed effect: "Yes") 


clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow

global cv " Economic_Development_Level	Per_Capita_Disposable_Income	Human_Capital_Level	Population_Size_2	Fixed_Asset_Investment	Industrial_Structure_2	Resident_Consumption_Level		Welfare_Level_1
   removed_time    " 

xtset CO_ID YEAR

reghdfe  Employment_Scale did c_did50-c_did350 c.($cv)#i.YEAR , absorb(CO_ID YEAR)  vce(cluster CO_ID) 
coefplot, keep(did c_did*)  coeflabels(did="0" c_did50="50" c_did100="100" c_did150="150" c_did200="200" c_did250="250" c_did300="300" c_did350="350")   ///
vertical  addplot(line @b @at) ytitle("employment scale") xtitle("Spatial distance (kilometers)")yline(0) levels(90) scheme(s1mono) ciopts(recast(rcap) lpattern(dash))





