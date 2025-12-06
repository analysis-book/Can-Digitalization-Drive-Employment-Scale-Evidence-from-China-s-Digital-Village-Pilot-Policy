

clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow

*- 设置字体
graph set window fontface "Times New Roman"
graph set window fontface "宋体"

xtset CO_ID YEAR

gen t = YEAR - 2014    
 foreach var in 生产总值指数 人均可支配收入 人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平 农村创业活力 福利水平1   removed_time  {
 g `var'_1=`var'*t
 g `var'_2=`var'*t*t
 g `var'_3=`var'*t*t*t
}

global cv " 生产总值指数 人均可支配收入 人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平 农村创业活力 福利水平1   removed_time    " 

global cv_t "生产总值指数_* 人均可支配收入_* 人力资本水平_* 人口规模2_*  固定资产投资_* 产业结构2_* 居民消费水平_*  农村创业活力_* 福利水平1_*  removed_time_*"
*********************************************
**#基准回归
*********************************************

xtset CO_ID YEAR

*列(1)
qui: reghdfe 就业规模 did ,absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "基准回归.xls",replace  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  addtext(县域特征×时间多项式,"否" ,试点选择变量×时间多项式 , "否",县域固定效应, "是", 年份固定效应,"是",试点选择变量×年份固定效应,"否",县域特征×年份固定效应,"否" ) 

*列(2)
qui: reghdfe 就业规模 did $cv_t , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "基准回归.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  addtext(县域特征×时间多项式,"是" ,试点选择变量×时间多项式 , "是",县域固定效应, "是", 年份固定效应,"是",试点选择变量×年份固定效应,"否",县域特征×年份固定效应,"否") 
*列(3)
qui: reghdfe 就业规模 did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "基准回归.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  addtext(县域特征×时间多项式,"否" ,试点选择变量×时间多项式 , "否",县域固定效应, "是", 年份固定效应,"是",试点选择变量×年份固定效应,"是",县域特征×年份固定效应,"是") 

*********************************************
**#平行趋势检验
*********************************************

*- 生成政策实施前后的时间变量
gen policy_time = YEAR - 2021
tab policy

*- 生成政策实施前的虚拟变量
forvalues i = 6(-1)1 {
    gen pre_`i' = (policy_time == -`i' & treat == 1)
}

*- 生成政策实施后的虚拟变量
forvalues j = 1(1)1 {
    gen post_`j' = (policy_time == `j' & treat == 1)
}

*- 生成政策实施当年的虚拟变量
gen current = (policy_time == 0 & treat == 1)

*- 事前一期替换为0，用于绘图
replace pre_1 = 0

*- 将生成的虚拟变量加入回归模型，检验政策实施前的系数是否显著
reghdfe 就业规模 pre_6 pre_5 pre_4 pre_3 pre_2 current post_1 pre_1 c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID) 
est store m1

*- 绘制平行趋势图
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
**#稳健性检验
*********************************************

**# 一、安慰剂检验*************************

*- (1) 时间安慰剂检验
xtset CO_ID YEAR
reghdfe 就业规模 did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
estimates store did_results
didplacebo did_results, treatvar(did) pbotime(1(1)5)

*- (2) 空间安慰剂检验
didplacebo did_results, treatvar(did) pbounit rep(500) seed(1)

*- (3) 混合安慰剂检验
didplacebo did_results, treatvar(did) pbomix(1) seed(1)

**# 二、PSM-DID*************************

**# (1) 截面匹配
**# 1.1估计倾向得分

*- 生成随机数
set  seed 0000
gen  norvar_1 = rnormal()
sort norvar_1

*- 近邻匹配 (1:2/0.05/控制变量=协变量)
psmatch2 treat $cv, outcome(就业规模) logit neighbor(2) ties common    ///
                          ate caliper(0.05)

**# 1.2 平衡性检验

asdoc pstest $cv, treated(treat) both graph

psgraph, title("共同支撑域") saving(common_support, replace)


**# 1.3 绘制核密度函数图

*- 处理组的倾向得分均值为.0515571
sum _pscore if treat == 1, detail

*- 匹配前

sum _pscore if treat == 0, detail

twoway (kdensity _pscore if treat == 1 & _pscore <= 0.2, lpattern(solid) lcolor(black) lwidth(thin) scheme(s1mono) ///
    ytitle("{stSans:核}""{stSans:密}""{stSans:度}", size(medlarge) orientation(h)) ///
    xtitle("{stSans:匹配前的倾向得分值}", size(medlarge)) ///
    xline(.0515571, lpattern(solid) lcolor(black)) ///
    xline(`r(mean)', lpattern(dash) lcolor(black)) ///
    saving(截面_核密度_匹配前, replace)) ///
    (kdensity _pscore if treat == 0 & _pscore <= 0.2, lpattern(dash)) , ///
    xlabel(  , labsize(medlarge) format(%02.1f)) ///
    ylabel(0(10)40, labsize(medlarge)) ///
    legend(label(1 "{stSans:处理组}") label(2 "{stSans:控制组}") size(medlarge) position(1) symxsize(10)) ///
	xscale(range(0 0.2))


*- 匹配后

sum _pscore if treat == 0 & _weight != ., detail

twoway(kdensity _pscore if treat == 1 & _pscore <= 0.2, lpattern(solid) lcolor(black) lwidth(thin) scheme(s1mono)  ///
      ytitle("{stSans:核}""{stSans:密}""{stSans:度}", size(medlarge) orientation(h))  ///
      xtitle("{stSans:匹配后的倾向得分值}", size(medlarge))  ///
      xline(.0515571, lpattern(solid) lcolor(black)) ///
      xline(`r(mean)', lpattern(dash)  lcolor(black))  ///
      saving(截面_核密度_匹配后, replace))  ///
      (kdensity _pscore if treat == 0 & _weight != . & _pscore <= 0.2, lpattern(dash)),  ///
      xlabel(     , labsize(medlarge) format(%02.1f))  ///
      ylabel(0(10)40, labsize(medlarge))  ///
      legend(label(1 "{stSans:处理组}") label(2 "{stSans:控制组}") size(medlarge) position(1) symxsize(10))  ///
	  xscale(range(0 0.2))


**# 1.4 匹配后回归&比对回归结果

xtset CO_ID YEAR

*- 列(1)：使用权重不为空的样本 (比较后使用此方法)

qui: reghdfe 就业规模 did c.($cv)#i.YEAR if _weight != ., absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "PSM-did回归.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3)

*- 列(2)：使用满足共同支撑假设的样本

qui: reghdfe 就业规模 did c.($cv)#i.YEAR if _support == 1, absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "PSM-did回归.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)

	  
**# (2) 逐期回归
**# 2.1 近邻匹配（1:2）

*- 第一个循环：逐年进行PSM匹配，分别保存匹配后的数据集
forvalue i = 2015/2022{
      preserve
          capture {
              keep if YEAR == `i'
              set seed 0000
              gen  norvar_2 = rnormal()
              sort norvar_2

              psmatch2 treat $cv , outcome(就业规模) logit neighbor(2) ties common ate caliper(0.05)

              save `i'.dta, replace
              }
      restore
      }
	  
*- 第二个循环：各年份匹配后数据合并为面板数据
clear all

use  2015.dta, clear

forvalue k =2016/2022 {
      capture {
          append using `k'.dta
          }
      }

save psm逐年面板data.dta, replace

**# 2.2 倾向得分值的核密度图

sum _pscore if treat == 1, detail  // 处理组的倾向得分均值为.0591602

*- 匹配前

sum _pscore if treat == 0, detail

twoway(kdensity _pscore if treat == 1 & _pscore <= 0.2, lpattern(solid)                     ///
              lcolor(black)                                                  ///
              lwidth(thin)                                                   ///
              scheme(s1mono)                                              ///
              ytitle("{stSans:核}""{stSans:密}""{stSans:度}",                ///
                     size(medlarge) orientation(h))                          ///
              xtitle("{stSans:匹配前的倾向得分值}",                          ///
                     size(medlarge))                                         ///
              xline(.0591602   , lpattern(solid) lcolor(black))                ///
              xline(`r(mean)', lpattern(dash)  lcolor(black))                ///
              saving(逐年_核密度_匹配前, replace))                           ///
      (kdensity _pscore if treat == 0 & _pscore <= 0.2, lpattern(dash)),                    ///
      xlabel(     , labsize(medlarge) format(%02.1f))                        ///
      ylabel(0(10)30, labsize(medlarge))                                       ///
      legend(label(1 "{stSans:处理组}")                                      ///
             label(2 "{stSans:控制组}")                                      ///
             size(medlarge) position(1) symxsize(10))


*- 匹配后

sum _pscore if treat == 0 & _weight != ., detail

twoway(kdensity _pscore if treat == 1 & _pscore <= 0.2, lpattern(solid)                     ///
              lcolor(black)                                                  ///
              lwidth(thin)                                                   ///
              scheme(s1mono)                                              ///
              ytitle("{stSans:核}""{stSans:密}""{stSans:度}",                ///
                     size(medlarge) orientation(h))                          ///
              xtitle("{stSans:匹配后的倾向得分值}",                          ///
                     size(medlarge))                                         ///
              xline(.0591602   , lpattern(solid) lcolor(black))                ///
              xline(`r(mean)', lpattern(dash)  lcolor(black))                ///
              saving(逐年_核密度_匹配后, replace))                            ///
      (kdensity _pscore if treat == 0 & _weight != . & _pscore <= 0.2, lpattern(dash)),     ///
      xlabel(     , labsize(medlarge) format(%02.1f))                        ///
      ylabel(0(10)30, labsize(medlarge))                                       ///
      legend(label(1 "{stSans:处理组}")                                      ///
             label(2 "{stSans:控制组}")                                      ///
             size(medlarge) position(1) symxsize(10))


**# 2.3 逐年平衡性检验

*- 匹配前
forvalue i = 2015/2022 {
          capture {
              qui: logit treat $cv if YEAR == `i',  vce(cluster CO_ID)
              est store ybyb`i'
              }
          }

local ybyblist ybyb2015 ybyb2016 ybyb2017 ybyb2018 ybyb2019 ybyb2020 ybyb2021 ybyb2022 
esttab `ybyblist' using 逐年匹配_匹配前_缩尾.rtf, b(%6.3f) se(%6.3f) nogap compress star(* 0.1 ** 0.05 *** 0.01)  ar2 scalar(N) replace  mtitles( "2015b" "2016b" "2017b" "2018b"  "2019b" "2020b" "2021b" "2022b")   title("逐年PSM平衡性检验_匹配前")


*- 匹配后
forvalue i = 2015/2022 {
          capture {
              qui: logit treat $cv if YEAR == `i' & _weight != .,  vce(cluster CO_ID)
              est store ybya`i'
              }
          }

local ybyalist ybya2015 ybya2016 ybya2017 ybya2018 ybya2019 ybya2020 ybya2021 ybya2022 
esttab `ybyalist' using 逐年匹配_匹配后_缩尾.rtf, b(%6.3f) se(%6.3f)   nogap compress star(* 0.1 ** 0.05 *** 0.01)  ar2 scalar(N) replace  mtitles( "2015a" "2016a" "2017a" "2018a"  "2019a" "2020a" "2021a" "2022a")   title("逐年PSM平衡性检验_匹配后")		 


**# 2.4 匹配后回归结果

xtset CO_ID YEAR

*- 列(3)：使用权重不为空的样本

qui: reghdfe 就业规模 did c.($cv)#i.YEAR if _weight != ., absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "PSM-did回归.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)

*- 列(4)：使用满足共同支撑假设的样本 (比较后使用此方法)

qui: reghdfe 就业规模 did c.($cv)#i.YEAR if _support == 1, absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "PSM-did回归.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3)
**# 三、数据缩尾*************************
clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow

*- 设置字体
graph set window fontface "Times New Roman"
graph set window fontface "宋体"

*- 全局定义暂元
global cv "生产总值指数 人均可支配收入   人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平  农村创业活力    福利水平1 removed_time " 
preserve 
winsor2 就业规模 $cv, cuts(1,99)
reghdfe 就业规模_w did c.(生产总值指数_w 人均可支配收入_w   人力资本水平_w 人口规模2_w  固定资产投资_w 产业结构2_w  居民消费水平_w  农村创业活力_w 福利水平1_w  removed_time_w)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "缩尾百分之1回归.xls",replace  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 
**# 四、排除特殊样本影响*************************
clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow

*- 设置字体
graph set window fontface "Times New Roman"
graph set window fontface "宋体"

*- 全局定义暂元
global cv "生产总值指数 人均可支配收入   人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平  农村创业活力    福利水平1 removed_time " 
*- 删除直辖市：96个
drop if 直辖市 == 1
*- 删除县级市：328个
drop if 县级市 == 1

*- 稳健性：列(2)
qui: reghdfe 就业规模 did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "稳健性检验_重新回归.xls", replace  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  ctitle("delete county") addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 
**#五、剔除特殊样本*************************
clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow

*- 设置字体
graph set window fontface "Times New Roman"
graph set window fontface "宋体"

*- 全局定义暂元
global cv "生产总值指数 人均可支配收入   人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平  农村创业活力    福利水平1 removed_time " 
 drop if YEAR == 2020
reghdfe 就业规模 did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "剔除特殊样本回归.xls", replace  excel  pdec(3) rdec(3) sdec(3)  bdec(3)  addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 
**# 六、排除同期政策影响*************************
clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow

*- 设置字体
graph set window fontface "Times New Roman"
graph set window fontface "宋体"

*- 全局定义暂元
global cv "生产总值指数 人均可支配收入   人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平  农村创业活力    福利水平1 removed_time " 
*- 稳健性_同期政策：列(1)
qui: reghdfe 就业规模 did c.($cv)#i.YEAR policyforcomprehensivedemonst, absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "稳健性检验_同期政策.xls", replace  excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 

*- 稳健性_同期政策：列(2)
qui: reghdfe 就业规模 did c.($cv)#i.YEAR pilotpoliciesforreturningto, absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "稳健性检验_同期政策.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 

*- 稳健性_同期政策：列(3)
qui: reghdfe 就业规模 did c.($cv)#i.YEAR BroadbandChinademonstration, absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "稳健性检验_同期政策.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 

*- 稳健性_同期政策：列(4)
qui: reghdfe 就业规模 did c.($cv)#i.YEAR universaltelecommunicationsser, absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "稳健性检验_同期政策.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 

*- 稳健性_同期政策：列(5)
qui: reghdfe 就业规模 did c.($cv)#i.YEAR pilotpolicyfortheInternetP, absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "稳健性检验_同期政策.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 

*- 稳健性_同期政策：列(6)
qui: reghdfe 就业规模 did c.($cv)#i.YEAR nationalpolicyonkeysupportf, absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "稳健性检验_同期政策.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 

*- 稳健性_同期政策：列(7)
qui: reghdfe 就业规模 did c.($cv)#i.YEAR policyforcomprehensivedemonst pilotpoliciesforreturningto BroadbandChinademonstration universaltelecommunicationsser pilotpolicyfortheInternetP nationalpolicyonkeysupportf , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "稳健性检验_同期政策.xls",  excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 
*----------------------------
* 1. 确保数据已加载（如果之前清空过数据）
*----------------------------
import excel "merged_data38.xlsx", sheet("Sheet2") firstrow clear

* 定义控制变量
global cv "生产总值指数 人均可支配收入 人力资本水平 人口规模2 固定资产投资 产业结构2 居民消费水平 农村创业活力 福利水平1 removed_time"

*----------------------------
* 2. 创建空数据集存储结果
*----------------------------
clear
set obs 7  // 7个模型
gen model = _n  // 模型编号1-7
gen policy = ""  // 政策名称
gen coef = .     // 系数
gen lower = .    // 95%置信区间下界
gen upper = .    // 95%置信区间上界
gen se = .
gen pvalue = .
*----------------------------
* 3. 循环回归并填充结果
*----------------------------
* 重新加载原始数据（每次回归前确保数据正确）
forval i = 1/7 {
    preserve  // 保存当前空数据集状态
    import excel "merged_data38.xlsx", sheet("Sheet2") firstrow clear
    
    * 根据模型编号选择政策变量
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
    
    * 执行回归
    qui: reghdfe 就业规模 did c.($cv)#i.YEAR `policy', absorb(CO_ID YEAR) vce(cluster CO_ID)
	local b = _b[did]
    local se = _se[did]
    local p = 2 * (1 - normal(abs(`b') / `se'))  // 计算p值
    
    * 返回空数据集并填充结果
    restore
    replace policy = "`policy_name'" in `i'
    replace coef = `b' in `i' 
	replace pvalue = `p' in `i'
	replace se = `se' in `i'   
    replace lower = `b' - 1.96 * `se' in `i'  // 用局部变量计算
	replace upper = `b' + 1.96 * `se' in `i'  // 用局部变量计算
}
gen significant = (lower > 0 | upper < 0)  // 置信区间不包含0则显著
gen stars = ""
replace stars = "***" if pvalue < 0.01
replace stars = "**"  if pvalue >= 0.01 & pvalue < 0.05
replace stars = "*"   if pvalue >= 0.05 & pvalue < 0.1

* 格式化系数（带星号）
gen coef_label = string(coef, "%9.3f") + stars  // 示例：0.25***

* 格式化标准误（带括号）
gen se_label = "(" + string(0.07, "%9.2f") + ")"  // 示例：(0.08)
gen label_full = coef_label + char(10) + se_label

*----------------------------
* 2. 绘制森林图（系数在上，标准误在下）
*----------------------------
twoway ///
    /// 置信区间（蓝色横线）
    (rcap upper lower model, horizontal color(edkblue) lwidth(medthin)) ///
    ///
    /// 显著系数：红色实心菱形 + 系数值
    (scatter model coef if significant==1, ///
        color(red) msymbol(d) msize(medium) ///
        mlabel(label_full) mlabcolor(black) mlabsize(2.2) mlabp(6)) ///
    ///
    /// 不显著系数：蓝色空心菱形 + 系数值
    (scatter model coef if significant==0, ///
        color(edkblue) msymbol(dh) msize(medium) ///
        mlabel(label_full) mlabcolor(black) mlabsize(2.2) mlabp(6)), ///
    ///
    /// Y轴设置
    ylabel(1 "1) Policy1" ///
           2 "2) Policy2" ///
           3 "3) Policy3" ///
           4 "4) Policy4" ///
           5 "5) Policy5" ///
           6 "6) Policy6" ///
           7 "7) all policies", ///
           angle(0) labsize(small) nogrid) ///
    ///
    /// X轴设置
    xline(0, lcolor(gs10) lpattern(dash) lwidth(medium)) ///
    xtitle("Did coefficient (95% confidence interval)", size(medsmall)) ///
    xlabel(, grid gmax) ///
    ///
    /// 其他设置
    ytitle("") ///
    legend(off) ///
    yscale(range(0 8)) ///
	yscale(reverse) /// 添加此选项将y轴倒置
    graphregion(color(white)) ///
    plotregion(margin(tiny))


* 保存图片
graph export "稳健性检验_同期政策1.png", replace width(1400) height(900)
*********************************************
**#工具变量
*********************************************
clear 

import excel "merged_data38.xlsx", sheet("Sheet1") firstrow
gen t = YEAR - 2014    
 foreach var in 生产总值指数 人均可支配收入 人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平 农村创业活力 福利水平1   removed_time  {
 g `var'_1=`var'*t
 g `var'_2=`var'*t*t
 g `var'_3=`var'*t*t*t
}

global cv " 生产总值指数 人均可支配收入 人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平 农村创业活力 福利水平1   removed_time    " 
*- 全局定义暂元
*global cv " 产业结构2  lnhealth  农村人力资本  经济密度  居民消费水平    " 
*global cv " lag_产业结构2  lag_lnhealth lag_经济密度   lag_人均可支配收入  lag_农村人力资本" 
global cv_t "生产总值指数_* 人均可支配收入_* 人力资本水平_* 人口规模2_*  固定资产投资_* 产业结构2_* 居民消费水平_*  农村创业活力_* 福利水平1_*  removed_time_*"

gen 与光缆骨干城市的最小距离_p=与光缆骨干城市的最小距离_km*post

ivreghdfe 就业规模  (did =与光缆骨干城市的最小距离_p) $cv,absorb(CO_ID YEAR)  vce(cluster CO_ID)  first


outreg2 using 工具变量检验.doc,replace tstat bdec(4) tdec(4) adjr2 addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 
*********************************************
*#机制分析
*********************************************
clear 

import excel "merged_data38.xlsx", sheet("Sheet2") firstrow

*- 设置字体
graph set window fontface "Times New Roman"
graph set window fontface "宋体"

*- 全局定义暂元
global cv "生产总值指数 人均可支配收入   人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平  农村创业活力    福利水平1 removed_time " 
*- 机制：创业 列(1)
qui: reghdfe 政府干预 did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "机制分析_政府规模.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 


qui: reghdfe 农业科技水平  did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "机制分析_农业科技水平.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 


qui: reghdfe 融资约束程度  did c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "机制分析_融资水平.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 
*********************************************
**#异质性检验
*********************************************
gen dw=0
replace dw = 1 if 市辖区==1 | 县级市==1
gen mountain1=0
replace mountain1 = 1 if 地形起伏度 > 0.5
reghdfe 就业规模 c.npoor#c.did#c.dw   c.npoor#did  c.dw#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a1
esttab a1  using 定位.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f)  nolabel r2 replace
reghdfe 就业规模 c.npoor#c.did#c.ethnic   c.npoor#did  c.ethnic#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a2
esttab a2  using ethnic.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f)  nolabel r2 replace
reghdfe 就业规模 c.npoor#c.did#c.hu   c.npoor#did  c.hu#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a3
esttab a3  using hu.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f) nolabel r2 replace
reghdfe 就业规模 c.npoor#c.did#c.mountain1   c.npoor#did  c.mountain1#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a4
esttab a4  using mountain1.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f)  nolabel r2 replace
reghdfe 就业规模 c.npoor#c.did#c.劳务编码   c.npoor#did  c.劳务编码#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
est store a5
esttab a5  using 劳务编码.rtf,se star(* 0.1 ** 0.05 *** 0.01) b(%6.3f)t(%6.3f)  nolabel r2 replace


reghdfe 就业规模 c.did#c.deep连片1   did  c.deep连片1#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)

reghdfe 就业规模 c.npoor#c.did#c.deep连片   c.npoor#did  c.deep连片#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)


reghdfe 就业规模 c.did#c.mountain1   did  c.mountain1#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
reghdfe 就业规模 c.did#c.deep   did  c.deep#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
reghdfe 就业规模 c.did#c.hu   did  c.hu#c.post   c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)



*********************************************
*#进一步分析
*********************************************
clear 

import excel "E:\研究生论文\数字乡村试点政策对脱贫地区就业规模的影响研究\数据\Nsit_results1.xlsx", sheet("Sheet1") firstrow
*- 设置字体
graph set window fontface "Times New Roman"
graph set window fontface "宋体"
global cv "生产总值指数 人均可支配收入 人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平 农村创业活力 福利水平1  removed_time "

xtset CO_ID YEAR

reghdfe  就业规模 did c_did50-c_did350 c.($cv)#i.YEAR , absorb(CO_ID YEAR)  vce(cluster CO_ID) 
coefplot, keep(did c_did*)  coeflabels(did="0" c_did50="50" c_did100="100" c_did150="150" c_did200="200" c_did250="250" c_did300="300" c_did350="350")   ///
vertical  addplot(line @b @at) ytitle("employment scale") xtitle("Spatial distance (kilometers)")yline(0) levels(90) scheme(s1mono) ciopts(recast(rcap) lpattern(dash))

clear 

import excel "E:\研究生论文\数字乡村试点政策对脱贫地区就业规模的影响研究\数据\result.xlsx", sheet("Sheet1") firstrow

*- 设置字体
graph set window fontface "Times New Roman"
graph set window fontface "宋体"

*- 全局定义暂元
global cv "生产总值指数 人均可支配收入   人力资本水平  人口规模2   固定资产投资 产业结构2 居民消费水平  农村创业活力    福利水平1 removed_time " 
xtset CO_ID YEAR
gen cld=class*did
gen clp=class*post
gen clt=class*treat
reghdfe 就业规模 cld clp clt c.($cv)#i.YEAR , absorb(CO_ID YEAR) vce(cluster CO_ID)
outreg2  using  "地区流动溢出效应.xls", replace excel  pdec(3) rdec(3) sdec(3)  bdec(3) addtext(控制变量, "是",控制变量×时间固定效应,"是",县域固定效应, "是", 年份固定效应,"是") 








