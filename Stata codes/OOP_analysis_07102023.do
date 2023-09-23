/*******************************************************************************
Title:		Impact of FPL on spending and utilization (2008-2020)
Programmer:	Mark Bounthavong
Date: 		18 September 2022
Updated:	10 July 2023
Updated by:	Mark Bounthavong
********************************************************************************/


/*******************************************************************************
Notes: 08 July 2023
Added the triple interaction of cara*narcotic*year


Notes: 23 June 2023
-- Combine graphs using the following URL instructions (grc1leg package):
https://www.techtips.surveydesign.com.au/post/combining-graphs-and-including-a-common-legend-in-stata


Notes: 18 June 2023
- Added 2020 data and included the HC-036 linkage file

Notes: 19 September 2022
Cost adjusted for CPI 2020
Look at OOP spending across different groups.
FPL level: Group from 5 to 4 groups (Poor/Near Poor, Low Income, Middle Income, High Income)


Note: 11-06-2022
This analysis focuses on looking at the impact of FPL on spending and utilization

Person-level income totals were then summed over family members as defined by CPSFAMID to yield the family-level total. POVCAT is constructed by dividing family income by the applicable poverty line (based on family size and composition), with the resulting percentages grouped into the following 5 categories:
1. Negative or poor (LT 100% poverty line)
2. Near poor (100-124% poverty line)
3. Low income (125-199% poverty line
4. Middle income (200-399% poverty line)
5. High income (GE 400% poverty line)
(source: https://meps.ipums.org/meps-action/variables/POVCAT#description_section)

Notes: 12-02-2022
Had to include tcpp1_1 == 191 (narcotic combination for 2017, 2018, and 2019 data)


Note: 12-19-2022
Perform OOP analyses.

. gen total_exp_sum = totmcr + totmcd + totprv + totslf + totother

. sum total_exp_sum

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
total_exp_~m |    295,725    5757.188    17118.98          0    2682725

. sum totexp

    Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
      totexp |    295,725    5757.172    17118.97          0    2682725



*******************************************************************************/

******** CPI ADJUSTED DATA *****************************************************
********************************************************************************
* For Macs:
clear all
cd "/Users/mbounthavong/Dropbox/Projects/Healthcare Expenditures and Opioid users/MEPS analysis/Data/Combined"
use adjusted_combined_data_post.dta

* For Windows:
clear all 
cd "C:\Users\mboun\Dropbox\Projects\Healthcare Expenditures and Opioid users\MEPS analysis\Data\Combined"
use adjusted_combined_data_post.dta

* For Windows2:
clear all 
cd "C:\Users\mbounthavong\Dropbox\Projects\Healthcare Expenditures and Opioid users\MEPS analysis\Data\Combined"
use adjusted_combined_data_post.dta
********************************************************************************



/*******************************************************************************
** CREATE GROUPS
********************************************************************************/
// Poverty groups (4-group category)
gen fpl_group = .
	replace fpl_group = 0 if povcat == 1
	replace fpl_group = 0 if povcat == 2
	replace fpl_group = 1 if povcat == 3
	replace fpl_group = 2 if povcat == 4
	replace fpl_group = 3 if povcat == 5

label define fpl_lbl 0 "Poor/Near Poor" 1 "Low Income" 2 "Middle Income" 3 "High Income"
label values fpl_group fpl_lbl
tab fpl_group, m


// Marital status
gen marital_status = .
	replace marital_status = 0 if marry == 5
	replace marital_status = 1 if marry == 1
	replace marital_status = 2 if marry == 2
	replace marital_status = 3 if marry == 3
	replace marital_status = 3 if marry == 4
	replace marital_status = 4 if marry == -7
	replace marital_status = 4 if marry == -8
	replace marital_status = 4 if marry == -9
label define marital_lbl 0 "Never married" 1 "Married" 2 "Widowed" 3 "Divorced/Separated" 4 "Unknown"
label values marital_status marital_lbl
tab marital_status, m

gen marital_status2 = .
	replace marital_status2 = 0 if marry == 5
	replace marital_status2 = 1 if marry == 1
	replace marital_status2 = 2 if marry == 2
	replace marital_status2 = 3 if marry == 3
	replace marital_status2 = 3 if marry == 4
	replace marital_status2 = 0 if marry == -7
	replace marital_status2 = 0 if marry == -8
	replace marital_status2 = 0 if marry == -9
label define marital_lbl2 0 "Never married/Unknown" 1 "Married" 2 "Widowed" 3 "Divorced/Separated"
label values marital_status2 marital_lbl2
tab marital_status2, m

// Education
gen educ2 = .
	replace educ2 = 0 if neweducode == 0
	replace educ2 = 1 if neweducode == 1
	replace educ2 = 2 if neweducode == 2
	replace educ2 = 3 if neweducode == 3
	replace educ2 = 4 if neweducode == 4
	replace educ2 = 5 if neweducode == 10
	replace educ2 = 5 if neweducode == 11
	replace educ2 = 5 if neweducode == 12
	replace educ2 = 5 if neweducode == 13
label define educ2_lbl 0 "No degree" 1 "GED or HS equivalent" 2 "Associate" 3 "BS" 4 "Master/Doctoral" 5 "Unknown/Refused/Not Ascertained"
label values educ2 educ2_lbl
tab educ2, m



// Keep if narcotic == 1 (Need to include the entire cohort for svy SEs)
*keep if narcotic == 1


/*******************************************************************************
DESCRIPTIVE TRENDS
********************************************************************************/
// Narcotic versus non-narcotic users

tab year narcotic, sum(public_exp) mean
tab year narcotic, sum(totprv) mean
tab year narcotic, sum(totslf) mean
tab year narcotic, sum(totother) mean



			 
// Total expenditures
preserve
egen mean_totalexp = mean(totexp), by(narcotic year) 
tab mean_totalexp year

// Generate the upper and low bound of the 95% CI variable
gen high = .
gen low = .

// Loop commmand
forvalues i = 2008/2020 {
	forvalues j = 0/3 {
		ci mean totexp if year == `i' & narcotic == `j'
		replace high = r(ub) if year == `i' & narcotic == `j'
		replace low = r(lb) if year == `i' & narcotic == `j'
	}	
}

sort year
graph twoway (rcap low high year if narcotic == 1, color(navy)) ///
			 (rcap low high year if narcotic == 0, color(cranberry)) ///
			 (connected mean_totalexp year if narcotic == 1, color(navy) msize(small)) ///
			 (connected mean_totalexp year if narcotic == 0, color(cranberry) msize(small) ///
                        title("Total healthcare expenditures (2008-2020)") ///
                        ytitle("Average total healthcare expenditures ($)") ///
                        xtitle("Time (year)") ///
                        xlab(2008(1)2020, labsize(vsmall)) ///
                        ylab(, labsize(vsmall) nogrid) ///
                        graphregion(color(white)) ///
                        bgcolor(white) ///
						legend(order(3 "Opioid users" 4 "Non-opioid users")))
restore



*** Total healthcare expenditure Trends by narcotic vs. non-narcotic users
egen mean_totalexp = mean(totexp), by(narcotic year) 

graph twoway (connected mean_totalexp year if narcotic == 1) ///
			 (connected mean_totalexp year if narcotic == 0, ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Total healthcare expenditures ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Public spending by opioid and non-opioid users"))

*** Public Spending (MCR and MCD) Trends by narcotic vs. non-narcotic users
gen public_exp = totmcr + totmcd
egen public_total = mean(public_exp), by(narcotic year)

graph twoway (connected public_total year if narcotic == 1) ///
			 (connected public_total year if narcotic == 0, ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Total public expenditures ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Public spending by opioid and non-opioid users"))
			 

*** Private Spending (MCR and MCD) Trends by narcotic vs. non-narcotic users
egen private_total = mean(totprv), by(narcotic year)

graph twoway (connected private_total year if narcotic == 1) ///
			 (connected private_total year if narcotic == 0, ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Total private expenditures ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Private spending by opioid and non-opioid users"))

*** OOP Trends by narcotic vs. non-narcotic users
egen oop_total = mean(totslf), by(narcotic year)

graph twoway (connected oop_total year if narcotic == 1) ///
			 (connected oop_total year if narcotic == 0, ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Total expenditures ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("OOP spending by opioid and non-opioid users"))			 

*** Other Spending (MCR and MCD) Trends by narcotic vs. non-narcotic users
egen other_total = mean(totother), by(narcotic year)

graph twoway (connected other_total year if narcotic == 1) ///
			 (connected other_total year if narcotic == 0, ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Total other expenditures ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Other spending by opioid and non-opioid users"))
			 
			 
// Stacked bar chart (proportion of total expenditures) Both narcotic and non-narcotic
graph bar public_exp totprv totslf totother, over(narcotic) ///
	over(year, label(labsize(vsmall))) ///
	stack percent ///
	graphregion(color(white)) ///
	legend(order(1 "Public" 2 "Private" 3 "Self" 4 "Other") region(lcolor(white))) ///
	b1title("Year") ///
	ytitle("Proportion of total expenditure (%)") ///
	ylabel(, labsize(vsmall)) ///
	blabel(bar, size(tiny) color(white) position(inside) format(%4.2f)) ///
	bar(1, fcolor("106 208 200") lwidth(none)) bar(2, fcolor("118 152 160") lwidth(none)) bar(3, fcolor("125 167 120") lwidth(none)) bar(4, fcolor("135 120 105") lwidth(none))

	
// Stacked bar chart (proportion of total expenditures) Narcotic == 1
graph bar public_exp totprv totslf totother if narcotic == 1, over(year, label(labsize(vsmall))) stack percent ///
	graphregion(color(white)) ///
	legend(order(1 "Public" 2 "Private" 3 "Self" 4 "Other") region(lcolor(white))) ///
	b1title("Year") ///
	ytitle("Proportion of total expenditure (%)") ///
	ylabel(, labsize(vsmall)) ///
	blabel(bar, size(tiny) color(white) position(inside) format(%4.2f)) ///
	bar(1, fcolor("106 208 200") lwidth(none)) bar(2, fcolor("118 152 160") lwidth(none)) bar(3, fcolor("125 167 120") lwidth(none)) bar(4, fcolor("135 120 105") lwidth(none))

// Stacked bar chart (proportion of total expenditures) Narcotic == 0 
graph bar public_exp totprv totslf totother if narcotic == 0, over(year, label(labsize(vsmall))) stack percent ///
	graphregion(color(white)) ///
	legend(order(1 "Public" 2 "Private" 3 "Self" 4 "Other") region(lcolor(white))) ///
	b1title("Year") ///
	ytitle("Proportion of total expenditure (%)") ///
	ylabel(, labsize(vsmall)) ///
	blabel(bar, size(tiny) color(white) position(inside) format(%4.2f)) ///
	bar(1, fcolor("106 208 200") lwidth(none)) bar(2, fcolor("118 152 160") lwidth(none)) bar(3, fcolor("125 167 120") lwidth(none)) bar(4, fcolor("135 120 105") lwidth(none))

	
	
// Horizontal bar chart (proportion of total expenditures) Both narcotic and non-narcotic
graph hbar public_exp totprv totslf totother, over(narcotic) ///
	over(year, label(labsize(vsmall))) ///
	stack  ///
	graphregion(color(white)) ///
	legend(order(1 "Public" 2 "Private" 3 "Self" 4 "Other") region(lcolor(white))) ///
	b1title("Year") ///
	ytitle("Proportion of total expenditure (%)") ///
	ylabel(, labsize(vsmall)) ///
	blabel(bar, size(tiny) color(white) position(inside) format(%4.2f)) ///
	bar(1, fcolor("106 208 200") lwidth(none)) bar(2, fcolor("118 152 160") lwidth(none)) bar(3, fcolor("125 167 120") lwidth(none)) bar(4, fcolor("135 120 105") lwidth(none))

	
// Horizontal bar chart (proportion of total expenditures) Narcotic == 1
graph hbar public_exp totprv totslf totother if narcotic == 1,  ///
	over(year, gap(*0.10) label(labsize(vsmall))) ///
	stack  ///
	graphregion(color(white)) ///
	legend(order(1 "Public" 2 "Private" 3 "Self" 4 "Other") region(lcolor(white)) pos(6) row(1)) ///
	ytitle("Total Healthcare costs ($)") ///
	ylabel(, labsize(vsmall)) ///
	bar(1, fcolor("106 208 200") lwidth(none)) bar(2, fcolor("118 152 160") lwidth(none)) bar(3, fcolor("125 167 120") lwidth(none)) bar(4, fcolor("135 120 105") lwidth(none)) ///
	title("Opioid users") name(narcotic_1, replace)
	

// Horizontal bar chart (proportion of total expenditures) Narcotic == 0
graph hbar public_exp totprv totslf totother if narcotic == 0,  ///
	over(year, gap(*0.10) label(labsize(vsmall))) ///
	stack  ///
	graphregion(color(white)) ///
	legend(order(1 "Public" 2 "Private" 3 "Self" 4 "Other") region(lcolor(white)) pos(6) row(1)) ///
	ytitle("Total Healthcare costs ($)") ///
	ylabel(, labsize(vsmall)) ///
	bar(1, fcolor("106 208 200") lwidth(none)) bar(2, fcolor("118 152 160") lwidth(none)) bar(3, fcolor("125 167 120") lwidth(none)) bar(4, fcolor("135 120 105") lwidth(none)) ///
	title("Non-opioid users") name(narcotic_0, replace)

// Combine horizontal bar charts (total expenditures - stacked)
*graph combine narcotic_1 narcotic_0, col(2) iscale(1) xcommon
grc1leg narcotic_1 narcotic_0, col(2) iscale(1) ycommon xcommon legendfrom(narcotic_1) /* common legend */
			

// DESCRIPTIVES			
********************************************************************************
* For Macs:
clear all
cd "/Users/mbounthavong/Dropbox/Projects/Healthcare Expenditures and Opioid users/MEPS analysis/Data/Combined"
use oop_data1.dta

* For Windows:
clear all 
cd "C:\Users\mboun\Dropbox\Projects\Healthcare Expenditures and Opioid users\MEPS analysis\Data\Combined"
use oop_data1.dta

* For Windows2:
clear all 
cd "C:\Users\mbounthavong\Dropbox\Projects\Healthcare Expenditures and Opioid users\MEPS analysis\Data\Combined"
use oop_data1.dta
********************************************************************************

			 
			 
/*******************************************************************************
** Survey weighted health expenditures
********************************************************************************/		 
svyset [pweight=poolwt2], strata(stra9620) psu(psu9620)

tab narcotic if subpop == 1, m
svy, sub(subpop): tab narcotic, format(%11.3g) count col 


svy, sub(subpop): mean age
estat sd
svy, sub(subpop): mean age, over(narcotic)
estat sd
svy, sub(subpop): regress age i.narcotic
mat list e(b)
test 0b.narcotic = 1.narcotic
pbalchk narcotic age if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab agecat narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.agecat if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab male narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.male if subpop == 1, wt(poolwt2)

svy, sub(subpop): tab racev1x narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.racev1x if subpop == 1, wt(poolwt2)  

svy, sub(subpop): tab hispanx narcotic, format(%11.3g) count col
xi: pbalchk narcotic i.hispanx if subpop == 1, wt(poolwt2) 
 
svy, sub(subpop): tab marital_status narcotic, format(%11.3g) count col  
svy, sub(subpop): tab marital_status2 narcotic, format(%11.3g) count col 
rename marital_status2 married2
xi: pbalchk narcotic i.married2 if subpop == 1, wt(poolwt2) 
rename married2 marital_status2

svy, sub(subpop): tab educ2 narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.educ2 if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab region narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.region if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab fpl_group narcotic, format(%11.3g) count col  
xi: pbalchk narcotic i.fpl_group if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab inscov narcotic, format(%11.3g) count col  
xi: pbalchk narcotic i.inscov if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab hibpdx narcotic, format(%11.3g) count col  
xi: pbalchk narcotic i.hibpdx if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab chddx narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.chddx if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab angidx narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.angidx if subpop == 1, wt(poolwt2) 
 
svy, sub(subpop): tab midx narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.midx if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab ohrtdx narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.ohrtdx if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab strkdx narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.strkdx if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab choldx narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.choldx if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab cancerdx narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.cancerdx if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab diabdx narcotic, format(%11.3g) count col 
xi: pbalchk narcotic i.diabdx if subpop == 1, wt(poolwt2) 

svy, sub(subpop): tab arthdx narcotic, format(%11.3g) count col
xi: pbalchk narcotic i.arthdx if subpop == 1, wt(poolwt2) 


***************************************************************
** CRUDE ANALYSIS
***************************************************************
// Expenditures /* public_exp totprv totslf totother */
svy, sub(subpop): mean public_exp	

*** Total expenditures
svy, sub(subpop): mean totexp	
estat sd
svy, sub(subpop): mean totexp, over(narcotic)
estat sd
svy, sub(subpop): regress totexp narcotic


*** Public expenditures
svy, sub(subpop): mean public_exp	
estat sd
svy, sub(subpop): mean public_exp, over(narcotic)
estat sd
svy, sub(subpop): regress public_exp narcotic

*** Private expenditures
svy, sub(subpop): mean totprv	
estat sd
svy, sub(subpop): mean totprv, over(narcotic)
estat sd
svy, sub(subpop): regress totprv narcotic

*** Self expenditures
svy, sub(subpop): mean totslf	
estat sd
svy, sub(subpop): mean totslf, over(narcotic)
estat sd
svy, sub(subpop): regress totslf narcotic

*** Other expenditures
svy, sub(subpop): mean totother	
estat sd
svy, sub(subpop): mean totother, over(narcotic)
estat sd
svy, sub(subpop): regress totother narcotic



/*******************************************************************************
Regression models 
********************************************************************************/


// DESCRIPTIVES			
********************************************************************************
* For Macs:
clear all
cd "/Users/mbounthavong/Dropbox/Projects/Healthcare Expenditures and Opioid users/MEPS analysis/Data/Combined"
use oop_data2.dta

* For Windows:
clear all 
cd "C:\Users\mboun\Dropbox\Projects\Healthcare Expenditures and Opioid users\MEPS analysis\Data\Combined"
use oop_data2.dta

* For Windows2:
clear all 
cd "C:\Users\mbounthavong\Dropbox\Projects\Healthcare Expenditures and Opioid users\MEPS analysis\Data\Combined"
use oop_data2.dta
********************************************************************************




****** CHANGE MARITAL STATUS AND EDUCATION VARIABLES "UNKNOWN RESPONSES"
gen marry2 = .
	replace marry2 = 1 if marry == 1
	replace marry2 = 2 if marry == 2
	replace marry2 = 3 if marry == 3
	replace marry2 = 4 if marry == 4
	replace marry2 = 5 if marry == 5
	replace marry2 = 6 if marry == -7
	replace marry2 = 6 if marry == -8
	replace marry2 = 6 if marry == -9
tab marry2, m

/*
gen neweducode2 = .
	replace neweducode2 = 1 if neweducode == 1
*/

****** totexp == 0
gen zero = .
	replace zero = 0 if totexp == 0
	replace zero = 1 if totexp > 0 & totexp < 9999999999999999
tab zero, m


****** totslf == 0
gen zero_slf = .
	replace zero_slf = 1 if totslf == 0
	replace zero_slf = 0 if totslf > 0 & totslf < 99999999999999999999
tab zero_slf, m



****** CARA implementation
gen cara = .
	replace cara = 0 if year < 2018
	replace cara = 1 if year >= 2018 & year < 99999
tab cara, m

****** Region
gen region2 = .
	replace region2 = 0 if region == 1
	replace region2 = 1 if region == 2
	replace region2 = 2 if region == 3 
	replace region2 = 3 if region == 4
label define region_lbl 0 "NE" 1 "MW" 2 "S" 3 "W"
label values region2 region_lbl
tab region2, m
	


// xlist (Variable list)
* (Don't inclue insurance coverage since that is part of the private and payer for the other regression models)
global xlist1 agecat i.male i.racev1x i.hispanx i.marital_status2 i.educ2 i.inscov i.region2 i.hibpdx i.chddx i.angidx i.midx i.ohrtdx i.strkdx i.choldx i.cancerdx i.diabdx i.arthdx i.fpl_group i.cara

global xlist2 agecat i.male i.racev1x i.hispanx i.marital_status2 i.educ2 i.region2 i.hibpdx i.chddx i.angidx i.midx i.ohrtdx i.strkdx i.choldx i.cancerdx i.diabdx i.arthdx i.fpl_group i.cara

global xlist3 agecat i.male i.racev1x i.hispanx i.marital_status2 i.educ2 i.region2 i.fpl_group i.cara




// Regression model - Total Expenditures
gen cara_year = cara*year
gen narcotic_year = narcotic*year
gen cara_narcotic = cara*narcotic
gen cara_narcotic_year = cara*narcotic*year



			 
			 
// PRIMARY AIM: POOLED ANALYSIS -- adjusted costs with pooled data.
*** Total costs
svy, sub(subpop): glm totexp i.narcotic c.year $xlist2, family(gaussian) link(identity)
predict adjustedmodel1
ci means adjustedmodel1
margins r.narcotic, noestimcheck
margins narcotic, noestimcheck


*** Public costs
svy, sub(subpop): glm public_exp i.narcotic c.year $xlist2, family(gaussian) link(identity)
predict adjustedmodel2
ci means adjustedmodel2
margins r.narcotic, noestimcheck
margins narcotic, noestimcheck

*** Private costs
svy, sub(subpop): glm totprv i.narcotic c.year $xlist2, family(gaussian) link(identity)
predict adjustedmodel3
ci means adjustedmodel3
margins r.narcotic, noestimcheck
margins narcotic, noestimcheck

*** Out-of-pocket
svy, sub(subpop): glm totslf i.narcotic c.year $xlist2, family(gaussian) link(identity)
predict adjustedmodel4
ci means adjustedmodel4
margins r.narcotic, noestimcheck
margins narcotic, noestimcheck

*** Other
svy, sub(subpop): glm totother i.narcotic c.year $xlist2, family(gaussian) link(identity)
predict adjustedmodel5
ci means adjustedmodel5
margins r.narcotic, noestimcheck
margins narcotic, noestimcheck




// SECONDARY AIMS
***** OLS - totexp
svy, sub(subpop): glm totexp c.year##i.cara##i.narcotic $xlist2, family(gaussian) link(identity)
predict ols_totexp
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- OLS model
gen rsme_ols_totexp = sqrt((totexp - ols_totexp)^2)
summ rsme_ols_totexp


***** GLM gamma - totexp
svy, sub(subpop): glm totexp c.year##i.cara##i.narcotic $xlist2, family(gamma) link(log)
predict glm_gamma_totexp
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */
 
***** RSME model fit test -- GLM gamma model
gen rsme_gamma_totexp = sqrt((totexp - glm_gamma_totexp)^2)
summ rsme_gamma_totexp


***** Two-part - totexp
svy, sub(subpop): twopm totexp c.year##i.cara##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict twopm1
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- two-part model
gen rsme_twopm_totexp = sqrt((totexp - twopm1)^2)
summ rsme_twopm_totexp


***** COMPARE RSME
summ rsme_ols_totexp rsme_gamma_totexp rsme_twopm_totexp



****** Plot - visualize the fit
sort year
graph twoway (scatter mean_totalexp year if narcotic == 1, mcol(cranberry)) ///
			 (scatter mean_totalexp year if narcotic == 0, mcol(navy)) ///
			 (lfit ols_totexp year if narcotic == 1 & cara == 0, lcol(cranberry)) ///
			 (lfit ols_totexp year if narcotic == 0 & cara == 0, lcol(navy)) ///
			 (lfit ols_totexp year if narcotic == 1 & cara == 1, lcol(cranberry)) ///
			 (lfit ols_totexp year if narcotic == 0 & cara == 1, lcol(navy) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Total healthcare expenditures ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Total spending by opioid and non-opioid users"))


// FIGURE
preserve 
// Generate the upper and low bound of the 95% CI variable
gen high = .
gen low = .

// Loop commmand
forvalues i = 2008/2020 {
	forvalues j = 0/1 {
		ci mean totexp if year == `i' & narcotic == `j' 
		replace high = r(ub) if year == `i' & narcotic == `j' 
		replace low = r(lb) if year == `i' & narcotic == `j' 
	}
}


graph twoway (rcap low high year if narcotic == 1, lcol("edkblue")) ///
			 (rcap low high year if narcotic == 0, lcol("sand")) ///
			 (scatter mean_totalexp year if narcotic == 1, mcol("edkblue")) ///
			 (scatter mean_totalexp year if narcotic == 0, mcol("sand") ///
			 xline(2018, lcol("cranberry")) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) xlabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(order(3 "Opioid users" 4 "Non-opioid users")) ///
			 ytitle("Total healthcare costs ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Total spending by opioid and non-opioid users"))
graph export Figure_total_exp.png, replace
graph save Figure_total_exp.gph, replace
restore






/* public_exp totprv totslf totother */
// Public expenditures

***** OLS model - PUBLIC
svy, sub(subpop): glm public_exp c.year##i.cara##i.narcotic $xlist2, family(gaussian) link(identity)
predict ols_public
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- OLS model
gen rsme_ols_public = sqrt((public_exp - ols_public)^2)
summ rsme_ols_public


***** GLM gamma model - PUBLIC
svy, sub(subpop): glm public_exp c.year##i.cara##i.narcotic $xlist2, family(gamma) link(log)
predict glm_public
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- GLM gamma model
gen rsme_glm_public = sqrt((public_exp - glm_public)^2)
summ rsme_glm_public


***** Two-part model - PUBLIC
svy, sub(subpop): twopm public_exp c.year##i.cara##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict twopm_public
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- Two-part model
gen rsme_twopm_public = sqrt((public_exp - twopm_public)^2)
summ rsme_twopm_public


***** COMPARE RSME
summarize rsme_ols_public rsme_glm_public rsme_twopm_public



****** Plot - visualize the fit
sort year
graph twoway (scatter public_total year if narcotic == 1, mcol(cranberry)) ///
			 (scatter public_total year if narcotic == 0, mcol(navy)) ///
			 (lfit twopm_public year if narcotic == 1 & cara == 0, lcol(cranberry)) ///
			 (lfit twopm_public year if narcotic == 0 & cara == 0, lcol(navy)) ///
			 (lfit twopm_public year if narcotic == 1 & cara == 1, lcol(cranberry)) ///
			 (lfit twopm_public year if narcotic == 0 & cara == 1, lcol(navy) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Average public expenditures ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Public spending by opioid and non-opioid users"))
			 

// FIGURE - PUBLIC
preserve 
// Change location
cd "/Users/mbounthavong/Library/CloudStorage/Dropbox/Projects/Healthcare Expenditures and Opioid users/OOP on healthcare expenditures/Figures"

// Generate the upper and low bound of the 95% CI variable
gen high = .
gen low = .

// Loop commmand
forvalues i = 2008/2020 {
	forvalues j = 0/1 {
		ci mean public_exp if year == `i' & narcotic == `j' 
		replace high = r(ub) if year == `i' & narcotic == `j' 
		replace low = r(lb) if year == `i' & narcotic == `j' 
	}
}


graph twoway (rcap low high year if narcotic == 1, lcol("edkblue")) ///
			 (rcap low high year if narcotic == 0, lcol("sand")) ///
			 (scatter public_total year if narcotic == 1, mcol("edkblue")) ///
			 (scatter public_total year if narcotic == 0, mcol("sand") ///
			 xline(2018, lcol("cranberry")) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) xlabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(order(3 "Opioid users" 4 "Non-opioid users")) ///
			 ytitle("Average public costs ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Public spending by opioid and non-opioid users"))
graph export Figure_public_spending.png, replace
graph save Figure_public_spending.gph, replace
restore
			 
			 
			 
// Private expenditures

*** OLS model - PRIVATE
svy, sub(subpop): glm totprv c.year##i.cara##i.narcotic $xlist2, family(gaussian) link(identity)
predict ols_private
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- OLS model
gen rsme_ols_private = sqrt((totprv - ols_private)^2)
summ rsme_ols_private


***** GLM Gamma model - PRIVATE 
/* agecat i.male i.racev1x i.hispanx i.marital_status2 i.educ2 i.region i.inscov i.fpl_group */
svy, sub(subpop): glm totprv c.year##i.cara##i.narcotic $xlist2, family(gamma) link(log)
predict glm_private
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- GLM gamma model
gen rsme_glm_private = sqrt((totprv - glm_private)^2)
summ rsme_glm_private


***** Two-part model - PRIVATE
svy, sub(subpop): twopm totprv c.year##i.cara##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict twopm_private
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- Two-part model
gen rsme_twopm_private = sqrt((totprv - twopm_private)^2)
summ rsme_twopm_private


***** COMPARE RSME
summarize rsme_ols_private rsme_glm_private rsme_twopm_private


****** Plot - visualize the fit
sort year
graph twoway (scatter private_total year if narcotic == 1, mcol(cranberry)) ///
			 (scatter private_total year if narcotic == 0, mcol(navy)) ///
			 (lfit twopm_private year if narcotic == 1 & cara == 0, lcol(cranberry)) ///
			 (lfit twopm_private year if narcotic == 0 & cara == 0, lcol(navy)) ///
			 (lfit twopm_private year if narcotic == 1 & cara == 1, lcol(cranberry)) ///
			 (lfit twopm_private year if narcotic == 0 & cara == 1, lcol(navy) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Average private expenditures ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Private spending by opioid and non-opioid users"))
			 
			 

// FIGURE - PRIVATE
preserve 
// Change location
cd "/Users/mbounthavong/Library/CloudStorage/Dropbox/Projects/Healthcare Expenditures and Opioid users/OOP on healthcare expenditures/Figures"

// Generate the upper and low bound of the 95% CI variable
gen high = .
gen low = .

// Loop commmand
forvalues i = 2008/2020 {
	forvalues j = 0/1 {
		ci mean totprv if year == `i' & narcotic == `j' 
		replace high = r(ub) if year == `i' & narcotic == `j' 
		replace low = r(lb) if year == `i' & narcotic == `j' 
	}
}


graph twoway (rcap low high year if narcotic == 1, lcol("edkblue")) ///
			 (rcap low high year if narcotic == 0, lcol("sand")) ///
			 (scatter private_total year if narcotic == 1, mcol("edkblue")) ///
			 (scatter private_total year if narcotic == 0, mcol("sand") ///
			 xline(2018, lcol("cranberry")) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) xlabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(order(3 "Opioid users" 4 "Non-opioid users")) ///
			 ytitle("Average private costs ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Private spending by opioid and non-opioid users"))
graph export Figure_private.pgn, replace
graph save Figure_private.gph, replace
restore

			 
			 
			 
			 


// Self expenditures
*** OLS model - OOP spending
svy, sub(subpop): glm totslf c.year##i.cara##i.narcotic $xlist2, family(gaussian) link(identity)
predict ols_oop
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- OLS model
gen rsme_ols_oop = sqrt((totslf - ols_oop)^2)
summ rsme_ols_oop


*** GLM model - OOP spending
svy, sub(subpop): glm totslf c.year##i.cara##i.narcotic $xlist2, family(gamma) link(log)
predict glm_oop
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- OLS model
gen rsme_glm_oop = sqrt((totslf - glm_oop)^2)
summ rsme_glm_oop


*** Two-part model - OOP spending
svy, sub(subpop): twopm totslf c.year##i.cara##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict twopm_oop
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- OLS model
gen rsme_twopm_oop = sqrt((totslf - twopm_oop)^2)
summ rsme_twopm_oop


summarize rsme_ols_oop rsme_glm_oop rsme_twopm_oop



****** Plot - visualize the fit
sort year
graph twoway (scatter oop_total year if narcotic == 1, mcol(cranberry)) ///
			 (scatter oop_total year if narcotic == 0, mcol(navy)) ///
			 (lfit twopm_oop year if narcotic == 1 & cara == 0, lcol(cranberry)) ///
			 (lfit twopm_oop year if narcotic == 0 & cara == 0, lcol(navy)) ///
			 (lfit twopm_oop year if narcotic == 1 & cara == 1, lcol(cranberry)) ///
			 (lfit twopm_oop year if narcotic == 0 & cara == 1, lcol(navy) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Average out-of-pocket costs ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Out-of-pocket spending by opioid and non-opioid users"))
			 


// FIGURE - SELF
preserve 
// Change location
cd "/Users/mbounthavong/Library/CloudStorage/Dropbox/Projects/Healthcare Expenditures and Opioid users/OOP on healthcare expenditures/Figures"

// Generate the upper and low bound of the 95% CI variable
gen high = .
gen low = .

// Loop commmand
forvalues i = 2008/2020 {
	forvalues j = 0/1 {
		ci mean totslf if year == `i' & narcotic == `j' 
		replace high = r(ub) if year == `i' & narcotic == `j' 
		replace low = r(lb) if year == `i' & narcotic == `j' 
	}
}


graph twoway (rcap low high year if narcotic == 1, lcol("edkblue")) ///
			 (rcap low high year if narcotic == 0, lcol("sand")) ///
			 (scatter oop_total year if narcotic == 1, mcol("edkblue")) ///
			 (scatter oop_total year if narcotic == 0, mcol("sand") ///
			 xline(2018, lcol("cranberry")) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) xlabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(order(3 "Opioid users" 4 "Non-opioid users")) ///
			 ytitle("Average out-of-pocket costs ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Out-of-pocket spending by opioid and non-opioid users"))
graph export Figure_oop.png, replace
graph save Figure_oop.gph, replace
restore			 
			 
			 
			 
			 

// Other expenditures
*** OLS model - Other spending
svy, sub(subpop): glm totother c.year##i.cara##i.narcotic $xlist2, family(gaussian) link(identity)
predict ols_other
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- OLS model
gen rsme_ols_other = sqrt((totother - ols_other)^2)
summ rsme_ols_other


*** GLM model - Other spending
svy, sub(subpop): glm totother c.year##i.cara##i.narcotic $xlist2, family(gamma) link(log)
predict glm_other
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- OLS model
gen rsme_glm_other = sqrt((totother - glm_other)^2)
summ rsme_glm_other

		
*** Two-part model - Other spending
svy, sub(subpop): twopm totother c.year##i.cara##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict twopm_other
margins narcotic, dydx(cara) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
*margins narcotic#cara, dydx(year) pwcompare noestimcheck /* Method 2: diff in slopes after CARA */
*lincom _b[1.narcotic#1.cara#year]  /* diff in slopes between groups before and after CARA */
margins ar.narcotic#ar.cara, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */
*margins, dydx(year) over(narcotic cara) pwcompare noestimcheck /* Method 3: diff in slopes after CARA */
*contrast narcotic#c.year#cara, nowald pveffects noestimcheck post /* DID */

***** RSME model fit test -- OLS model
gen rsme_twopm_other = sqrt((totother - twopm_other)^2)
summ rsme_twopm_other
	 

summarize rsme_ols_other rsme_glm_other rsme_twopm_other


****** Plot - visualize the fit
sort year
graph twoway (scatter other_total year if narcotic == 1, mcol(cranberry)) ///
			 (scatter other_total year if narcotic == 0, mcol(navy)) ///
			 (fpfit twopm_other year if narcotic == 1 & cara == 0, lcol(cranberry)) ///
			 (fpfit twopm_other year if narcotic == 0 & cara == 0, lcol(navy)) ///
			 (fpfit twopm_other year if narcotic == 1 & cara == 1, lcol(cranberry)) ///
			 (fpfit twopm_other year if narcotic == 0 & cara == 1, lcol(navy) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(label(1 "Opioid users") label(2 "Non-opioid users")) ///
			 ytitle("Average Other expenditures ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Other spending by opioid and non-opioid users"))			 
			 


// FIGURE - OTHER
preserve 
// Change location
cd "/Users/mbounthavong/Library/CloudStorage/Dropbox/Projects/Healthcare Expenditures and Opioid users/OOP on healthcare expenditures/Figures"

// Generate the upper and low bound of the 95% CI variable
gen high = .
gen low = .

// Loop commmand
forvalues i = 2008/2020 {
	forvalues j = 0/1 {
		ci mean totother if year == `i' & narcotic == `j' 
		replace high = r(ub) if year == `i' & narcotic == `j' 
		replace low = r(lb) if year == `i' & narcotic == `j' 
	}
}


graph twoway (rcap low high year if narcotic == 1, lcol("edkblue")) ///
			 (rcap low high year if narcotic == 0, lcol("sand")) ///
			 (scatter other_total year if narcotic == 1, mcol("edkblue")) ///
			 (scatter other_total year if narcotic == 0, mcol("sand") ///
			 xline(2018, lcol("cranberry")) ///
			 graphregion(color(white)) bgcolor(white) ylabel(, nogrid) xlabel(, nogrid) ///
			 legend(region(lcolor(white))) ///
			 legend(order(3 "Opioid users" 4 "Non-opioid users")) ///
			 ytitle("Average Other costs ($)", size(large)) ///
			 ylabel( , labsize(small)) ///
			 xtitle("Year", size(large)) ///
			 xlabel(2008(1)2020 , labsize(small)) ///
			 title("Other spending by opioid and non-opioid users"))
graph export Figure_other.png, replace
graph save Figure_other.gph, replace
restore			 
		
		
		

		
		
// COMBINE FIGURES - Method 1
preserve
// Change location
cd "/Users/mbounthavong/Library/CloudStorage/Dropbox/Projects/Healthcare Expenditures and Opioid users/OOP on healthcare expenditures/Figures"

graph combine "Figure_total_exp.gph" "Figure_public_spending.gph" "Figure_private.gph" "Figure_oop.gph" "Figure_other.gph", col(2) ///
		xsize(5) ///
		ysize(5) ///
		iscale(*0.45) ///
		graphregion(color(white))
graph export combined_costs.png, replace
restore
			 

		
// COMBINE FIGURES - Method 2
preserve
// Change location
cd "/Users/mbounthavong/Library/CloudStorage/Dropbox/Projects/Healthcare Expenditures and Opioid users/OOP on healthcare expenditures/Figures"

grc1leg "Figure_total_exp.gph" "Figure_public_spending.gph" "Figure_private.gph" "Figure_oop.gph" "Figure_other.gph", col(2) ///
		iscale(*0.55) ///
		graphregion(color(white)) ///
		legendfrom("Figure_total_exp.gph")
*graph export combined_costs.png, replace
restore	 
			 
			 
			 
			 
			 
			 
// SECONDARY AIMS -- CHANGE CARA = 2017
****** CARA implementation change to 2017 for the sensitivity analysis
gen cara2 = .
	replace cara2 = 0 if year < 2017
	replace cara2 = 1 if year >= 2017 & year < 99999
tab cara2, m



// TOTAL COSTS
svy, sub(subpop): twopm totexp c.year##i.cara2##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict as_totexp
margins narcotic, dydx(cara2) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara2 == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara2 = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
margins ar.narcotic#ar.cara2, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */

***** RSME model fit test -- two-part model
gen rsme_sa_totexp = sqrt((totexp - as_totexp)^2)
summ rsme_sa_totexp



// PUBLIC COSTS
svy, sub(subpop): twopm public_exp c.year##i.cara2##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict sa_public
margins narcotic, dydx(cara2) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara2 == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara2 = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
margins ar.narcotic#ar.cara2, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */

***** RSME model fit test -- two-part model
gen rsme_sa_public = sqrt((public_exp - sa_public)^2)
summ rsme_sa_public



// PRIVATE COSTS
svy, sub(subpop): twopm totprv c.year##i.cara2##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict sa_private
margins narcotic, dydx(cara2) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara2 == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara2 = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
margins ar.narcotic#ar.cara2, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */

***** RSME model fit test -- two-part model
gen rsme_sa_private = sqrt((totprv - sa_private)^2)
summ rsme_sa_private



// OOP COSTS
svy, sub(subpop): twopm totslf c.year##i.cara2##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict sa_oop
margins narcotic, dydx(cara2) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara2 == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara2 = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
margins ar.narcotic#ar.cara2, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */

***** RSME model fit test -- two-part model
gen rsme_sa_oop = sqrt((totslf - sa_oop)^2)
summ rsme_sa_oop




// OTHER COSTS
svy, sub(subpop): twopm totother c.year##i.cara2##i.narcotic $xlist2, firstpart(logit) secondpart(glm, family(gamma) link(log))
predict sa_other
margins narcotic, dydx(cara2) at(year = 2018) pwcompare noestimcheck
margins narcotic, dydx(year) at(cara2 == 0) pwcompare noestimcheck /* diff in slopes before CARA */
margins narcotic, dydx(year) at(cara2 = (0 1)) pwcompare noestimcheck /* Method 1: diff in slopes after CARA */
margins ar.narcotic#ar.cara2, dydx(year) contrast noestimcheck /* Method 3: diff in slopes after CARA */

***** RSME model fit test -- two-part model
gen rsme_sa_other = sqrt((totother - sa_other)^2)
summ rsme_sa_other



// RSME comparison
summarize rsme_sa_totexp rsme_sa_public rsme_sa_private rsme_sa_oop rsme_sa_other

   
