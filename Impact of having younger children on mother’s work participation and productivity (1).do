/*TERM PAPER - APPLIED MICROECONOMETRICS

Impact of having younger children on mother’s work participation and productivity  

DATA USED : Indian Human Development Survey 2012 
AUTHOR: SRI VIDYA
*/


*IMPORTING ELIGIIBLE WOMEN QUESTIONAIRE DATA
clear
use "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\DS0004\36151-0004-Data.dta"

*PART 1
*Dealing with kid-mother features

*CREATING UNIQUE ID FOR MOTHERS 
*EW3- Roster id of mother, IDHH- Household unique id
/*keep EW3 HHID IDHH*/
drop if EW3 == .
tostring EW3, replace
replace EW3 = substr("00"+trim(EW3),-2,2)
gen mom_id = IDHH+EW3

duplicates report mom_id

*BH2 - HH roster id of the child
drop if BH2==.
tostring BH2, replace
replace BH2 = substr("00"+trim(BH2),-2,2)

*creating a unique id based on documentation to merge with main dataset
gen IDPERSON = IDHH+BH2
duplicates report IDPERSON
duplicates drop IDPERSON, force

duplicates report mom_id

count

save "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\kid_id2012.dta",replace

*matching age and gender with kids (There were many missing values in the age variable given in birth history data. so the age variable (RO5) from individual dataset)

merge 1:1 IDPERSON using "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\DS0001\36151-0001-Data.dta" , force
rename _merge merge0

keep if merge0 == 3  
keep IDPERSON mom_id RO5 RO3 BH4 BHED BH8A BH8B 

*RO3 - gender, dropping the inconsistent ones who have different gender in the eligible women questionnaire
count if RO3!=BH4
drop if RO3!=BH4


*removing mothers with children above 18, RO5- age
count
count if RO5<=18
br

bysort mom_id: gen above18 = (RO5>18)
bysort mom_id: egen has_child_above18 = max(above18)
br
drop if has_child_above18==1

drop above18
count

*removing mothers without atleast 1 kid above age 5
bysort mom_id: gen above5 = (RO5>5)
bysort mom_id: egen has_child_above5 = max(above5)
br
drop if has_child_above5!=1

drop above5
count

*No of kids
bysort mom: gen num_children=_N
count

*dropping if dead
count if BH8A==0 & (BH8B!=. | BH8B!=0)
count if (BH8A!=. | BH8A!=0) & (BH8B!=. | BH8B!=0)

*Creating number of boys/girls/total under 6 and dummy for if(1) or not(0)
egen num_boys_0_5 = total(RO5 <= 5 & RO3==1), by (mom_id)
bysort mom_id: gen has_boy_under6 = (num_boys_0_5!=0) 
egen num_girls_0_5 = total(RO5 <= 5 & RO3==2), by (mom_id)
bysort mom_id: gen has_girl_under6 = (num_girls_0_5!=0)
egen tot_kids_0_5 = total(RO5 <=5), by (mom_id)
bysort mom_id: gen has_kid_under6 = (tot_kids_0_5!=0) 

*Creating number of boys/girls/total above 5 and dummy for if(1) or not(0) 
egen num_boys_6_18 = total(RO5 > 5 & RO3==1), by (mom_id)
bysort mom_id: gen has_boy_above5 = (num_boys_6_18!=0) 
egen num_girls_6_18 = total(RO5 > 5 & RO3==2), by (mom_id)
bysort mom_id: gen has_girl_above5 = (num_girls_6_18!=0)
egen tot_kids_6_18 = total(RO5 > 5), by (mom_id)
bysort mom_id: gen has_kid_above5= (tot_kids_6_18!=0)

*sex composition of kids above age 5
gen sex_comp_above5 = num_girls_6_18/num_boys_6_18
//should improve on the equation-indeterminate values

*if all child above 5 are female(1) or not(0) 
bysort mom_id: gen all_girls_above5 = (tot_kids_6_18 == num_girls_6_18)
bysort mom_id:gen all_boys_above5 = (tot_kids_6_18 == num_boys_6_18)

gen above5 = 1 if all_girls_above5==1
replace above5 = 2 if all_boys_above5==1
replace above5 = 3 if all_girls_above5==0 & all_boys_above5==0

duplicates drop mom_id, force

drop RO5 RO3 BH4 BHED BH8A BH8B

count
save "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\merge0-kid_age2012.dta", replace


*PART 2
*matching mothers in birth history and eligible women
clear
*importing eligible women questionnaire
use "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\DS0003\36151-0003-Data.dta"
rename IDPERSON mom_id
 
merge 1:1 mom_id using "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\merge0-kid_age2012.dta",force
rename _merge merge1

keep if merge1==3
count

*EW9 - no.of children in EW questionnaire
count if num_children!=EW9
drop if num_children!=EW9
// It can be because age is higher or missing in birth history

tab EW6

*dropping women not in range age<=15 & age<=49
keep if EW6>=15 &  EW6<=49

br EW6 EW9 EW13A EW13B EW13C EW13D ID13 ASSETS NPERSONS WKANY5 WKHOURS INCOME INCOMEPC

*If a mother/father/mother-in-law/father-in-law is present(1) or (0)
rename EW13A mother_present
rename EW13B father_present
rename EW13C mother_in_law_present
rename EW13D father_in_law_present
replace  mother_present = 0 if  mother_present==.|mother_present==2
replace  father_present = 0 if  father_present==.|father_present==2 
replace  mother_in_law_present = 0 if  mother_in_law_present==.| mother_in_law_present==2
replace  father_in_law_present = 0 if  father_in_law_present==.|father_in_law_present==2

*hh size excluding kids
gen hh_size_excl_kids = NPERSONS-num_children
count if hh_size_excl_kids <0

br INCOME INCOMEPC NPERSONS

count

save "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\merge1_mother_kid2012.dta",replace

*PART 3
*Final merging
clear
use "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\DS0001\36151-0001-Data.dta" 
rename IDPERSON mom_id

merge 1:1 mom_id using "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\merge1_mother_kid2012.dta" , force
rename _merge merge2

keep if merge2==3

*creating variable percapita income excluding women's income, RSUNEARN- income of household minus the individual's income
count if RSUNEARN<0
drop if RSUNEARN<0
gen percapita_inc_excl_women = RSUNEARN/NPERSONS
replace percapita_inc_excl_women = log(percapita_inc_excl_women+1)

*Working hours 
gen dummy_working = (WKHOURS >= 240)
tab dummy_working

gen log_hours= log(WKHOURS+1)
gen age_sq = RO5*RO5

*willingness to work variable
tab GR48
count
replace GR48=0 if GR48==.

*Permission to work
tab GR49
count
replace GR49=9 if GR49==.

*Occupation
tab RO7
mdesc RO7

replace HHEDUCM=99 if HHEDUCM==.
drop if GROUPS==. | ASSETS==.

* Recode WS5 according to NIC sections
recode WS5 (0/9 = 1 "Agriculture, Forestry and Fishing") ///
(10/19 = 2 "Mining and Quarrying") ///
(20 /39 = 3 "Manufacturing") ///
(40 /49 = 4 "Electricity, Gas, Steam and Air Conditioning Supply") ///
(50/59 = 5 "Construction") ///
(60/68 = 6 "Wholesale and Retail Trade") ///
(69/73 = 7 "Transportation and Storage") ///
(75 = 8 "Information and Communication") ///
(80/81 = 9 "Financial and Insurance Activities") ///
(83/89 = 10 "Professional, Scientific and Technical Activities") ///
(90/91 = 11 "Public Administration and Defence; Compulsory Social Security") ///
(92 = 12 "Education") ///
(93 = 13 "Human Health and Social Work Activities") ///
(94/95 = 14 "Arts, Entertainment and Recreation") ///
(96/99 = 15 "Other Service Activities"), generate(ind_classification)
replace ind_classification=16 if ind_classification==.

replace GROUPS=2 if GROUPS==1

save "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\IHDS2012.dta",replace


//DESCRIPTIVE STATISTICS 

sum dummy_working has_kid_under6 RO5 EDUC7 mother_in_law_present hh_size_excl_kids father_in_law_present percapita_inc_excl_women HHEDUCM URBAN2011 STATEID ASSETS GR48 GR49 WS4 GROUPS WKHOURS

*participation rate of mothers with and without child at age 0-5
tab dummy_working has_kid_under6

*relation between education, mother-in-laws presence, mother's presence, income quantile and work participation

xtile income_quantiles = percapita_inc_excl_women, nq(4)

tab dummy_working EDUC7
tab dummy_working mother_present
tab dummy_working mother_in_law_present
tab dummy_working income_quantiles

tab num_children EDUC7
tab num_children mother_present
tab num_children mother_in_law_present
tab num_children income_quantiles

tab has_kid_under6 EDUC7
tab has_kid_under6 mother_present
tab has_kid_under6 mother_in_law_present
tab has_kid_under6 income_quantiles

*relation between has a kid below 5 and sex composition of kid above 5
tab has_kid_under6 above5

//ANALYSIS

* I. Impact of having young children on the productivity levels of the mothers

churdle linear log_hours tot_kids_0_5 RO5 i.EDUC7 hh_size_excl_kids i.URBAN2011 percapita_inc_excl_women i.ind_classification ASSETS i.STATEID , select(has_kid_under6  RO5 i.EDUC7 i.mother_in_law_present i.father_in_law_present i.mother_present i.father_present i.HHEDUCM i.URBAN2011 i.STATEID  percapita_inc_excl_women ASSETS i.GR49 i.GROUPS) ll(0) 
eststo churdle

esttab churdle using "Churdle_Results.rtf", replace ///
    drop(*.HHEDUCM *.STATEID *.ind_classification) ///
    nobaselevels star(* 0.1 ** 0.05 *** 0.01) scalars(r2 N)
	
*II. Impact of having young children on the labour market participation decision of the mothers

*IV MODELS:/* reference/ justification: woolridge explanation : https://www.statalist.org/forums/forum/general-stata-discussion/general/1379449-two-step-iv-method-with-binary-dependent-variable*/

*IV 2SLS model - done as referred in most literature
ivregress 2sls dummy_working RO5  i.EDUC7  i.mother_in_law_present  i.father_in_law_present i.mother_present i.father_present i.HHEDUCM i.URBAN2011  hh_size_excl_kids i.ind_classification i.STATEID ASSETS percapita_inc_excl_women i.GR49 i.GROUPS ( has_kid_under6 = has_boy_above5 )
eststo iv2sls

probit dummy_working has_kid_under6 RO5 i.EDUC7 i.mother_present i.father_present i.mother_in_law_present hh_size_excl_kids i.father_in_law_present i.HHEDUCM i.URBAN2011  i.GR49 i.ind_classification i.STATEID  percapita_inc_excl_women i.GROUPS ASSETS
eststo probit

*this we use when dependent variable, independent variable both are binary variables and IV has to be incorporated 
biprobit (dummy_working has_kid_under6 RO5 i.EDUC7 i.mother_present i.father_present i.mother_in_law_present hh_size_excl_kids i.father_in_law_present i.HHEDUCM i.URBAN2011 i.GR49 i.ind_classification i.STATEID  percapita_inc_excl_women i.GROUPS ASSETS) (has_kid_under6 has_boy_above5 RO5 i.EDUC7 i.mother_present i.father_present i.mother_in_law_present hh_size_excl_kids i.father_in_law_present i.HHEDUCM i.URBAN2011  i.GR49 i.ind_classification i.STATEID  percapita_inc_excl_women i.GROUPS ASSETS), robust
eststo biprobit

* Extract results for each model
* Export to Excel, dropping unnecessary variables
esttab iv2sls probit biprobit_eq1 biprobit_eq2 using "C:\Users\91934\Downloads\ICPSR_36151-V6\ICPSR_36151\Results.rtf", replace ///
	cells(b(star)) stats(N r2) title("Model Comparisons") ///
    label ///
    drop(*.HHEDUCM *.STATEID *.ind_classification) ///
	addnote("IV 2SLS", "Probit", "Biprobit First Stage", "Biprobit Second Stage")

*Tests for IV 

*1) Pearson corr
pwcorr dummy_working has_boy_above5, sig /*very low*/
pwcorr has_kid_under6 has_boy_above5, sig /*decently high- good instrument*/

*2)other tests using ivreg2
ssc install ivreg2

ivreg2 dummy_working RO5 i.EDUC7 i.mother_present i.father_present i.mother_in_law_present hh_size_excl_kids i.father_in_law_present i.HHEDUCM i.URBAN2011  i.GR49 i.ind_classification i.STATEID  percapita_inc_excl_women i.GROUPS ASSETS ( has_kid_under6 = has_boy_above5 ),robust first 
/*f value greater than 10*/

/*
Underidentification Test (Kleibergen-Paap rk LM Statistic):
The large Chi-square value (1021.883) with a p-value of 0.0000 indicates that your instrument(s) are relevant and that the model is not underidentified. This confirms that there’s a statistically significant correlation between the instruments and the endogenous regressor.

Weak Identification Test (Cragg-Donald Wald F Statistic and Kleibergen-Paap rk Wald F Statistic):
Both the Cragg-Donald and Kleibergen-Paap statistics (1277.594 and 1254.199, respectively) are well above the Stock-Yogo critical values for weak instruments (16.38 for 10% maximal IV size). This indicates strong instruments, suggesting that weak instruments are not a concern in our model.

Hansen J Statistic (Overidentification Test):
The Hansen J test indicates if the instruments are exogenous. However, here, the model is exactly identified, meaning there are just enough instruments for the endogenous variables. As a result, the Hansen test is unnecessary (and thus outputs as "exactly identified"), confirming that all instruments are valid.

All the above-tests are estimated by running 2SLS. Although these tests were especially developed for nonlinear models, their estimation is important in the assessing the biprobit (Nichols 2007).

Why use IV? - because of endogeneity : Wald test of endogeneity in biprobit model
The p-value of 0.0475 is less than the conventional significance level of 0.05. This suggests that you can reject the null hypothesis at the 5% significance level.

Endogeneity Exists: Since the test indicates that ρ is significantly different from zero, it implies that the error terms are correlated. Thus, the variables involved are likely endogenous. Given this evidence of endogeneity, it is appropriate to use the biprobit model over a single-stage probit model. The biprobit model accounts for the correlation between the equations, providing more reliable estimates.

*/






























