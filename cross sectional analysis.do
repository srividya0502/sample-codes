clear 
use "C:\Users\Manoj\Desktop\DISSERTATION\DS0002_hh.dta" 
tab SURVEY
replace INCOMEPC=0 if INCOMEPC==.
replace COPC=0 if COPC==.
sort INCOMEPC
drop if INCOMEPC<=0




replace CO1X =0 if CO1X==.
replace CO2X =0 if CO2X==.
replace CO3X =0 if CO3X==.
replace CO4X =0 if CO4X==.
replace CO5X =0 if CO5X==.
replace CO6X =0 if CO6X==.
replace CO7X =0 if CO7X==.
replace CO8X =0 if CO8X==.
replace CO9X =0 if CO9X==.
replace CO10X =0 if CO10X==.
replace CO11X =0 if CO11X==.
replace CO12X =0 if CO12X==.
replace CO13X =0 if CO13X==.
replace CO14X =0 if CO14X==.
replace CO15 =0 if CO15==.
replace CO16 =0 if CO16==.
replace CO17 =0 if CO17==.
replace CO18 =0 if CO18==.
replace CO19 =0 if CO19==.
replace CO20 =0 if CO20==.
replace CO21 =0 if CO21==.
replace CO22 =0 if CO22==.
replace CO23 =0 if CO23==.
replace CO24 =0 if CO24==.
replace CO25 =0 if CO25==.
replace CO26 =0 if CO26==.
replace CO27 =0 if CO27==.
replace CO28 =0 if CO28==.
replace CO29 =0 if CO29==.
replace CO31 =0 if CO31==.
replace CO30 =0 if CO30==.
replace CO30B =0 if CO30B==.
replace CO33 =0 if CO33==.
replace CO34 =0 if CO34==.
replace CO35 =0 if CO35==.
replace CO36 =0 if CO36==.
replace CO37 =0 if CO37==.
replace CO38 =0 if CO38==.
replace CO39 =0 if CO39==.
replace CO40 =0 if CO40==.
replace CO41 =0 if CO41==.
replace CO42 =0 if CO42==.
replace CO43 =0 if CO43==.
replace CO44 =0 if CO44==.
replace CO45 =0 if CO45==.
replace CO46 =0 if CO46==.
replace CO47 =0 if CO47==.
replace CO48 =0 if CO48==.
replace CO49 =0 if CO49==.
replace CO50 =0 if CO50==.
replace CO51 =0 if CO51==.
replace CO52 =0 if CO52==.

gen COH = CO30 + CO30B

gen cons_14 = CO1X+ CO2X+ CO3X+ CO4X+ CO5X+ CO6X+ CO7X+ CO8X+ CO9X+ CO10X+ CO11X+ CO12X+ CO13X+ CO14X
replace cons_14=0 if cons_14==.
gen cons_15 = CO15+ CO16+CO17+ CO18+ CO19+ CO20+ CO21+CO22+ CO23+ CO24+ CO25+ CO26+CO27+ CO28+CO29+ COH+ CO31+ CO32+ CO33
gen cons33_yr = CO34+ CO35+CO36+ CO37+ CO38+ CO39+ CO40+ CO41+ CO42+ CO43+ CO44+ CO45+ CO46+ CO47+ CO48+ CO49+ CO50+ CO51+ CO52
replace cons_14=0 if cons_14==.
replace cons_15=0 if cons_15==.
replace cons33_yr=0 if cons33_yr==.
gen cons_33 = cons33_yr/12
gen cons_tot = (cons_14 +cons_15 + cons_33)*12
gen conspc_m=cons_tot/NPERSONS
gen cons_pc = conspc_m*12

gen group_count = .
bysort DISTRICT (DISTRICT): replace group_count = _N
drop if group_count < 100


gen high_exp= (CO23*12)+ CO43+CO52+ CO51 +CO46+ CO47+ CO45+ CO39+ CO40 + CO38+ CO44+ ( COH*12)+ CO49
gen high_exp_m=high_exp/12
gen high_exp_propn = high_exp/cons_tot

gen log_cons=log( cons_tot+1)
gen log_inc=log( INCOME+1)
gen log_hep=log( high_exp+1)
gen log_assets=log(ASSETS2005+1)


replace DB5=0 if DB5==.
gen log_debt=log(DB5+1)

replace DB2C=0 if DB2C==.

egen mean_income = mean(INCOMEPC), by(DISTRICT)

 
gen log_dist_inc=log(mean_income)

egen mean_assets = mean(ASSETS2005), by(DISTRICT)
gen log_dist_assets=log(mean_assets)

xtile income_quartile = INCOMEPC, n(5)
gen inc_q = income_quartile-1



gen spopln=.
* Assign 2011 population values if SURVEY == 2
replace spopln = 199812 if SURVEY == 2 & STATEID == 9   // Uttar Pradesh
replace spopln = 112374 if SURVEY == 2 & STATEID == 27  // Maharashtra
replace spopln = 104099 if SURVEY == 2 & STATEID == 10  // Bihar
replace spopln = 91276  if SURVEY == 2 & STATEID == 19  // West Bengal
replace spopln = 72627  if SURVEY == 2 & STATEID == 23  // Madhya Pradesh
replace spopln = 72147  if SURVEY == 2 & STATEID == 33  // Tamil Nadu
replace spopln = 68548  if SURVEY == 2 & STATEID == 8   // Rajasthan
replace spopln = 61095  if SURVEY == 2 & STATEID == 29  // Karnataka
replace spopln = 60440  if SURVEY == 2 & STATEID == 24  // Gujarat
replace spopln = 49577  if SURVEY == 2 & STATEID == 28  // Andhra Pradesh
replace spopln = 41974  if SURVEY == 2 & STATEID == 21  // Odisha
replace spopln = 33406  if SURVEY == 2 & STATEID == 32  // Kerala
replace spopln = 32988  if SURVEY == 2 & STATEID == 20  // Jharkhand
replace spopln = 31206  if SURVEY == 2 & STATEID == 18  // Assam
replace spopln = 27743  if SURVEY == 2 & STATEID == 3   // Punjab
replace spopln = 25545  if SURVEY == 2 & STATEID == 22  // Chhattisgarh
replace spopln = 25351  if SURVEY == 2 & STATEID == 6   // Haryana
replace spopln = 16788  if SURVEY == 2 & STATEID == 7   // Delhi
replace spopln = 12541  if SURVEY == 2 & STATEID == 1   // Jammu & Kashmir
replace spopln = 10086  if SURVEY == 2 & STATEID == 5   // Uttarakhand
replace spopln = 6865   if SURVEY == 2 & STATEID == 2   // Himachal Pradesh
replace spopln = 3674   if SURVEY == 2 & STATEID == 16  // Tripura
replace spopln = 1097   if SURVEY == 2 & STATEID == 15  // Mizoram
replace spopln = 611    if SURVEY == 2 & STATEID == 11  // Sikkim
replace spopln = 1979   if SURVEY == 2 & STATEID == 39  // Nagaland
replace spopln = 2967   if SURVEY == 2 & STATEID == 14  // Meghalaya
replace spopln = 1248   if SURVEY == 2 & STATEID == 34  // Puducherry
replace spopln = 1459   if SURVEY == 2 & STATEID == 30  // Goa
replace spopln = 344    if SURVEY == 2 & STATEID == 35  // Dadra and Nagar Haveli
replace spopln = 243    if SURVEY == 2 & STATEID == 36  // Daman and Diu
replace spopln = 381    if SURVEY == 2 & STATEID == 37  // Andaman & Nicobar Islands
replace spopln = 64     if SURVEY == 2 & STATEID == 38  // Lakshadweep
replace spopln = 1055   if SURVEY == 2 & STATEID == 31  // Chandigarh
replace spopln = 2856   if SURVEY == 2 & STATEID == 13  // Manipur
replace spopln = 1384   if SURVEY == 2 & STATEID == 12  // Arunachal Pradesh

gen log_popln=log(spopln)


gen sgdp=.
replace sgdp = 662592 if SURVEY == 2 & STATEID == 28 // Andhra Pradesh
replace sgdp = 10619  if SURVEY == 2 & STATEID == 12 // Arunachal Pradesh
replace sgdp = 125820 if SURVEY == 2 & STATEID == 18 // Assam
replace sgdp = 247318 if SURVEY == 2 & STATEID == 10 // Bihar
replace sgdp = 132872 if SURVEY == 2 & STATEID == 22 // Chhattisgarh
replace sgdp = 36025  if SURVEY == 2 & STATEID == 30 // Goa
replace sgdp = 594563 if SURVEY == 2 & STATEID == 24 // Gujarat
replace sgdp = 301959 if SURVEY == 2 & STATEID == 06 // Haryana
replace sgdp = 64957  if SURVEY == 2 & STATEID == 02 // Himachal Pradesh
replace sgdp = 65759  if SURVEY == 2 & STATEID == 01 // Jammu & Kashmir
replace sgdp = 458894 if SURVEY == 2 & STATEID == 29 // Karnataka
replace sgdp = 307906 if SURVEY == 2 & STATEID == 32 // Kerala
replace sgdp = 311670 if SURVEY == 2 & STATEID == 23 // Madhya Pradesh
replace sgdp = 1199548 if SURVEY == 2 & STATEID == 27 // Maharashtra
replace sgdp = 7198  if SURVEY == 2 & STATEID == 15 // Mizoram
replace sgdp = 20982  if SURVEY == 2 & STATEID == 16 // Tripura
replace sgdp = 214583 if SURVEY == 2 & STATEID == 21 // Odisha
replace sgdp = 256430 if SURVEY == 2 & STATEID == 03 // Punjab
replace sgdp = 403422 if SURVEY == 2 & STATEID == 08 // Rajasthan
replace sgdp = 8616   if SURVEY == 2 & STATEID == 11 // Sikkim
replace sgdp = 665312 if SURVEY == 2 & STATEID == 33 // Tamil Nadu
replace sgdp = 679007 if SURVEY == 2 & STATEID == 09 // Uttar Pradesh
replace sgdp = 97696  if SURVEY == 2 & STATEID == 05 // Uttarakhand
replace sgdp = 538209 if SURVEY == 2 & STATEID == 19 // West Bengal
replace sgdp = 4746   if SURVEY == 2 & STATEID == 34 // Pondicherry
replace sgdp = 143891 if SURVEY == 2 & STATEID == 20 // Jharkhand
replace sgdp = 296957 if SURVEY == 2 & STATEID == 07 // Delhi

gen log_sgdp=log(sgdp)


gen gini_s=.
levels DISTRICT, local(levels)
foreach i of local levels {
    di "Processing subgroup `i'..."
    ineqdeco INCOMEPC [fw=FWT] if DISTRICT == `i'
    replace gini_s = $S_gini if DISTRICT == `i'
}

gen gini_sq= gini_s*gini_s

tobit log_hep gini_s log_sgdp log_popln log_assets log_dist_assets log_debt  i.HHEDUC NPERSONS i.DB6 i.GROUPS i.ID14 i.RO3 RO5 i.URBAN4_2011 log_cons i.STATEID  [pweight=FWT], ll(0)  vce(cluster IDHH)
drop if log_hep<6
qreg2 log_hep gini_s log_sgdp log_popln log_assets log_dist_assets log_debt i.HHEDUC NPERSONS i.GROUPS log_cons
qregplot gini_s, q(1(5)99) yscale(range(-0.4 0.4))

graph export  "C:\Users\Manoj\Desktop\DISSERTATION\QR Plot CS 1.png", replace

gen logincpc= log( INCOMEPC+1)
replace logincpc=0 if logincpc==.
mlogit CI3 gini_s  logincpc i.HHEDUC RO5 log_popln log_sgdp log_dist_assets [pweight= FWT], baseoutcome(1) vce(cluster IDHH)
mlogit CI4 gini_s logincpc i.HHEDUC RO5 log_popln log_sgdp log_dist_assets i.STATEID [pweight= FWT], baseoutcome(1) vce(cluster IDHH)
mlogit CI6 gini_s logincpc i.HHEDUC RO5 log_popln log_sgdp log_dist_assets [pweight= FWT], baseoutcome(1) vce(cluster IDHH)
mlogit CI7 gini_s logincpc i.HHEDUC RO5 log_popln log_sgdp log_dist_assets i.STATEID[pweight= FWT], baseoutcome(1) vce(cluster IDHH)
mlogit CI9 gini_s logincpc i.HHEDUC RO5 log_popln log_sgdp log_dist_assets  [pweight= FWT], baseoutcome(1) vce(cluster IDHH)

save "C:\Users\Manoj\Desktop\DISSERTATION\pre loan.dta", replace



xtile gini_quartile = gini_s, n(10)
tab DB2D if gini_quartile>5
tab DB2C if gini_quartile>5
drop if DB2C==0
count
tab DB2C if gini_quartile<4
tab DB2C if gini_quartile>6
tab DB2C

clear
use"C:\Users\Manoj\Desktop\DISSERTATION\pre loan.dta"
gen log_dowry_low=log( MP7A +1)
gen log_dowry_high=log( MP7B +1)



reg log_hep log_dowry_low log_cons log_assets log_dist_inc log_inc
predict predicted_log_hep, xb
twoway (scatter predicted_log_hep log_dowry_low) (line predicted_log_hep log_dowry_low, sort), ///
    title("Predicted vs log_dowry_low") ///
    ytitle("Predicted log_hep") ///
    xtitle("log_dowry_low")


reg log_hep log_dowry_high log_cons log_assets log_dist_inc log_inc
predict predicted_log_hep, xb
twoway (scatter predicted_log_hep log_dowry_high) (line predicted_log_hep log_dowry_high, sort), ///
    title("Predicted vs log_dowry_low") ///
    ytitle("Predicted log_hep") ///
    xtitle("log_dowry_low")
	

*************************************************************************************************************************************************************




clear 
use "C:\Users\Manoj\Desktop\DISSERTATION\DS0002_hh.dta" 
tab SURVEY
replace INCOMEPC=0 if INCOMEPC==.
replace COPC=0 if COPC==.
sort INCOMEPC
drop if INCOMEPC<=0

replace CO1X =0 if CO1X==.
replace CO2X =0 if CO2X==.
replace CO3X =0 if CO3X==.
replace CO4X =0 if CO4X==.
replace CO5X =0 if CO5X==.
replace CO6X =0 if CO6X==.
replace CO7X =0 if CO7X==.
replace CO8X =0 if CO8X==.
replace CO9X =0 if CO9X==.
replace CO10X =0 if CO10X==.
replace CO11X =0 if CO11X==.
replace CO12X =0 if CO12X==.
replace CO13X =0 if CO13X==.
replace CO14X =0 if CO14X==.
replace CO15 =0 if CO15==.
replace CO16 =0 if CO16==.
replace CO17 =0 if CO17==.
replace CO18 =0 if CO18==.
replace CO19 =0 if CO19==.
replace CO20 =0 if CO20==.
replace CO21 =0 if CO21==.
replace CO22 =0 if CO22==.
replace CO23 =0 if CO23==.
replace CO24 =0 if CO24==.
replace CO25 =0 if CO25==.
replace CO26 =0 if CO26==.
replace CO27 =0 if CO27==.
replace CO28 =0 if CO28==.
replace CO29 =0 if CO29==.
replace CO31 =0 if CO31==.
replace CO30 =0 if CO30==.
replace CO30B =0 if CO30B==.
replace CO33 =0 if CO33==.
replace CO34 =0 if CO34==.
replace CO35 =0 if CO35==.
replace CO36 =0 if CO36==.
replace CO37 =0 if CO37==.
replace CO38 =0 if CO38==.
replace CO39 =0 if CO39==.
replace CO40 =0 if CO40==.
replace CO41 =0 if CO41==.
replace CO42 =0 if CO42==.
replace CO43 =0 if CO43==.
replace CO44 =0 if CO44==.
replace CO45 =0 if CO45==.
replace CO46 =0 if CO46==.
replace CO47 =0 if CO47==.
replace CO48 =0 if CO48==.
replace CO49 =0 if CO49==.
replace CO50 =0 if CO50==.
replace CO51 =0 if CO51==.
replace CO52 =0 if CO52==.

gen COH = CO30 + CO30B

gen cons_14 = CO1X+ CO2X+ CO3X+ CO4X+ CO5X+ CO6X+ CO7X+ CO8X+ CO9X+ CO10X+ CO11X+ CO12X+ CO13X+ CO14X
replace cons_14=0 if cons_14==.
gen cons_15 = CO15+ CO16+CO17+ CO18+ CO19+ CO20+ CO21+CO22+ CO23+ CO24+ CO25+ CO26+CO27+ CO28+CO29+ COH+ CO31+ CO32+ CO33
gen cons33_yr = CO34+ CO35+CO36+ CO37+ CO38+ CO39+ CO40+ CO41+ CO42+ CO43+ CO44+ CO45+ CO46+ CO47+ CO48+ CO49+ CO50+ CO51+ CO52
replace cons_14=0 if cons_14==.
replace cons_15=0 if cons_15==.
replace cons33_yr=0 if cons33_yr==.
gen cons_33 = cons33_yr/12
gen cons_tot = (cons_14 +cons_15 + cons_33)*12
gen conspc_m=cons_tot/NPERSONS
gen cons_pc = conspc_m*12

gen group_id=STATEID*100+URBAN4_2011
gen group_count = .
bysort group_id (group_id): replace group_count = _N
drop if group_count < 100

gen high_exp= (CO23*12)+ CO43+CO52+ CO51 +CO46+ CO47+ CO45+ CO39+ CO40 + CO38+ CO44+ ( COH*12)+ CO49
gen high_exp_m=high_exp/12
gen high_exp_propn = high_exp/cons_tot

gen log_cons=log( cons_tot+1)
gen log_inc=log( INCOME+1)
gen log_hep=log( high_exp+1)
gen log_assets=log(ASSETS2005+1)


replace DB5=0 if DB5==.
gen log_debt=log(DB5+1)

replace DB2C=0 if DB2C==.

egen mean_income = mean(INCOMEPC), by(group_id)

 
gen log_dist_inc=log(mean_income)

egen mean_assets = mean(ASSETS2005), by(group_id)
gen log_dist_assets=log(mean_assets)

xtile income_quartile = INCOMEPC, n(4)
gen inc_q = income_quartile-1



gen spopln=.
* Assign 2011 population values if SURVEY == 2
replace spopln = 199812 if SURVEY == 2 & STATEID == 9   // Uttar Pradesh
replace spopln = 112374 if SURVEY == 2 & STATEID == 27  // Maharashtra
replace spopln = 104099 if SURVEY == 2 & STATEID == 10  // Bihar
replace spopln = 91276  if SURVEY == 2 & STATEID == 19  // West Bengal
replace spopln = 72627  if SURVEY == 2 & STATEID == 23  // Madhya Pradesh
replace spopln = 72147  if SURVEY == 2 & STATEID == 33  // Tamil Nadu
replace spopln = 68548  if SURVEY == 2 & STATEID == 8   // Rajasthan
replace spopln = 61095  if SURVEY == 2 & STATEID == 29  // Karnataka
replace spopln = 60440  if SURVEY == 2 & STATEID == 24  // Gujarat
replace spopln = 49577  if SURVEY == 2 & STATEID == 28  // Andhra Pradesh
replace spopln = 41974  if SURVEY == 2 & STATEID == 21  // Odisha
replace spopln = 33406  if SURVEY == 2 & STATEID == 32  // Kerala
replace spopln = 32988  if SURVEY == 2 & STATEID == 20  // Jharkhand
replace spopln = 31206  if SURVEY == 2 & STATEID == 18  // Assam
replace spopln = 27743  if SURVEY == 2 & STATEID == 3   // Punjab
replace spopln = 25545  if SURVEY == 2 & STATEID == 22  // Chhattisgarh
replace spopln = 25351  if SURVEY == 2 & STATEID == 6   // Haryana
replace spopln = 16788  if SURVEY == 2 & STATEID == 7   // Delhi
replace spopln = 12541  if SURVEY == 2 & STATEID == 1   // Jammu & Kashmir
replace spopln = 10086  if SURVEY == 2 & STATEID == 5   // Uttarakhand
replace spopln = 6865   if SURVEY == 2 & STATEID == 2   // Himachal Pradesh
replace spopln = 3674   if SURVEY == 2 & STATEID == 16  // Tripura
replace spopln = 1097   if SURVEY == 2 & STATEID == 15  // Mizoram
replace spopln = 611    if SURVEY == 2 & STATEID == 11  // Sikkim
replace spopln = 1979   if SURVEY == 2 & STATEID == 39  // Nagaland
replace spopln = 2967   if SURVEY == 2 & STATEID == 14  // Meghalaya
replace spopln = 1248   if SURVEY == 2 & STATEID == 34  // Puducherry
replace spopln = 1459   if SURVEY == 2 & STATEID == 30  // Goa
replace spopln = 344    if SURVEY == 2 & STATEID == 35  // Dadra and Nagar Haveli
replace spopln = 243    if SURVEY == 2 & STATEID == 36  // Daman and Diu
replace spopln = 381    if SURVEY == 2 & STATEID == 37  // Andaman & Nicobar Islands
replace spopln = 64     if SURVEY == 2 & STATEID == 38  // Lakshadweep
replace spopln = 1055   if SURVEY == 2 & STATEID == 31  // Chandigarh
replace spopln = 2856   if SURVEY == 2 & STATEID == 13  // Manipur
replace spopln = 1384   if SURVEY == 2 & STATEID == 12  // Arunachal Pradesh

gen log_popln=log(spopln)


gen sgdp=.
replace sgdp = 662592 if SURVEY == 2 & STATEID == 28 // Andhra Pradesh
replace sgdp = 10619  if SURVEY == 2 & STATEID == 12 // Arunachal Pradesh
replace sgdp = 125820 if SURVEY == 2 & STATEID == 18 // Assam
replace sgdp = 247318 if SURVEY == 2 & STATEID == 10 // Bihar
replace sgdp = 132872 if SURVEY == 2 & STATEID == 22 // Chhattisgarh
replace sgdp = 36025  if SURVEY == 2 & STATEID == 30 // Goa
replace sgdp = 594563 if SURVEY == 2 & STATEID == 24 // Gujarat
replace sgdp = 301959 if SURVEY == 2 & STATEID == 06 // Haryana
replace sgdp = 64957  if SURVEY == 2 & STATEID == 02 // Himachal Pradesh
replace sgdp = 65759  if SURVEY == 2 & STATEID == 01 // Jammu & Kashmir
replace sgdp = 458894 if SURVEY == 2 & STATEID == 29 // Karnataka
replace sgdp = 307906 if SURVEY == 2 & STATEID == 32 // Kerala
replace sgdp = 311670 if SURVEY == 2 & STATEID == 23 // Madhya Pradesh
replace sgdp = 1199548 if SURVEY == 2 & STATEID == 27 // Maharashtra
replace sgdp = 7198  if SURVEY == 2 & STATEID == 15 // Mizoram
replace sgdp = 20982  if SURVEY == 2 & STATEID == 16 // Tripura
replace sgdp = 214583 if SURVEY == 2 & STATEID == 21 // Odisha
replace sgdp = 256430 if SURVEY == 2 & STATEID == 03 // Punjab
replace sgdp = 403422 if SURVEY == 2 & STATEID == 08 // Rajasthan
replace sgdp = 8616   if SURVEY == 2 & STATEID == 11 // Sikkim
replace sgdp = 665312 if SURVEY == 2 & STATEID == 33 // Tamil Nadu
replace sgdp = 679007 if SURVEY == 2 & STATEID == 09 // Uttar Pradesh
replace sgdp = 97696  if SURVEY == 2 & STATEID == 05 // Uttarakhand
replace sgdp = 538209 if SURVEY == 2 & STATEID == 19 // West Bengal
replace sgdp = 4746   if SURVEY == 2 & STATEID == 34 // Pondicherry
replace sgdp = 143891 if SURVEY == 2 & STATEID == 20 // Jharkhand
replace sgdp = 296957 if SURVEY == 2 & STATEID == 07 // Delhi

gen log_sgdp=log(sgdp)


drop if group_count < 100
*drop if group_count > 700

gen gini_s=.
levels group_id, local(levels)
foreach i of local levels {
    di "Processing subgroup `i'..."
    ineqdeco INCOMEPC [fw=FWT] if group_id == `i'
    replace gini_s = $S_gini if group_id == `i'
}

gen gini_sq= gini_s*gini_s

tobit log_hep gini_s log_sgdp log_popln log_assets log_dist_assets log_debt  i.HHEDUC NPERSONS i.DB6 i.GROUPS i.ID14 i.RO3 RO5 log_cons i.STATEID  [pweight=FWT], ll(0)  vce(cluster IDHH)

qreg2 log_hep gini_s log_sgdp log_popln log_assets log_dist_assets log_debt i.HHEDUC NPERSONS i.GROUPS log_cons
qregplot gini_s, q(1(5)99) yscale(range(-0.4 0.4))

graph export  "C:\Users\Manoj\Desktop\DISSERTATION\QR Plot CS 2.png", replace