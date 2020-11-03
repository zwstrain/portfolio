/*NOTE TO READER: the following file uses a dataset containing a preselected 
set of responses from a household survey carried out in Moldova. The aim of 
the below analysis is to explore what kind of effects received monetary 
remittances have on household expenditures.*/

pwd
cd "/Users/zacharystrain/Documents/Stata/from MPP/Quantitative week/

use "data_cleaned.dta", clear

br
/*
log using codebook.txt, text
codebook
log close 
*/
*-------------------------------------------------------------------------------
*EXPLORING THE DATA
*-------------------------------------------------------------------------------
describe

*-------------------------------------------------------------------DEMOGRAPHICS
sum a_007
tab a_007 /*respondent's sex*/

sum a_006 /*respondent's relation to household head*/
tab a_006
graph bar, over(a_006, sort(1) descending label(angle(45)))
*most survey respondents are either the household head (hhh), spouse of hhh, or biological child of hhh

sum a_009a /*age*/
hist (a_009a)

tab a_015 /*ethnicity --> 86.5% Moldovan*/

tab a_023 /*years of education completed*/ 
tab a_024 /*level of education*/

tab region 
graph bar, over(region, sort(1) descending label(angle(45))) 

tab hhsize
graph bar, over(hhsize) // most (26.61%) households are a size of 4

sum hh_id
codebook hh_id
graph bar (count), over(hh_id)

sum hhid
codebook hhid
graph bar (count), over(hhid)
	/*multiple individuals were interviewed per household. there are 1499 unique 
	hhids. will create one observation per household*/

sum hh_inc_norem hh_inc_rem
*----------------------------------------------------------------HH_EXPENDITURES
sum b_043 b_044 b_045 b_046 b_047 b_048 b_049 b_050 b_051

hist b_043, percent /*food*/
hist b_044, percent /*utilities*/ 
hist b_045, percent /*communications*/
hist b_046, percent /*adult clothing*/
hist b_047, percent /*child clothing*/
hist b_048, percent /*entertainment*/
hist b_049, percent /*tobacco*/
hist b_050, percent /*alcohol*/
hist b_051, percent /*print media*/

*----------------------------------------------------------------------HH_ASSETS
tab b_057_1 /*bicycles*/ 
tab b_058_1 /*washing machine*/
tab b_059_1 /*fridge/freezer*/
tab b_060_1 /*radio*/
tab b_052_1 //farmland
hist b_052_1 if b_052_1!=0, percent 
*----------------------------------------------------------------------WELLBEING
tab f_045 /*smoking*/
tab f_046b /*wine*/
tab f_046c /*vodka*/ 
tab f_047 /*meals/day*/
tab f_058 /*life satisfaction*/
hist f_058 if f_058<11 /*make an average life satisfation per household var*/

sum BMI BMI_under BMI_over BMI_obese
hist BMI
tab1 BMI_under BMI_over BMI_obese

*----------------------------------------------------------------------MIGRATION
sum hh_remittances
hist hh_remittances if a_006==1	// most (87.39%) individuals did not receive remittances. 

tab migrant_perma /*Migrant was abroad for more than 9 months in a row since 1999*/

tab p_migexp_total /*personal migration experience in months since 1999*/
hist p_migexp_total if p_migexp_total>0, percent 

tab p_migexp_west /*personal migration experience to west in months since 1999*/
hist p_migexp_west if p_migexp_west>0, percent 

tab p_migexp_east
hist p_migexp_east if p_migexp_east>0, percent 

tab p_currmigr2011 p_currmigr1999
tab p_currmigr2011 p_currmigr1999 if a_006==1 
tab member_id
*/
*-------------------------------------------------------------------------------
*CLEANING & RESHAPING
*-------------------------------------------------------------------------------
egen hh_migrant_perma=total (migrant_perma), by(hhid)
tab hh_migrant_perma
cap drop hhm_mig_perm99
recode hh_migrant_perma (0=0) (1/5 = 1), gen(hhm_mig_perm99) // household had a migrant abroad for more than 9 consecutive months in 1999
label var hhm_mig_perm99 "Household has long-term migrant(s)"
tab hhm_mig_perm99

graph box hh_remittances, over(hhm_mig_perm99) // HHs without migrants rarely receive remittances

corr hh_remittances hhm_mig_perm99
*tw(scatter hh_remittances hhm_mig_perm99) (lfit hh_remittances hhm_mig_perm99)

egen total_exp=rowtotal(b_043 b_044 b_045 b_046 b_047 b_048 b_049 b_050 b_051)
sum total_exp

*creating controls--------------------------------------------------------------
gen adult=0
replace adult=1 if a_009a>17 & a_009a<60
egen nadults=total (adult), by (hhid)

gen child=0
replace child=1 if a_009a<18
egen nchild=total (child), by (hhid)

gen elderly=0
replace elderly=1 if a_009a>59 & a_009a<1000
egen nelderly=total (elderly), by (hhid)

tab1 nchild nadult elderly

br hhid nadult nchild nelderly

*------
gen agehead= a_009a if a_006==1 // generated the ages of the household heads
egen agehh=max (agehead), by(hhid)

gen malehead=1 if a_006==1 & a_007==1
replace malehead=0 if a_006==1 & a_007==2

bysort hhid: egen malehh=max (malehead)
tab malehh /* most household heads are male */

*------
recode a_024 (1 2=1 "up to primary") (3 4 5=2 "secondary") (6 7 8=3 "university"), gen (education)
recode education 9 777 888 999=.

egen maxedu=max (education), by (hhid)
tab maxedu, gen (maxedu)
graph bar, over(maxedu)

rename maxedu1 uptoprimary
rename maxedu2 secondary
rename maxedu3 university 

tab1 uptoprimary secondary university

*------
tab region, gen (region)

rename region1 Balti
rename region2 Centre
rename region3 Chisinau
rename region4 North
rename region5 South

gen land=0
replace land=1 if b_052_1>0 & b_052_1!=.
tab land

*------
recode hh_remittances (0=0 "HH does not receive remittances") (else = 1 "HH receives remittances"), gen(receive_remit )

tab receive_remit

gen exppc= hh_exp/hhsize
label var exppc "HH expenditures per capita"
sum exppc

*---- share of specific expenditures relative to total exp.
foreach var of varlist b_043 b_044 b_045 b_046 b_047 b_048 b_049 b_050 b_051 {
	gen `var'_rel = `var'/total_exp
}
sum b_043_rel b_044_rel b_045_rel b_046_rel b_047_rel b_048_rel b_049_rel b_050_rel b_051_rel
*food is, on average, the highest relative HH expenditure, followed by utilities

*------ life satisfaction per HH 
egen maxhhsatisfaction= max(f_058) if f_058<11, by(hhid)
br maxhhsatisfaction

*looking at a hh level: we keep one observation per household
keep if member_id==1
br /*1499 observations remain*/

*-------------------------------------------------------------------------------
*TESTING 
*-------------------------------------------------------------------------------

global controls "agehh malehh nchild nadult elderly Balti Chisinau North South uptoprimary university"

// What kind of households have a migrant?
* logistic regression	
logit hhm_mig_perm99 $controls, or 

// do receiving remittances and/or having a migrant from the HH affect patterns of HH expenditures? 
*FOOD EXPENDITURES--------------------------------------------------------------
*simple linear regression: 
reg  b_043_rel receive_remit if hhm_mig_perm99==1 , r
reg  b_043_rel receive_remit, r

reg  b_043_rel hh_remittances if hhm_mig_perm99==1 , r
reg  b_043_rel hh_remittances, r

*multiple linear regression: 
reg b_043_rel receive_remit $controls, r
reg  b_043_rel receive_remit $controls if hhm_mig_perm99==1 , r

reg b_043_rel hh_remittances $controls, r
reg  b_043_rel hh_remittances $controls if hhm_mig_perm99==1 , r
	//doesn't look like there's a significant effect on food expenditures

*UTILITIES EXPENDITURES---------------------------------------------------------
*simple linear regression: UTILITIES expenditures 
reg  b_044_rel receive_remit if hhm_mig_perm99==1 , r
reg  b_044_rel receive_remit, r

reg  b_044_rel hh_remittances if hhm_mig_perm99==1 , r
reg  b_044_rel hh_remittances, r

*multiple linear regression: UTILITIES expenditures 
reg b_044_rel receive_remit $controls, r
reg b_044_rel receive_remit $controls if hhm_mig_perm99==1 , r

reg b_044_rel hh_remittances $controls, r
reg b_044_rel hh_remittances $controls if hhm_mig_perm99==1 , r
