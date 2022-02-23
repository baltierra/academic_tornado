* version 16.0 of STATA
* PPHA 31202 Advanced Statistics for Data Analysis 1, Fall 2020
* Homework 7: Fabián A. Araneda Baltierra

* Housekeeping
clear 
capture log close
set more off
log using HW7-FAAB.log, replace


*load data*
use ppha312x2021
label data "Data is from IPUMS-USA  restricted to Albuquerque,New Mexico (2018)"

************************
* QUESTION 1: BOOSTRAP *
************************

*Data cleaning: Only consider incomes greate or equal than zero 
keep if inctot >= 0
keep if incwage >= 0

*Data cleaning: Remove data to keep only what's necessary to answer the question.
drop if race != 2
drop if sex != 2
drop if empstat != 1

*Create two variables with data from the dataset, to calculate correlation
gen x = age
gen y = incwage

*Calculate Booststrap
bootstrap r(rho), nodots nowarn seed(110821) reps(10000): correlate x y


*****************************************
* QUESTION 4: AMERICAN COMMUNITY SURVEY *
*****************************************
clear
use Homework_7

****** START PREPARING DATA ******
keep if inctot >= 0
keep if incwage >= 0

*We only keep individuals between 25 and 55, inclusive.
drop if age < 25 | age > 55

*Construct missing indicator for income
gen miss = qinctot == 4

*Following command is to analize labels for the "educd" variable
*codebook educd, tabulate(21)

*Here we replace values according problem's prompt and current information in "educd"
replace educ = 0 if (educd == 2 | educd == 11 | educd == 14 | educd == 17 | educd == 22 | educd == 23 | educd == 25 | educd == 26 | educd == 30 | educd == 40 | educd == 50 | educd == 61 | educd == 64)
replace educ = 1 if (educd == 63)
replace educ = 2 if (educd == 65 | educd == 71)
replace educ = 3 if (educd == 81)
replace educ = 4 if (educd == 101)
replace educ = 5 if (educd == 114 | educd == 115 | educd == 116)

*Next we redefine the labels
label define educlabel 0 "Less than High School" 1 "High School" 2 "Some College" 3 "Associate Degree" 4 "Bachelors Degree" 5 "More than Bachelors"
label values educ educlabel

*Now we repeat the same process for "race"
replace race = 0 if (race == 1)
replace race = 1 if (race == 2)
replace race = 2 if (race == 3 | race == 4 | race == 5 | race == 6 | race == 7 | race == 8 | race == 9)
label define racelabel 0 "White" 1 "African American" 2 "Other Race"
label values race racelabel
****** END PREPARING DATA ******

****** Question 4.A ******
*Creates a unique variable for each age and sex category
egen sexrace = group(sex race)
tab sexrace
tabstat incwage, by(sexrace) s(mean sd n)

****** Question 4.B ******
*Calculate income from wages and salaries by sex and race using the perwt weights
tabstat incwage [aw=perwt], by(sexrace) s(mean sd n)


*****************************************
* QUESTION 5: AMERICAN COMMUNITY SURVEY *
*****************************************
*Create new variable to store age catogories
gen age_category = 0

*Replace values according problem's prompt
replace age_category = 1 if (age >= 30 & age <= 34)
replace age_category = 2 if (age >= 35 & age <= 39)
replace age_category = 3 if (age >= 40 & age <= 44)
replace age_category = 4 if (age >= 45 & age <= 49)
replace age_category = 5 if (age >= 50 & age <= 55)

label define agelabel 0 "25-29" 1 "30-34" 2 "35-39" 3 "40-44" 4 "45-49" 5 "50-55"
label values age_category agelabel

*Now proceed with calculations related to weights

*First, we create a group variable
egen y = group(race age_category sex educ)

*Total people in each category
egen den = sum(perwt), by(y)

*People with non-missing data
egen num = sum((1-miss)*perwt), by(y)

*Probability of responding
gen phat = num/den

*Generate new weights called "wt". 
gen wt = perwt/phat if miss == 0

*Calculate income from wages and salaries by sex and race using the IPW weights
tabstat incwage [aw=wt], by(sexrace) s(mean sd n)

*And show again the calculation of income from wages and salaries
*by sex and race using the perwt weights, to compare
tabstat incwage [aw=perwt], by(sexrace) s(mean sd n)

* They are very similar, but the biggest difference can be found in the category "2".

log close
 

