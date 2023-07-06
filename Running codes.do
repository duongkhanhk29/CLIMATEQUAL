
* This code for the published paper: DOI:10.1007/s10644-022-09462-9
* The project materials: https://github.com/duongkhanhk29/CLIMATEQUAL

********** DATA CLEANING 

* To clean data
encode Country, generate(Country_ID) /// to code country names
xtset Country_ID Time, yearly /// to declare panel data
encode Income_group, generate(Income_ID) /// to code income groups
gen Poverty190_320 = Poverty320 - Poverty190 /// to create poverty bands
gen Poverty550_320 = Poverty550 - Poverty320 /// to create poverty bands
gen log_Poverty550 = log(Poverty550) /// to tranform in log
gen log_Agriculture_employment = log(Agriculture_employment)
gen log_Poverty190_320 = log(Poverty190_320)
gen log_Poverty320_550 = log(Poverty320_550)
gen log_Poverty190 = log(Poverty190)
gen log_GDP_capita = log(GDP_capita)
gen log_Emissions = log(Emissions)
gen log_Inequality = log(Inequality)
gen log_Population = log(Population)
gen log_GDP_capita2 = log_GDP_capita ^ 2
gen log_Urbanization = log(Urbanization)
gen log_Renewable_consumption = log(Renewable_consumption)
gen low_income = Income_ID >1 /// to dummy the low-income group
gen high_income = Income_ID ==1 /// to dummy the low-income group

* to create two-way interaction
gen GDP_emissions = log_Emissions * log_GDP_capita
gen GDP_inequality = log_Inequality * log_GDP_capita
gen Emissions_inequality = log_Inequality * log_Emissions
gen Poverty190_emissions = log_Poverty190 * log_Emissions
gen Poverty550_emissions = log_Poverty550 * log_Emissions


********** DATA ANALYSIS

* Research method: Arellano–Bover/Blundell–Bond linear dynamic panel-data estimation
* predetermined variables: GDP_capitar
* endogenous variables: Emissions, Inequality and their products
* exogenous variables: Agriculture, Renewable, Urbanization, Population

* to run Descriptive analysis
estpost summarize log_Poverty550 log_GDP_capita log_Emissions log_Inequality ///
		  log_Renewable_consumption log_Urbanization log_Population
		  
* to run two-sample mean-comparison tests (ttest)
qui: estpost tabstat log_Poverty550 log_GDP_capita log_Emissions log_Inequality, ///
               by(high_income) statistics(mean sd) columns(statistics) listwise
esttab using "doc.rtf", main(mean) aux(sd) nostar unstack noobs nonote nomtitle nonumber
qui: estpost ttest log_Poverty550 log_GDP_capita log_Emissions log_Inequality ///
		  , by(high_income)
esttab using "doc.rtf", nonumber mtitle("diff.") noobs replace

* Plotting the relationships among variables
twoway (scatter log_Poverty550 log_GDP_capita || lowess log_Poverty550 log_GDP_capita)
twoway (scatter log_Poverty550 log_Emissions || lowess log_Poverty550 log_Emissions)
twoway (scatter log_Poverty550 log_Inequality || lowess log_Poverty550 log_Inequality)
twoway (scatter log_Emissions log_GDP_capita || lowess log_Emissions log_GDP_capita)
twoway (scatter log_Inequality log_GDP_capita || lowess log_Inequality log_GDP_capita)

* Plotting the relationships by income groups
twoway (scatter log_Poverty550 log_GDP_capita || lowess log_Poverty550 log_GDP_capita), ///
by(high_income)
twoway (scatter log_Poverty550 log_Emissions || lowess log_Poverty550 log_Emissions), ///
by(high_income)
twoway (scatter log_Poverty550 log_Inequality || lowess log_Poverty550 log_Inequality), ///
by(high_income)

* Plotting the relationships within countries
twoway (scatter log_Poverty190 log_GDP_capita || lowess log_Poverty190 log_GDP_capita)
twoway (scatter log_Poverty190_320 log_GDP_capita || lowess log_Poverty190_320 log_GDP_capita)
twoway (scatter log_Poverty320_550 log_GDP_capita || lowess log_Poverty320_550 log_GDP_capita)
twoway (scatter log_Poverty190 log_Emissions || lowess log_Poverty190 log_Emissions)
twoway (scatter log_Poverty190_320 log_Emissions || lowess log_Poverty190_320 log_Emissions)
twoway (scatter log_Poverty320_550 log_Emissions || lowess log_Poverty320_550 log_Emissions)

* Original impacts
eststo O1: xtdpdsys log_Poverty550 ///
         log_Renewable_consumption log_Urbanization log_Population, ///
		 pre(log_GDP_capita) ///
		 end(log_Emissions log_Inequality) ///
		 vce(robust)
		 estadd scalar parm2 = 2*normal(-abs(e(arm2)))
* Mediation effects
eststo M1: xtdpdsys log_Poverty550 ///
         log_Renewable_consumption log_Urbanization log_Population, ///
		 pre(log_GDP_capita) ///
		 end(GDP_emissions GDP_inequality) ///
		 vce(robust)
		 estadd scalar parm2 = 2*normal(-abs(e(arm2)))		 
* With square of income	 
eststo S1: xtdpdsys log_Poverty550 ///
         log_Renewable_consumption log_Urbanization log_Population, ///
		 pre(log_GDP_capita log_GDP_capita2) ///
		 end(GDP_emissions GDP_inequality) ///
		 vce(robust)
		 estadd scalar parm2 = 2*normal(-abs(e(arm2)))
* Combining Emissions and Inequality
eststo C1: xtdpdsys log_Poverty550 ///
         log_Renewable_consumption log_Urbanization log_Population, ///
		 pre(log_GDP_capita) ///
		 end(GDP_emissions GDP_inequality Emissions_inequality) ///
		 vce(robust)
		 estadd scalar parm2 = 2*normal(-abs(e(arm2)))

* to export the results
esttab O1 M1 S1 C1 using mydoc1.rtf, se label replace ///
       mtitles star( * 0.10 ** 0.05 *** 0.010) ///
	   drop (log_Renewable_consumption log_Urbanization log_Population) ///
	   order (log_GDP_capita log_GDP_capita2 log_Emissions ///
	   log_Inequality GDP_emissions GDP_inequality Emissions_inequality) ///
	   scalar(N "parm2 AR2 test (p-value)")
		 
* Difference between low-income and high-income
eststo High_income: xtdpdsys log_Poverty550 ///
         log_Renewable_consumption log_Urbanization log_Population ///
		 if high_income == 1, ///
		 pre(log_GDP_capita) ///
		 end(GDP_emissions GDP_inequality Emissions_inequality) ///
		 vce(robust)
		 estadd scalar parm2 = 2*normal(-abs(e(arm2)))
eststo Low_income: xtdpdsys log_Poverty550 ///
         log_Renewable_consumption log_Urbanization log_Population ///
		 if low_income == 1, ///
		 pre(log_GDP_capita) ///
		 end(GDP_emissions GDP_inequality Emissions_inequality) ///
		 vce(robust)
		 estadd scalar parm2 = 2*normal(-abs(e(arm2)))
		 
esttab High_income Low_income using mydoc2.rtf, se label replace ///
       mtitles star( * 0.10 ** 0.05 *** 0.010) ///
	   drop (log_Renewable_consumption log_Urbanization log_Population) ///
	   order (log_GDP_capita ///
	   GDP_emissions GDP_inequality Emissions_inequality) ///
	   scalar(N "parm2 AR2 test (p-value)")		 
		 
* Difference among bands of poverty
eststo P190: xtdpdsys log_Poverty190 ///
         log_Renewable_consumption log_Urbanization log_Population, ///
		 pre(log_GDP_capita) ///
		 end(GDP_emissions GDP_inequality Emissions_inequality) ///
		 vce(robust)
		 estadd scalar parm2 = 2*normal(-abs(e(arm2)))
eststo P190_320: xtdpdsys log_Poverty190_320 ///
         log_Renewable_consumption log_Urbanization log_Population, ///
		 pre(log_GDP_capita) ///
		 end(GDP_emissions GDP_inequality Emissions_inequality) ///
		 vce(robust)
		 estadd scalar parm2 = 2*normal(-abs(e(arm2)))
eststo P320_550: xtdpdsys log_Poverty320_550 ///
         log_Renewable_consumption log_Urbanization log_Population, ///
		 pre(log_GDP_capita) ///
		 end(GDP_emissions GDP_inequality Emissions_inequality)	/// 
		 vce(robust)
		 estadd scalar parm2 = 2*normal(-abs(e(arm2)))

esttab P190 P190_320 P320_550 using mydoc3.rtf, se label replace ///
       mtitles ("$0.0-1.90" "$1.90-3.20" "$3.20-5.50") ///
	   star( * 0.10 ** 0.05 *** 0.010) ///
	   drop (log_Renewable_consumption log_Urbanization log_Population) ///
	   order (log_GDP_capita ///
	   GDP_emissions GDP_inequality Emissions_inequality) ///
	   scalar(N "parm2 AR2 test (p-value)")
		 


