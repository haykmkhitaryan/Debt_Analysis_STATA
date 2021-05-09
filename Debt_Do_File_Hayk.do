

sum 


describe
**Creating Time series environment**
gen date = tq(2000q1) + _n-1
format %tq date
list date in 1/4
tsset date


*Seasonality 
egen quarter = seq(), to(4)
tab quarter, gen(q)
reg real_gdp_arm q2-q4
test q2=q3=q4=0
drop q1-q4

/* Seasonality component rejected at any conventional signficance level, 
after transforming nominal to real GDP based on CPI*/


gen arm_growth = (real_gdp_arm/l4.real_gdp_arm -1)*100
gen rus_growth = (real_gdp_rus/l4.real_gdp_rus -1)*100
gen debt_growth = (cum_debt/l1.cum_debt-1)*100

twoway (tsline real_gdp_arm, yaxis(1)) (tsline cum_debt, yaxis(2))

** Checking stationarity

dfuller arm_growth
dfuller arm_growth, trend
dfuller arm_growth, drift

*We reject existence of random walk for real GDP growth of Armenia, thus series do NOT contain unit root.

dfuller debt_growth
dfuller debt_growth, trend
dfuller debt_growth, drift

*We reject existence of first order unit root for debt growth, thus series are stationary**

*
dfuller rus_growth
**Fail to reject P-Value = 0.16

dfuller rus_growth, trend
**Fail to reject P-Value = 0.25
dfuller rus_growth, drift
hist rus_growth

pperron rus_growth
pperron rus_growth, trend
**Reject at 90% percent significance level
ac rus_growth
 

/*Dickey-Fuller test fails to reject unit root, however Philips Perron 
test implies statinarity of the series at 90% significance level*/
tsline rus_growth
twoway (tsline rus_growth if date <= tq(2007q3)| date >= tq(2009q4))
dfuller rus_growth if (date <= tq(2007q3)| date >= tq(2009q4))
gen drus_growth = d.rus_growt
dfuller drus_growth


** When we exclude period of reccesion, the variable is long-term stationary**

 
gen crisis = 1 if (date > tq(2007q3) & date < tq(2009q4))
replace crisis = 0 if (date <= tq(2007q3)| date >= tq(2009q4))



*Comparing 
twoway (tsline arm_growth, yaxis(1)) (tsline rus_growth, yaxis (2))


twoway (tsline arm_growth if date >= tq(2007q1)&date <= tq(2010q1)) (tsline debt_growth if date >= tq(2007q1)&date <= tq(2010q1)) 
twoway (tsline arm_growth if date >= tq(2014q3)&date <= tq(2016q3)) (tsline debt_growth if date >= tq(2014q3)&date <= tq(2016q3)) 




** Estimating the models



*Loop for ARDL model

 * empty matrix for Schwarts written 
matrix AIC = J(8,8,0)
matrix SIC = J(8,8,0)
 set more off
forvalues j = 1(1)8{
forvalues i = 1(1)8{
ardl arm_growth debt_growth rus_growth, lag(`j',`i,',.)
estat ic
matrix A  = r(S)
matrix AIC[`j',`i'] = A[1,5]  
matrix SIC [`j',`i'] = A[1,6]
}
}
matrix list AIC
matrix list SIC
*Best AIC&SIC at r8c3

**********Model 1
 ardl arm_growth debt_growth rus_growth, lag(8,3,.)
  
predict res1, res
swilk res1 /*Fail to reject at 90 */
sfrancia res1 /*reject at 90 */
hist res1
** Normality assumption does not hold
ac res1
corrgram res1
** No autocorrelation errors


/*Given the fact that we want to have at least 8 lags of debt to observe long-term efftct
variable we use the following matrix*/

*Loop for 8th lag

*empty matrix for Schwarts written 
 matrix AIC = J(8,1,0)
 matrix SIC = J(8,1,0)
 set more off
forvalue j = 1(1)8{
ardl arm_growth debt_growth rus_growth, lag(`j',8,.)
estat ic
matrix A  = r(S)
matrix AIC[`j',1] = A[1,5]  
matrix SIC [`j',1] = A[1,6]
}
matrix list AIC 
matrix list SIC

*Best AIC&SIC at r8


*****************Model 2
ardl arm_growth debt_growth rus_growth, lag(8,8,.)

predict res2, res
swilk res2 /*Fail to reject */
sfrancia res2 /*Fail to reject */
hist res2 
* Normality assumption not rejected at any conventional level of significance

ac res2
* No serial autocorrelation
pac res2
*There is no partial autocorrelation of errors in the model
corrgram res2


*Experimenting with 12th lag, (arbitary choice)
*****************Model 3
ardl arm_growth debt_growth rus_growth, lag(.,12,.)
estat ic
predict res3, res
swilk res3 /*Fail to reject */ 
sfrancia res3 /*Fail to reject */
hist res3


* Normality not rejected at any conventional level of significance

ac res3
pac res3
corrgram res3
** There is no autocorrealtion of errors.



*Model 4

 matrix AIC = J(8,1,0)
 matrix SIC = J(8,1,0)
 set more off
forvalue j = 1(1)8{
ardl arm_growth debt_growth rus_growth crisis, lag(`j',12,.,.)
estat ic
matrix A  = r(S)
matrix AIC[`j',1] = A[1,5]  
matrix SIC [`j',1] = A[1,6]
}
matrix list AIC 
matrix list SIC

*r4

ardl arm_growth debt_growth rus_growth crisis, lag(4,12,.,.)
predict res4, res
swilk res4
sfrancia res4 
hist res4
*Nomrality not rejected
ac res4
pac res4
*No serial auto correlation


test l4.debt_growth l5.debt_growth l6.debt_growth l7.debt_growth l8.debt_growth l9.debt_growth l10.debt_growth l11.debt_growth l12.debt_growth
*Failed to reject joint insignifiacne of long-term lags of debt_growth with p-value 0.394


*********** OUR FINAL MOST IMPORTANT MODEL **************

 *empty matrix for Akaike written
matrix AIC = J(3,3,0)
 * empty matrix for Schwarts written 
 matrix SIC = J(3,3,0)
 set more off
 forvalues i= 1(1)3 {
forvalues j = 1(1)2 {
arima arm_growth l(1/3 8).arm_growth l(0/3 8 12).debt_growth l(0).rus_growth crisis, ar(1/`i' 8) ma(1/`j') 
arimafit
matrix AIC[`i',`j'] = r(aic)  
matrix SIC [`i',`j'] = r(sic)
}
}
matrix list AIC
matrix list SIC

*r1c1
 
*MODEL 5, FINAL*
arima arm_growth l(1/3 8).arm_growth l(0/3 8 12).debt_growth l(0).rus_growth crisis, ar(1/1 8) ma(1/1) 
arimafit
armaroots
*Based on the results, we can say that the model is stable, for all the components are at inside.

predict res5, res
corrgram res5
ac res5
** No serial autocorellations
pac res5
** No serial partial autocorellations
swilk res5
sfrancia res5
** Normality not rejected
hist res5
test  l8.debt_growth l12.debt_growth
** P-value 0.597, jointly insignificant
