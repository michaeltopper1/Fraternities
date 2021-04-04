clear

import delimited "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/xMaster_data_2021/daily_panel.csv"
destring sexual_assault, replace force
destring alcohol_offense, replace force
destring alcohol_offense, replace force

gen temp = mdy(month,day,year)

egen temp_university = group(university)

//Poisson
ppmlhdfe sexual_assault  treatment , vce(cluster university) exposure(total_students_all) absorb(i.temp temp_university )

//fixed effects with month-by-day-by-year and university
reghdfe sexual_assault treatment, vce(cluster temp_university) absorb(temp temp_university)
