clear all
set more off

sysuse cpi_annual, clear
keep year cpiurs

tempfile cpiurs
save `cpiurs'


load_epiextracts, begin(2007m1) end(2022m12) sample(org) keep(year orgwgt emp selfemp selfinc wage wbhao cbsafips)
gen wgt = orgwgt/12

keep if emp == 1

drop if selfinc == 1
drop if selfemp == 1

drop if cbsafips == 0

merge m:1 year using `cpiurs'
drop if _merge == 2


/*use 2021 as base */
sum cpiurs if year == 2022
local cpi_base = `r(mean)'

gen realwage = wage * `cpi_base'/cpiurs

binipolate realwage [pw = wgt], binsize(0.25) p(50) by(year wbhao cbsafips)
rename realwage_binned p_binned
reshape wide p@_binned, i(year cbsafips) j(wbhao)

export delim test_katie_stata.csv, replace 




