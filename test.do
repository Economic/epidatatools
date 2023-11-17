clear all
set more off

sysuse cpi_annual, clear
keep year cpiurs

tempfile cpiurs
save `cpiurs'


load_epiextracts, begin(2021m1) end(2022m12) sample(org) 
gen wgt = orgwgt/12

tempfile cps_main
save `cps_main'

binipolate wage [pw = wgt], binsize(0.25) p(50 75) by(year wbho)

export excel using test_output_stata.xlsx, sheet("wbho_50_75") sheetreplace firstrow(variables)
