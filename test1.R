library(tidyverse)
library(here)
library(epiextractr)
library(haven)

cps_main <- load_org(2021:2022)

tester <- binipolate(cps_main, var = "wage", p = c(50, 75), group_vars = c("year", "wbho"), w = orgwgt/12, binsize = 0.25)


test2 <- binipolate(mtcars, var = "mpg", group_vars = c("cyl", "gear"), w = wt, binsize = 0.25)
