# R&D on wages of whiteness book findings

# 2 sets of numbers for 100 largest MSAs, $inflation adjusted, 2007-2019
## 1. Wage ratios:
## 2. White median wage levels


library(tidyverse)
library(here)
library(epiextractr)
library(haven)
library(realtalk)


#Prepare inflation adjustment and load CPS data
cpi_base <- cpi_u_rs_annual$cpi_u_rs[cpi_u_rs_annual$year == 2022]

cps <- load_org(2007:2019, year, orgwgt, emp, selfemp, selfinc, wage, wbhao, cbsafips) %>%
  #filter(selfinc!=1 | is.na(selfinc), selfemp!=1, emp==1) %>%
  filter(selfinc!=1, selfemp!=1, emp==1) %>%
  mutate(wgt = orgwgt/12) %>%
  # filter cbsafips = 0
  filter(cbsafips != 0) %>%
  # merge CPI-U-RS data
  left_join(cpi_u_rs_annual, by = 'year') %>%
  # inflation adjust wages to 2022$
  mutate(realwage = wage*(cpi_base/cpi_u_rs)) %>%
  # flatten out wbhao label by making it into factor (number and label) and then into character value (just)
  mutate(wbhao = as.character(as_factor(wbhao)))


#### WAGE CALCULATIONS ####
calcs <- cps %>%
  # fitler out rows for wbhao that we do not need
  filter(wbhao %in% c("White","Black")) %>%
  # group_by in order to cluster data as you want, does not perform any calculations
  binipolate(var = "realwage", p = 50, group_vars = c("year", "wbhao", "cbsafips"), w = wgt, binsize = 0.25) %>%
  # reshape to make output cleaner
  pivot_wider(id_cols = c(year, cbsafips), values_from = value, names_from = c(wbhao, p), names_sep = "_") %>%
  # select columns to keep in the order that I want
  select(cbsafips, year, White = White_50, Black = Black_50) %>%
  # generate W-B wage ratio
  mutate(wageratio = Black/White) %>%
  write_csv(here("test_katie_r.csv"))






