#source(here("wages_dataset.R"), echo = TRUE)

#load libraries
library(realtalk)
library(epiextractr)
library(tidyverse)
library(here)
library(epidatatools)
library(MetricsWeighted)

#### DATA IMPORTATION ####

wage_years <- 2020:2022

# Load data for the "may" period (pre-1978)

# Load Outgoing Rotation Group (ORG) data
org <- load_org(wage_years)


# standard restrictions
data <- org  %>%
  # Age and selfemp restrictions
  filter(selfemp == 0, age >= 16, !is.na(wage),
         case_when(selfinc == 0 & !is.na(selfinc) ~ TRUE, # filter selfinc == 0 for year >= 1989
                   # keep any year that doesn't have selfinc (selfinc is NA)
                   is.na(selfinc) ~ TRUE,
                   # exclude all other cases
                   TRUE ~ FALSE))

#### MASTER WAGE DATASET ####
# define inflation base year
#note: use annual cpi-u-rs from RealTalk
cpi_base <- cpi_u_rs_extended_annual$cpi_u_rs_extended[cpi_u_rs_extended_annual$year == max(wage_years)]

# master wage dataset
wage_master <- data %>%
  # merge CPI-U-RS
  left_join(cpi_u_rs_extended_annual, by = "year") %>%
  # inflation adjust wages to current years dollars
  mutate(realwage = wage * (cpi_base/cpi_u_rs_extended)) %>%
  # flatten labels and adjust weight variables
  mutate(across(female | wbho | educ, ~ as.character(as_factor(.x))),
         # annual weight adjustment
         wgt = case_when(
           # may data wgt ~ finalwgt
           year < 1979 ~ finalwgt,
           year >= 1979 ~ orgwgt/12),
         # helpful indicator to group by all simultaneously
         all = "all")



percentiles <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95)

deciles_all <- binipolate(wage_master, var = "wage", p = percentiles, group_vars = c("year"), w = orgwgt/12, binsize = 0.25) %>%
  left_join(cpi_u_rs_extended_annual, by = "year") %>%
  mutate(realwage = value * (cpi_base/cpi_u_rs_extended))



deciles_f <- wage_master %>% group_by(year, female) %>%
  reframe(across(realwage, ~ quantile(.x, probs = percentiles/100, weights = orgwgt))) %>%
  data.frame(percentile = percentiles, .)

deciles_wbho <- wage_master %>% group_by(year, wbho) %>%
  reframe(across(realwage, ~ quantile(.x, probs = percentiles/100, weights = orgwgt))) %>%
  data.frame(percentile = percentiles, .)

deciles_f_wbho <- wage_master %>% group_by(year, female, wbho) %>%
  reframe(across(realwage, ~ quantile(.x, probs = percentiles/100, weights = orgwgt))) %>%
  data.frame(percentile = percentiles, .)

#reshape
wage_deciles <- bind_rows(deciles_all, deciles_f, deciles_wbho, deciles_f_wbho) %>%
  mutate(female = case_when(is.na(female) ~ "",
                            female == "Female" ~ "_g_female",
                            female == "Male" ~ "_g_male"),
         wbho = case_when(is.na(wbho) ~ "",
                          wbho == "White" ~ "_r_white",
                          wbho == "Black" ~ "_r_black",
                          wbho == "Hispanic" ~ "_r_hisp",
                          wbho == "Other" ~ "_r_other"),
         percentile = case_when(is.na(percentile) ~ "",
                                percentile == "10" ~ "_p_10",
                                percentile == "20" ~ "_p_20",
                                percentile == "30" ~ "_p_30",
                                percentile == "40" ~ "_p_40",
                                percentile == "50" ~ "_p_50",
                                percentile == "60" ~ "_p_60",
                                percentile == "70" ~ "_p_70",
                                percentile == "80" ~ "_p_80",
                                percentile == "90" ~ "_p_90",
                                percentile == "95" ~ "_p_95"),
         `_r_s_p` = ifelse(female == "wage_g_all",
                           paste0(wbho, percentile),
                           paste0(female, wbho, percentile)),
         `_r_s_p` = ifelse(`_r_s_p` == "", "wage_all", paste0("wage", `_r_s_p`))) %>%
  filter(wbho != "_r_other") %>%
  select(-c("female", "wbho", "percentile")) %>%
  pivot_wider(id_cols = year, names_from = `_r_s_p`, values_from = realwage)
