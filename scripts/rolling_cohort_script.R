# rolling cohort script

# load required packages
library(ggplot2)
library(tidyr)
library(reshape2)
library(cowplot)
library(stringr)
library(foreach)
library(doParallel)
library(dplyr)
# load morevac package
setwd("~/Documents/morevac")
devtools::load_all()
library(morevac)
# determine vaccination coverages by age
vac_cov_dat <- data.frame(Age = 0:79,
                          No_Vaccination = c(rep(0,80)),
                          Annual_Constant_Vac = c(rep(0,2),rep(0.5,78)),           # vaccination coverage is 50% for everyone <= 2
                          Biannual_Constant_Vac = c(rep(0,2),rep(c(0.5,0),39)),
                          Annual_Total_Vac = c(rep(0,2),rep(1,78)),               # vaccination coverage is 100% for everyone <= 2
                          Biannual_Total_Vac = c(rep(0,2),rep(c(1,0),39)),
                          Annual_Off_At_10 = c(rep(0,2),rep(0.75,8),rep(0,8),rep(0.5,62)),
                          Biannual_Off_At_10 = c(rep(0,2),rep(c(0.75,0),4),rep(0,8),rep(c(0.5,0),31)),
                          Annual_Off_At_16 = c(rep(0,2),rep(0.75,14),rep(0,2),rep(0.5,62)),
                          Biannual_Off_At_16 = c(rep(0,2),rep(c(0.75,0),7),rep(0,2),rep(c(0.5,0),31))
                          )

# run multi-annual model
out <- multiannual(n=10000, years = 1820:2028, betas = c(0.4, rep(0.2,208)), vac_coverage = vac_cov_dat$Biannual_Constant_Vac, vac_strategy = 2)

# get attack rates
ar_out <- get_attack_rates(inf_history = out$inf_history$inf_hist_mat, ages_mat = out$ages, years = 1820:2028)
# plot total attack rates
p_out <- plot_attack_rates(dat = ar_out$attack_rates)
p_out

# isolate birth cohorts starting in 2000 (years 181:200)
years = 1820:2019
desired_years <- 181:200
# current_age <- 0

#cohort <- which(out$ages[,200] <= 19)
#cohort_inf_hist <- out$inf_history$inf_hist_mat[cohort,desired_years]
#cohort_vac_hist <- out$vac_history$vac_hist_mat[cohort,desired_years]
#cohort_ages <- out$ages[cohort,desired_years]

cohort_ar <- get_cohort_ar(inf_history = out$inf_history$inf_hist_mat, vac_history = out$vac_history$vac_hist_mat, ages = out$ages,
                           years = 1820:2019, year_index = 181:200)
# plot attack rates
p_cohort <- plot_attack_rates(dat = cohort_ar)
p_cohort

### simulation
## single run
# returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test0 <- run_sim_2(sim = 100, wane = 0.5, take = 0.7, vac_cov = vac_cov_dat$Annual_Off_At_16, vac_strategy = 0)
sim_test1 <- run_sim_2(sim = 100, wane = 0.5, take = 0.7, vac_cov = vac_cov_dat$Annual_Off_At_16, vac_strategy = 1)
sim_test2 <- run_sim_2(sim = 100, wane = 0.5, take = 0.7, vac_cov = vac_cov_dat$Biannual_Off_At_16, vac_strategy = 2)

# post process sim results
sim_results <- postprocess_sim_results_for_rolling_cohort(sim0 = sim_test0, sim1 = sim_test1, sim2 = sim_test2)
# plot results
p_test_ar <- plot_attack_rates(dat = sim_results, by_vac_strategy = TRUE, c_bands = TRUE)
p_test_ar
