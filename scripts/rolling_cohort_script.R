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
                          Annual_Constant_Vac = c(rep(0,2),rep(0.5,78)),           # vaccination coverage is 50% for everyone <= 2
                          Biannual_Constant_Vac = c(rep(0,2),rep(c(0.5,0),39)),
                          Annual_Total_Vac = c(rep(0,2),rep(1,78)),               # vaccination coverage is 100% for everyone <= 2
                          Biannual_Total_Vac = c(rep(0,2),rep(c(1,0),39))
                          )

# run multi-annual model
out <- multiannual(n=10000, vac_coverage = vac_cov_dat$Biannual_Constant_Vac, vac_strategy = 2)

# get attack rates
ar_out <- get_attack_rates(inf_history = out$inf_history$inf_hist_mat, ages_mat = out$ages, years = 1820:2019)
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

# initialize objects
 cohort = list(); cohort_inf_hist = list(); cohort_vac_hist = list(); cohort_ages = list(); ar_cohort = list()
 ar <- data.frame(Year = years[desired_years], Attack_Rate = c(rep(0,length(desired_years))))
 for (j in 1:length(desired_years)){
  cohort[[j]] <- which(out$ages[,desired_years[j]] <= current_age)
  cohort_inf_hist[[j]] <- out$inf_history$inf_hist_mat[cohort[[j]],desired_years[j]]
  cohort_vac_hist[[j]] <- out$vac_history$vac_hist_mat[cohort[[j]],desired_years[j]]
  cohort_ages[[j]] <- out$ages[cohort[[j]],desired_years[j]]
  # get attack rate
  ar[j,2] <- sum(cohort_inf_hist[[j]])/length(cohort[[j]])
  # update current age
  current_age <- current_age + 1
 }

# plot attack rates
p_cohort <- plot_attack_rates(dat = ar)
p_cohort
