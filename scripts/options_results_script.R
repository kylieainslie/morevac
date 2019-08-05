# minimal script for multiannual()

# load required packages
library(ggplot2)
library(tidyr)
library(reshape2)
library(cowplot)
library(stringr)

# load morevac package
# setwd("C:/Users/kainslie/Documents/GitHub/morevac")
  setwd("~/Documents/morevac")
  devtools::load_all()

# determine vaccination coverages by age
# vaccine coverage data from PHE Seasonal influenza vaccine uptake in GP patients: winter season 2018 to 2019
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/804889/Seasonal_influenza_vaccine_uptake_in_GP_patients_1819.pdf
vac_cov_dat <- data.frame(Age = 0:79,
                          Same_Vac = c(rep(0.24,80)), # vaccination coverage is 24% for everyone
                          By_Group = c(rep(0.005,2),rep(0.44,2),0.379,
                                       rep(0.233,11),rep(0.105,49),rep(0.729,15))))
# run multi-annual model
out <- multiannual(n=10000, vac_coverage = 1, stop_vac_age = 18)
# get attack rates
ar_out <- get_attack_rates(inf_history = out$inf_history$inf_hist_mat,
                           ages_mat = out$ages, years = 1820:2019)
# outputs
# plot attack rates
p1 <- plot_attack_rates(dat = ar_out$attack_rates)
# plot lifetime infections
inf_dat <- out$history[,,1]
lifetime_inf <- get_lifetime_inf(inf_history = inf_dat, ages = 1:80, maxage = 80)
