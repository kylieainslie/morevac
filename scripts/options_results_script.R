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
                                       rep(0.233,11),rep(0.105,49),rep(0.729,15)),
                          Total_Vac = c(rep(1,80))) # vaccination coverage is 100% for everyone
# run multi-annual model
out <- multiannual(n=100000, vac_coverage = vac_cov_dat$Total_Vac)

### Debugging ###
person <- data.frame(Year = years, Vac_History = vac_history[1,], Susceptibility = suscept_mat[1,],
                     Inf_History = inf_history[1,], Delta_X = out$inf_history$delta_x[1,])

#################
# get attack rates
ar_out <- get_attack_rates(inf_history = out$inf_history$inf_hist_mat,
                           ages_mat = out$ages, years = 1820:2019)
# plot total attack rates
p1 <- plot_attack_rates(dat = ar_out$attack_rates)

# bin ages into groups
age_group_dat <- ar_out$ar_by_age
age_group_dat$Age <- as.numeric(as.character(age_group_dat$Age))
age_group_dat$Age_Group <- age_group_dat$Age
for (i in 1:dim(ar_by_age)[1]){
  if(age_group_dat$Age[i] >= 0 & age_group_dat$Age[i] < 2){age_group_dat$Age_Group[i] <- '<2'}
  else if (age_group_dat$Age[i] >= 2 & age_group_dat$Age[i] < 5){age_group_dat$Age_Group[i] <- '2-4'}
  else if (age_group_dat$Age[i] >= 5 & age_group_dat$Age[i] < 11){age_group_dat$Age_Group[i] <- '5-10'}
  else if (age_group_dat$Age[i] >= 11 & age_group_dat$Age[i] < 16){age_group_dat$Age_Group[i] <- '11-15'}
  else if (age_group_dat$Age[i] >= 16 & age_group_dat$Age[i] < 65){age_group_dat$Age_Group[i] <- '16-64'}
  else {age_group_dat$Age_Group[i] <- '65+'}
}
# plot attack rates by age group
p2 <- plot_attack_rates(dat = age_group_dat, by_age_group = TRUE)
