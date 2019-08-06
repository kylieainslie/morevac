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
out <- multiannual(n=100000, vac_coverage = vac_cov_dat$By_Group)

# get attack rates
ar_out <- get_attack_rates(inf_history = out$inf_history$inf_hist_mat,
                           ages_mat = out$ages, years = 1820:2019)
# plot total attack rates
p1 <- plot_attack_rates(dat = ar_out$attack_rates)
p1
# bin ages into groups
age_group_dat <- ar_out$ar_by_age
age_group_dat$Year <- as.numeric(as.character(age_group_dat$Year))
age_group_dat$Age <- as.numeric(as.character(age_group_dat$Age))
age_group_dat$Age_Group <- age_group_dat$Age
for (i in 1:dim(age_group_dat)[1]){
  if(age_group_dat$Age[i] >= 0 & age_group_dat$Age[i] < 2){age_group_dat$Age_Group[i] <- '<2'}
  else if (age_group_dat$Age[i] >= 2 & age_group_dat$Age[i] < 5){age_group_dat$Age_Group[i] <- '2-4'}
  else if (age_group_dat$Age[i] >= 5 & age_group_dat$Age[i] < 11){age_group_dat$Age_Group[i] <- '5-10'}
  else if (age_group_dat$Age[i] >= 11 & age_group_dat$Age[i] < 16){age_group_dat$Age_Group[i] <- '11-15'}
  else if (age_group_dat$Age[i] >= 16 & age_group_dat$Age[i] < 65){age_group_dat$Age_Group[i] <- '16-64'}
  else if (age_group_dat$Age[i] >= 65){age_group_dat$Age_Group[i] <- '65+'}
}
age_group_dat$Age_Group <- as.factor(age_group_dat$Age_Group)
age_group_dat$Age_Group <- factor(age_group_dat$Age_Group,levels(age_group_dat$Age_Group)[c(1,4,5,2,3,6)])
# plot attack rates by age group
p2 <- plot_attack_rates(dat = age_group_dat, by_age_group = TRUE)

# simulation - returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test <- run_sim_2()

# post process sim results
# overall attack rates
sim = 100
years <- 1820:2019
sim_ar <- matrix(numeric(length(years)*sim),nrow = length(years))

for (s in 1: sim){
  sim_ar[,s] <- get_attack_rates(sim_test$inf_history[,,s], years = years)$Attack_Rate
}
sim_ar <- cbind(years,sim_ar)
colnames(sim_ar) <- c("Year",paste0("Sim",1:sim))
# find mean and 95% CI of AR in each year (ARs within a given year are assumed Normally distributed)
# shapiro.test(sim_ar[1,-1]): returns p-value>0.05
sim_ar_dat <- data.frame(Year = sim_ar[,1],
                         Attack_Rate = apply(sim_ar[,-1],1,mean),
                         SD_AR = apply(sim_ar[,-1],1,sd))
sim_ar_dat$Lower <- sim_ar_dat$Attack_Rate - (qnorm(0.975)*sim_ar_dat$SD_AR/sqrt(sim))
sim_ar_dat$Upper <- sim_ar_dat$Attack_Rate + (qnorm(0.975)*sim_ar_dat$SD_AR/sqrt(sim))

# plot results
p_ar <- plot_attack_rates(dat = sim_ar_dat, c_bands = TRUE)
p_ar







