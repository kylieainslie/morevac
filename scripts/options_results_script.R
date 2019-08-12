# minimal script for multiannual()

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
# vaccine coverage data from PHE Seasonal influenza vaccine uptake in GP patients: winter season 2018 to 2019
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/804889/Seasonal_influenza_vaccine_uptake_in_GP_patients_1819.pdf
vac_cov_dat <- data.frame(Age = 0:79,
                          Same_Vac = c(rep(0.24,80)), # vaccination coverage is 24% for everyone
                          By_Group = c(rep(0.005,2),rep(0.44,2),0.379,
                                       rep(0.233,11),rep(0.105,49),rep(0.729,15)),
                          Off_At_10 = c(rep(0.005,2),rep(0.44,2),0.379,
                                        rep(0.233,6),rep(0,7),rep(0.105,47),rep(0.729,15)),
                          Off_At_16 = c(rep(0.005,2),rep(0.44,2),0.379,
                                        rep(0.233,11),rep(0,2),rep(0.105,47),rep(0.729,15)),
                          Fifty_Off_At_10 = c(rep(0,2),rep(0.5,9),rep(0,8),rep(0.5,61)),
                          Fifty_Off_At_10 = c(rep(0,2),rep(0.5,15),rep(0,2),rep(0.5,61)),
                          Total_Vac = c(rep(1,80))) # vaccination coverage is 100% for everyone
# run multi-annual model
out <- multiannual(n=100000, vac_coverage = vac_cov_dat$Off_At_10, vac_strategy = 2)
# get attack rates
ar_out <- get_attack_rates(inf_history = out$inf_history$inf_hist_mat,
                           ages_mat = out$ages, years = 1820:2019)
# plot total attack rates
p_out <- plot_attack_rates(dat = ar_out$attack_rates)
p_out

# isolate birth cohort in 2000
birth_cohort <- which(out$ages[,181]==0)
bc_inf_hist <- out$inf_history$inf_hist_mat[birth_cohort,181:200]
bc_vac_hist <- out$vac_history$vac_hist_mat[birth_cohort,181:200]
bc_ages <- out$ages[birth_cohort,181:200]
# get attack rates for birth cohort
ar_bc <- get_attack_rates(inf_history = bc_inf_hist, ages_mat = bc_ages, years = 2000:2019)
# plot attack rates for birth cohort
p_bc <- plot_attack_rates(dat = ar_bc$attack_rates)
p_bc

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

### simulation
## single run
# returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test0 <- run_sim_2(wane = 0.5, take = 0.25, vac_cov = vac_cov_dat$Off_At_10, vac_strategy = 0)
sim_test1 <- run_sim_2(wane = 0.5, take = 0.25, vac_cov = vac_cov_dat$Off_At_10, vac_strategy = 1)
sim_test2 <- run_sim_2(wane = 0.5, take = 0.25, vac_cov = vac_cov_dat$Off_At_10, vac_strategy = 2)

# post process sim results
# overall attack rates
sim = 100
years <- 2000:2019
nyears <- length(years)
sim_test_ar0 <- matrix(numeric(length(years)*sim),nrow = length(years))
sim_test_ar1 <- sim_test_ar0; sim_test_ar2 <- sim_test_ar0

for (s in 1: sim){
  bc0 <- which(sim_test0$ages[,181,s]==0)
  bc_inf_hist0 <- sim_test0$inf_history[bc0,181:200,s]
  sim_test_ar0[,s] <- get_attack_rates(bc_inf_hist0, years = years)$Attack_Rate

  bc1 <- which(sim_test1$ages[,181,s]==0)
  bc_inf_hist1 <- sim_test1$inf_history[bc1,181:200,s]
  sim_test_ar1[,s] <- get_attack_rates(bc_inf_hist1, years = years)$Attack_Rate

  bc2 <- which(sim_test2$ages[,181,s]==0)
  bc_inf_hist2 <- sim_test2$inf_history[bc2,181:200,s]
  sim_test_ar2[,s] <- get_attack_rates(bc_inf_hist2, years = years)$Attack_Rate
}
sim_test_ar <- rbind(sim_test_ar0,sim_test_ar1,sim_test_ar2)
# find mean and 95% CI of AR in each year (ARs within a given year are assumed Normally distributed)
# shapiro.test(sim_ar[1,-1]): returns p-value>0.05
vac_strategy <- c(rep("No Vaccination",nyears),rep("Annual",nyears),rep("Every Other Year",nyears))
years_x3 <- c(rep(years,3))

sim_test_ar_dat <- data.frame(Year = years_x3,
                              Vac_Strategy = vac_strategy,
                              Attack_Rate = apply(sim_test_ar,1,mean),
                              SD_AR = apply(sim_test_ar,1,sd))
sim_test_ar_dat$Lower <- sim_test_ar_dat$Attack_Rate - (qnorm(0.975)*sim_test_ar_dat$SD_AR/sqrt(sim))
sim_test_ar_dat$Upper <- sim_test_ar_dat$Attack_Rate + (qnorm(0.975)*sim_test_ar_dat$SD_AR/sqrt(sim))

# plot results
p_test_ar <- ggplot(data = sim_test_ar_dat, aes(x = Year, y = Attack_Rate, colour= Vac_Strategy)) +
             geom_line() +
             geom_ribbon(aes(x=Year,ymin=Lower,ymax=Upper,linetype=NA,fill=Vac_Strategy),alpha=0.2)+
             xlab('Year') +
             ylab('Attack Rate') +
             scale_y_continuous(limits = c(0,0.3), expand = c(0,0)) +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = c(0.95, 0.95),
                   legend.justification = c("right", "top"),
                   legend.box.just = "right",
                   legend.margin = margin(6, 6, 6, 6),
                   legend.key = element_rect(fill = "white")
                   )
p_test_ar

## multiple run - foreach causes error
# make cluster
#ncl <- detectCores()
cl <- makeCluster(3)
registerDoParallel(cl)
# input parameters
sim <- 100; n <- 10000; vc <- vac_cov_dat$Off_At_10
v <- 2; r <- 0.9; w <- 0.5; take <- 0.75; vs <- c(0,1,2)
# parallel all three vac strategies
sim_out <- foreach (i=1:3, .packages = c('morevac','Rcpp')) %dopar%
  run_sim_2(sim = sim,n = n,vac_cov = vc, suscept_version = v,
            rho = r, wane = w, take = take, vac_strategy = vs[i])
# stop cluster
stopCluster(cl)

# post process sim results
# overall attack rates
years <- 2000:2019
year_index <- 181:200
nyears <- length(years)
sim_ar0 <- matrix(numeric(length(years)*sim),nrow = length(years));
sim_ar1 <- sim_ar0; sim_ar2 <- sim_ar0

for (s in 1:sim){
  # subset birth cohort
  bc0 <- which(sim_out[[1]]$ages[,year_index[1],s]==0)
  bc_inf_hist0 <- sim_out[[1]]$inf_history[bc0,year_index,s]
  sim_ar0[,s] <- get_attack_rates(bc_inf_hist0, years = years)$Attack_Rate

  bc1 <- which(sim_out[[2]]$ages[,year_index[1],s]==0)
  bc_inf_hist1 <- sim_out[[2]]$inf_history[bc1,year_index,s]
  sim_ar1[,s] <- get_attack_rates(bc_inf_hist1, years = years)$Attack_Rate

  bc2 <- which(sim_out[[3]]$ages[,year_index[1],s]==0)
  bc_inf_hist2 <- sim_out[[2]]$inf_history[bc2,year_index,s]
  sim_ar2[,s] <- get_attack_rates(bc_inf_hist2, years = years)$Attack_Rate
}
sim_ar <- rbind(sim_ar0,sim_ar1,sim_ar2)
# find mean and 95% CI of AR in each year (ARs within a given year are assumed Normally distributed)
# shapiro.test(sim_ar[1,-c(1,2)]): returns p-value>0.05
vac_strategy <- c(rep("No Vaccination",nyears),rep("Annual",nyears),rep("Every Other Year",nyears))
years_x3 <- c(rep(years,3))
sim_ar_dat <- data.frame(Year = years_x3,
                         Vac_Strategy = vac_strategy,
                         Attack_Rate = apply(sim_ar,1,mean),
                         SD_AR = apply(sim_ar,1,sd))
sim_ar_dat$Lower <- sim_ar_dat$Attack_Rate - (qnorm(0.975)*sim_ar_dat$SD_AR/sqrt(sim))
sim_ar_dat$Upper <- sim_ar_dat$Attack_Rate + (qnorm(0.975)*sim_ar_dat$SD_AR/sqrt(sim))

# plot results
# p_ar <- plot_attack_rates(dat = sim_ar_dat, by_vac = TRUE, by_age_group = FALSE, c_bands = TRUE)
# p_ar

p1 <- ggplot(data = sim_ar_dat, aes(x = Year, y = Attack_Rate, colour= Vac_Strategy)) +
      geom_line() +
      geom_ribbon(aes(x=Year,ymin=Lower,ymax=Upper,linetype=NA,fill=Vac_Strategy),alpha=0.2)+
      xlab('Year') +
      ylab('Attack Rate') +
      scale_y_continuous(limits = c(0,0.3), expand = c(0,0)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.position = c(0.95, 0.95),
            legend.justification = c("right", "top"),
            legend.box.just = "right",
            legend.margin = margin(6, 6, 6, 6),
            legend.key = element_rect(fill = "white")
            )
p1







