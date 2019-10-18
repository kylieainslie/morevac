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
library(plyr)
library(lhs)
# load morevac package
setwd("~/Documents/morevac")
devtools::load_all()
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
my_cohorts <- get_cohorts(inf_history = out$inf_history$inf_hist_mat, vac_history = out$vac_history$vac_hist_mat, ages = out$ages,
                          total_year_range = 1820:2028)
cohort_sizes <- sapply(my_cohorts$cohort_ids, length)
ninfs <- sapply(my_cohorts$inf_hist,function(x) apply(x,2,sum))
colnames(ninfs) <- paste0("Cohort",1:10)
cohort_ar <- sweep(ninfs, 2, cohort_sizes, FUN="/")
avg_ar <- data.frame(Age = 0:18, Attack_Rate = apply(cohort_ar,1,mean))

# plot attack rates
p_cohort <- plot_attack_rates(dat = cohort_ar)
p_cohort

#######################################
### simulation
## create latin hypercube of parameter values to simulate over
set.seed(1234)
mylhc <- randomLHS(100, 6)
colnames(mylhc) <- c("Vac_Cov", "Waning", "Take", "Epsilon", "Rho", "VE")
mylhc[, "Epsilon"] <- qunif(mylhc[,"Epsilon"], min = 0.001, max = 0.05)

# log_test <- log(qunif(mylhc[,"Epsilon"], min = 0.001, max = 0.05), base = 10)
# log_test2 <- 10^quantile(log_test,probs = mylhc[,"Epsilon"])
## single run
setwd("~/Dropbox/Kylie/Projects/morevac_manuscript/data")
for (i in 1:dim(mylhc)[1]){
# parameters
n_sim = 10
nindiv <- 30000
row_lhc <- i
max_age = 80
myyears <- 1820:2028
mybetas <- c(0.4,rep(0.2,length(myyears)-1))
vac_cut_off <- 16
vac_cov_none <- c(rep(0,max_age))
vac_cov_annual <- c(rep(0,2),rep(mylhc[row_lhc, "Vac_Cov"], vac_cut_off - 2), rep(0, max_age - vac_cut_off))
vac_cov_biennial <- c(rep(0,2),rep(c(mylhc[row_lhc, "Vac_Cov"], 0), vac_cut_off/2 - 1), rep(0, max_age - vac_cut_off))
# returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test0 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$No_Vaccination, vac_strategy = 0,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
sim_test1 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_annual, vac_strategy = 1,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
sim_test2 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_biennial, vac_strategy = 2,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])

# post process sim results
sim0_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test0, total_year_range = myyears, nsim = n_sim)
sim1_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test1, total_year_range = myyears, nsim = n_sim)
sim2_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test2, total_year_range = myyears, nsim = n_sim)

# combine AR sim results into single data set to plot
length_study <- 19
sim_results <- rbind(sim0_results$Attack_Rate,sim1_results$Attack_Rate,sim2_results$Attack_Rate)
vac_strategy <- c(rep("No Vaccination",length_study),rep("Annual", length_study), rep("Every Other Year", length_study))
age_x3 <- c(rep(0:(length_study-1),3))

dat <- data.frame(Age = age_x3, Vac_Strategy = vac_strategy,
                  Attack_Rate = apply(sim_results, 1, mean),
                  Lower = apply(sim_results, 1, quantile, probs=c(0.025)),
                  Upper = apply(sim_results, 1, quantile, probs=c(0.975)),
                  Vac_Cov = mylhc[row_lhc, "Vac_Cov"],
                  Waning = mylhc[row_lhc, "Waning"],
                  Take = mylhc[row_lhc, "Take"],
                  Epsilon = mylhc[row_lhc, "Epsilon"],
                  Rho = mylhc[row_lhc, "Rho"],
                  VE = mylhc[row_lhc, "VE"])
if (i == 1){ ar_out <- dat
} else {ar_out <- rbind(ar_out, dat)}

# post-post-processing of lifetime infections
lifetime_infs0 <- data.frame(sim0_results$Lifetime_Infections, Vac_Strategy = c(rep("No Vaccination",dim(sim0_results$Lifetime_Infections)[1])))
lifetime_infs1 <- data.frame(sim1_results$Lifetime_Infections, Vac_Strategy = c(rep("Annual",dim(sim1_results$Lifetime_Infections)[1])))
lifetime_infs2 <- data.frame(sim2_results$Lifetime_Infections, Vac_Strategy = c(rep("Every Other Year",dim(sim2_results$Lifetime_Infections)[1])))

all_lifetime_infs <- rbind(lifetime_infs0,lifetime_infs1,lifetime_infs2)
all_lifetime_infs$Vac_Cov <- mylhc[row_lhc, "Vac_Cov"]
all_lifetime_infs$Waning <- mylhc[row_lhc, "Waning"]
all_lifetime_infs$Take <- mylhc[row_lhc, "Take"]
all_lifetime_infs$Epsilon <- mylhc[row_lhc, "Epsilon"]
all_lifetime_infs$Rho <- mylhc[row_lhc, "Rho"]
all_lifetime_infs$VE <- mylhc[row_lhc, "VE"]

avg_lifetime_infs <- ddply(all_lifetime_infs,.(Num_Vacs, Vac_Strategy),summarise,mean=mean(mean))
avg_lifetime_infs$Vac_Cov <- mylhc[row_lhc, "Vac_Cov"]
avg_lifetime_infs$Waning <- mylhc[row_lhc, "Waning"]
avg_lifetime_infs$Take <- mylhc[row_lhc, "Take"]
avg_lifetime_infs$Epsilon <- mylhc[row_lhc, "Epsilon"]
avg_lifetime_infs$Rho <- mylhc[row_lhc, "Rho"]
avg_lifetime_infs$VE <- mylhc[row_lhc, "VE"]

if(i == 1){li_out <- all_lifetime_infs
           li_avg_out <- avg_lifetime_infs
} else {li_out <- rbind(li_out, all_lifetime_infs)
        li_avg_out <- rbind(li_avg_out, avg_lifetime_infs)
}
# gc() # clear R memory between runs
}

# write results to file
write.csv(ar_out, file = "ar_sim_data.csv")
write.csv(li_out, file = "li_sim_data.csv")
write.csv(li_avg_out, file = "li_avg_sim_data.csv")

