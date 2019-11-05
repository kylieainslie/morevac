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
library(boot)
# load morevac package
setwd("~/Documents/morevac")
devtools::load_all()
#######################################
### simulation
## create latin hypercube of parameter values to simulate over
set.seed(1234)
#set.seed(5678)
mylhc <- randomLHS(500, 6)
colnames(mylhc) <- c("Vac_Cov", "Waning", "Take", "Epsilon", "Rho", "VE")
mylhc[, "Epsilon"] <- qunif(mylhc[,"Epsilon"], min = 0.001, max = 0.05)
mylhc[, "Vac_Cov"] <- qunif(mylhc[,"Vac_Cov"], min = 0.301, max = 0.806) # range from 2018-2019 PHE estimates of VC in school-aged children: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/806289/Childhood_flu_annual_report_2018_19_FINAL_.pdf
mylhc[, "Take"] <- qunif(mylhc[,"Take"], min = 0.5, max = 1)
# run simulations
setwd("~/Dropbox/Kylie/Projects/morevac_manuscript/data")
# parallelize
bins <- list(seq(1,100),seq(101,200),seq(201,300),seq(301,400),seq(401,500))
# make cluster
# ncl <- detectCores()
# cl <- makeCluster(ncl)
# registerDoParallel(cl)
for (b in 1:5){
#foreach(j=1:5, .packages = c('morevac','Rcpp')) %dopar% {

loop_length <- length(bins[[b]])
for (i in 1:loop_length){
cat("\n Simulation ",i," of",loop_length,"\n")
# parameters
n_sim = 5
nindiv <- 30000
row_lhc <- bins[[b]][i]
max_age = 80
myyears <- 1820:2028
mybetas <- c(0.4,rep(0.2,length(myyears)-1))
vac_cut_off <- 10
vac_cov_dat <- data.frame(Age = 0:(max_age-1), No_Vac = numeric(max_age), Annual = numeric(max_age), Biennial = numeric(max_age))
vac_cov_dat$Annual[3:(vac_cut_off + 1)] <- mylhc[row_lhc,"Vac_Cov"]
vac_cov_dat$Biennial[seq(3,vac_cut_off+1,2)] <- mylhc[row_lhc,"Vac_Cov"]
# output note to user
cat("\n No vaccination simulation running... \n")
# returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test0 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$No_Vac, vac_strategy = 0,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
cat("\n Annual vaccination simulation running... \n")
sim_test1 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Annual, vac_strategy = 1,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])
cat("\n Every other year vaccination simulation running... \n")
sim_test2 <- run_sim_2(sim = n_sim, n = nindiv, years = myyears, betas = mybetas, vac_cov = vac_cov_dat$Biennial, vac_strategy = 2,
                       wane = mylhc[row_lhc, "Waning"], take = mylhc[row_lhc, "Take"], epsilon = mylhc[row_lhc, "Epsilon"], vac_protect = mylhc[row_lhc, "VE"], rho = mylhc[row_lhc, "Rho"])

# post process sim results
sim0_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test0, total_year_range = myyears, nsim = n_sim)
sim1_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test1, total_year_range = myyears, nsim = n_sim)
sim2_results <- postprocess_sim_results_for_rolling_cohort(simdat = sim_test2, total_year_range = myyears, nsim = n_sim)

cat("\n Bootstrapping... \n")
# bootstrapping
myAR0 <- boot_ar(dat = sim0_results$Attack_Rate_Median, vac_strategy = "No Vaccination", stat = "mean")
myAR1 <- boot_ar(dat = sim1_results$Attack_Rate_Median, vac_strategy = "Annual", stat = "mean")
myAR2 <- boot_ar(dat = sim2_results$Attack_Rate_Median, vac_strategy = "Every Other Year", stat = "mean")

# combine AR sim results into single data set to plot
### median dataset
dat <- rbind(myAR0, myAR1, myAR2)
dat$Vac_Cov <- mylhc[row_lhc, "Vac_Cov"]
dat$Waning <- mylhc[row_lhc, "Waning"]
dat$Take = mylhc[row_lhc, "Take"]
dat$Epsilon = mylhc[row_lhc, "Epsilon"]
dat$Rho = mylhc[row_lhc, "Rho"]
dat$VE = mylhc[row_lhc, "VE"]

# bootstrapping
myLI0 <- boot_li(dat = sim0_results$Lifetime_Infections, vac_strategy = "No Vaccination", stat = "mean")
myLI1 <- boot_li(dat = sim1_results$Lifetime_Infections, vac_strategy = "Annual", stat = "mean")
myLI2 <- boot_li(dat = sim2_results$Lifetime_Infections, vac_strategy = "Every Other Year", stat = "mean")

# bootstrap difference
li_diff <- sim1_results$Total_Lifetime_Infections$total_mean - sim2_results$Total_Lifetime_Infections$total_mean
boot_diff <- boot(li_diff, statistic = function(x, d) {return(mean(x[d]))}, R = 1000)
diff_lower <- boot.ci(boot_diff, index=1, type='perc')$percent[4]
diff_upper<- boot.ci(boot_diff, index=1, type='perc')$percent[5]

# combine LI sim results into single data set to plot
dat2 <- rbind(myLI0, myLI1, myLI2)
dat2$Diff <- boot_diff$t0
dat2$Diff_Lower <- diff_lower
dat2$Diff_Upper <- diff_upper
dat2$Vac_Cov <- mylhc[row_lhc, "Vac_Cov"]
dat2$Waning <- mylhc[row_lhc, "Waning"]
dat2$Take = mylhc[row_lhc, "Take"]
dat2$Epsilon = mylhc[row_lhc, "Epsilon"]
dat2$Rho = mylhc[row_lhc, "Rho"]
dat2$VE = mylhc[row_lhc, "VE"]

if (i == 1){
   ar_out <- dat
   li_out <- dat2
} else {
   ar_out <- rbind(ar_out, dat)
   li_out <- rbind.fill(li_out, dat2)
}
}
cat("\n Complete!")
# write results to file
#write.csv(ar_out_a, file = paste0("attack_rates/ar_sim_data_median_",b,".csv"))
write.csv(ar_out, file = paste0("attack_rates/ar_sim_data_mean_",b,".csv"))
#write.csv(li_out_a, file = paste0("lifetime_infs/li_sim_data_median_",b,".csv"))
write.csv(li_out, file = paste0("lifetime_infs/li_sim_data_mean_",b,".csv"))

} # end b loop
# } # end foreach

# stop cluster
# stopCluster(cl)
