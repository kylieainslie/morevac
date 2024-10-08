avg_ar <- matrix(numeric(nsim*length_study), nrow = length_study)
rownames(avg_ar) <- paste0("Age",0:(length_study-1))
colnames(avg_ar) <- paste0("Sim",1:nsim)
med_ar <- avg_ar


# calculate avg attack rate over cohorts
cohort_sizes <- sapply(my_cohorts$cohort_ids, length)
ninfs <- sapply(my_cohorts$inf_hist,function(x) apply(x,2,sum))
colnames(ninfs) <- paste0("Cohort",1:10)
cohort_ar <- sweep(ninfs, 2, cohort_sizes, FUN="/")
avg_ar[,s] <- apply(cohort_ar,1,mean)
med_ar[,s] <- apply(cohort_ar,1,median)


# calculate avg num lifetime infections by number of vaccinations
lifetime_infs <- sapply(my_cohorts$inf_hist,function(x) apply(x,1,sum))
num_vacs <- sapply(my_cohorts$vac_hist,function(x) apply(x,1,sum))
tmp <- data.frame(Lifetime_Infs = unlist(lifetime_infs), Num_Vacs = unlist(num_vacs))
avg_tmp <- ddply(tmp,~Num_Vacs,summarise,med = median(Lifetime_Infs), mean = mean(Lifetime_Infs))
avg_tmp$sim <- c(rep(s,dim(avg_tmp)[1]))
tmp2 <- data.frame(sim = s, total_mean = mean(tmp$Lifetime_Infs))


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
