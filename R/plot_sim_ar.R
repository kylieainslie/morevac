plot_sim_ar <- function(sim = 100, years = 2000:2019, year_index = 181:200,
                        wane = 0.5, take = 0.75, vac_cov, show_legend = TRUE){
### simulation
## single run
# returns 3 arrays with inf_hist_mat, vac_hist_mat, and ages_mat from each sim
sim_test0 <- run_sim_2(wane = wane, take = take, vac_cov = vac_cov, vac_strategy = 0)
sim_test1 <- run_sim_2(wane = wane, take = take, vac_cov = vac_cov, vac_strategy = 1)
sim_test2 <- run_sim_2(wane = wane, take = take, vac_cov = vac_cov, vac_strategy = 2)

# post process sim results
# overall attack rates
#sim = 100
#years <- 2000:2019
nyears <- length(years)
sim_test_ar0 <- matrix(numeric(length(years)*sim),nrow = length(years))
sim_test_ar1 <- sim_test_ar0; sim_test_ar2 <- sim_test_ar0

for (s in 1:sim){
  bc0 <- which(sim_test0$ages[,year_index[1],s]==0)
  bc_inf_hist0 <- sim_test0$inf_history[bc0,year_index,s]
  sim_test_ar0[,s] <- get_attack_rates(bc_inf_hist0, years = years)$Attack_Rate

  bc1 <- which(sim_test1$ages[,year_index[1],s]==0)
  bc_inf_hist1 <- sim_test1$inf_history[bc1,year_index,s]
  sim_test_ar1[,s] <- get_attack_rates(bc_inf_hist1, years = years)$Attack_Rate

  bc2 <- which(sim_test2$ages[,year_index[1],s]==0)
  bc_inf_hist2 <- sim_test2$inf_history[bc2,year_index,s]
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
if (show_legend){
p1 <- ggplot(data = sim_test_ar_dat, aes(x = Year, y = Attack_Rate, colour= Vac_Strategy)) +
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
} else{
  p1 <- ggplot(data = sim_test_ar_dat, aes(x = Year, y = Attack_Rate, colour= Vac_Strategy)) +
        geom_line() +
        geom_ribbon(aes(x=Year,ymin=Lower,ymax=Upper,linetype=NA,fill=Vac_Strategy),alpha=0.2)+
        xlab('Year') +
        ylab('Attack Rate') +
        scale_y_continuous(limits = c(0,0.3), expand = c(0,0)) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              legend.position = "none"
              )

}
return(p1)
}




