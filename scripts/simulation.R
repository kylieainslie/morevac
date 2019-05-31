### run simulations

library(foreach)
library(doParallel)

# make cluster
# cl <- makeCluster(2)
# registerDoParallel(cl)
# clusterEvalQ(cl, library(morevac))
# clusterExport(cl, list=ls())
# test <- foreach(i=1:5, .packages = 'morevac') %dopar% initialize_pop()


vac_status <- c('no vaccination', 'annual', 'biannual')
vac_cov <- c(0, 0.25, 0.5, 0.75, 1)
yearRange <- c(2000:2019)
ageRange <- c(0:19)

sim_out <- foreach (i=1:3, j=1:5, .packages = 'morevac') %do%
              run_sim(sim = 10,nindiv = 5000, year_range = yearRange,
                      age_range = ageRange,vaccov = vac_cov[j],
                      version = 1, rho = 0.9, flag = vac_status[i])

#stopCluster(cl)

### output
cohort2 <- data.frame(Year = c(rep(yearRange,3)),
                     Attack_Rate = c(apply(sim_out[[1]],1,mean),
                                     apply(sim_out[[2]],1,mean),
                                     apply(sim_out[[3]],1,mean)),
                     Lower = c(apply(sim_out[[1]],1,FUN = function(x) quantile(x, c(0.025))),
                               apply(sim_out[[2]],1,FUN = function(x) quantile(x, c(0.025))),
                               apply(sim_out[[3]],1,FUN = function(x) quantile(x, c(0.025)))),
                     Upper = c(apply(sim_out[[1]],1,FUN = function(x) quantile(x, c(0.975))),
                               apply(sim_out[[2]],1,FUN = function(x) quantile(x, c(0.975))),
                               apply(sim_out[[3]],1,FUN = function(x) quantile(x, c(0.975)))),
                     Age = c(rep(ageRange,3)),
                     Vac_Strategy = c(rep('No Vaccination',length(yearRange)),
                                      rep('Annual',length(yearRange)),
                                      rep('Biannual',length(yearRange)))
)

p_cohort2 <- plot_attack_rates(dat = cohort2, by_vac = TRUE, c_bands = TRUE)
#
# pdf(file = paste0(filename,"_ar.pdf"))
# plot(p_cohort)
# dev.off()
# #
# # lifetime infections
# life_inf_dat <- data.frame(Sim = c(rep(1:sim,3)),
#                            Vac_Strategy = c(rep('No Vaccination',sim),
#                                             rep('Annual',sim),
#                                             rep('Biannual',sim)),
#                            rbind(life_inf0,life_infa,life_infb)
# )
# names(life_inf_dat) <- c('Sim','Vac_Strategy',c(paste0("Age",age_range)))
# data_long <- gather(life_inf_dat, Age, Life_Inf, Age0:Age19, factor_key=TRUE)
# data_long$Age <- as.factor(str_remove(data_long$Age, 'Age'))
#
# data_long$Age = with(data_long, reorder(Age, Life_Inf, mean))
#
# p1 <- plot_lifetime_infections(dat = data_long, by_vac = TRUE)
#
# pdf(file = paste0(filename,"_life_inf.pdf"))
# plot(p1)
# dev.off()
