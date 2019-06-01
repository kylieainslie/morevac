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

sim_out <- foreach (j=1:5, .packages = 'morevac') %:%
            foreach (i=1:3, .packages = 'morevac') %do%
              run_sim(sim = 100,nindiv = 10000, year_range = yearRange,
                      age_range = ageRange,vaccov = vac_cov[j],
                      version = 1, rho = 0.9, flag = vac_status[i])

#stopCluster(cl)

### output

dat1 <- process_sim_output(sim_out, j=1, year_range = yearRange, age_range = ageRange)
pa1 <- plot_attack_rates(dat = dat1, by_vac = TRUE, c_bands = TRUE)
pl1 <- plot_lifetime_infections(dat = dat1, by_vac = TRUE)

dat2 <- process_sim_output(sim_out, j=2, year_range = yearRange, age_range = ageRange)
pa2 <- plot_attack_rates(dat = dat2, by_vac = TRUE, c_bands = TRUE)
pl2 <- plot_lifetime_infections(dat = dat2, by_vac = TRUE)

dat3 <- process_sim_output(sim_out, j=3, year_range = yearRange, age_range = ageRange)
pa3 <- plot_attack_rates(dat = dat3, by_vac = TRUE, c_bands = TRUE)
pl3 <- plot_lifetime_infections(dat = dat3, by_vac = TRUE)

dat4 <- process_sim_output(sim_out, j=4, year_range = yearRange, age_range = ageRange)
pa4 <- plot_attack_rates(dat = dat4, by_vac = TRUE, c_bands = TRUE)
pl4 <- plot_lifetime_infections(dat = dat4, by_vac = TRUE)

dat5 <- process_sim_output(sim_out, j=5, year_range = yearRange, age_range = ageRange)
pa5 <- plot_attack_rates(dat = dat5, by_vac = TRUE, c_bands = TRUE)
pl5 <- plot_lifetime_infections(dat = dat5, by_vac = TRUE)

