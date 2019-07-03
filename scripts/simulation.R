### run simulations

library(foreach)
library(doParallel)

# make cluster
 ncl <- detectCores()
 cl <- makeCluster(ncl)
 registerDoParallel(cl)
# clusterEvalQ(cl, library(morevac))
# clusterExport(cl, list=ls())
# test <- foreach(i=1:5, .packages = 'morevac') %dopar% initialize_pop()

vac_status <- c('no vaccination', 'annual', 'biannual')
vac_cov <- c(0, 0.25, 0.5, 0.75, 1)
yearRange <- c(2000:2019)
ageRange <- c(0:19)
# diff vac covs
sim_out <- foreach (j=1:5, .packages = c('morevac','Rcpp')) %:%
            foreach (i=1:3, .packages = c('morevac','Rcpp')) %dopar%
              run_sim(sim = 10,nindiv = 1000, year_range = yearRange,
                      age_range = ageRange,vaccov = vac_cov[j],
                      version = 1, rho = 0, flag = vac_status[i])

# different rho values
rhos <- c(0, 0.2, 0.5, 0.9)
sim_out <- foreach (j=1:4, .packages = 'morevac') %:%
  foreach (i=1:3, .packages = 'morevac') %do%
  run_sim(sim = 100,nindiv = 10000, year_range = yearRange,
          age_range = ageRange,vaccov = 0.5,
          version = 2, rho = rhos[j], file.out = TRUE)

# submitting individual jobs
out0 <- run_sim(sim = 25,nindiv = 5000,vaccov = 0.5,version = 2,
                rho = 0.9, wane = 0.84, vac_strategy = 0,
                file.out = FALSE)
out1 <- run_sim(sim = 25,nindiv = 5000,vaccov = 0.5,version = 2,
                rho = 0, wane = 0.84, vac_strategy = 1,
                file.out = FALSE)
out2 <- run_sim(sim = 25,nindiv = 5000,vaccov = 0.5,version = 2,
                rho = 0, wane = 0.84, vac_strategy = 2,
                file.out = FALSE)

sim_out <- list(no_vac = list(attack_rate = out0$attack_rate,
                              lifetime_infections = out0$lifetime_infections),
                annual = list(attack_rate = out1$attack_rate,
                              lifetime_infections = out1$lifetime_infections),
                biannual = list(attack_rate = out2$attack_rate,
                                lifetime_infections = out2$lifetime_infections))
#stopCluster(cl)

### output
library(ggplot2)
library(cowplot)

# read in dim results files
setwd("Q:/morevac_sims/data")
tags <- c("vs0vc0r0v1","vs1vc50r0v1","vs2vc50r0v1")
  sim_out <- list(no_vac = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags[1],".csv"),header = TRUE)[,-1],
                                lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags[1],".csv"),header = TRUE)[,-1]),
                  annual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags[2],".csv"),header = TRUE)[,-1],
                                lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags[2],".csv"),header = TRUE)[,-1]),
                  biannual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags[3],".csv"),header = TRUE)[,-1],
                                  lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags[3],".csv"),header = TRUE)[,-1])
                  )

dat1 <- process_sim_output(sim_out, year_range = yearRange, age_range = ageRange)
pa1 <- plot_attack_rates(dat = dat1[[1]], by_vac = TRUE, c_bands = TRUE)
pl1 <- plot_lifetime_infections(dat = dat1[[2]], by_vac = TRUE, x=0.5)

dat2 <- process_sim_output(sim_out, j=2, year_range = yearRange, age_range = ageRange)
pa2 <- plot_attack_rates(dat = dat2[[1]], by_vac = TRUE, c_bands = TRUE, no_legend = TRUE)
pl2 <- plot_lifetime_infections(dat = dat2[[2]], by_vac = TRUE, no_legend = TRUE)

dat3 <- process_sim_output(sim_out, j=3, year_range = yearRange, age_range = ageRange)
pa3 <- plot_attack_rates(dat = dat3[[1]], by_vac = TRUE, c_bands = TRUE, no_legend = TRUE)
pl3 <- plot_lifetime_infections(dat = dat3[[2]], by_vac = TRUE, no_legend = TRUE)

dat4 <- process_sim_output(sim_out, j=4, year_range = yearRange, age_range = ageRange)
pa4 <- plot_attack_rates(dat = dat4[[1]], by_vac = TRUE, c_bands = TRUE, no_legend = TRUE)
pl4 <- plot_lifetime_infections(dat = dat4[[2]], by_vac = TRUE, no_legend = TRUE)

dat5 <- process_sim_output(sim_out, j=5, year_range = yearRange, age_range = ageRange)
pa5 <- plot_attack_rates(dat = dat5[[1]], by_vac = TRUE, c_bands = TRUE, no_legend = TRUE)
pl5 <- plot_lifetime_infections(dat = dat5[[2]], by_vac = TRUE, no_legend = TRUE)

# plots
theme_set(theme_cowplot(font_size=10)) # reduce default font size
attack_rate_plot <- plot_grid(pa1, pa2, pa3, pa4, labels = "AUTO", ncol = 2,
                              align = 'v', axis = 'l') # aligning vertically along the left axis
lifetime_inf_plot <- plot_grid(pl1, pl2, pl3, pl4, labels = "AUTO", ncol = 2,
                               align = 'v', axis = 'l') # aligning vertically along the left axis

filename <- c('attack_rates_by_rho_vc0.5','lifetime_infections_by_rho_vc0.5')
path <- '/Users/Kylie/Google Drive/morevac_manuscript/presentations/MRC symposium'
setwd(path)

jpeg(file = paste0(filename[1],'.jpeg'))
plot(attack_rate_plot)
dev.off()

jpeg(file = paste0(filename[2],'.jpeg'))
plot(lifetime_inf_plot)
dev.off()

