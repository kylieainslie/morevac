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
out0 <- run_sim(sim = 5,nindiv = 5000,vaccov = 0.5,version = 2,
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

# version 1
# read in output data
tags1 <- c("vs0vc50r09v1w84","vs1vc50r09v1w84","vs2vc50r09v1w84")
sim_out1 <- list(no_vac = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags1[1],".csv"),header = TRUE)[,-1],
                               lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags1[1],".csv"),header = TRUE)[,-1]),
                 annual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags1[2],".csv"),header = TRUE)[,-1],
                               lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags1[2],".csv"),header = TRUE)[,-1]),
                 biannual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags1[3],".csv"),header = TRUE)[,-1],
                                 lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags1[3],".csv"),header = TRUE)[,-1])
)
# process data to be plotted
dat1 <- process_sim_output(sim_out1, year_range = yearRange, age_range = ageRange)
# plot attack rates
pa1 <- plot_attack_rates(dat = dat1[[1]], by_vac = TRUE, c_bands = TRUE)
# plot lifetime infections
pl1 <- plot_lifetime_infections(dat = dat1[[2]], by_vac = TRUE, x=0.5)

# version 2
tags2 <- c("vs0vc50r09v2w84","vs1vc50r09v2w84","vs2vc50r09v2w84")
sim_out2 <- list(no_vac = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags2[1],".csv"),header = TRUE)[,-1],
                               lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags2[1],".csv"),header = TRUE)[,-1]),
                 annual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags2[2],".csv"),header = TRUE)[,-1],
                               lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags2[2],".csv"),header = TRUE)[,-1]),
                 biannual = list(attack_rate = read.csv(file = paste0("attack_rates/attack_rate_data_",tags2[3],".csv"),header = TRUE)[,-1],
                                 lifetime_infections = read.csv(file = paste0("lifetime_infections/lifetime_inf_data_",tags2[3],".csv"),header = TRUE)[,-1])
)

dat2 <- process_sim_output(sim_out2, year_range = yearRange, age_range = ageRange)
pa2 <- plot_attack_rates(dat = dat2[[1]], by_vac = TRUE, c_bands = TRUE)
pl2 <- plot_lifetime_infections(dat = dat2[[2]], by_vac = TRUE, x=0.5)

# plots
theme_set(theme_cowplot(font_size=10)) # reduce default font size
attack_rate_plot <- plot_grid(pa1, pa2, labels = "AUTO", ncol = 2, align = 'v', axis = 'l')
lifetime_inf_plot <- plot_grid(pl1, pl2, labels = "AUTO", ncol = 2, align = 'v', axis = 'l')

# all years sim
test <- multiannual2()
sim <- 100
empty <- matrix(c(rep(0,sim*200)),nrow=200)
all_years_ar <- cbind(test[[2]]$Year,empty)
all_years_ar <- as.data.frame(all_years_ar)
names(all_years_ar) <- c('Year',paste0('Sim',1:sim))

for (s in 1:sim){
  out <- multiannual2(wane=0, vac_strategy = 2)
  all_years_ar[,s+1] <- out[[2]]$Attack_Rate
}

# mean_ar <- apply(all_years_ar[,-1], 1, mean)
# ar_dat <- data.frame(Year = all_years_ar$Year, Attack_Rate = mean_ar)
#
# p_ar <- plot_attack_rates(ar_dat)
# p_ar <- p_ar +
#         geom_vline(xintercept=2000, linetype="dashed", colour = 'red')
# p_ar

mean_ar2 <- apply(all_years_ar[,-1], 1, mean)
yearsx2 <- c(rep(all_years_ar$Year,2))
ar_dat2 <- data.frame(Year = yearsx2, Attack_Rate = c(mean_ar,mean_ar2),Vac_Strategy = c(rep('Annual',200),rep('Every Other Year',200)))
p_ar2 <- plot_attack_rates(ar_dat2, by_vac = TRUE, legend_x = 0.25)
p_ar2 <- p_ar2 +
         geom_vline(xintercept=2000, linetype="dashed", colour = 'black')
p_ar2
