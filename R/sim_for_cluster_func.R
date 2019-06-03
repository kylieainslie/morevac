### run simulations

#library(foreach)
#library(doParallel)

# make cluster
# cl <- makeCluster(2)
# registerDoParallel(cl)
# clusterEvalQ(cl, library(morevac))
# clusterExport(cl, list=ls())
# test <- foreach(i=1:5, .packages = 'morevac') %dopar% initialize_pop()

sim_for_cluster_func <- function(s = 100, n = 10000, v = 1, r = 0.9){
vac_status <- c('no vaccination', 'annual', 'biannual')
vac_cov <- c(0, 0.25, 0.5, 0.75, 1)
yearRange <- c(2000:2019)
ageRange <- c(0:19)

sim_out <- foreach (j=1:5, .packages = 'morevac') %:%
            foreach (i=1:3, .packages = 'morevac') %do%
              run_sim(sim = s,nindiv = n, year_range = yearRange,
                      age_range = ageRange,vaccov = vac_cov[j],
                      version = v, rho = r, flag = vac_status[i])

#stopCluster(cl)

### output
#library(ggplot2)
#library(cowplot)

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

# plots
theme_set(theme_cowplot(font_size=10)) # reduce default font size
attack_rate_plot <- plot_grid(pa1, pa2, pa3, pa4, pa5, labels = "AUTO", ncol = 2,
                              align = 'v', axis = 'l') # aligning vertically along the left axis
lifetime_inf_plot <- plot_grid(pl1, pl2, pl3, pl4, pl5, labels = "AUTO", ncol = 2,
                               align = 'v', axis = 'l') # aligning vertically along the left axis

#path <- 'C:/Users/kainslie/Google Drive/morevac_manuscript/figures/'

pdf(file = paste0('attack_rates_rho_',r,'_v',v,'.pdf'))
plot(attack_rate_plot)
dev.off()

pdf(file = paste0('lifetime_infections_rho_',r,'_v',v,'.pdf'))
plot(lifetime_inf_plot)
dev.off()
}
