
### Results figures ###
### Figure 1 - wane = 0.5, vac stop @ 10
# a) take = 1
fig1a <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                   wane = 0.5, take = 1, vac_cov = vac_cov_dat$Off_At_10)
# b) take = 0.75
fig1b <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.75, vac_cov = vac_cov_dat$Off_At_10,
                     show_legend = FALSE)
# c) take = 0.5
fig1c <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.5, vac_cov = vac_cov_dat$Off_At_10,
                     show_legend = FALSE)

# d) take = 0.25
fig1d <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.25, vac_cov = vac_cov_dat$Off_At_10,
                     show_legend = FALSE)

theme_set(theme_cowplot(font_size=10)) # reduce default font size
fig1 <- plot_grid(fig1a, fig1b, fig1c, fig1d, labels = "AUTO", ncol = 2,
                  align = 'v', axis = 'l')
# save figure
png(filename = "figure1_penalty.png", width = 6, height = 6, units = "in", res = 300)
plot(fig1)
dev.off()

### Figure 2 - wane = 0.5, vac stop @ 16
# a) take = 1
fig2a <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 1, vac_cov = vac_cov_dat$Off_At_16)
# b) take = 0.75
fig2b <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.75, vac_cov = vac_cov_dat$Off_At_16,
                     show_legend = FALSE)
# c) take = 0.5
fig2c <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.5, vac_cov = vac_cov_dat$Off_At_16,
                     show_legend = FALSE)

# d) take = 0.25
fig2d <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.25, vac_cov = vac_cov_dat$Off_At_16,
                     show_legend = FALSE)

theme_set(theme_cowplot(font_size=10)) # reduce default font size
fig2 <- plot_grid(fig2a, fig2b, fig2c, fig2d, labels = "AUTO", ncol = 2,
                  align = 'v', axis = 'l')

# save figure
png(filename = "figure2.png", width = 6, height = 6, units = "in", res = 300)
plot(fig2)
dev.off()

### Figure 3
fig3b <- plot_sim_ar(sim = 25, years = 2000:2019, year_index = 181:200,
                     wane = 0, take = 0.5, vac_cov = vac_cov_dat$Fifty_Off_At_10, drift_off = FALSE)

### Model schematic figures
# run multi-annual model
out <- multiannual(n=10000, vac_coverage = vac_cov_dat$Off_At_10, vac_strategy = 2)
# isolate birth cohort in 2000
birth_cohort <- which(out$ages[,181]==0)
bc_inf_hist <- out$inf_history$inf_hist_mat[birth_cohort,181:200]
bc_vac_hist <- out$vac_history$vac_hist_mat[birth_cohort,181:200]
bc_suscept <- out$inf_history$suscept_mat[birth_cohort,181:200]
bc_ages <- out$ages[birth_cohort,181:200]
# pick one person to plot susceptibility
i <- 17
person <- data.frame(Year = 2000:2019,
                     inf_hist = bc_inf_hist[i,],
                     vac_hist = bc_vac_hist[i,],
                     suscept  = bc_suscept[i,])
vax <- person$Year[which(person$vac_hist == 1)]
infections <- person$Year[which(person$inf_hist == 1)]
person
# drift plot
drift_dat <- data.frame(Year = 2000:2019, Drift = out$drift[181:200], Vac_Update = out$vac_update[181:200],
                        Gammas = out$gammas[181:200])
drift_dat$Shade_Group <- c(rep(1,dim(drift_dat)[1]))
for (i in 2:dim(drift_dat)[1]){
  if (drift_dat$Vac_Update[i]==1){drift_dat$Shade_Group[i] <- drift_dat$Shade_Group[i-1] + 1
  } else {drift_dat$Shade_Group[i] <- drift_dat$Shade_Group[i-1]}
}
drift_dat$Cum_Drift <- cumsum(drift_dat$Drift)
drift_dat$Shade_Group <- as.factor(drift_dat$Shade_Group)
drift_dat_thinned <- drift_dat[which(drift_dat$Vac_Update==1),]
# add row for coloring under the curve
p_drift <- ggplot(data = drift_dat, aes(x = Year, y = Cum_Drift)) +
           geom_line() +
           geom_ribbon(aes(ymax = Cum_Drift, fill = Shade_Group),ymin=0,alpha=0.2) +
           geom_point(data=drift_dat_thinned,aes(x=Year,y=Cum_Drift),colour = 'red') +
           ylab('Cumulative Antigenic Drift') +
           theme(legend.position = "none")
p_drift

# vaccine protection plot
p_gamma <- ggplot(data = drift_dat, aes(x = Year, y = Gammas)) +
  geom_line() +
  geom_ribbon(aes(ymax = Cum_Drift,fill = Shade_Group),ymin=0,alpha=0.2,) +
  geom_point(data=drift_dat_thinned,aes(x=Year,y=Cum_Drift),colour = 'red') +
  ylab('Cumulative Antigenic Drift')
p_drift

# susceptibility plot
p_suscept <- ggplot(data = person, aes(x = Year, y = suscept)) +
             geom_line() +
             geom_ribbon(aes(ymax = suscept),ymin=0,alpha=0.2,) +
             xlab('Year') +
             ylab('Susceptibility') +
             scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

p_suscept <- p_suscept +
             geom_vline(xintercept=vax, linetype="dashed", colour = 'blue') +
             geom_vline(xintercept=infections, colour = 'red')
p_suscept
library(cowplot)
theme_set(theme_cowplot(font_size=10)) # reduce default font size
p <- plot_grid(p_vac, p_no_vac_suscept,p_suscept, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')
p
