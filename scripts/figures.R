
### Results figures ###
### Figure 1 - wane = 0.5, vac stop @ 10
# a) take = 1
fig1a <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                   wane = 0.5, take = 1, vac_cov = vac_cov_dat$Off_At_10)
# b) take = 0.75
fig1b <- plot_sim_ar(sim = 500, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.7, vac_cov = vac_cov_dat$Off_At_10,
                     show_legend = TRUE)
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

theme_set(theme_cowplot(font_size=9)) # reduce default font size
alt_fig1 <- plot_grid(fig1b, p_inf, labels = "AUTO", ncol = 2, align = 'v', axis = 'l')
png(filename = "alt_figure1.png", width = 7, height = 4, units = "in", res = 300)
plot(alt_fig1)
dev.off()

### Figure 2 - diferent values of waning and vac coverage

# a) wane = 0.5, take = 0.7, vac_cov = 0.75
fig2a <- plot_sim_ar(sim = 50, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.7, vac_cov = vac_cov_dat$SeventyFive_Off_At_10,
                     title = "Vaccinate 2-10 years, Vac Coverage = 75%, Wane = 0.5")
# b) wane = 0.25, take = 0.7, vac_cov = 0.75
fig2b <- plot_sim_ar(sim = 50, years = 2000:2019, year_index = 181:200,
                     wane = 0.25, take = 0.7, vac_cov = vac_cov_dat$SeventyFive_Off_At_10,
                     show_legend = FALSE,
                     title = "Vaccinate 2-10 years, Vac Coverage = 75%, Wane = 0.25")
# include this one!
# c) wane = 0.5, take = 0.7, off @ 16
fig2c <- plot_sim_ar(sim = 50, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.7, vac_cov = vac_cov_dat$Off_At_16,
                     show_legend = FALSE,
                     title = "Vaccinate 2-16 years, Vac Coverage = 75%, Wane = 0.5")
### include this one!
# d) wane = 0.25, take = 0.7, vac_cov = 0.75, off @ 16
fig2d <- plot_sim_ar(sim = 50, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.7, vac_cov = vac_cov_dat$SeventyFive_Off_At_16,
                     show_legend = FALSE,
                     "Vaccinate 2-16 years, Vac Coverage = 75%, Wane = 0.25")

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
out <- multiannual(n=10000, vac_coverage = vac_cov_dat$Off_At_10, vac_strategy = 1)
# isolate birth cohort in 2000
birth_cohort <- which(out$ages[,181]==0)
bc_inf_hist <- out$inf_history$inf_hist_mat[birth_cohort,181:200]
bc_vac_hist <- out$vac_history$vac_hist_mat[birth_cohort,181:200]
bc_suscept <- out$inf_history$suscept_mat[birth_cohort,181:200]
bc_ages <- out$ages[birth_cohort,181:200]
# pick one person to plot susceptibility
i <- 29
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
drift_dat$Shade_Group <- (drift_dat$Shade_Group)
#calculate cumulative drift
drift_dat$Cum_Drift <- cumsum(drift_dat$Drift)
drift_dat$Year  <-(drift_dat$Year)
# determine years when a vaccine update occurred
drift_dat_thinned <- drift_dat[which(drift_dat$Vac_Update==1),]
drift_dat_thinned$prev <-c(min(drift_dat$Year),drift_dat_thinned$Year[-nrow(drift_dat_thinned)])
# plot
ymx <- max(drift_dat$Cum_Drift)
p_drift <- ggplot(data = drift_dat, mapping=aes( x=Year,y = Cum_Drift))+
  scale_x_continuous(limits = c(2000,2019), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,ymx), expand = c(0,0)) +
  geom_rect(data=drift_dat_thinned,aes(xmin=prev,ymin=0,ymax=ymx,xmax=Year,fill=factor(Shade_Group),col=factor(Shade_Group)),alpha=0.2) +
  geom_ribbon(aes(ymin=Cum_Drift,ymax=ymx),fill="white" ,alpha=1)+  # color shading under cumulative drift by shade group
  ylab('Cumulative Antigenic Drift') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_line()
p_drift
# determine colors used
cols <- ggplot_build(p_drift)$data[[1]]$colour
# add points in same colors
p_drift <- p_drift + geom_point(data = drift_dat_thinned,aes(x=Year,y=Cum_Drift),colour = c(cols[-1],"#CC6666"))
p_drift
# vaccine protection plot
p_gamma <- ggplot(data = drift_dat, aes(x = Year, y = Gammas)) +
  geom_line() +
  geom_ribbon(aes(ymax = Cum_Drift,fill = Shade_Group),ymin=0,alpha=0.2,) +
  geom_point(data=drift_dat_thinned,aes(x=Year,y=Cum_Drift),colour = 'blue') +
  ylab('Cumulative Antigenic Drift')
p_gamma

# susceptibility plot
p_suscept <- ggplot(data = person, aes(x = Year, y = suscept)) +
             geom_line() +
             geom_ribbon(aes(ymax = suscept),ymin=0,alpha=0.1,) +
             xlab('Year') +
             ylab('Susceptibility') +
             scale_x_continuous(limits = c(2000,2019), expand = c(0,0)) +
             scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))
vac_cols <- cols[drift_dat$Shade_Group[which(drift_dat$Year %in% vax)]]
p_suscept <- p_suscept +
             geom_vline(xintercept=vax, linetype="dashed", colour = vac_cols) +
             geom_vline(xintercept=infections, colour = 'red')
p_suscept
library(cowplot)
theme_set(theme_cowplot(font_size=12)) # reduce default font size
p <- plot_grid(p_drift, p_suscept, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')
p

# save figure
png(filename = "model_schematic_fig.png", width = 6, height = 6, units = "in", res = 300)
plot(p)
dev.off()

