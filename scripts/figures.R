### Manuscript figures ###

##################################
### Figure 1 - Model schematic ###
##################################
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
figure1 <- plot_grid(p_drift, p_suscept, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

# save figure
filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure1.png"), width = 6, height = 8,
    units = "in", pointsize = 8, res = 300)
plot(figure1)
dev.off()

###################################
### Figure 2 - baseline results ###
###################################
# baseline figures: wane = 100%,
#     take = 100%, epsilon = 0%,
#     VE = 70%, rho = 90%
###################################
# AR, vac off at 10
p_ar_baseline <- ggplot(data = chocolate_sundae2, aes(x = Age, y = Mean_AR, colour= Vac_Strategy)) +
  geom_line() +
  geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
  xlab("Age (years)") +
  ylab("Attack Rate") +
  #labs(fill = "Vaccination Strategy") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key = element_rect(fill = "white"))
# AR, vac off at 16
p_ar_baseline16 <- ggplot(data = chocolate_sundae2, aes(x = Age, y = Mean_AR, colour = Vac_Strategy)) +
  geom_line() +
  geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
  xlab("Age (years)") +
  ylab("Attack Rate") +
  #labs(x = "Age (years)", y = "Attack Rate", colour = "Vaccination Strategy") +
  # scale_fill_discrete(name="Vaccination Strategy",
  #                     labels=c("Annual", "Biennial", "No Vaccination")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none")
        # legend.position = c(0.95, 0.95),
        # legend.justification = c("right", "top"),
        # legend.box.just = "right",
        # legend.margin = margin(6, 6, 6, 6),
        # legend.key = element_rect(fill = "white"))

# lifetime infections
banana_boat2$Vac_Off <- 10
banana_boat2a$Vac_Off <- 16
banana_peel <- rbind(banana_boat2,banana_boat2a)

p_li_baseline <- ggplot(data=banana_peel, aes(x=Vac_Strategy, y=Mean_Infs, fill=as.factor(Vac_Off))) +
  geom_bar(stat="identity", color = "black", position=position_dodge(), width = 0.65) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2, position=position_dodge(.65)) +
  labs(x = "Vaccination Strategy", y = "Number of Lifetime Infections", fill = "Vaccination \nAge Cutoff") +
  #ylab('Number of Lifetime Infections') +
  #xlab("Vaccination Strategy") +
  scale_x_discrete(labels=c("Annual", "Biennial", "No Vaccination")) +
  scale_y_continuous(limits = c(0,3)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
p_li_baseline

ar_plots <- plot_grid(p_ar_baseline,p_ar_baseline16, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')
figure2 <- plot_grid(ar_plots,p_li_baseline, labels = c("","C"), ncol = 2, align = 'v', axis = 'l')

filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure2_alt.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
figure2
dev.off()
################################
### Figure 3 - scatter plots ###
################################


# from image files
# from_file_fig2a <- ggdraw() + draw_image("figure2a.png")
#
# from_file_fig2 <- plot_grid(from_file_fig2a, from_file_fig2b, from_file_fig2c, from_file_fig2d, labels = "AUTO", ncol = 2,
#                             align = 'v', axis = 'l')
# save figure
# png(filename = "figure2.png", width = 6, height = 6, units = "in", res = 300)
# plot(from_file_fig2)
# dev.off()


