##################################
### Figure 0 - Model schematic ###
##################################
# run multi-annual model
max_age = 80
vac_cut_off <- 10
my_years <- 1918:2028
vac_cov_dat <- data.frame(Age = 0:(max_age-1), No_Vac = numeric(max_age), Annual = numeric(max_age), Biennial = numeric(max_age))
vac_cov_dat$Annual[3:(vac_cut_off + 1)] <- 0.44
vac_cov_dat$Biennial[seq(3,vac_cut_off+1,2)] <- 0.44

out <- multiannual(n=30000, years = my_years, max_age = max_age, betas = c(0.4,rep(0.2,length(my_years)-1)),
                   vac_coverage = vac_cov_dat$Annual, vac_strategy = 1)

#my_cohorts <- get_cohorts(inf_history = out$inf_history$inf_hist_mat, vac_history = out$vac_history$vac_hist_mat, ages = out$ages, total_year_range = my_years)
# pick random subset of people susceptibility
birth_cohort <- which(out$ages[,"year_2020"] == 0)
indivs <- sample(birth_cohort, 9, replace = FALSE)
person <- data.frame(Year = 2000:2019,
                     inf_hist = indivs$inf_hist[i,],
                     vac_hist = indivs$vac_hist[i,],
                     suscept  = bc_suscept[i,])
vax <- person$Year[which(person$vac_hist == 1)]
infections <- person$Year[which(person$inf_hist == 1)]
person
# drift plot
drift_dat <- data.frame(Year = 2000:2019, Drift = out$drift$y[181:200], Vac_Update = out$vac_update[181:200],
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
