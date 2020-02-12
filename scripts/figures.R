### Manuscript figures ###

##################################
### Figure 1 - Model schematic ###
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

my_cohorts <- get_cohorts(inf_history = out$inf_history$inf_hist_mat, vac_history = out$vac_history$vac_hist_mat, ages = out$ages, total_year_range = my_years)
# pick random subset of people susceptibility
indivs <- sample(nrow(my_cohorts$inf_history), 9, replace = FALSE)
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
#     take = 100%, vac_protect = 70%,
#     exposure_penalty = 0%, rho = 90%
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
### read in summarised and bootstrapped data
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
#setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/")

# cutoff = 10
banana_cream_pie <- vroom(file = "banana_cream_pie.csv", delim = ",", col_names = TRUE) %>%
                      mutate(Diff_Color = ifelse(Upper < 0, '<0',ifelse(Lower <=0 & Upper >=0, 'zero',ifelse(Lower >0, '>0', 'something else'))))
banana_bread <- vroom(file = "banana_bread.csv", delim = ",", col_names = TRUE)
# cutoff = 16
banana_cream_pie_16 <- vroom(file = "banana_cream_pie_16.csv", delim = ",", col_names = TRUE) %>%
                          mutate(Diff_Color = ifelse(Upper < 0, '<0',ifelse(Lower <=0 & Upper >=0, 'zero',ifelse(Lower >0, '>0', 'something else'))))
banana_bread_16 <- vroom(file = "banana_bread_16.csv", delim = ",", col_names = TRUE)

# a) cutoff = 10: scatter plots of all mean diff by exposure_penalty & vac_protect
banana_hammock_zero <- banana_cream_pie %>% filter(Type == "Diff_AB", Diff_Color == 'zero')
banana_hammock <- banana_cream_pie %>% filter(Type == "Diff_AB", Diff_Color != 'zero') %>% mutate(Abs_Val = abs(Mean_Diff))

p3a1 <- ggplot(banana_hammock, aes(x = exposure_penalty, y = Mean_Diff, color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF")) +
  xlab("Exposure Penalty") +
  ylab('Difference') +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_point(data = banana_hammock_zero, aes(x = exposure_penalty, y = Mean_Diff), alpha = 0.4) +
  geom_errorbar(data = banana_hammock_zero, aes(ymin = Lower, ymax = Upper), alpha = 0.4)

p3a2 <- ggplot(banana_hammock, aes(x = vac_protect, y = Mean_Diff, color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF")) +
  xlab("Vaccine Effectiveness") +
  ylab('Difference') +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_point(data = banana_hammock_zero, aes(x = vac_protect, y = Mean_Diff), alpha = 0.4) +
  geom_errorbar(data = banana_hammock_zero, aes(ymin = Lower, ymax = Upper), alpha = 0.4)

p3a <- plot_grid(p3a1, p3a2, ncol = 1, align = 'v', axis = 'l')

# b) cutoff = 10: scatter plot of vac_protect x exposure_penalty with only sig diff points
p3b <- ggplot(data = banana_hammock, aes(x = exposure_penalty, y = vac_protect, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(name = "Difference", values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') +
  ylab('Vaccine Effectiveness') +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

# c) cutoff = 16: scatter plots of all mean diff by exposure_penalty & vac_protect
banana_hammock_zero_16 <- banana_cream_pie_16 %>% filter(Type == "Diff_AB", Diff_Color == 'zero')
banana_hammock_16 <- banana_cream_pie_16 %>% filter(Type == "Diff_AB",Diff_Color != 'zero') %>% mutate(Abs_Val = abs(Mean_Diff))

p3c1 <- ggplot(banana_hammock_16, aes(x = exposure_penalty, y = Mean_Diff, color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF")) +
  xlab("Exposure Penalty") +
  ylab('Difference') +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_point(data = banana_hammock_zero_16, aes(x = exposure_penalty, y = Mean_Diff), alpha = 0.4) +
  geom_errorbar(data = banana_hammock_zero_16, aes(ymin = Lower, ymax = Upper), alpha = 0.4)

p3c2 <- ggplot(banana_hammock_16, aes(x = vac_protect, y = Mean_Diff, color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF")) +
  xlab("Vaccine Effectiveness") +
  ylab('Difference') +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_point(data = banana_hammock_zero_16, aes(x = vac_protect, y = Mean_Diff), alpha = 0.4) +
  geom_errorbar(data = banana_hammock_zero_16, aes(ymin = Lower, ymax = Upper), alpha = 0.4)

p3c <- plot_grid(p3c1, p3c2, ncol = 1, align = 'v', axis = 'l')

# d) cutoff = 16: scatter plot of vac_protect x exposure_penalty with only sig diff points
p3d <- ggplot(data = banana_hammock_16, aes(x = exposure_penalty, y = vac_protect, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(name = "Difference", values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') +
  ylab('VaccineEffectiveness') +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

figure3 <- plot_grid(p3a, p3b, p3c, p3d, labels = "AUTO", ncol = 2, align = 'v', axis = 'l')

filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure3.png"), width = 14, height = 10,
    units = "in", pointsize = 8, res = 300)
figure3
dev.off()

################################
### Histogram of differences ###
################################

### cutoff = 10
banana_cream_pie_ab <- banana_cream_pie %>% filter(Type == "Diff_AB")
banana_cream_pie_an <- banana_cream_pie %>% filter(Type == "Diff_AN")
banana_cream_pie_bn <- banana_cream_pie %>% filter(Type == "Diff_BN")


hist_ab <- ggplot(data = banana_cream_pie_ab, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram() +
  scale_fill_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF"), labels = c('<0', '>0', '0')) +
  labs(x = "Difference", y = "Frequency", title = "Annual vs Biennial") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 14, face = 'bold'))

hist_an <- ggplot(data = banana_cream_pie_an, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram() +
  scale_fill_manual(name = 'Difference', values = c("#F8766D", "#619CFF","#00BA38"), labels = c('<0', '0', '>0')) +
  labs(x = "Difference", y = "Frequency", title = "Annual vs No Vaccination") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 14, face = 'bold'))

hist_bn <- ggplot(data = banana_cream_pie_bn, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram() +
  scale_fill_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF"), labels = c('<0', '>0', '0')) +
  labs(x = "Difference", y = "Frequency", title = "Biennial vs No Vaccination") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 14, face = 'bold'))


hists <- plot_grid(hist_ab, hist_an, hist_bn, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

legend_b <- get_legend(
  hist_ab +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
hists <- plot_grid(hists, legend_b, ncol = 1, rel_heights = c(1, .1))

filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"histograms.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
hists
dev.off()

### cutoff = 10
banana_cream_pie_ab <- banana_cream_pie %>% filter(Type == "Diff_AB")
banana_cream_pie_an <- banana_cream_pie %>% filter(Type == "Diff_AN")
banana_cream_pie_bn <- banana_cream_pie %>% filter(Type == "Diff_BN")


hist_ab <- ggplot(data = banana_cream_pie_ab, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram() +
  scale_fill_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF"), labels = c('<0', '>0', '0')) +
  labs(x = "Difference", y = "Frequency", title = "Annual vs Biennial") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 14, face = 'bold'))

hist_an <- ggplot(data = banana_cream_pie_an, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram() +
  scale_fill_manual(name = 'Difference', values = c("#F8766D", "#619CFF","#00BA38"), labels = c('<0', '0', '>0')) +
  labs(x = "Difference", y = "Frequency", title = "Annual vs No Vaccination") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 14, face = 'bold'))

hist_bn <- ggplot(data = banana_cream_pie_bn, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram() +
  scale_fill_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF"), labels = c('<0', '>0', '0')) +
  labs(x = "Difference", y = "Frequency", title = "Biennial vs No Vaccination") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 14, face = 'bold'))


hists <- plot_grid(hist_ab, hist_an, hist_bn, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

legend_b <- get_legend(
  hist_ab +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
hists <- plot_grid(hists, legend_b, ncol = 1, rel_heights = c(1, .1))

filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"histograms.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
hists
dev.off()

###########################################################
### Supplemental Figure 1 - all pairwaise scatter plots ###
###########################################################
# cutoff = 10
###########################################################
banana_cream <- banana_cream_pie %>% filter(Type == "Diff_AB") %>% mutate(Abs_Val = abs(Mean_Diff))

p_epsilon_ve <- ggplot(data = banana_hammock, aes(x = exposure_penalty, y = vac_protect, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38"), name = 'Difference in Childhood Infections') +
  xlab('Exposure Penalty') + ylab('Vaccine Effectiveness') +
  theme(legend.position = 'none')
p_epsilon_waning <- ggplot(data = banana_hammock, aes(x = exposure_penalty, y = wane, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab('Waning') +
  theme(legend.position = 'none')
p_epsilon_take <- ggplot(data = banana_hammock, aes(x = exposure_penalty, y = take, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab('Take') +
  theme(legend.position = 'none')
p_epsilon_rho <- ggplot(data = banana_hammock, aes(x = exposure_penalty, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab("Correlation of Vaccination") +
  theme(legend.position = 'none')
p_epsilon_vaccov <- ggplot(data = banana_hammock, aes(x = exposure_penalty, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab("Vaccination Coverage") +
  theme(legend.position = 'none')
p_ve_waning <- ggplot(data = banana_hammock, aes(x = vac_protect, y = wane, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Waning') +
  theme(legend.position = 'none')
p_ve_take <- ggplot(data = banana_hammock, aes(x = vac_protect, y = take, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Take') +
  theme(legend.position = 'none')
p_ve_rho <- ggplot(data = banana_hammock, aes(x = vac_protect, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Correlation of Vaccination') +
  theme(legend.position = 'none')
p_ve_vaccov <- ggplot(data = banana_hammock, aes(x = vac_protect, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Vaccination Coverage') +
  theme(legend.position = 'none')
p_waning_take <- ggplot(data = banana_hammock, aes(x = wane, y = take, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Waning') + ylab('Vaccination Coverage') +
  theme(legend.position = 'none')
p_waning_rho <- ggplot(data = banana_hammock, aes(x = wane, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Waning') + ylab("Correlation of Vaccination") +
  theme(legend.position = 'none')
p_waning_vaccov <- ggplot(data = banana_hammock, aes(x = wane, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Waning') + ylab("Vaccination Coverage") +
  theme(legend.position = 'none')
p_take_rho <- ggplot(data = banana_hammock, aes(x = take, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Take') + ylab("Correlation of Vaccination") +
  theme(legend.position = 'none')
p_take_vaccov <- ggplot(data = banana_hammock, aes(x = take, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Take') + ylab("Vaccination Coverage") +
  theme(legend.position = 'none')
p_rho_vaccov <- ggplot(data = banana_hammock, aes(x = rho, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab("Correlation of Vaccination") + ylab("Vaccination Coverage") +
  theme(legend.position = 'none')
sm_figure1 <- plot_grid(p_epsilon_ve, p_epsilon_waning, p_epsilon_take,p_epsilon_vaccov, p_epsilon_rho,
                         p_ve_waning, p_ve_take, p_ve_rho, p_ve_vaccov, p_waning_take, p_waning_rho,
                         p_waning_vaccov, p_take_rho, p_take_vaccov, p_rho_vaccov,
                         labels = "AUTO", ncol = 3, align = 'v', axis = 'l')

legend_b <- get_legend(
              p_epsilon_ve +
              guides(color = guide_legend(nrow = 1)) +
              theme(legend.position = "bottom")
            )
sm_figure1 <- plot_grid(sm_figure1, legend_b, ncol = 1, rel_heights = c(1, .1))

filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
#filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
#filename <- "~/Dropbox/Kylie/Presentations/MRC Symposium/figures/"

png(file = paste0(filename,"SuppMatFig1.png"), width = 12, height = 16,
    units = "in", pointsize = 8, res = 300)
sm_figure1
dev.off()
###########################################################
### Supplemental Figure 2 - all pairwaise scatter plots ###
###########################################################
# cutoff = 16
###########################################################
#banana_cream_16 <- banana_cream_pie_16 %>% filter(Type == "Diff_AB") %>% mutate(Abs_Val = abs(Mean_Diff))

p_epsilon_ve2 <- ggplot(data = banana_hammock_16, aes(x = exposure_penalty, y = vac_protect, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38"), name = 'Difference in Childhood Infections') +
  xlab('Exposure Penalty') + ylab('Vaccine Effectiveness') +
  theme(legend.position = 'none')
p_epsilon_waning2 <- ggplot(data = banana_hammock_16, aes(x = exposure_penalty, y = wane, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab('Waning') +
  theme(legend.position = 'none')
p_epsilon_take2 <- ggplot(data = banana_hammock_16, aes(x = exposure_penalty, y = take, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab('Take') +
  theme(legend.position = 'none')
p_epsilon_rho2 <- ggplot(data = banana_hammock_16, aes(x = exposure_penalty, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab("Correlation of Vaccination") +
  theme(legend.position = 'none')
p_epsilon_vaccov2 <- ggplot(data = banana_hammock_16, aes(x = exposure_penalty, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab("Vaccination Coverage") +
  theme(legend.position = 'none')
p_ve_waning2 <- ggplot(data = banana_hammock_16, aes(x = vac_protect, y = wane, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Waning') +
  theme(legend.position = 'none')
p_ve_take2 <- ggplot(data = banana_hammock_16, aes(x = vac_protect, y = take, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Take') +
  theme(legend.position = 'none')
p_ve_rho2 <- ggplot(data = banana_hammock_16, aes(x = vac_protect, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Correlation of Vaccination') +
  theme(legend.position = 'none')
p_ve_vaccov2 <- ggplot(data = banana_hammock_16, aes(x = vac_protect, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Vaccination Coverage') +
  theme(legend.position = 'none')
p_waning_take2 <- ggplot(data = banana_hammock_16, aes(x = wane, y = take, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Waning') + ylab('Vaccination Coverage') +
  theme(legend.position = 'none')
p_waning_rho2 <- ggplot(data = banana_hammock_16, aes(x = wane, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Waning') + ylab("Correlation of Vaccination") +
  theme(legend.position = 'none')
p_waning_vaccov2 <- ggplot(data = banana_hammock_16, aes(x = wane, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Waning') + ylab("Vaccination Coverage") +
  theme(legend.position = 'none')
p_take_rho2 <- ggplot(data = banana_hammock_16, aes(x = take, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Take') + ylab("Correlation of Vaccination") +
  theme(legend.position = 'none')
p_take_vaccov2 <- ggplot(data = banana_hammock_16, aes(x = take, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Take') + ylab("Vaccination Coverage") +
  theme(legend.position = 'none')
p_rho_vaccov2 <- ggplot(data = banana_hammock_16, aes(x = rho, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab("Correlation of Vaccination") + ylab("Vaccination Coverage") +
  theme(legend.position = 'none')
sm_figure2 <- plot_grid(p_epsilon_ve2, p_epsilon_waning2, p_epsilon_take2,p_epsilon_vaccov2, p_epsilon_rho2,
                        p_ve_waning2, p_ve_take2, p_ve_rho2, p_ve_vaccov2, p_waning_take2, p_waning_rho2,
                        p_waning_vaccov2, p_take_rho2, p_take_vaccov2, p_rho_vaccov2,
                        labels = "AUTO", ncol = 3, align = 'v', axis = 'l')

legend_b <- get_legend(
  p_epsilon_ve2 +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)
sm_figure2 <- plot_grid(sm_figure2, legend_b, ncol = 1, rel_heights = c(1, .1))

filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"SuppMatFig2.png"), width = 12, height = 16,
    units = "in", pointsize = 8, res = 300)
sm_figure2
dev.off()
