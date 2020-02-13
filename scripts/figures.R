### Manuscript figures ###

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

######################################
### Figure 1 - Annual vs No Vac AR ###
######################################
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/")
#setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
banana_boat <- vroom(file = "banana_boat.csv", delim = ",", col_names = TRUE) %>%
  mutate(Diff_Color = ifelse(Upper < 0, '<0',ifelse(Lower <=0 & Upper >=0, '0',ifelse(Lower >0, '>0', 'something else'))))
chocolate_sundae <- vroom(file = "chocolate_sundae.csv", delim = ",", col_names = TRUE)

### baseline conditions
chocolate_sundae1 <- chocolate_sundae %>% filter(Vac_Strategy != "Biennial")
banana_boat1 <- banana_boat %>% filter(Vac_Strategy != "Biennial")
p1_ar_baseline <- ggplot(data = chocolate_sundae1, aes(x = Age, y = Mean_AR, colour= Vac_Strategy)) +
  geom_line() + geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
  xlab("Age (years)") + ylab("Attack Rate") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key = element_rect(fill = "white"))

p1_li_baseline <- ggplot(data = banana_boat1, aes(x = Vac_Strategy, y = Mean_Infs, fill= Vac_Strategy)) +
  geom_bar(stat="identity", color = "black", position = position_dodge(), width = 0.65) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2, position = position_dodge(.65)) +
  labs(x = "Vaccination Strategy", y = "Number of Lifetime Infections", fill = "Vaccination \nAge Cutoff") +
  scale_x_discrete(labels = c("Annual", "Biennial", "No Vaccination")) +
  scale_y_continuous(limits = c(0,3)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

figure1a <- plot_grid(p1_ar_baseline,p1_li_baseline, labels = "AUTO", ncol = 2, align = 'v', axis = 'l')
# filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure1a.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
figure1a
dev.off()
### read in summarised and bootstrapped data
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/cutoff10/")
#setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
banana_cream_pie <- vroom(file = "banana_cream_pie.csv", delim = ",", col_names = TRUE) %>%
                      mutate(Diff_Color = ifelse(Upper < 0, '<0',ifelse(Lower <=0 & Upper >=0, '0',ifelse(Lower >0, '>0', 'something else'))))
banana_bread <- vroom(file = "banana_bread.csv", delim = ",", col_names = TRUE)
chocolate_surprise <-  vroom(file = "chocolate_surprise.csv", delim = ",", col_names = TRUE)

### Scatter plots of difference between annual and no vaccination
banana_hammock_an <- banana_cream_pie %>% filter(Type == "Diff_AN", Diff_Color != '0') %>% mutate(Abs_Val = abs(Mean_Diff))

p1_scatter <- ggplot(data = banana_hammock_an, aes(x = exposure_penalty, y = vac_protect, color = Diff_Color)) +
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
# filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure1b.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
p1_scatter
dev.off()

### potential next figure or supplemental figure
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/")
#setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
chocolate_surprise <- vroom(file = "chocolate_surprise_baseline.csv", delim = ",", col_names = TRUE)
banana_bread <- vroom(file = "banana_bread_baseline.csv", delim = ",", col_names = TRUE)
### AR for different values of epsilon
chocolate_surprise2 <- chocolate_surprise %>% filter(Vac_Strategy != "Biennial") %>%  select(-c(n_sim, n_indiv, max_age, start_year, end_year, pandemic_beta, epidemic_beta)) %>%
                          filter(exposure_penalty %in% c(0, 0.01, 0.03, 0.05, 0.08, 0.1) & vac_protect == 0.7 & Param_Index != 18)
figure1c <- ggplot(data = chocolate_surprise2, aes(x = Age, y = Mean_AR, colour= Vac_Strategy)) +
  geom_line() + geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
  xlab("Age (years)") + ylab("Attack Rate") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.95, 0.15),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~exposure_penalty, nrow=3)

filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure1c_vert.png"), width = 8, height = 12,
    units = "in", pointsize = 8, res = 300)
figure1c
dev.off()

# add inset for number of childhood infections by vac_strategy
banana_bread2 <- banana_bread %>% filter(Vac_Strategy != "Biennial") %>%  select(-c(n_sim, n_indiv, max_age, start_year, end_year, pandemic_beta, epidemic_beta)) %>%
  filter(exposure_penalty %in% c(0, 0.01, 0.03, 0.05, 0.08, 0.1) & vac_protect == 0.7 & Param_Index != 18)

figure1c_bars <- ggplot(data = banana_bread2, aes(x = as.factor(exposure_penalty), y = Mean_Infs, fill= Vac_Strategy)) +
  geom_bar(stat="identity", color = "black", position = position_dodge(), width = 0.65) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2, position = position_dodge(.65)) +
  labs(x = "Exposure Penalty", y = "Number of Childhood Infections", fill = "Vaccination Strategy") +
  scale_y_continuous(limits = c(0,3)) +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure1c_bars.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
figure1c_bars
dev.off()


# get_inset <- function(df){
#   p <- ggplot(data=df, aes(x=Vac_Strategy, y = Mean_Infs, fill=Vac_Strategy)) +
#     geom_bar(stat="identity", color = "black", position = position_dodge(), width = 0.65) +
#     geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2, position = position_dodge(.65)) +
#     scale_x_discrete(drop=FALSE) +
#     guides(fill=FALSE) +
#     theme_bw(base_size=9) +  ## makes everything smaller
#     theme(panel.background = element_rect(fill="white"),  ## white plot background
#           axis.title.y = element_blank(),
#           axis.title.x = element_blank(),
#           axis.text.x = element_text(size=rel(0.7)), ## tiny axis text
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           plot.background = element_blank())
#   return(p)
# }
# #inset_plot <- get_inset(banana_bread2)
#
# annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data)
# {
#   layer(data = data, stat = StatIdentity, position = PositionIdentity,
#         geom = ggplot2:::GeomCustomAnn,
#         inherit.aes = TRUE, params = list(grob = grob,
#                                           xmin = xmin, xmax = xmax,
#                                           ymin = ymin, ymax = ymax))
# }
#
# insets <- banana_bread2 %>%
#   split(f = .$exposure_penalty) %>%
#   purrr::map(~annotation_custom2(
#     grob = ggplotGrob(get_inset(.) +
#                         scale_y_continuous(limits=c(0,105), breaks = c(0, 50, 100))),
#     data = data.frame(category=unique(.$exposure_penalty)),
#     ymin = -0.5, ymax=0.2, xmin=10, xmax=18)
#   )
#
#
# # add insets to main plot
# figure1c + insets

###################################
### Figure 2 - baseline results ###
###################################
# baseline figures: wane = 100%,
# take = 100%, vac_protect = 70%,
# exposure_penalty = 0%, rho = 90%
###################################
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/")
#setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
banana_boat <- vroom(file = "banana_boat.csv", delim = ",", col_names = TRUE) %>%
                      mutate(Diff_Color = ifelse(Upper < 0, '<0',ifelse(Lower <=0 & Upper >=0, '0',ifelse(Lower >0, '>0', 'something else'))))
chocolate_sundae <- vroom(file = "chocolate_sundae.csv", delim = ",", col_names = TRUE)

### AR, vac off at 10
p_ar_baseline <- ggplot(data = chocolate_sundae, aes(x = Age, y = Mean_AR, colour= Vac_Strategy)) +
  geom_line() + geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
  xlab("Age (years)") + ylab("Attack Rate") +
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
# p_ar_baseline16 <- ggplot(data = chocolate_sundae2, aes(x = Age, y = Mean_AR, colour = Vac_Strategy)) +
#   geom_line() +
#   geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
#   xlab("Age (years)") +
#   ylab("Attack Rate") +
#   #labs(x = "Age (years)", y = "Attack Rate", colour = "Vaccination Strategy") +
#   # scale_fill_discrete(name="Vaccination Strategy",
#   #                     labels=c("Annual", "Biennial", "No Vaccination")) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"),
#         legend.position = "none")
#         # legend.position = c(0.95, 0.95),
#         # legend.justification = c("right", "top"),
#         # legend.box.just = "right",
#         # legend.margin = margin(6, 6, 6, 6),
#         # legend.key = element_rect(fill = "white"))

### lifetime infections
# banana_boat2a$Vac_Off <- 16
# banana_peel <- rbind(banana_boat2,banana_boat2a)
p_li_baseline <- ggplot(data=banana_boat, aes(x=Vac_Strategy, y=Mean_Infs, fill=Vac_Strategy)) +
  geom_bar(stat="identity", color = "black", position=position_dodge(), width = 0.65) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2, position=position_dodge(.65)) +
  labs(x = "Vaccination Strategy", y = "Number of Lifetime Infections", fill = "Vaccination \nAge Cutoff") +
  scale_x_discrete(labels=c("Annual", "Biennial", "No Vaccination")) +
  scale_y_continuous(limits = c(0,3)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

#ar_plots <- plot_grid(p_ar_baseline,p_ar_baseline16, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')
figure2 <- plot_grid(p_ar_baseline,p_li_baseline, labels = "AUTO", ncol = 2, align = 'v', axis = 'l')

# filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure2.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
figure2
dev.off()

### AR for different values of epsilon
chocolate_surprise3 <- chocolate_surprise %>% select(-c(n_sim, n_indiv, max_age, start_year, end_year, pandemic_beta, epidemic_beta)) %>%
                          filter(exposure_penalty %in% c(0, 0.01, 0.03, 0.05, 0.08, 0.1) & vac_protect == 0.7 & Param_Index != 18)
figure2c <- ggplot(data = chocolate_surprise3, aes(x = Age, y = Mean_AR, colour= Vac_Strategy)) +
  geom_line() + geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
  xlab("Age (years)") + ylab("Attack Rate") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.95, 0.15),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key = element_rect(fill = "white")) +
  facet_wrap(~exposure_penalty)

filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure2c.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
figure2c
dev.off()

# add inset for number of childhood infections by vac_strategy
banana_bread3 <- banana_bread %>% select(-c(n_sim, n_indiv, max_age, start_year, end_year, pandemic_beta, epidemic_beta)) %>%
  filter(exposure_penalty %in% c(0, 0.01, 0.03, 0.05, 0.08, 0.1) & vac_protect == 0.7 & Param_Index != 18)

figure2c_bars <- ggplot(data = banana_bread3, aes(x = as.factor(exposure_penalty), y = Mean_Infs, fill= Vac_Strategy)) +
  geom_bar(stat="identity", color = "black", position = position_dodge(), width = 0.65) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width=.2, position = position_dodge(.65)) +
  labs(x = "Exposure Penalty", y = "Number of Childhood Infections", fill = "Vaccination Strategy") +
  scale_y_continuous(limits = c(0,3)) +
  theme(legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure2c_bars.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
figure2c_bars
dev.off()
################################
### Figure 3 - scatter plots ###
################################
# only vac cutoff 10
################################
# a) cutoff = 10: scatter plots of all mean diff by exposure_penalty & vac_protect
banana_hammock_zero <- banana_cream_pie %>% filter(Type == "Diff_AB", Diff_Color == '0')
banana_hammock <- banana_cream_pie %>% filter(Type == "Diff_AB", Diff_Color != '0') %>% mutate(Abs_Val = abs(Mean_Diff))

p3a <- ggplot(banana_hammock, aes(x = exposure_penalty, y = Mean_Diff, color = Diff_Color)) +
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

p3b <- ggplot(banana_hammock, aes(x = vac_protect, y = Mean_Diff, color = Diff_Color)) +
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

p3_left <- plot_grid(p3a, p3b, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')
legend_b <- get_legend(p3a + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))
p3_left2 <- plot_grid(p3_left, legend_b, ncol = 1, rel_heights = c(1, .1))

# b) cutoff = 10: scatter plot of vac_protect x exposure_penalty with only sig diff points
p3c <- ggplot(data = banana_hammock, aes(x = exposure_penalty, y = vac_protect, color = Diff_Color)) +
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


figure3 <- plot_grid(p3_left2, p3c, labels = c("","C"), ncol = 2, align = 'v', axis = 'l')

# filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure3.png"), width = 14, height = 10,
    units = "in", pointsize = 8, res = 300)
figure3
dev.off()

###########################################################
### Supplemental Figure 0 - all pairwaise scatter plots ###
###########################################################
# cutoff = 10, annual vs no vaccination
###########################################################
p_epsilon_ve0 <- ggplot(data = banana_hammock_an, aes(x = exposure_penalty, y = vac_protect, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38"), name = 'Difference in Childhood Infections') +
  xlab('Exposure Penalty') + ylab('Vaccine Effectiveness') +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_epsilon_waning0 <- ggplot(data = banana_hammock_an, aes(x = exposure_penalty, y = wane, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab('Waning') +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_epsilon_take0 <- ggplot(data = banana_hammock_an, aes(x = exposure_penalty, y = take, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab('Take') +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_epsilon_rho0 <- ggplot(data = banana_hammock_an, aes(x = exposure_penalty, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab("Correlation of Vaccination") +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_epsilon_vaccov0 <- ggplot(data = banana_hammock_an, aes(x = exposure_penalty, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') + ylab("Vaccination Coverage") +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_ve_waning0 <- ggplot(data = banana_hammock_an, aes(x = vac_protect, y = wane, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Waning') +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_ve_take0 <- ggplot(data = banana_hammock_an, aes(x = vac_protect, y = take, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Take') +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_ve_rho0 <- ggplot(data = banana_hammock_an, aes(x = vac_protect, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Correlation of Vaccination') +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_ve_vaccov0 <- ggplot(data = banana_hammock_an, aes(x = vac_protect, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Vaccine Effectiveness') + ylab('Vaccination Coverage') +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_waning_take0 <- ggplot(data = banana_hammock_an, aes(x = wane, y = take, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Waning') + ylab('Vaccination Coverage') +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_waning_rho0 <- ggplot(data = banana_hammock_an, aes(x = wane, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Waning') + ylab("Correlation of Vaccination") +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_waning_vaccov0 <- ggplot(data = banana_hammock_an, aes(x = wane, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Waning') + ylab("Vaccination Coverage") +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_take_rho0 <- ggplot(data = banana_hammock_an, aes(x = take, y = rho, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Take') + ylab("Correlation of Vaccination") +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_take_vaccov0 <- ggplot(data = banana_hammock_an, aes(x = take, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab('Take') + ylab("Vaccination Coverage") +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p_rho_vaccov0 <- ggplot(data = banana_hammock_an, aes(x = rho, y = vac_cov, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val), alpha = 0.7) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  xlab("Correlation of Vaccination") + ylab("Vaccination Coverage") +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
sm_figure0 <- plot_grid(p_epsilon_ve0, p_epsilon_waning0, p_epsilon_take0,p_epsilon_vaccov0, p_epsilon_rho0,
                        p_ve_waning0, p_ve_take0, p_ve_rho0, p_ve_vaccov0, p_waning_take0, p_waning_rho0,
                        p_waning_vaccov0, p_take_rho0, p_take_vaccov0, p_rho_vaccov0,
                        labels = "AUTO", ncol = 3, align = 'v', axis = 'l')

legend_b <- get_legend( p_epsilon_ve0 + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))
sm_figure0 <- plot_grid(sm_figure0, legend_b, ncol = 1, rel_heights = c(1, .1))

# filename <- "~/Dropbox/Kylie/Projects/Morevac/figures/"
filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
# filename <- "~/Dropbox/Kylie/Presentations/MRC Symposium/figures/"

png(file = paste0(filename,"SuppMatFig0.png"), width = 12, height = 16,
    units = "in", pointsize = 8, res = 300)
sm_figure0
dev.off()

###########################################################
### Supplemental Figure 1 - all pairwaise scatter plots ###
###########################################################
# cutoff = 10
###########################################################
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
# cutoff = 16
banana_cream_pie_16 <- vroom(file = "banana_cream_pie_16.csv", delim = ",", col_names = TRUE) %>%
  mutate(Diff_Color = ifelse(Upper < 0, '<0',ifelse(Lower <=0 & Upper >=0, 'zero',ifelse(Lower >0, '>0', 'something else'))))
banana_bread_16 <- vroom(file = "banana_bread_16.csv", delim = ",", col_names = TRUE)
banana_hammock_zero_16 <- banana_cream_pie_16 %>% filter(Type == "Diff_AB", Diff_Color == 'zero')
banana_hammock_16 <- banana_cream_pie_16 %>% filter(Type == "Diff_AB",Diff_Color != 'zero') %>% mutate(Abs_Val = abs(Mean_Diff))

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

#######################################################
### Supplemental Figure 3 - bivariate scatter plots ###
#######################################################
# cutoff = 16
# VE x exposure penalty
#######################################################
# c) cutoff = 16: scatter plots of all mean diff by exposure_penalty & vac_protect
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

