################################
### plots for rolling cohort ###
################################
### read in output files
library(data.table)
setwd("~/Dropbox/Kylie/Projects/Morevac/data/attack_rates")
files1 = list.files(pattern="*.csv")
dt_ar = do.call(rbind, lapply(files1, fread))[,-1]

setwd("~/Dropbox/Kylie/Projects/Morevac/data/lifetime_infs/mean/vac_off_at_16_sim10")
files2 <- list.files(pattern="*.csv")
dt_li = do.call(rbind, lapply(files2, fread))[,-1]

### set working directory to save figures in
#setwd("~/Dropbox/Kylie/Projects/Morevac/figures")

################################
### lifetime infection plots ###
################################
setwd("~/Dropbox/Kylie/Projects/Morevac/figures/lifetime_infections/vac_off_at_16/")

#dt_li2 <- cbind(dt_li[,1:2],round(dt_li[,c(6:11)],2))
#dt_li[,-c(1,2)] <- round(dt_li[,-c(1,2)],2)

### characterising difference in lifetime infections between annual and every other year
# dt_li2 <- dt_li[dt_li$Epsilon < 0.1,]
diff_sub <- dt_li[dt_li$Vac_Strategy == "Annual",]
# color by diff conf interval values (<0 blue, 0 black, >0 green)
diff_sub$Diff_Color <- ifelse(diff_sub$Diff_Upper < 0, '<0',
                              ifelse(diff_sub$Diff_Lower <=0 & diff_sub$Diff_Upper >=0, 'zero',
                                     ifelse(diff_sub$Diff_Lower >0, '>0', 'something else')))
p_hist <- ggplot(data = diff_sub, aes(Diff, fill = cut(Diff,100))) +
          geom_histogram(bins = 30, show.legend = FALSE) +
          scale_fill_viridis_d() +
          labs(x = "Difference", y = "Frequency") +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))
p_hist
### univariate scatter plots
p_ve <- ggplot(data = diff_sub, aes(x = VE, y = Diff, color = Diff)) +
  geom_point() +
  geom_errorbar(aes(ymin=Diff_Lower, ymax=Diff_Upper)) +
  viridis::scale_color_viridis() +
  #facet_grid(. ~ Vac_Strategy) +
  geom_hline(yintercept = 0, color = "black") +
  xlab('VE') +
  ylab('Difference')
p_take <- ggplot(data = diff_sub, aes(x = Take, y = Diff, color = Diff)) +
  geom_point() +
  geom_errorbar(aes(ymin=Diff_Lower, ymax=Diff_Upper)) +
  viridis::scale_color_viridis() +
  geom_hline(yintercept = 0, color = "black") +
  ylab('Difference')
p_waning <- ggplot(data = diff_sub, aes(x = Waning, y = Diff, color = Diff)) +
  geom_point() +
  geom_errorbar(aes(ymin=Diff_Lower, ymax=Diff_Upper)) +
  viridis::scale_color_viridis() +
  geom_hline(yintercept = 0, color = "black") +
  ylab('Difference')
p_rho <- ggplot(data = diff_sub, aes(x = Rho, y = Diff, color = Diff)) +
  geom_point() +
  geom_errorbar(aes(ymin=Diff_Lower, ymax=Diff_Upper)) +
  viridis::scale_color_viridis() +
  geom_hline(yintercept = 0, color = "black") +
  xlab('Correlation of Vaccination') +
  ylab('Difference')
p_epsilon <- ggplot(data = diff_sub, aes(x = Epsilon, y = Diff, color = Diff)) +
  geom_point() +
  geom_errorbar(aes(ymin=Diff_Lower, ymax=Diff_Upper)) +
  viridis::scale_color_viridis() +
  geom_hline(yintercept = 0, color = "black") +
  xlab('Exposure Penalty') +
  ylab('Difference')
p_vaccov <- ggplot(data = diff_sub, aes(x = Vac_Cov, y = Diff, color = Diff)) +
  geom_point() +
  geom_errorbar(aes(ymin=Diff_Lower, ymax=Diff_Upper)) +
  viridis::scale_color_viridis() +
  geom_hline(yintercept = 0, color = "black") +
  xlab('Vaccination Coverage') +
  ylab('Difference')

theme_set(theme_cowplot(font_size=10)) # reduce default font size
p_all <- plot_grid(p_ve, p_waning,p_epsilon,p_vaccov, p_rho, p_take, labels = "AUTO", ncol = 3, align = 'v', axis = 'l')

png(file = "li_diff_all.png", width = 10, height = 8,
    units = "in", pointsize = 8, res = 300)
p_all
dev.off()

### bivariate plots
#
p_ve_waning <- ggplot(data = diff_sub, aes(x = VE, y = Waning, color = Diff)) +
  geom_point() +
  viridis::scale_color_viridis() +
  #xlab('Exposure Penalty') +
  ylab('Waning')
p_ve_take <- ggplot(data = diff_sub, aes(x = VE, y = Take, color = Diff)) +
  geom_point() +
  viridis::scale_color_viridis() +
  #xlab('Exposure Penalty')
p_ve_epsilon <- ggplot(data = diff_sub, aes(x = VE, y = Epsilon, color = Diff)) +
  geom_point() +
  viridis::scale_color_viridis() +
  ylab('Exposure Penalty')
p_ve_rho <- ggplot(data = diff_sub, aes(x = VE, y = Rho, color = Diff)) +
  geom_point() +
  viridis::scale_color_viridis() +
  #xlab('Exposure Penalty') +
  ylab('Correlation of Vaccination')
p_ve_vaccov <- ggplot(data = diff_sub, aes(x = VE, y = Vac_Cov, color = Diff)) +
  geom_point() +
  viridis::scale_color_viridis() +
  #xlab('Exposure Penalty') +
  ylab('Vaccination Coverage')

theme_set(theme_cowplot(font_size=10)) # reduce default font size
p_all2 <- plot_grid(p_epsilon_waning, p_epsilon_ve,p_epsilon_take,p_epsilon_vaccov, p_epsilon_rho, labels = "AUTO", ncol = 3, align = 'v', axis = 'l')

png(file = "li_diff_bivariate_ve.png", width = 10, height = 8,
    units = "in", pointsize = 8, res = 300)
p_all2
dev.off()

###
p_nv <- ggplot(dt_li2, aes(x = VE, y = Lifetime_Infs, color = Vac_Strategy)) +
          geom_point() +
          geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
          #viridis::scale_color_viridis() +
          xlab('Vaccine Effectiveness') +
          facet_wrap(~Num_Vacs)
p_nv

### scatter plot with marginal histogram
p <- ggplot(diff_sub, aes(x = Epsilon, y = Diff,color = Diff_Color)) +
      geom_point() +
      geom_errorbar(aes(ymin = Diff_Lower, ymax = Diff_Upper)) +
      geom_hline(yintercept = 0, color = "black") +
      xlab("Exposure Penalty") +
      theme(legend.position = "bottom")
p_marg <- ggExtra::ggMarginal(p, type = "histogram", margins = "y")

png(file = "li_diff_w_marginal_epsilon.png", width = 10, height = 8,
    units = "in", pointsize = 8, res = 300)
p_marg
dev.off()

### plot fully vaccinated individuals
max_vac_annual <- max(dt_li[dt_li$Vac_Strategy == "Annual",]$Num_Vacs)
max_vac_biennial <- max(dt_li[dt_li$Vac_Strategy == "Every Other Year",]$Num_Vacs)

fully_vac <- dt_li[(dt_li$Num_Vacs == max_vac_biennial & dt_li$Vac_Strategy == "Every Other Year")
                   | (dt_li$Num_Vacs == max_vac_annual & dt_li$Vac_Strategy == "Annual"),]

### bar chart
#   with CI for number of lifetime infections by vac_strategy (binned by number of vacs)
# aggregate over all param values
aggdata <-aggregate(fully_vac, by=list(fully_vac$Num_Vacs,fully_vac$Vac_Strategy),FUN=mean, na.rm=TRUE)
p_point <- ggplot(li_dat, aes(x=Num_Vacs, y=Lifetime_Infs)) +
  geom_dotplot(
    aes(fill = Vac_Strategy, color = Vac_Strategy), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.8)
  )
# try facet_wrap()

#########################
### attack rate plots ###
#########################
# create AR difference and AR ratio variables
dt_ar_sub <- dt_ar[dt_ar$Vac_Strategy == "Annual",]
dt_ar_sub$Diff <- dt_ar[dt_ar$Vac_Strategy == "Annual",3] - dt_ar[dt_ar$Vac_Strategy == "Every Other Year",3]
dt_ar_sub$Ratio <- dt_ar[dt_ar$Vac_Strategy == "Annual",3] / dt_ar[dt_ar$Vac_Strategy == "Every Other Year",3]

### heatmap
# for better heatmap resolution, round parameter values
dt_ar2 <- cbind(dt_ar[,1:2],round(dt_ar[,c(3:11)],2)) # round to 2 decimal places
dt_ar3 <- cbind(dt_ar[,1:3],round(dt_ar[,c(3:11)],1)) # round to 1 decimal place

p_heat2 <- ggplot(dt_ar2[dt_ar2$Vac_Strategy!="No Vaccination"], aes(x = VE, y = Vac_Cov, fill = Attack_Rate)) +
  geom_tile(alpha=0.2) +
  viridis::scale_fill_viridis() +
  #facet_wrap(~Age) +
  facet_grid(Vac_Strategy ~ Age) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
p_heat2

png(file = "ar_heatmap_VE_vs_VacCov.png", width = 8, units = 'in')
p_heat2
dev.off()

### heatmap using ratio: annual AR / biennial AR
# create new ratio variable
p_heat_ratio <- ggplot(dt_ar2_sub1, aes(x = VE, y = Waning, fill = Ratio)) +
  geom_tile(alpha=0.2) +
  viridis::scale_fill_viridis() +
  facet_wrap(~Age) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        )
p_heat_ratio

### spaghetti plot
# create group variable for each unique set of parameter values
library(dplyr)
dt_ar <- dt_ar %>%
         mutate(ID = group_indices(., Vac_Cov, Waning,Take,Epsilon,Rho,VE),
                EpsilonGroup = cut(Epsilon, breaks = 5,
                                   labels = c("<0.1","[0.1,0.2)","[0.2,0.3)","[0.3,0.4)","[0.4,0.5)")))

p_spaghetti <- ggplot(dt_ar, aes(x = Age, y = Attack_Rate, group = ID, color = ID)) +
               geom_line() + viridis::scale_fill_viridis() +
               facet_grid(Vac_Strategy ~ EpsilonGroup) +
               theme(legend.position = "none")
p_spaghetti

png(file = "ar_spaghetti_epsilongroup2.png")
p_spaghetti
dev.off()

### univariate scatter plots
p1 <- ggplot(data = dt_ar_sub, aes(x = VE, y = Diff,color = Diff)) +
      geom_point() +
      viridis::scale_color_viridis() +
      geom_hline(yintercept = 0) +
      #xlab('VE') +
      ylab('Difference in Attack Rate') +
      #scale_x_continuous(labels = c(0,.01,.02,.03,.04,.05))+
      facet_wrap(~Age)
p1

png(file = "ar_diff_scatter_ve.png", width = 10, height = 8,
    units = "in", pointsize = 8, res = 300)
p1
dev.off()

### bivariate scatter plots
p2 <- ggplot(data = dt_ar_sub, aes(x = Rho, y = Vac_Cov, color = Diff)) +
      geom_point() +
      viridis::scale_color_viridis() +
      facet_wrap(~Age) +
      ylab("Correlation of Vaccination (Rho)") +
      labs(color = "Difference") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")
            )
p2

png(file = "ar_diff_scatter_rho+vaccov.png", width = 10, height = 8,
    units = "in", pointsize = 8, res = 300)
p2
dev.off()

### Miscallaneous
legend_x <- 0.25
legend_y <- 0.95
tmp$Num_Vacs <- factor(tmp$Num_Vacs)
p2 <- ggplot(tmp, aes(x = Num_Vacs, y = mean, fill = Vac_Strategy)) +
  geom_boxplot() +
  ylab('Number of Lifetime Infections') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = c(legend_x, legend_y),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.key = element_rect(fill = "white")
  )
p2
# theme_set(theme_cowplot(font_size=10)) # reduce default font size
# p_combined <- plot_grid(p1, p2, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

png(file = "rolling_cohort_off_at_10_sim500.png")
p1
dev.off()

# p1 <- p1 + theme(panel.grid.major = element_blank(),
#                  panel.grid.minor = element_blank(),
#                  panel.background = element_blank(),
#                  axis.line = element_line(colour = "black"),
#                  legend.position = c(legend_x, legend_y),
#                  legend.justification = c("right", "top"),
#                  legend.box.just = "right",
#                  legend.margin = margin(6, 6, 6, 6),
#                  legend.key = element_rect(fill = "white"))
# p1
