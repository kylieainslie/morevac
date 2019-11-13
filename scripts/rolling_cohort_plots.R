################################
### lifetime infection plots ###
################################
### read in output files
library(data.table)
library(ggExtra)

setwd("~/Dropbox/Kylie/Projects/Morevac/data/lifetime_infs/mean/vac_off_at_16_sim10")
files2 <- list.files(pattern="*.csv")
dt_li = do.call(rbind, lapply(files2, fread))[,-1]

setwd("~/Dropbox/Kylie/Projects/Morevac/data/lifetime_infs/mean/vac_off_at_10_sim10")
files2 <- list.files(pattern="*.csv")
dt_li2 = do.call(rbind, lapply(files2, fread))[,-1]

### set working directory to save figures in
#setwd("~/Dropbox/Kylie/Projects/Morevac/figures")

setwd("~/Dropbox/Kylie/Projects/Morevac/figures/lifetime_infections/")

#dt_li2 <- cbind(dt_li[,1:2],round(dt_li[,c(6:11)],2))
#dt_li[,-c(1,2)] <- round(dt_li[,-c(1,2)],2)

### characterising difference in lifetime infections between annual and every other year

diff_sub <- dt_li[dt_li$Vac_Strategy == "Annual",]
diff_sub2 <- dt_li2[dt_li2$Vac_Strategy == "Annual",]

# color by diff conf interval values (<0 blue, 0 black, >0 green)
diff_sub$Diff_Color <- ifelse(diff_sub$Diff_Upper < 0, '<0',
                              ifelse(diff_sub$Diff_Lower <=0 & diff_sub$Diff_Upper >=0, 'zero',
                                     ifelse(diff_sub$Diff_Lower >0, '>0', 'something else')))

diff_sub2$Diff_Color <- ifelse(diff_sub2$Diff_Upper < 0, '<0',
                              ifelse(diff_sub2$Diff_Lower <=0 & diff_sub2$Diff_Upper >=0, 'zero',
                                     ifelse(diff_sub2$Diff_Lower >0, '>0', 'something else')))

p_hist2 <- ggplot(data = diff_sub2, aes(Diff, fill = cut(Diff,100))) +
          geom_histogram(bins = 30, show.legend = FALSE) +
          scale_fill_viridis_d() +
          labs(x = "Difference", y = "Frequency") +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(colour = "black"))
theme_set(theme_cowplot(font_size=10)) # reduce default font size
p_hists <- plot_grid(p_hist2,p_hist, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

png(file = "li_diff_hists.png", width = 10, height = 8,
    units = "in", pointsize = 8, res = 300)
p_hists
dev.off()

 # A) off at 10, B) off at 16
p_hists
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

###
p_nv <- ggplot(dt_li2, aes(x = VE, y = Lifetime_Infs, color = Vac_Strategy)) +
          geom_point() +
          geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
          #viridis::scale_color_viridis() +
          xlab('Vaccine Effectiveness') +
          facet_wrap(~Num_Vacs)
p_nv

### scatter plot with marginal histogram
p2 <- ggplot(diff_sub2, aes(x = Epsilon, y = Diff,color = Diff_Color)) +
      geom_point() +
      geom_errorbar(aes(ymin = Diff_Lower, ymax = Diff_Upper)) +
      geom_hline(yintercept = 0, color = "black") +
      xlab("Exposure Penalty") +
      theme(legend.position = "bottom")
p_marg2 <- ggExtra::ggMarginal(p2, type = "histogram", margins = "y")

theme_set(theme_cowplot(font_size=10)) # reduce default font size
p_marges <- plot_grid(p_marg2, p_marg, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

png(file = "li_diff_w_marg_epsilon.png", width = 10, height = 8,
    units = "in", pointsize = 8, res = 300)
p_marges
dev.off()

### bivariate plots
# bivar_plot <- function(my_dat, my_x, my_y, my_col, my_xlab = NULL, my_ylab = NULL){
#   p <- ggplot(data = my_dat, aes(x = my_x, y = my_y, color = my_col)) +
#     geom_point() +
#     scale_color_manual(values = c("#F8766D","#00BA38"))
#     if(!is.null(my_xlab)){p <- p + xlab(my_xlab)}
#     if(!is.null(my_ylab)){p <- p + ylab(my_ylab)}
#   return(p)
# }
# subset only points with CIs that don't include 0
non_zeros <- diff_sub[diff_sub$Diff_Color != "zero",]
point_range <- c(min(abs(non_zeros$Diff)), max(abs(non_zeros$Diff)))*5
# deterimine colors
# library(scales)
# show_col(hue_pal()(3)

p_epsilon_ve <- ggplot(data = non_zeros, aes(x = Epsilon, y = VE, color = Diff_Color)) +
  geom_point() + # aes(size = Diff)
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  xlab('Exposure Penalty')
p_epsilon_waning <- ggplot(data = non_zeros, aes(x = Epsilon, y = Waning, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  xlab('Exposure Penalty')
p_epsilon_take <- ggplot(data = non_zeros, aes(x = Epsilon, y = Take, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  xlab('Exposure Penalty')
p_epsilon_rho <- ggplot(data = non_zeros, aes(x = Epsilon, y = Rho, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  xlab('Exposure Penalty') +
  ylab("Correlation of Vaccination")
p_epsilon_vaccov <- ggplot(data = non_zeros, aes(x = Epsilon, y = Vac_Cov, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  xlab('Exposure Penalty') +
  ylab("Vaccination Coverage")
p_ve_waning <- ggplot(data = non_zeros, aes(x = VE, y = Waning, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38"))
  #scale_size_continuous(range = point_range)
p_ve_take <- ggplot(data = non_zeros, aes(x = VE, y = Take, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38"))
  #scale_size_continuous(range = point_range)
p_ve_rho <- ggplot(data = non_zeros, aes(x = VE, y = Rho, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  ylab('Correlation of Vaccination')
p_ve_vaccov <- ggplot(data = non_zeros, aes(x = VE, y = Vac_Cov, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  ylab('Vaccination Coverage')
p_waning_take <- ggplot(data = non_zeros, aes(x = Waning, y = Take, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38"))
  #scale_size_continuous(range = point_range)
p_waning_rho <- ggplot(data = non_zeros, aes(x = Waning, y = Rho, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  ylab("Correlation of Vaccination")
p_waning_vaccov <- ggplot(data = non_zeros, aes(x = Waning, y = Vac_Cov, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  ylab("Vaccination Coverage")
p_take_rho <- ggplot(data = non_zeros, aes(x = Take, y = Rho, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  ylab("Correlation of Vaccination")
p_take_vaccov <- ggplot(data = non_zeros, aes(x = Take, y = Vac_Cov, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  ylab("Vaccination Coverage")
p_rho_vaccov <- ggplot(data = non_zeros, aes(x = Rho, y = Vac_Cov, color = Diff_Color)) +
  geom_point() +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  #scale_size_continuous(range = point_range) +
  xlab("Correlation of Vaccination") +
  ylab("Vaccination Coverage")

theme_set(theme_cowplot(font_size=10)) # reduce default font size
p_bivar_all <- plot_grid(p_epsilon_ve, p_epsilon_waning, p_epsilon_take,p_epsilon_vaccov, p_epsilon_rho,
                    p_ve_waning, p_ve_take, p_ve_rho, p_ve_vaccov, p_waning_take, p_waning_rho,
                    p_waning_vaccov, p_take_rho, p_take_vaccov, p_rho_vaccov,
                    labels = "AUTO", ncol = 3, align = 'v', axis = 'l')
#p_bivar_all
png(file = "li_diff_bivar_all.png", width = 8, height = 10,
    units = "in", pointsize = 8, res = 300)
p_bivar_all
dev.off()

# 2-panel plot
p_hist <- ggplot(data = diff_sub, aes(Diff, fill = Diff_Color)) +
  geom_histogram(bins = 50, show.legend = FALSE) +
  scale_fill_manual(values = c("#F8766D","#00BA38","gray73")) +
  labs(x = "Difference", y = "Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

p_epsilon_ve <- ggplot(data = non_zeros, aes(x = Epsilon, y = VE, color = Diff_Color)) +
  geom_point(aes(size = Diff)) +
  scale_color_manual(values = c("#F8766D","#00BA38")) +
  scale_size_continuous(range = point_range) +
  xlab('Exposure Penalty')

theme_set(theme_cowplot(font_size=10)) # reduce default font size
p_panel <- plot_grid(p_hist, p_epsilon_ve,
                         labels = "AUTO", ncol = 2, align = 'v', axis = 'l')

png(file = "li_diff_2-panel.png", width = 10, height = 8,
    units = "in", pointsize = 8, res = 300)
p_panel
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

