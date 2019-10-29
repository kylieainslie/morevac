################################
### plots for rolling cohort ###
################################
### read in output files
#ar_dat <- read.csv(file = "ar_sim_data.csv", header = TRUE)[,-1]
#li_dat <- read.csv(file = "li_sim_data.csv", header = TRUE)[,-1]
library(data.table)
setwd("~/Dropbox/Kylie/Projects/morevac_manuscript/data/attack_rates")
files1 = list.files(pattern="*.csv")
dt_ar = do.call(rbind, lapply(files1, fread))[,-1]

setwd("~/Dropbox/Kylie/Projects/morevac_manuscript/data/lifetime_infs")
files2 <- list.files(pattern="*.csv")
dt_li = do.call(rbind, lapply(files2, fread))[,-1]
dt_li2 <- cbind(dt_li[,1:2],round(dt_li[,c(6:11)],1))
# The same using `rbindlist`
#DT = rbindlist(lapply(files, fread))

### set working directory to save figures in
setwd("~/Dropbox/Kylie/Projects/morevac_manuscript/figures")

### lifetime infection plots
# gradient plot of lifetime infections exposure penalty (epsilon) and take
# heatmap (looks better)
p_heat <- ggplot(dt_li2, aes(x = Take, y = Waning, fill = Lifetime_Infs)) +
  geom_tile(alpha=0.2) +
  viridis::scale_fill_viridis()
p_heat
# gradient (raster)
p_raster <- ggplot(dt_li2, aes(x = Take, y = Waning, z = Lifetime_Infs))+
  geom_raster(aes(fill = Lifetime_Infs), alpha = 0.5) +
  scale_fill_gradient() + #low = "white", high = "blue"
  labs(fill = "Lifetime Infections")
p_raster

# bar plot with CI for number of lifetime infections by vac_strategy (binned by number of vacs)
# need to subset by different parameter values
# aggregate over all param values
aggdata <-aggregate(li_dat, by=list(li_dat$Num_Vacs,li_dat$Vac_Strategy),FUN=median, na.rm=TRUE)
p_point <- ggplot(li_dat, aes(x=Num_Vacs, y=Lifetime_Infs)) +
  geom_dotplot(
    aes(fill = Vac_Strategy, color = Vac_Strategy), trim = FALSE,
    binaxis='y', stackdir='center', dotsize = 0.8,
    position = position_dodge(0.8)
  )

### attack rate plots
dt_ar2 <- cbind(dt_ar[,1:2],round(dt_ar[,c(3:11)],2))
dt_ar3 <- cbind(dt_ar[,1:3],round(dt_ar[,c(3:11)],1))

# create new variable that is ratio of annual AR and biennial AR
dt_ar2_sub1 <- dt_ar2[dt_ar2$Vac_Strategy == "Annual",]
dt_ar2_sub2 <- dt_ar2[dt_ar2$Vac_Strategy == "Every Other Year",]

dt_ar2_sub1$Ratio <- round(dt_ar2_sub1$Attack_Rate/dt_ar2_sub2$Attack_Rate,2)

## 2 variable plots

p_heat2_vs0 <- ggplot(dt_ar3[dt_ar3$Vac_Strategy == "No Vaccination"], aes(x = Take, y = Waning, fill = Ratio)) +
  geom_tile(alpha=0.2) +
  #scale_fill_gradient(breaks=seq(0,0.2, by=0.05)) +
  #scale_fill_continuous(limits=c(0, 0.2), breaks=seq(0,0.2,by=0.05)) +
  viridis::scale_fill_viridis() +
  facet_wrap(~Age) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

p_heat2_vs1 <- ggplot(dt_ar3[dt_ar3$Vac_Strategy == "Annual"], aes(x = VE, y = Vac_Cov, fill = Attack_Rate)) +
  geom_tile(alpha=0.2) +
  viridis::scale_fill_viridis() +
  facet_wrap(~Age) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
p_heat2_vs1

p_heat2_vs2 <- ggplot(dt_ar3[dt_ar3$Vac_Strategy == "Every Other Year"], aes(x = Take, y = Waning, fill = Attack_Rate)) +
  geom_tile(alpha=0.2) +
  viridis::scale_fill_viridis() +
  facet_wrap(~Age) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())


theme_set(theme_cowplot(font_size=10)) # reduce default font size
p_heat2_all <- plot_grid(p_heat2_vs1, p_heat2_vs2,labels = "AUTO", ncol = 1, align = 'v', axis = 'l')


png(file = "ar_take_vs_waning_by_age.png")
p_heat2_all
dev.off()

# plot ratio
# lower resolution
dt_ar3_sub <- cbind(dt_ar3[dt_ar3$Vac_Strategy == "Annual",], Ratio = dt_ar2_sub1$Ratio)

p_heat_ratio <- ggplot(dt_ar3_sub, aes(x = VE, y = Waning, fill = Ratio)) +
  geom_tile(alpha=0.2) +
  viridis::scale_fill_viridis() +
  facet_wrap(~Age) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
p_heat_ratio

## single variable plots
y_max <- 0.3
legend_x <- 0.95
legend_y <- 0.95

p1 <- ggplot(data = dt_ar2, aes(x = VE, y = Attack_Rate, colour= Vac_Strategy)) +
  geom_point() +
  #geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2)+
  xlab('VE') +
  ylab('Attack Rate') +
  scale_y_continuous(limits = c(0,y_max), expand = c(0,0)) +
  facet_wrap(~Age)
p1

p1 <- p1 + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 legend.position = c(legend_x, legend_y),
                 legend.justification = c("right", "top"),
                 legend.box.just = "right",
                 legend.margin = margin(6, 6, 6, 6),
                 legend.key = element_rect(fill = "white"))
p1

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
