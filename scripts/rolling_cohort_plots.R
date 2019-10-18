################################
### plots for rolling cohort ###
################################
# read in output files
ar_dat <- read.csv(file = "ar_sim_data.csv", header = TRUE)[,-1]
li_dat <- read.csv(file = "li_sim_data.csv", header = TRUE)[,-1]
avg_li_dat <- read.csv(file = "li_avg_sim_data.csv", header = TRUE)[,-1]
# subset data

li_sub <- subset(avg_li_dat, avg_li_dat$Vac_Strategy == "Annual" & avg_li_dat$Num_Vacs %in% (1:5))
li_sub$mean <- round(li_sub$mean, digits = 2)
li_sub$Take <- round(li_sub$Take, digits = 1)
li_sub$Epsilon <- round(li_sub$Epsilon, digits = 2)

# gradient plot of lifetime infections exposure penalty (epsilon) and take
# heatmap (looks better)
p_heat <- ggplot(li_sub, aes(x = Take, y = Epsilon, fill = mean)) +
  geom_tile(alpha=0.2) +
  scale_fill_viridis()
p_heat
# gradient (raster)
p_raster <- ggplot(li_sub, aes(x = Take, y = Epsilon, z = mean))+
  geom_raster(aes(fill = mean), alpha = 0.5) +
  scale_fill_gradient() + #low = "white", high = "blue"
  labs(fill = "Lifetime Infections", y = "Exposure Penalty")
p_raster

# plot results
y_max <- 0.4
legend_x <- 0.95
legend_y <- 0.95

p1 <- ggplot(data = dat, aes(x = Age, y = Attack_Rate, colour= Vac_Strategy)) +
  geom_line() +
  geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2)+
  xlab('Age') +
  ylab('Attack Rate') +
  scale_y_continuous(limits = c(0,y_max), expand = c(0,0))

p1 <- p1 + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 legend.position = c(legend_x, legend_y),
                 legend.justification = c("right", "top"),
                 legend.box.just = "right",
                 legend.margin = margin(6, 6, 6, 6),
                 legend.key = element_rect(fill = "white"))

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
