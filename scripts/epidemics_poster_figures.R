### Epidemics poster figures ###

#######################################
setwd("~/Dropbox/Kylie/Presentations/Epidemics 2019")

# Figure 1 - histograms of difference for off at 10 (a) & 16 (b)
# a)
p1a <- ggplot(data = banana_split2, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram(bins = 50, show.legend = FALSE) +
  scale_fill_manual(values = c("#F8766D","#00BA38","gray85")) +
  labs(x = "Difference", y = "Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

p1b <- ggplot(data = banana_split2, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram(bins = 50, show.legend = FALSE) +
  scale_fill_manual(values = c("#F8766D","#00BA38","gray85")) +
  labs(x = "Difference", y = "Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

theme_set(theme_cowplot(font_size=10)) # reduce default font size
p1 <- plot_grid(p1a, p1b, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

png(file = "figure1.png", width = 6, height = 4,
    units = "in", pointsize = 8, res = 300)
p1
dev.off()

# Figure 2 - multi-panel with:
# a) off at 10: scatter plots of all mean diff by epsilon & VE
# b) off at 10: scatter plot of VE x Epsilon with only sig diff points
# c) off at 16: scatter plots of all mean diff by epsilon & VE
# d) off at 16: scatter plot of VE x Epsilon with only sig diff points

# a)
p2a1 <- ggplot(banana_split2, aes(x = Epsilon, y = Mean_Diff,color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference',values = c("#F8766D","#00BA38","gray85")) +
  xlab("Exposure Penalty") +
  ylab('Difference') +
  theme(legend.position = "none")

p1a2 <- ggplot(banana_split2, aes(x = VE, y = Mean_Diff,color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference',labels = c("<0",">0","0"),
                     values = c("#F8766D","#00BA38","gray85")) +
  xlab("Vaccine Effectiveness") +
  ylab('Difference') +
  theme(legend.position = "none")

p2a <- plot_grid(p2a1, p2a2, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

# b)
# subset only points with CIs that don't include 0
non_zeros <- banana_split2 %>% filter(Diff_Color != "zero") %>% mutate(Abs_Val = abs(Mean_Diff))

p2b <- ggplot(data = non_zeros, aes(x = Epsilon, y = VE, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val)) +
  scale_size_continuous(range = point_range, name = "|Difference|") +
  scale_color_manual(name = "Difference", values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') +
  ylab('VaccineEffectiveness') +
  theme(legend.position = "bottom")

# d)
p2c1 <- ggplot(banana_split2, aes(x = Epsilon, y = Mean_Diff,color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference',values = c("#F8766D","#00BA38","gray85")) +
  xlab("Exposure Penalty") +
  ylab('Difference') +
  theme(legend.position = "none")

p1c2 <- ggplot(banana_split2, aes(x = VE, y = Mean_Diff,color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference',labels = c("<0",">0","0"),
                     values = c("#F8766D","#00BA38","gray85")) +
  xlab("Vaccine Effectiveness") +
  ylab('Difference') +
  theme(legend.position = "none")

p2c <- plot_grid(p2c1, p2c2, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

# d)
# subset only points with CIs that don't include 0
non_zeros <- banana_split2 %>% filter(Diff_Color != "zero") %>% mutate(Abs_Val = abs(Mean_Diff))

p2d <- ggplot(data = non_zeros, aes(x = Epsilon, y = VE, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val)) +
  scale_size_continuous(range = point_range, name = "|Difference|") +
  scale_color_manual(name = "Difference", values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') +
  ylab('VaccineEffectiveness') +
  theme(legend.position = "bottom")

theme_set(theme_cowplot(font_size=10)) # reduce default font size
p1bc <- plot_grid(p1b,p1c, ncol = 1, align = 'v', axis = 'l')
p1 <- plot_grid(p1a, p1bc, p1d, labels = "AUTO", ncol = 3, align = 'v', axis = 'l')

png(file = "figure1.png", width = 10, height = 4,
    units = "in", pointsize = 8, res = 300)
p1
dev.off()

# Figure 3 - multi-panel of fully vac indiv
# a)
p3a <- ggplot(data = banana_pancake2, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram(bins = 50, show.legend = FALSE) +
  scale_fill_manual(values = c("#F8766D","#00BA38","gray85")) +
  labs(x = "Difference", y = "Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
#b)
p3b <- ggplot(data = banana_split2, aes(Mean_Diff, fill = Diff_Color)) +
  geom_histogram(bins = 50, show.legend = FALSE) +
  scale_fill_manual(values = c("#F8766D","#00BA38","gray85")) +
  labs(x = "Difference", y = "Frequency") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
#c)
non_zeros2 <- banana_pancake2 %>% filter(Diff_Color != "zero") %>% mutate(Abs_Val = abs(Mean_Diff))
p3c <- ggplot(data = non_zeros2, aes(x = Epsilon, y = VE, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val)) +
  scale_size_continuous(range = point_range, name = "|Difference|") +
  scale_color_manual(name = "Difference", values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') +
  ylab('VaccineEffectiveness') +
  theme(legend.position = "bottom")

theme_set(theme_cowplot(font_size=10)) # reduce default font size
pw <- plot_grid(p3a, p3b, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

png(file = "figure3.png", width = 6, height = 4,
    units = "in", pointsize = 8, res = 300)
p3
dev.off()


