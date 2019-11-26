### Epidemics poster figures ###

#######################################
setwd("~/Dropbox/Kylie/Presentations/Epidemics 2019")

# Figure 1 - histograms of difference for off at 10 (a) & 16 (b)
# a)
p1a <- ggplot(data = banana_split2, aes(Mean_Diff, fill = Diff_Color)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF"), labels = c('<0', '>0', '0')) +
  labs(x = "Difference", y = "Frequency") +
  theme(legend.position = c(0.1, 0.9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

p1b <- ggplot(data = banana_split2, aes(Mean_Diff, fill = Diff_Color)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF"), labels = c('<0', '>0', '0')) +
  labs(x = "Difference", y = "Frequency") +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

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
banana_hammock0 <- banana_split2 %>% filter(Diff_Color == 'zero')
banana_hammock <- banana_split2 %>% filter(Diff_Color != 'zero')
# a)
p2a1 <- ggplot(banana_hammock, aes(x = Epsilon, y = Mean_Diff, color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF")) +
  xlab("Exposure Penalty") +
  ylab('Difference') +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_point(data = banana_hammock0, aes(x = Epsilon, y = Mean_Diff), alpha = 0.4) +
  geom_errorbar(data = banana_hammock0, aes(ymin = Lower, ymax = Upper), alpha = 0.4)
p2a1

p2a2 <- ggplot(banana_hammock, aes(x = VE, y = Mean_Diff, color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF")) +
  xlab("Vaccine Effectiveness") +
  ylab('Difference') +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_point(data = banana_hammock0, aes(x = VE, y = Mean_Diff), alpha = 0.4) +
  geom_errorbar(data = banana_hammock0, aes(ymin = Lower, ymax = Upper), alpha = 0.4)

p2a <- plot_grid(p2a1, p2a2, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

# b)
# subset only points with CIs that don't include 0
non_zeros <- banana_split2 %>% filter(Diff_Color != "zero") %>% mutate(Abs_Val = abs(Mean_Diff))

p2b <- ggplot(data = non_zeros, aes(x = Epsilon, y = VE, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val)) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(name = "Difference", values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') +
  ylab('VaccineEffectiveness') +
  theme(legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

# c)
# a)
p2c1 <- ggplot(banana_hammock, aes(x = Epsilon, y = Mean_Diff, color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF")) +
  xlab("Exposure Penalty") +
  ylab('Difference') +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_point(data = banana_hammock0, aes(x = Epsilon, y = Mean_Diff), alpha = 0.4) +
  geom_errorbar(data = banana_hammock0, aes(ymin = Lower, ymax = Upper), alpha = 0.4)

p2c2 <- ggplot(banana_hammock, aes(x = VE, y = Mean_Diff, color = Diff_Color)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(name = 'Difference', values = c("#F8766D","#00BA38", "#619CFF")) +
  xlab("Vaccine Effectiveness") +
  ylab('Difference') +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_point(data = banana_hammock0, aes(x = VE, y = Mean_Diff), alpha = 0.4) +
  geom_errorbar(data = banana_hammock0, aes(ymin = Lower, ymax = Upper), alpha = 0.4)

p2c <- plot_grid(p2c1, p2c2, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

# d)
# subset only points with CIs that don't include 0
non_zeros <- banana_split2 %>% filter(Diff_Color != "zero") %>% mutate(Abs_Val = abs(Mean_Diff))

p2d <- ggplot(data = non_zeros, aes(x = Epsilon, y = VE, color = Diff_Color)) +
  geom_point(aes(size = Abs_Val)) +
  scale_size_continuous(name = "|Difference|") +
  scale_color_manual(name = "Difference", values = c("#F8766D","#00BA38")) +
  xlab('Exposure Penalty') +
  ylab('VaccineEffectiveness') +
  theme(legend.position = "bottom",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12))

p2 <- plot_grid(p2a, p2b, p2c, p2d, labels = "AUTO", ncol = 2, align = 'v', axis = 'l')

png(file = "figure2.png", width = 8, height = 8,
    units = "in", pointsize = 8, res = 300)
p2
dev.off()

# Figure 3 - multi-panel of fully vac indiv
# a) off at 10: histogram of difference
# b) off at 16: histogram of difference
# a)
# p3a <- ggplot(data = banana_pancake2, aes(Mean_Diff, fill = Diff_Color)) +
#   geom_histogram(bins = 50, show.legend = FALSE) +
#   scale_fill_manual(values = c("#F8766D","#00BA38","gray85")) +
#   labs(x = "Difference", y = "Frequency") +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"))
# # b)
# p3b <- ggplot(data = banana_pancake2, aes(Mean_Diff, fill = Diff_Color)) +
#   geom_histogram(bins = 50, show.legend = FALSE) +
#   scale_fill_manual(values = c("#F8766D","#00BA38","gray85")) +
#   labs(x = "Difference", y = "Frequency") +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"))
#
# p3 <- plot_grid(p3a, p3b, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')
#
# png(file = "figure3.png", width = 6, height = 4,
#     units = "in", pointsize = 8, res = 300)
# p3
# dev.off()


# Figure 3 - alternative (fully vac)
# a) off at 10: density of difference with entire pop diff superimposed
# b) off at 16: density of difference '' ''
# a)
  p3a_alt <- ggplot() +
              geom_density(data = banana_pancake2, aes(x = Mean_Diff, fill = Diff_Color), alpha = 0.3) +
              scale_fill_manual(name="Difference",values=c("#F8766D","#00BA38","#619CFF"), labels = c('<0', '>0', '0')) +
              geom_density(data = banana_split2, aes(x = Mean_Diff, color = 'black'), linetype = 'dashed', fill = 'gray', alpha = 0.3) +
              scale_color_manual(name = '', values = c('black'), labels = c('Entire Population')) +
              xlab('Difference')+ylab('Density') +
              theme(legend.position = c(0.8, 0.8),
                    axis.text=element_text(size=12),
                    axis.title=element_text(size=14),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 12))
# b)
  p3b_alt <- ggplot() +
              geom_density(data = banana_pancake2, aes(x = Mean_Diff, fill = Diff_Color), alpha = 0.3) +
              scale_fill_manual(name="Difference",values=c("#F8766D","#00BA38","#619CFF"), labels = c('<0', '>0', '0')) +
              geom_density(data = banana_split2, aes(x = Mean_Diff, color = 'black'), linetype = 'dashed', fill = 'gray', alpha = 0.3) +
              scale_color_manual(name = '', values = c('black'), labels = c('Entire Population')) +
              xlab('Difference')+ylab('Density') +
              theme(legend.position = 'none',
                    axis.text=element_text(size=12),
                    axis.title=element_text(size=14)
                    )

p3_alt <- plot_grid(p3a_alt, p3b_alt, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

png(file = "figure3.png", width = 6, height = 4,
    units = "in", pointsize = 8, res = 300)
p3_alt
dev.off()

# Table 1 of param values for |diff| > 1
# a) off at 10: table of values where |diff| > 1
# b) off at 16: table of value where |diff| > 1
banana_cream_pie <- banana_pancake2 %>%
                      filter(abs(Mean_Diff) > 1.5) %>%
                      mutate(Difference = ifelse(Mean_Diff <= 0.0,
                        color_tile(customRed, "transparent")(Mean_Diff*c(Mean_Diff<=0)),
                        color_tile("transparent", customGreen)(Mean_Diff*c(Mean_Diff>=0)))) %>%
                      select(Difference, Vac_Cov, Waning, Take, Epsilon, Rho, VE) %>%
                      arrange(Difference)

 customGray0 = "gray85"
 customGray = "gray55"
 customGreen = "#00BA38" #"#71CA97"
 customRed = "#F8766D" #"#ff7f7f"

t1a <- formattable(banana_cream_pie,
                   align =c("c","c","c","c","c", "c", "c"), list(
        #`Mean_Diff` = color_tile(customRed, customGreen),
        `Vac_Cov`= color_tile(customGray0, customGray),
        `Waning`= color_tile(customGray0, customGray),
        `Take`= color_tile(customGray0, customGray),
        `Epsilon`= color_tile(customGray0, customGray),
        `Rho`= color_tile(customGray0, customGray),
        `VE`= color_tile(customGray0, customGray)
        ))
# b)

t1b <- formattable(banana_cream_pie,
                   align =c("l","c","c","c","c", "c", "c"), list(
         #`Mean_Diff` = color_bar(customRed),
                     `Vac_Cov`= color_tile(customGray0, customGray),
                     `Waning`= color_tile(customGray0, customGray),
                     `Take`= color_tile(customGray0, customGray),
                     `Epsilon`= color_tile(customGray0, customGray),
                     `Rho`= color_tile(customGray0, customGray),
                     `VE`= color_tile(customGray0, customGray)
                   ))
t1b


