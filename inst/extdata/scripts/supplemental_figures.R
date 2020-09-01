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
