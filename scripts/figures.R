
### Figure 1 - wane = 0.5, vac stop @ 10
# a) take = 1
fig1a <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                   wane = 0.5, take = 1, vac_cov = vac_cov_dat$Off_At_10)
# b) take = 0.75
fig1b <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.75, vac_cov = vac_cov_dat$Off_At_10,
                     show_legend = FALSE)
# c) take = 0.5
fig1c <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.5, vac_cov = vac_cov_dat$Off_At_10,
                     show_legend = FALSE)

# d) take = 0.25
fig1d <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.25, vac_cov = vac_cov_dat$Off_At_10,
                     show_legend = FALSE)

theme_set(theme_cowplot(font_size=10)) # reduce default font size
fig1 <- plot_grid(fig1a, fig1b, fig1c, fig1d, labels = "AUTO", ncol = 2,
                  align = 'v', axis = 'l')
# save figure
png(filename = "figure1.png", width = 6, height = 6, units = "in", res = 300)
plot(fig1)
dev.off()

### Figure 2 - wane = 0.5, vac stop @ 16
# a) take = 1
fig2a <- plot_sim_ar(sim = 10, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 1, vac_cov = vac_cov_dat$Off_At_16)
# b) take = 0.75
fig2b <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.75, vac_cov = vac_cov_dat$Off_At_16,
                     show_legend = FALSE)
# c) take = 0.5
fig2c <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.5, vac_cov = vac_cov_dat$Off_At_16,
                     show_legend = FALSE)

# d) take = 0.25
fig2d <- plot_sim_ar(sim = 100, years = 2000:2019, year_index = 181:200,
                     wane = 0.5, take = 0.25, vac_cov = vac_cov_dat$Off_At_16,
                     show_legend = FALSE)

theme_set(theme_cowplot(font_size=10)) # reduce default font size
fig2 <- plot_grid(fig2a, fig2b, fig2c, fig2d, labels = "AUTO", ncol = 2,
                  align = 'v', axis = 'l')

# save figure
png(filename = "figure2.png", width = 6, height = 6, units = "in", res = 300)
plot(fig2)
dev.off()
