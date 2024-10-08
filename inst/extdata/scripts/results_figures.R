### Manuscript results figures ###
library(ggplot2)
library(vroom)
library(dplyr)
library(cowplot)

### specify file path to save figures
filename <- "C:/Users/ainsliek/Dropbox/Kylie/Projects/Morevac/figures/"

##############################################
### Figure 1 - baseline AR and LI plot    ###
##############################################
# a) attack rates at baseline
# b) lifetime infections by age at baseline
##############################################
### attack rates
setwd("C:/Users/ainsliek/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/sim1000")

mean_ar_baseline <- vroom(file = "mean_ar_baseline_sim1000.csv",
                          delim = ",",
                          col_names = TRUE)

mean_ar_baseline1 <- mean_ar_baseline %>%
  select(-c(n_sim, n_indiv, max_age, start_year, end_year, pandemic_beta, epidemic_beta)) %>%
  filter(exposure_penalty == 0 & vac_protect == 0.7) %>%
  mutate(Vac_Strategy = ifelse(Vac_Strategy == "No_Vac", "No Vaccination", Vac_Strategy))


figure1a <- ggplot(data = mean_ar_baseline1, aes(x = Age, y = Mean_AR, colour = Vac_Strategy)) +
  geom_line() +
  geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
  scale_y_continuous(limits = c(0, 0.2)) +
  #scale_fill_discrete(breaks = c("Annual", "Biennial", "No Vaccination")) +
  xlab("Age (years)") + ylab("Attack Rate") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        #legend.margin = margin(6, 6, 6, 6),
        #legend.key = element_rect(fill = "white")
  )

### b) childhood infections
mean_infs_tab <- vroom(file = "mean_infs_tab.csv", delim = ",", col_names = TRUE)

df_for_plot <- mean_infs_tab %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  filter(vac_cat != "<50%",
         (Vac_Strategy_char == "Annual" & vac_cat == ">50%") |
           (Vac_Strategy_char == "Biennial" & vac_cat == ">50%") |
           (Vac_Strategy_char == "No_Vac")) %>%
  mutate(age_group = factor(age_group, levels = c("0-1", "2-10", "11-18", "Total")),
         Vac_Strategy_char = ifelse(Vac_Strategy_char == "No_Vac", "No Vaccination", Vac_Strategy_char))

figure1b <- ggplot(data = df_for_plot, aes(x = age_group, y = Mean, fill = Vac_Strategy_char)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(), width = 0.65) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = .2, position = position_dodge(.65)) +
  scale_x_discrete(drop=FALSE) +
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +
  labs(y = "Mean Number of Infections", x = "Age Group", fill = "Vaccination Strategy") +
  #guides(fill=FALSE) +
  #theme_bw(base_size=9) +  ## makes everything smaller
  theme(panel.background = element_rect(fill="white"),  ## white plot background
        legend.position = "bottom",
        axis.line = element_line(colour = "black"),
        #axis.title.y = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.x = element_text(size=rel(0.7)), ## tiny axis text
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())
figure1b

figure1 <- plot_grid(figure1a, figure1b, labels = "AUTO",
                     ncol = 2, rel_widths = c(1, 0.6))
figure1

png(file = paste0(filename,"figure1_sim1000.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
figure1
dev.off()
##############################################
### Figure 2 - AR plot by exposure penalty ###
##############################################
setwd("C:/Users/ainsliek/Dropbox/Kylie/Projects/Morevac/data/sim_data/baseline/sim1000")

mean_ar_baseline <- vroom(file = "mean_ar_baseline_sim1000.csv",
                          delim = ",",
                          col_names = TRUE)

### AR for different values of epsilon
mean_ar_baseline1 <- mean_ar_baseline %>%
  select(-c(n_sim, n_indiv, max_age, start_year, end_year, pandemic_beta, epidemic_beta)) %>%
  filter(exposure_penalty %in% c(0, 0.01, 0.03, 0.05, 0.08, 0.1) & vac_protect == 0.7)

figure2 <- ggplot(data = mean_ar_baseline1, aes(x = Age, y = Mean_AR, colour= Vac_Strategy)) +
  geom_line() +
  geom_ribbon(aes(x=Age,ymin=Lower,ymax=Upper,linetype=NA, fill = Vac_Strategy),alpha=0.2) +
  scale_y_continuous(limits = c(0, 0.2)) +
  #scale_fill_discrete(breaks = c("Annual", "Biennial", "No Vaccination")) +
  xlab("Age (years)") + ylab("Attack Rate") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_blank(),
        #legend.margin = margin(6, 6, 6, 6),
        #legend.key = element_rect(fill = "white")
        ) +
  facet_wrap(~exposure_penalty, nrow=2)
figure2

png(file = paste0(filename,"figure2_sim1000.png"), width = 12, height = 8,
    units = "in", pointsize = 8, res = 300)
figure2
dev.off()


###################################
### Figure 3 - scatter plots    ###
###################################
# a) annual vs. no vac
# b) annual vs. biennial
###################################
setwd("C:/Users/ainsliek/Dropbox/Kylie/Projects/Morevac/data/sim_data/cutoff10/sim1000/")
mean_diff_10 <- vroom(file = "mean_diff_10_sim1000.csv", delim = ",", col_names = TRUE) %>%
                      mutate(Diff_Color = ifelse(Upper < 0, '<0',ifelse(Lower <=0 & Upper >=0, '0',ifelse(Lower >0, '>0', 'something else'))))

# a) Scatter plot of difference between annual and no vaccination
mean_diff_10_an <- mean_diff_10 %>%
  filter(Type == "Diff_AN", Diff_Color != '0') %>%
  mutate(Abs_Val = abs(Mean_Diff))

figure3a <- ggplot(data = mean_diff_10_an, aes(x = exposure_penalty, y = vac_protect, color = Diff_Color)) +
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


# b) Scatter plot of difference between annual and biennial
mean_diff_10_ab <- mean_diff_10 %>%
  filter(Type == "Diff_AB", Diff_Color != '0') %>%
  mutate(Abs_Val = abs(Mean_Diff))

figure3b <- ggplot(data = mean_diff_10_ab, aes(x = exposure_penalty, y = vac_protect, color = Diff_Color)) +
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

figure3 <- plot_grid(figure3a, figure3b, labels = "AUTO", nrow = 1) # rel_heights = c(1,1.5)

png(file = paste0(filename,"figure3_sim1000.png"), width = 13, height = 6,
    units = "in", pointsize = 8, res = 300)
figure3
dev.off()

# Unused code- #################################################################
### Add insets to figure 1 - can't get it to work
# banana_bread <- vroom(file = "banana_bread_baseline2.csv", delim = ",", col_names = TRUE)

# add inset for number of childhood infections by vac_strategy
# banana_bread2 <- banana_bread %>%
#   select(-c(n_sim, n_indiv, max_age, start_year, end_year, pandemic_beta, epidemic_beta)) %>%
#   filter(exposure_penalty %in% c(0, 0.01, 0.03, 0.05, 0.08, 0.1) & vac_protect == 0.7 & Param_Index != 18)

# get_inset <- function(df){
#   p <- ggplot(data = df, aes(x = Vac_Strategy, y = Mean_Infs, fill = Vac_Strategy)) +
#     geom_bar(stat = "identity", color = "black", position = position_dodge(), width = 0.65) +
#     geom_errorbar(aes(ymin = Lower, ymax = Upper), width = .2, position = position_dodge(.65)) +
#     scale_x_discrete(drop=FALSE) +
#     scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF"))
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
# # test for one panel
# figure1 +
#   annotation_custom2(grob=ggplotGrob(inset_plot),
#                      data = data.frame(exposire_penalty=0),
#                      ymin = -0.5, ymax=0.2, xmin=10, xmax=18)
#
# insets <- banana_bread2 %>%
#   split(f = .$exposure_penalty) %>%
#   purrr::map(~annotation_custom2(
#     grob = ggplotGrob(get_inset(.) +
#                         scale_y_continuous(limits=c(0,105), breaks = c(0, 50, 100))),
#     data = data.frame(category = unique(.$exposure_penalty)),
#     ymin = -0.5, ymax = 0.2, xmin = 10, xmax = 18)
#   )
#
#
# # add insets to main plot
# figure1 + insets


