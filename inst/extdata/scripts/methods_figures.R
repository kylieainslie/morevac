##################################
### Figure 0 - Model schematic ###
##################################

# load required pacakges
library(ggplot2)
library(tidyr)
library(reshape2)
library(cowplot)
library(stringr)
library(dplyr)
library(lhs)
library(boot)
library(data.table)
library(rdist)
library(Matrix)
library(vroom)
#devtools::load_all()
devtools::install_github("kylieainslie/morevac")
library(morevac)
# run multi-annual model
max_age = 80
vac_cut_off <- 10
my_years <- 1918:2028
vac_cov_dat <- data.frame(
  Age = 0:(max_age-1),
  No_Vac = numeric(max_age),
  Annual = numeric(max_age),
  Biennial = numeric(max_age)
  )

vac_cov_dat$Annual[3:(vac_cut_off + 1)] <- 0.44
vac_cov_dat$Biennial[seq(3,vac_cut_off+1,2)] <- 0.44

out <- multiannual(n = 30000,
                   years = my_years,
                   max_age = max_age,
                   betas = c(0.4,rep(0.2,length(my_years)-1)),
                   vac_coverage = vac_cov_dat$Annual,
                   vac_strategy = 1,
                   epsilon = 0.005,
                   seed = 1234
                   )

# pick random subset of people susceptibility
colnames(out$ages) <- paste0("year_", my_years)

birth_cohort <- which(out$ages[,"year_2000"] == 0)
indivs <- data.frame(id = sample(birth_cohort, 9, replace = FALSE))
year_index <- which(my_years %in% 2000:2020)

sample_profiles <- list(
  inf_hist = out$inf_history[indivs$id, year_index],
  vac_hist = out$vac_history[indivs$id, year_index],
  suscept_mat = out$suscept_mat[indivs$id, year_index]
)

for (i in seq_along(sample_profiles)){
  colnames(sample_profiles[[i]]) <- paste0("year_",2000:2020)
}

person <- bind_rows(data.frame(id = indivs, sample_profiles$inf_hist),
                    data.frame(id = indivs, sample_profiles$vac_hist),
                    data.frame(id = indivs, sample_profiles$suscept_mat),
                    .id = "profile") %>%
  mutate(profile = case_when(
    profile == 1 ~ "inf_hist",
    profile == 2 ~ "vac_hist",
    profile == 3 ~ "suscept_mat"
  )) %>%
  pivot_longer(cols = starts_with("year_"),
               names_to = "year",
               names_prefix = "year_",
               values_to = "value") %>%
  mutate(year = as.numeric(year))

###########

### Top panel ###
# drift plot
drift_dat <- data.frame(Year = my_years[year_index],
                        Drift = out$drift$drift$y[year_index],
                        Distance = out$drift$antigenic_dist[1, year_index],
                        Vac_Update = out$vac_update[year_index],
                        Distance_Update = out$distance[year_index]
                        #Gammas = out$gammas[year_index]
                        ) %>%
  select(Year, Vac_Update, Distance_Update)
# find update rows
update_rows <- which(drift_dat$Vac_Update == 1)

# add rows where Distance Update = 0 (for plot)
for (i in 1:length(update_rows)){
  print(update_rows[i])
  drift_dat <- drift_dat %>%
    add_row(Year = drift_dat[update_rows[i],]$Year,
            Vac_Update = 1,
            Distance_Update = 0,
            .after = update_rows[i])
  update_rows <- update_rows + 1
}

# colors for plot
drift_dat$Shade_Group <- c(rep(1,dim(drift_dat)[1]))

for (i in 2:dim(drift_dat)[1]){
  if (drift_dat$Vac_Update[i]==1){drift_dat$Shade_Group[i] <- drift_dat$Shade_Group[i-1] + 1
  } else {drift_dat$Shade_Group[i] <- drift_dat$Shade_Group[i-1]}
}

drift_dat$Shade_Group <- (drift_dat$Shade_Group)

#calculate cumulative drift
drift_dat$Cum_Drift <- cumsum(drift_dat$Drift)
drift_dat$Year  <- (drift_dat$Year)

# determine years when a vaccine update occurred
drift_dat_thinned <- drift_dat[which(drift_dat$Vac_Update==1),]
drift_dat_thinned$prev <-c(min(drift_dat$Year),drift_dat_thinned$Year[-nrow(drift_dat_thinned)])
drift_dat_thinned$ymn <-c(min(drift_dat$Distance_Update),drift_dat_thinned$Distance_Update[-nrow(drift_dat_thinned)])
drift_dat_thinned <- drift_dat_thinned %>%
  add_row(Year = 2020,
          #Drift = drift_dat[drift_dat$Year == 2020,]$Drift,
          Vac_Update = drift_dat[drift_dat$Year == 2020,]$Vac_Update,
          Distance_Update = drift_dat[drift_dat$Year == 2020,]$Distance_Update,
          Shade_Group = drift_dat[drift_dat$Year == 2020,]$Shade_Group + 1,
          #Cum_Drift = drift_dat[drift_dat$Year == 2020,]$Cum_Drift,
          prev = 2018,
          ymn = drift_dat[drift_dat$Year == 2018,]$Distance_Update)

# plot
ymn <- min(drift_dat$Distance_Update)
ymx <- max(drift_dat$Distance_Update)

p_drift <- ggplot(data = drift_dat, mapping=aes( x=Year, y = Distance_Update))+
  scale_x_continuous(limits = c(2000,2021), expand = c(0,0)) +
  scale_y_continuous(limits = c(ymn,ymx), expand = c(0,0)) +
  geom_rect(data = drift_dat_thinned,
            aes(xmin = prev, xmax = Year,
                ymin = ymn, ymax = Distance_Update,
                fill = factor(Shade_Group), col = factor(Shade_Group)), alpha = 0.2) +
  # geom_ribbon(aes(xmin = Year, xmax = lead(Year),
  #                 ymin = ymn, ymax = Distance_Update),
  #             fill = "white", alpha = 1)+  # color shading under cumulative drift by shade group
  ylab('Antigenic Distance') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  geom_step()
p_drift

# determine colors used
cols <- ggplot_build(p_drift)$data[[1]]$colour

# add points in same colors
p_drift <- p_drift + geom_point(data = drift_dat_thinned,aes(x=Year,y = Distance_Update), colour = c(cols[-1],"#CC6666"))
p_drift
#################

### Bottom panel ###
# susceptibility plot

# get suscept mat
my_id <- sample(unique(person$id),1)

suscept_mat <- person %>%
  filter(profile == "suscept_mat",
         id == my_id)

# get vax and infection years
vax <- person %>%
  filter(profile == "vac_hist",
         id == my_id,
         value == 1)

infections <- person %>%
  filter(profile == "inf_hist",
         id == my_id,
         value == 1)


# plot

p_suscept <- ggplot(data = suscept_mat, aes(x = year, y = value)) +
  geom_step() +
  geom_rect(aes(xmin = year, xmax = lead(year),
                ymin = 0,ymax = value),alpha=0.1,) +
  xlab('Year') +
  ylab('Susceptibility') +
  scale_x_continuous(limits = c(1999,2021), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

vac_cols <- cols[drift_dat$Shade_Group[which(drift_dat$Year %in% vax$year)]]
p_suscept <- p_suscept +
  geom_vline(xintercept=vax$year, linetype="dashed", colour = vac_cols) +
  geom_vline(xintercept=infections$year, colour = 'red')
p_suscept
#################


### Put the two panels together
library(cowplot)
theme_set(theme_cowplot(font_size=12)) # reduce default font size
figure1 <- plot_grid(p_drift, p_suscept, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')

### Output
# save figure
filename <- "C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/figures/"
png(file = paste0(filename,"figure0.png"), width = 6, height = 8,
    units = "in", pointsize = 8, res = 300)
plot(figure1)
dev.off()



### old code

# vaccine protection plot
# p_gamma <- ggplot(data = drift_dat, aes(x = Year, y = Gammas)) +
#   geom_line() +
#   geom_ribbon(aes(ymax = Cum_Drift,fill = Shade_Group),ymin=0,alpha=0.2,) +
#   geom_point(data=drift_dat_thinned,aes(x=Year,y=Cum_Drift),colour = 'blue') +
#   ylab('Cumulative Antigenic Drift')
# p_gamma

