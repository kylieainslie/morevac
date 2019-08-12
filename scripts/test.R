### testing script ###
test <- multiannual2(n = 10000,
                     wane = 0,
                     vac_coverage = 1,
                     stop_vac_age = 80,
                     take = 1,
                     suscept_func_version = 2,
                     vac_strategy = 2)
p_test <- plot_attack_rates(dat = test$attack_rate)
p_test
# check drift and vac update
mydat <- test$drift
mydat$Cum_Drift <- cumsum(mydat$Drift)
mydat_thinned <- mydat[which(mydat$Vac_Update==1),]
p_drift <- ggplot(data = mydat, aes(x=Year, y=Cum_Drift)) +
           geom_line() +
           geom_point(data=mydat_thinned,aes(x=Year,y=Cum_Drift),colour = 'red')
p_drift

# zoom in on vaccinated years
mydat_vac <- mydat[181:200,]
p_vac <- ggplot(data = mydat_vac, aes(x=Year, y=Cum_Drift)) +
         geom_line() +
         ylab('Cummulative Drift') +
         geom_point(data=mydat_thinned,aes(x=Year,y=Cum_Drift),colour = 'red')
p_vac

# check susceptibility calculation
inf_hist <- test$history[,,1]
vac_hist <- test$history[,,2]
 suscept <- test$history[,,3]
       x <- test$history[,,4]
       v <- test$history[,,5]

# original cohort
mydat_orig <- mydat[161:200,]
orig_cohort <- which(rownames(vac_hist)==39)
oc_vac_hist <- vac_hist[orig_cohort,1:40]
oc_inf_hist <- inf_hist[orig_cohort,1:40]
oc_suscept <- suscept[orig_cohort,1:40]
oc_x <- x[orig_cohort,1:40]
oc_v <- v[orig_cohort,1:40]

# i <- 1
# person <- data.frame(inf_hist = oc_inf_hist[i,],
#                      x = oc_x[i,],
#                      vac_hist = oc_vac_hist[i,],
#                      v = oc_v[i,],
#                      suscept  = oc_suscept[i,],
#                      drift = mydat_orig$Drift,
#                      vac_dist = mydat_orig$Vac_Distance,
#                      update = mydat_orig$Vac_Update)
# person

# birth cohort
birth_cohort <- which(rownames(vac_hist)==19)
bc_vac_hist <- vac_hist[birth_cohort,1:20]
bc_inf_hist <- inf_hist[birth_cohort,1:20]
 bc_suscept <- suscept[birth_cohort,1:20]
       bc_x <- x[birth_cohort,1:20]
       bc_v <- v[birth_cohort,1:20]

i <- 5
person <- data.frame(inf_hist = bc_inf_hist[i,],
                            x = bc_x[i,],
                     vac_hist = bc_vac_hist[i,],
                           v = bc_v[i,],
                     suscept  = bc_suscept[i,],
                     drift = round(mydat_vac$Drift,4),
                     vac_dist = mydat_vac$Vac_Distance,
                     update = mydat_vac$Vac_Update)
person$Year <- 2000:2019
person$no_vac_suscept <- c(rep(0,20))
for (i in 1:20){
  if (person$x[i]>=999){
          person$no_vac_suscept[i] <- 1
  } else if (person$x[i] == 0){
          person$no_vac_suscept[i] <- 0
  } else {
          person$no_vac_suscept[i] <- min(1,person$no_vac_suscept[i-1] + person$drift[i])
  }
}
vax <- person$Year[which(person$vac_hist == 1)]
infections <- person$Year[which(person$inf_hist == 1)]
person
# plot susceptibility
p_no_vac_suscept <- ggplot(data = person, aes(x = Year, y = no_vac_suscept)) +
                    geom_line(colour = 'blue') +
                    xlab('Year') +
                    ylab('Susceptibility') +
                    scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
                    theme(panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          axis.line = element_line(colour = "black"))
p_no_vac_suscept <- p_no_vac_suscept +
                    geom_vline(xintercept=infections, colour = 'red')

p_suscept <- ggplot(data = person, aes(x = Year, y = suscept)) +
             geom_line() +
             xlab('Year') +
             ylab('Susceptibility') +
             scale_y_continuous(limits = c(0,1), expand = c(0,0)) +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"))

p_suscept <- p_suscept +
             geom_vline(xintercept=vax, linetype="dashed", colour = 'black') +
             geom_vline(xintercept=infections, colour = 'red')
p_suscept
library(cowplot)
#theme_set(theme_cowplot(font_size=10)) # reduce default font size
p <- plot_grid(p_vac, p_no_vac_suscept,p_suscept, labels = "AUTO", ncol = 1, align = 'v', axis = 'l')
p

### test multiannual() ###
test <- multiannual(n = 1000, years = 2000:2019, max_age = 20, vac_coverage = c(rep(0.5,20)),
                    betas = c(0.4,rep(0.2,19)), start_vac_year = 2000, vac_protect = 0.7, vac_strategy = 2)
# get attack rates
ar_test <- get_attack_rates(inf_history = test$inf_history$inf_hist_mat,
                           ages_mat = test$ages, years = 2000:2019)
# plot total attack rates
p1 <- plot_attack_rates(dat = ar_test$attack_rates)
p1

### Debugging ###
i <- 101 # choose random person
person <- data.frame(Year = 2000:2019, Age = test$ages[i,],
                     Vac_History = test$vac_history$vac_hist_mat[i,],
                     V = test$vac_history$v[i,],
                     Inf_History = test$inf_history$inf_hist_mat[i,],
                     X = test$inf_history$x[i,],
                     Delta_X = test$inf_history$delta_x[i,],
                     Susceptibility = test$inf_history$suscept_mat[i,],
                     Drift = test$drift,
                     Vac_This_Year = test$vac_this_year
                     )
person

# number vaccinated in each year

vac_numbers <- colSums(out$vac_history$vac_hist_mat)
#################
library(foreach)
library(doParallel)
# make cluster
ncl <- detectCores()
cl <- makeCluster(ncl)
registerDoParallel(cl)

foreach_test <- foreach (i=1:5, .packages = c('morevac','Rcpp')) %dopar%
  morevac::multiannual(n = 10000,
              years = 1820:2019,
              max_age = 80,
              start_vac_year = 2000,
              vac_coverage = c(rep(0.5,80)),
              betas = c(0.4,rep(0.2,199)),
              vac_protect = 0.7,
              suscept_func_version = 2,
              vac_strategy = 1,
              rho = 0.9,
              wane = 0,
              take = 1,
              seed = NULL)
  # run_sim_2(sim = s,n = n,vac_cov = vc, suscept_version = v,
  #           rho = r, wane = w, take = take, vac_strategy = vs[i])
# stop cluster
stopCluster(cl)



