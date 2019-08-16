### testing script ###

### test multiannual() ###
test <- multiannual(n = 10000, years = 1820:2019, max_age = 80, vac_coverage = vac_cov_dat$Fifty_Off_At_10,
                    betas = c(0.4,rep(0.2,199)), start_vac_year = 2000, vac_protect = 0.7, vac_strategy = 2,
                    drift_off = FALSE, suscept_func_version = 1)
bc <- which(test$ages[,181]==0)
bc_inf_hist <- test$inf_history$inf_hist_mat[bc,181:200]
bc_vac_hist <- test$vac_history$vac_hist_mat[bc,181:200]
bc_person <- data.frame(Year = 2000:2019, Age = test$ages[bc[1],181:200],
                        Vac_History = test$vac_history$vac_hist_mat[bc[1],181:200],
                        V = test$vac_history$v[bc[1],181:200],
                        Inf_History = test$inf_history$inf_hist_mat[bc[1],181:200],
                        X = test$inf_history$x[bc[1],181:200],
                        Delta_X = test$inf_history$delta_x[bc[1],181:200],
                        Susceptibility = test$inf_history$suscept_mat[bc[1],181:200],
                        Drift = test$drift[181:200],
                        Vac_This_Year = test$vac_this_year[181:200]
)

# get attack rates
ar_test <- get_attack_rates(inf_history = bc_inf_hist,
                            ages_mat = test$ages[bc,181:200], years = 2000:2019)
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

# exploratory plot
exp_fig <- plot_sim_ar(sim = 25, years = 2000:2019, year_index = 181:200,
                       wane = 0.5, take = 0.7, vac_cov = vac_cov_dat$Off_At_10,
                       show_legend = TRUE)
exp_fig





