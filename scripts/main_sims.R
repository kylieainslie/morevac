### Latin Hypercube simulations script ###
# last modified: 22/01/2020

### preamble
# load required packages
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
# library(foreach)
# library(doParallel)
# load morevac package
# setwd("~/Documents/morevac") # Mac path
setwd("~/morevac") # PC path
devtools::load_all()
###

#######################################
# specify save directory
# setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data")
setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data")
### run simulations
params <- create_params_file(n_sim = 10, n_indiv = 5000, lhc_size = 10, out_file = "param_values_test",
                             vac_cutoff = 10, seed = 1234)
run_sims_all(params_file = "param_values_test.csv", out_file = "test")
#######################################
### read in results (rather than re-run simulations)
# setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
# setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/")
param_values <- read.csv("param_values.csv", header = TRUE)
names(param_values)[1] <- "Param_Index"

setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/cutoff10")
# setwd("C:/Users/kainslie/Dropbox/Kylie/Projects/Morevac/data/sim_data/cutoff10")
files_inf <- list.files(pattern="*inf_hist.csv")
matches <- regmatches(files_inf, gregexpr("[[:digit:]]+", files_inf))
param_indices <- as.numeric(unlist(matches))
files_inf <- files_inf[order(param_indices)] # re-order in numerical order

files_vac <- list.files(pattern="*vac_hist.csv")
matches2 <- regmatches(files_vac, gregexpr("[[:digit:]]+", files_vac))
param_indices2 <- as.numeric(unlist(matches2))
files_vac <- files_vac[order(param_indices2)] # re-order in numerical order

matching_elements <- which(gsub("_inf_hist.csv","",files_inf) %in% gsub("_vac_hist.csv","",files_vac))
files_inf <- files_inf[matching_elements]

n_files <- length(files_inf)

### create progress bar
for (i in 1:n_files){
### read in data from list of files
  dt_inf = vroom(file = files_inf[i], delim = ",", col_names = TRUE)  %>%
              mutate(Num_Infs = rowSums(select(.,Age0:Age18)), Param_Index = i)
  dt_vac = vroom(file = files_vac[i], delim = ",", col_names = TRUE) %>%
              mutate(Num_Vacs = rowSums(select(.,Age0:Age18)), Param_Index = i)
  if(length(unique(dt_vac$Vac_Strategy))<3){next} # make sure vac data contains all three vac strategies

### summarise raw data for childhood infs
  banana <- bind_cols(dt_inf[,c("Vac_Strategy", "Sim", "Cohort","Param_Index", "ID", "Num_Infs")], Num_Vacs = dt_vac[,c("Num_Vacs")]) %>%
                group_by(Vac_Strategy, Sim) %>% summarise(Mean_Infs = mean(Num_Infs))

### bootstrap to get CI for childhood infs
  foo1 <- function(data, indices){
    dt<-data[indices,]
    mean(dt$Mean_Infs)
  }
  my_bootstrap <- plyr::dlply(banana, "Vac_Strategy", function(dat) boot(dat, foo1, R=100)) # boostrap for each set of param values
  my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals
  banana_boat <- banana %>% group_by(Vac_Strategy) %>% summarise(Mean_Infs = mean(Mean_Infs)) %>%
                  mutate(Lower = my_ci[1,], Upper = my_ci[2,], Param_Index = i)

### Difference in childhood infs
  banana_split <- banana %>% spread(Vac_Strategy, Mean_Infs) %>%
                    mutate(Diff_AB = Annual - Biennial, Diff_AN = Annual - No_Vac, Diff_BN = Biennial - No_Vac) %>%
                    select(Sim, Diff_AB, Diff_AN, Diff_BN) %>%
                    gather(Type, Difference, Diff_AB:Diff_BN)

### bootstrap to get CI for Difference
  foo2 <- function(data, indices){
    dt<-data[indices,]
    mean(dt$Difference)
  }
  my_bootstrap <- plyr::dlply(banana_split, "Type", function(dat) boot(dat, foo2, R=100)) # boostrap for each set of param values
  my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

  banana_split2 <- banana_split %>% group_by(Type) %>% summarise(Mean_Diff = mean(Difference)) %>%
                      mutate(Lower = my_ci[1,], Upper = my_ci[2,], Param_Index = i)
### summarise raw data for attack rates
  chocolate_sprinkles <- dt_inf %>% group_by(Vac_Strategy, Sim, Cohort, Param_Index) %>% do(tail(.,1))
  chocolate_bar <- dt_inf %>% group_by(Vac_Strategy, Sim, Cohort, Param_Index) %>% select(-ID) %>% summarise_all(list(sum))
  chocolate_bar$ID <- chocolate_sprinkles$ID
  chocolate_sundae <- chocolate_bar %>% mutate_at(vars(Age0:Age18), funs(./ID)) %>%
                        select(Param_Index, Vac_Strategy, Sim, Cohort, Age0:Age18) %>% group_by(Param_Index, Vac_Strategy, Sim) %>%
                        summarise_at(vars(Age0:Age18), mean) %>% gather(Age, Attack_Rate, Age0:Age18) %>%
                        mutate(Age = as.numeric(str_remove(Age, 'Age')))
### bootstrap to get CI for ARs
  foo3 <- function(data, indices){
    dt<-data[indices,]
    mean(dt$Attack_Rate)
  }
  my_bootstrap <- plyr::dlply(chocolate_sundae, c("Param_Index","Vac_Strategy","Age"), function(dat) boot(dat, foo3, R=1000)) # boostrap for each set of param values
  my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

  chocolate_sundae2 <- chocolate_sundae %>% group_by(Param_Index, Vac_Strategy, Age) %>% summarise(Mean_AR = mean(Attack_Rate)) %>%
                          ungroup() %>% mutate(Lower = my_ci[1,], Upper = my_ci[2,])

  if(i > 1) {
    banana_cream_pie <- bind_rows(banana_cream_pie,banana_split2)
    banana_bread <- bind_rows(banana_bread,banana_boat)
    chocolate_surprise <- bind_rows(chocolate_surprise, chocolate_sundae2)
  } else {banana_cream_pie <- banana_split2
          banana_bread <- banana_boat
          chocolate_surprise <- chocolate_sundae2
  }
}

### bind columns with parameter values
banana_cream_pie <- left_join(banana_cream_pie, param_values, by = c("Param_Index"))
banana_bread <- left_join(banana_bread, param_values, by = c("Param_Index"))
chocolate_surprise <- left_join(chocolate_surprise, param_values, by = c("Param_Index"))

### write files to avoid having to read in raw data again
data.table::fwrite(banana_cream_pie, file = "banana_cream_pie.csv", col.names = TRUE,
                   row.names = FALSE, sep = ",")
data.table::fwrite(banana_bread, file = "banana_bread.csv", col.names = TRUE,
                   row.names = FALSE, sep = ",")
data.table::fwrite(chocolate_surprise, file = "chocolate_surprise.csv", col.names = TRUE,
                   row.names = FALSE, sep = ",")

############################################
### Fully vaccinated individuals
# vac_max <- c(max(banana[banana$Vac_Strategy == "Annual",]$Num_Vacs), max(banana[banana$Vac_Strategy == "Biennial",]$Num_Vacs))
# banana_pancake <- banana %>%
#   filter((Vac_Strategy == 'Annual' & Num_Vacs == vac_max[1]) |
#            (Vac_Strategy == 'Biennial' & Num_Vacs == vac_max[2])) %>%
#   group_by(Vac_Strategy, Param_Index, Sim) %>%
#   summarise(Mean_Infs = mean(Num_Infs)) %>%
#   spread(Vac_Strategy, Mean_Infs) %>%
#   mutate(Difference = Annual - Biennial) %>%
#   filter(!is.na(Difference))
#
# # bootstrap
# my_bootstrap2 <- plyr::dlply(banana_pancake, "Param_Index", function(dat) boot(dat, foo, R=1000)) # boostrap for each set of param values
# my_ci2 <- sapply(my_bootstrap2, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals
# my_ci2[vapply(my_ci2, is.null, logical(1))] <- list(c(NA, NA))
# my_ci2a <- data.frame(matrix(unlist(my_ci2), nrow=length(my_ci2), byrow=T)) #bind_rows(my_ci2)
#
# banana_pancake2 <- banana_pancake %>% group_by(Param_Index) %>% summarise(Mean_Diff = mean(Difference))
# banana_pancake2$Lower <- my_ci2a[,1]
# banana_pancake2$Upper <- my_ci2a[,2]
# banana_pancake2 <- left_join(banana_pancake2, param_values, by = c("Param_Index"))
# # add Diff_Color column for plotting
# banana_pancake2$Diff_Color <- ifelse(banana_pancake2$Upper < 0, '<0',
#                                    ifelse(banana_pancake2$Lower <=0 & banana_pancake2$Upper >=0, 'zero',
#                                           ifelse(banana_pancake2$Lower >0, '>0', 'something else')))
# banana_pancake2$Diff_Color <- ifelse(is.na(banana_pancake2$Upper) & banana_pancake2$Mean_Diff <0, '<0', banana_pancake2$Diff_Color)
# banana_pancake2$Diff_Color <- ifelse(is.na(banana_pancake2$Lower) & banana_pancake2$Mean_Diff >0, '>0', banana_pancake2$Diff_Color)
# randomly select fully vac individuals and plot exposure history

