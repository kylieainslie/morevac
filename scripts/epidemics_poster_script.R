### Epidemics Poster Script ###

#######################################
### read in results (rather than re-run simulations)
setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/infection_histories")
files_inf <- list.files(pattern="*.csv")
dt_inf = do.call(rbind, lapply(files_inf, fread))

setwd("~/Dropbox/Kylie/Projects/Morevac/data/sim_data/vaccination_histories")
files_vac <- list.files(pattern="*.csv")
dt_vac = do.call(rbind, lapply(files_vac, fread))

setwd("~/Dropbox/Kylie/Projects/Morevac/data")
param_values <- read.csv("parameter_values.csv", header = TRUE)
names(param_values)[1] <- "Param_Index"

### summarise raw data
dt_inf1 <- dt_inf %>% mutate(Num_Infs = rowSums(select(.,Age0:Age18)))
dt_vac1 <- dt_vac %>% mutate(Num_Vacs = rowSums(select(.,Age0:Age18)))

banana <- cbind(dt_inf1[,c("Vac_Strategy", "Sim", "Cohort", "ID", "Param_Index", "Num_Infs")],dt_vac1[,c("Num_Vacs")])
banana_boat <- banana %>% group_by(Vac_Strategy, Param_Index, Sim) %>% summarise(Mean_Infs = mean(Num_Infs))
banana_split <- banana_boat %>% spread(Vac_Strategy, Mean_Infs)
banana_split$Difference <- banana_split$Annual - banana_split$Biennial

# bootstrap to get CI for Difference
foo <- function(data, indices){
  dt<-data[indices,]
  mean(dt$Difference)
}
my_bootstrap <- plyr::dlply(banana_split, "Param_Index", function(dat) boot(dat, foo, R=100)) # boostrap for each set of param values
my_ci <- sapply(my_bootstrap, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

banana_split2 <- banana_split %>% group_by(Param_Index) %>% summarise(Mean_Diff = mean(Difference))
banana_split2$Lower <- my_ci[1,]
banana_split2$Upper <- my_ci[2,]
banana_split2 <- left_join(banana_split2, param_values, by = c("Param_Index"))
# add Diff_Color column for plotting
banana_split2$Diff_Color <- ifelse(banana_split2$Upper < 0, '<0',
                                   ifelse(banana_split2$Lower <=0 & banana_split2$Upper >=0, 'zero',
                                          ifelse(banana_split2$Lower >0, '>0', 'something else')))

### Fully vaccinated individuals (Figure 3)
vac_max <- c(max(banana[banana$Vac_Strategy == "Annual",]$Num_Vacs), max(banana[banana$Vac_Strategy == "Biennial",]$Num_Vacs))
banana_pancake <- banana %>%
  filter((Vac_Strategy == 'Annual' & Num_Vacs == vac_max[1]) |
           (Vac_Strategy == 'Biennial' & Num_Vacs == vac_max[2])) %>%
  group_by(Vac_Strategy, Param_Index, Sim) %>%
  summarise(Mean_Infs = mean(Num_Infs)) %>%
  spread(Vac_Strategy, Mean_Infs) %>%
  mutate(Difference = Annual - Biennial) %>%
  filter(!is.na(Difference))

# bootstrap
my_bootstrap2 <- plyr::dlply(banana_pancake, "Param_Index", function(dat) boot(dat, foo, R=100)) # boostrap for each set of param values
my_ci2 <- sapply(my_bootstrap2, function(x) boot.ci(x, index = 1, type='perc')$percent[c(4,5)]) # get confidence intervals

banana_pancake2 <- banana_pancake %>% group_by(Param_Index) %>% summarise(Mean_Diff = mean(Difference))
banana_pancake2$Lower <- my_ci2[1,]
banana_pancake2$Upper <- my_ci2[2,]
banana_pancake2 <- left_join(banana_pancake2, param_values, by = c("Param_Index"))
# add Diff_Color column for plotting
banana_pancake2$Diff_Color <- ifelse(banana_pancake2$Upper < 0, '<0',
                                   ifelse(banana_pancake2$Lower <=0 & banana_pancake2$Upper >=0, 'zero',
                                          ifelse(banana_pancake2$Lower >0, '>0', 'something else')))


