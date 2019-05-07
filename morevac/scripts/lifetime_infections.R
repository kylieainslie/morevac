
# test simulation
sim = 100
n=5000
AY5a <-matrix(c(rep(999,sim*2)),nrow=sim)
colnames(AY5a) <-c("Vaccinated", "Unvaccinated")
AY5b <-matrix(c(rep(999,sim*2)),nrow=sim)
colnames(AY5b) <-c("Vaccinated", "Unvaccinated")

for(s in 1:sim){
  testa <- run_annual1(n = 5000, years = 17, vac_coverage = 0.5,mydelta = 0.2, biannual = FALSE, same_indiv = TRUE)
  testb <- run_annual1(n = 5000, years = 17, vac_coverage = 0.5,mydelta = 0.2, biannual = TRUE, same_indiv = TRUE)
  
  # compare attack rate between never vac and vac
  va <- which(apply(testa[[1]]$vac_history,1,sum)>0)
  uva <- which(apply(testa[[1]]$vac_history,1,sum)==0)
  AY5a[s,] <- c(sum(testa[[1]]$infection_matrix[va,6]),sum(testa[[1]]$infection_matrix[uva,6]))/n # AY5 = year 6
  
  vb <- which(apply(testb[[1]]$vac_history,1,sum)>0)
  uvb <- which(apply(testb[[1]]$vac_history,1,sum)==0)
  AY5b[s,] <- c(sum(testb[[1]]$infection_matrix[vb,6]),sum(testb[[1]]$infection_matrix[uvb,6]))/n # AY5 = year 6
}

t.test(AY5a[,1],AY5a[,2],alternative = "two.sided")
t.test(AY5b[,1],AY5b[,2],alternative = "two.sided")

# compare number of lifetime infections between individuals' whose first exposure was infection vs. vaccination

