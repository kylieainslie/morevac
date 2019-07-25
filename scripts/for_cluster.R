### For cluster
#setwd('/Volumes/kainslie/cluster')
setwd('Q:/cluster')
# set up your details and which cluster you want (big or small)
options(didehpc.username = "kainslie",
        didehpc.cluster = "big")

# dont really know what these lines do but you need them
context::context_log_start()
root <- "contexts2"

# setting up the context: what files need to be sourced for your function to work, what packages you need
# package sources only required here because the package I am using is not on CRAN
ctx <- context::context_save(path = root,
                             packages = c("morevac","Rcpp"),
                             package_sources = provisionr::package_sources(github = "kylieainslie/morevac",
                                                                           cran = "https://cran.ma.imperial.ac.uk/")
                             )

# this line should give you a login prompt
obj <- didehpc::queue_didehpc(ctx)

# 'job' is the thing I am running on the cluster
# set parameter values
s <- 100
n <- 10000
vc <- 0
v <- 2
r <- 0.9
w <- 1
take <- 1
vs <- c(0,1,2)
tags <- c(paste0('vs',vs[1],'vc',vc,'r',r,'v',v,'w',w,'t',take),
          paste0('vs',vs[2],'vc',vc,'r',r,'v',v,'w',w,'t',take),
          paste0('vs',vs[3],'vc',vc,'r',r,'v',v,'w',w,'t',take))
# separate job for each vac strategy
job1 <- obj$enqueue(run_sim(sim = s,nindiv = n,vaccov = vc,version = v,
                            rho = r, wane = w, take = take, vac_strategy = vs[1],
                            file.out = TRUE, tag = tags[1]))

job2 <- obj$enqueue(run_sim(sim = s,nindiv = n,vaccov = vc,version = v,
                            rho = r, wane = w, take = take, vac_strategy = vs[2],
                            file.out = TRUE, tag = tags[2]))

job3 <- obj$enqueue(run_sim(sim = s,nindiv = n,vaccov = vc,version = v,
                            rho = r, wane = w, take = take, vac_strategy = vs[3],
                            file.out = TRUE, tag = tags[3]))
