### For cluster
#setwd('/Volumes/kainslie/cluster')
setwd('Q:/cluster')
# set up your details and which cluster you want (big or small)
options(didehpc.username = "kainslie",
        didehpc.cluster = "big")

# dont really know what these lines do but you need them
context::context_log_start()
root <- "contexts"

# setting up the context: what files need to be sourced for your function to work, what packages you need
# package sources only required here because the package I am using is not on CRAN
ctx <- context::context_save(path = root,
                             packages = c("morevac","Rcpp"),
                             package_sources = provisionr::package_sources(github = "kylieainslie/morevac")
                             )

# this line should give you a login prompt
obj <- didehpc::queue_didehpc(ctx)

# 'job' is the thing I am running on the cluster
# separate job for each vac strategy
job1 <- obj$enqueue(run_sim(sim = 100,nindiv = 10000,vaccov = 0.5,version = 2,
                            rho = 0.9, wane = 0, vac_strategy = 0,
                            file.out = TRUE, tag = "vs0vc50r09v2w0"))

job2 <- obj$enqueue(run_sim(sim = 100,nindiv = 10000,vaccov = 0.5,version = 2,
                            rho = 0.9, wane = 0, vac_strategy = 1,
                            file.out = TRUE, tag = "vs1vc50r09v2w0"))

job3 <- obj$enqueue(run_sim(sim = 100,nindiv = 10000,vaccov = 0.5,version = 2,
                            rho = 0.9, wane = 0, vac_strategy = 2,
                            file.out = TRUE, tag = "vs2vc50r09v2w0"))

job4 <- obj$enqueue(run_sim(sim = 100,nindiv = 10000,vaccov = 0.5,version = 1,
                            rho = 0.9, wane = 0,vac_strategy = 0,
                            file.out = TRUE, tag = "vs0vc50r09v1w0"))

job5 <- obj$enqueue(run_sim(sim = 100,nindiv = 10000,vaccov = 0.5,version = 1,
                            rho = 0.9, wane = 0, vac_strategy = 1,
                            file.out = TRUE, tag = "vs1vc50r09v1w0"))

job6 <- obj$enqueue(run_sim(sim = 100,nindiv = 10000,vaccov = 0.5,version = 1,
                            rho = 0.9, wane = 0, vac_strategy = 2,
                            file.out = TRUE, tag = "vs2vc50r09v1w0"))
