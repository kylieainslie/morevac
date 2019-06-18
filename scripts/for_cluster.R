### For cluster
setwd('/Volumes/kainslie/cluster')
# set up your details and which cluster you want (big or small)
options(didehpc.username = "kainslie",
        didehpc.cluster = "small")

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

job1 <- obj$enqueue(run_sim(sim = 10,nindiv = 1000,vaccov = 0,version = 1,
                            rho = 0, vac_strategy = 0,file.out = TRUE,tag = ""))

job2 <- obj$enqueue(run_sim(sim = 10,nindiv = 1000,vaccov = 0,version = 1,
                            rho = 0, vac_strategy = 1,file.out = TRUE))

job3 <- obj$enqueue(run_sim(sim = 10,nindiv = 1000,vaccov = 0,version = 1,
                            rho = 0, vac_strategy = 2,file.out = TRUE))


