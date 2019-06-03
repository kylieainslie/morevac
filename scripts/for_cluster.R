### For cluster
setwd('/Volumes/kainslie')
# set up your details and which cluster you want (big or small)
options(didehpc.username = "kainslie",
        didehpc.cluster = "small")

# dont really know what these lines do but you need them
context::context_log_start()
root <- "contexts"

# setting up the context: what files need to be sourced for your function to work, what packages you need
# package sources only required here because the package I am using is not on CRAN
ctx <- context::context_save(path = root,
                             packages = c("morevac","ggplot2","reshape2","stringr",
                                          "foreach","parallel","cowplot","doParallel"),
                             package_sources = provisionr::package_sources(github = "kylieainslie/morevac")
                             )

# this line should give you a login prompt
obj <- didehpc::queue_didehpc(ctx)

# 'job' is the thing I am running on the cluster
job1 <- obj$enqueue(sim_for_cluster_func(s = 100, n = 10000, v = 1, r = 0.9))
job2 <- obj$enqueue(sim_for_cluster_func(s = 100, n = 10000, v = 2, r = 0.5))



