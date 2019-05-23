### For cluster

# set up your details and which cluster you want (big or small)
options(didehpc.username = "kainslie",
        didehpc.cluster = "big")

# dont really know what these lines do but you need them
context::context_log_start()
root <- "contexts"

# setting up the context: what files need to be sourced for your function to work, what packages you need
# package sources only required here because the package I am using is not on CRAN
ctx <- context::context_save(path = root,
                             packages = c("morevac","ggplot2","reshape2","stringr"),
                             package_sources = provisionr::package_sources(github = "kylieainslie/MoreVac"),
                             sources = c("functions/likelihood_functions.R",
                                         "functions/model_functions.R",
                                         "functions/lazy_mcmc_functions.R",
                                         "functions/cluster_functions.R"))


# this line should give you a login prompt
obj <- didehpc::queue_didehpc(ctx)


vaccov <- c(0, 0.25, 0.5, 0.75, 1)
# 'job' is the thing I am running on the cluster
job <- obj$enqueue(run_sim(sim = 10,
                           nindiv = 1000,
                           year_range = c(2000:2019),
                           age_range = c(0:19))
                  )
Collapse



