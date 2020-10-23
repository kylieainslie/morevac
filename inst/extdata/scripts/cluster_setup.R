### For cluster
# getting started
 # install.packages("drat") # if you don't have it already
 # drat:::add("mrc-ide")
 # install.packages("provisionr")
 # install.packages("didehpc")
 # install.packages("buildr)

# setwd('/Volumes/kainslie/cluster')
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
                             packages = c("morevac","Rcpp","dplyr","data.table","rdist", "Matrix", "logitnorm"),
                             package_sources = provisionr::package_sources(#github = "kylieainslie/morevac",
                                                                           local = "Q:/cluster/morevac_1.0.zip",
                                                                           cran = "https://cran.ma.imperial.ac.uk/")
                             )

# this line should give you a login prompt
obj <- didehpc::queue_didehpc(ctx)

# run a single job
# 'job' is the thing I am running on the cluster
job <- obj$enqueue(
        run_sims_all(params_file = "param_values_baseline_time-varying_beta.csv",
                     index = c(1,2,4,6,9,11),
                     out_file = "sim_baseline2_")
        )


# bulk jobs
job_index <- c(1,2,4,6,9,11)

for (i in job_index){
  job <- obj$enqueue(
           run_sims_all(params_file = "param_values_baseline_time-varying_beta.csv",
                        index = i,
                        out_file = "sim_baseline2_")
        )

}
# job_bulk <- obj$enqueue_bulk(params_baseline, run_sims_clust, do_call = TRUE)


