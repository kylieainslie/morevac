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
### version 1
# vac_cov = 0
job1 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0,version = 1,
                            rho = 0, vac_strategy = 0,file.out = TRUE,
                            tag = "vs0vc0r0v1"))

job2 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0,version = 1,
                            rho = 0, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc0r0v1"))

job3 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0,version = 1,
                            rho = 0, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc0r0v1"))

# vac_cov = 0.25
job4 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.25,version = 1,
                            rho = 0, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc25r0v1"))

job5 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.25,version = 1,
                            rho = 0, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc25r0v1"))

# vac_cov = 0.50
job6 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.50,version = 1,
                            rho = 0, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc50r0v1"))

job7 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.50,version = 1,
                            rho = 0, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc50r0v1"))

# vac_cov = 0.75
job8 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.75,version = 1,
                            rho = 0, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc75r0v1"))

job9 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.75,version = 1,
                            rho = 0, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc75r0v1"))

# vac_cov = 1
job10 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 1,version = 1,
                            rho = 0, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc1r0v1"))

job11 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 1,version = 1,
                            rho = 0, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc1r0v1"))

### version 2
# vac_cov = 0
job12 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0,version = 2,
                            rho = 0, vac_strategy = 0,file.out = TRUE,
                            tag = "vs0vc0r0v2"))

job13 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0,version = 2,
                            rho = 0, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc0r0v2"))

job14 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0,version = 2,
                            rho = 0, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc0r0v2"))

# vac_cov = 0.25
job15 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.25,version = 2,
                            rho = 0, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc25r0v2"))

job16 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.25,version = 2,
                            rho = 0, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc25r0v2"))

# vac_cov = 0.50
job17 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.50,version = 2,
                            rho = 0, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc50r0v2"))

job18 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.50,version = 2,
                            rho = 0, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc50r0v2"))

# vac_cov = 0.75
job19 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.75,version = 2,
                            rho = 0, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc75r0v2"))

job20 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.75,version = 2,
                            rho = 0, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc75r0v2"))

# vac_cov = 1
job21 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 1,version = 2,
                             rho = 0, vac_strategy = 1,file.out = TRUE,
                             tag = "vs1vc1r0v2"))

job22 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 1,version = 2,
                             rho = 0, vac_strategy = 2,file.out = TRUE,
                             tag = "vs2vc1r0v2"))
#### change rho
### version 1
# vac_cov = 0.25
job23 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.25,version = 1,
                            rho = 0.9, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc25r09v1"))

job24 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.25,version = 1,
                            rho = 0.9, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc25r09v1"))

# vac_cov = 0.50
job25 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.50,version = 1,
                            rho = 0.9, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc50r09v1"))

job26 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.50,version = 1,
                            rho = 0.9, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc50r09v1"))

# vac_cov = 0.75
job27 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.75,version = 1,
                            rho = 0.9, vac_strategy = 1,file.out = TRUE,
                            tag = "vs1vc75r09v1"))

job28 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.75,version = 1,
                            rho = 0.9, vac_strategy = 2,file.out = TRUE,
                            tag = "vs2vc75r09v1"))

# vac_cov = 1
job29 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 1,version = 1,
                             rho = 0.9, vac_strategy = 1,file.out = TRUE,
                             tag = "vs1vc1r09v1"))

job30 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 1,version = 1,
                             rho = 0.9, vac_strategy = 2,file.out = TRUE,
                             tag = "vs2vc1r09v1"))

### version 2
# vac_cov = 0
job31 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0,version = 2,
                             rho = 0.9, vac_strategy = 0,file.out = TRUE,
                             tag = "vs0vc0r09v2"))

job32 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0,version = 2,
                             rho = 0.9, vac_strategy = 1,file.out = TRUE,
                             tag = "vs1vc0r09v2"))

job33 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0,version = 2,
                             rho = 0.9, vac_strategy = 2,file.out = TRUE,
                             tag = "vs2vc0r09v2"))

# vac_cov = 0.25
job34 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.25,version = 2,
                             rho = 0.9, vac_strategy = 1,file.out = TRUE,
                             tag = "vs1vc25r09v2"))

job35 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.25,version = 2,
                             rho = 0.9, vac_strategy = 2,file.out = TRUE,
                             tag = "vs2vc25r09v2"))

# vac_cov = 0.50
job36 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.50,version = 2,
                             rho = 0.9, vac_strategy = 1,file.out = TRUE,
                             tag = "vs1vc50r09v2"))

job37 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.50,version = 2,
                             rho = 0.9, vac_strategy = 2,file.out = TRUE,
                             tag = "vs2vc50r09v2"))

# vac_cov = 0.75
job38 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.75,version = 2,
                             rho = 0.9, vac_strategy = 1,file.out = TRUE,
                             tag = "vs1vc75r09v2"))

job39 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 0.75,version = 2,
                             rho = 0.9, vac_strategy = 2,file.out = TRUE,
                             tag = "vs2vc75r09v2"))

# vac_cov = 1
job40 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 1,version = 2,
                             rho = 0.9, vac_strategy = 1,file.out = TRUE,
                             tag = "vs1vc1r09v2"))

job41 <- obj$enqueue(run_sim(sim = 1000,nindiv = 10000,vaccov = 1,version = 2,
                             rho = 0.9, vac_strategy = 2,file.out = TRUE,
                             tag = "vs2vc1r09v2"))

