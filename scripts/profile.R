### profile multiannual2

Rprof(tmp <-tempfile())
multiannual2()
Rprof()
summaryRprof(tmp)
plotProfileCallGraph(readProfileData(tmp),score = "total")
