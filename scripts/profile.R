### profile multiannual2
library(Rgraphviz)
library(proftools)

Rprof(tmp <-tempfile(),line.profiling = TRUE)
multiannual2()
Rprof()
summaryRprof(tmp)
plotProfileCallGraph(readProfileData(tmp),score = "total")
