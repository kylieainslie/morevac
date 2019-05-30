### profile multiannual2
library(Rgraphviz)
library(proftools)
Rprof(tmp <-tempfile())
multiannual2()
Rprof()
summaryRprof(tmp)
plotProfileCallGraph(readProfileData(tmp),score = "total")
