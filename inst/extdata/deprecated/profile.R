### profile multiannual2
library(Rgraphviz)
library(proftools)

Rprof(tmp <-tempfile(),line.profiling = TRUE)
multiannual2() # function to profile
Rprof(line.profiling = TRUE)
summaryRprof(tmp)
plotProfileCallGraph(readProfileData(tmp),score = "total")

test1 <- multiannual(n = 10000)
test2 <- multiannual2(n = 10000)
