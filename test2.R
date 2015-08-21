library(devtools)
library(checkmate)
library(testthat)
library(foreign)
library(RWeka)
library(data.table)
library(microbenchmark)
library(BBmisc)

load_all()


path = "/home/bischl/cos/farff/inst/arffs/iris.arff"

d1 = readARFF(path, show.info = FALSE)
print(head(d1))


