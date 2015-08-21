library(devtools)
library(checkmate)
library(testthat)
library(foreign)
library(RWeka)
library(data.table)
library(microbenchmark)
library(BBmisc)

load_all()


# path = "/home/bischl/cos/farff/inst/arffs/iris.arff"
# path = "/home/bischl/cos/farff/inst/arffs/anneal.arff"
# path = "/home/bischl/cos/farff/inst/arffs/audiology.arff"
# path = "/home/bischl/cos/farff/inst/arffs/house.arff"
# path = "/home/bischl/cos/farff/inst/arffs/covtype-normalized.arff"
path = "/home/bischl/cos/farff/inst/arffs/kr-vs-kp.arff"

d1 = readARFF(path, tmp.file = "/home/bischl/cos/farff/bla.arff", show.info = TRUE)
# print(head(d1))

d2 = RWeka::read.arff(path)
expect_equal(d1, d2)

# mb = microbenchmark(
#   readARFF(path, show.info = FALSE),
#   foreign::read.arff(path),
#   RWeka::read.arff(path),
#   times = 20L
# )
# print(mb)
