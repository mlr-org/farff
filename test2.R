library(devtools)
library(checkmate)
library(testthat)
library(foreign)
library(RWeka)
library(data.table)
library(microbenchmark)
library(BBmisc)
library(OpenML)

load_all()


# path = "/home/bischl/cos/farff/inst/arffs/iris.arff"

# d1 = readARFF(path, show.info = FALSE)
# print(head(d1))

data.id = 3

oml.conf = getOMLConfig()
cachedir = oml.conf$cachedir
getOMLDataSet(data.id)

path = file.path(cachedir, "datasets", data.id, "dataset.arff")

d1 = readARFF(path, tmp.file = "/home/bischl/cos/farff/bla.arff")
d2 = RWeka::read.arff(path)
expect_equal(d1, d2)

