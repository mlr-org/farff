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

# dids = setdiff(dids, c(374, 376,  379,  380))
data.id = 374

oml.conf = getOMLConfig()
cachedir = oml.conf$cachedir
getOMLDataSet(data.id)

path = file.path(cachedir, "datasets", data.id, "dataset.arff")

d1 = readARFF(path, tmp.file = "/home/bischl/cos/farff/bla.arff", data.reader = "readr")
d2 = RWeka::read.arff(path)
expect_equal(d1, d2)

# j = which.first(d1$text != d2$text)
# x = explode(d1$text[j], sep="")
# y = explode(d2$text[j], sep="")
# k = which.first(x != y)
# dd = cbind(x, y)
# print(dd[(k-10):(k+10),])

