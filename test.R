library(devtools)
library(checkmate)
library(testthat)
library(foreign)
library(RWeka)
library(data.table)
library(microbenchmark)
library(BBmisc)

load_all()


# path = "/home/bischl/cos/arff/iris.arff"
# path = "/home/bischl/cos/arff/audiology.arff"
# path = "/home/bischl/cos/arff/house.arff"
path = "/home/bischl/cos/arff/covtype-normalized.arff"

d1 = readARFF(path)
# print(head(d1))

d2 = foreign::read.arff(path)
expect_equal(d1, d2)

d3 = RWeka::read.arff(path)
expect_equal(d1, d3)


mb = microbenchmark(
  readARFF(path),
  foreign::read.arff(path),
  RWeka::read.arff(path),
  times = 2L
)
print(mb)
