library(checkmate)
INST_ARFF_DIR = file.path("..", "..", "inst", "arffs")

compareRWeka = function(dir, path) {
  path2 = file.path(dir, path)
  d1 = readARFF(path2)
  d2 = RWeka::read.arff(path2)
  expect_equal(d1, d2)
}

compareOML = function(data.id) {
  oml.conf = getOMLConfig()
  cachedir = oml.conf$cache
  oml.ds = getOMLDataset(data.id)
  d = oml.ds$data
  path2 = file.path(dir, path)
  d1 = readARFF(path2)
  d2 = RWeka::read.arff(path2)
  expect_equal(d1, d2)
}


