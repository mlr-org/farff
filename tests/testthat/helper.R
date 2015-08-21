library(checkmate)
INST_ARFF_DIR = file.path("..", "..", "inst", "arffs")

compareRWeka = function(dir, path) {
  path2 = file.path(dir, path)
  d1 = readARFF(path2)
  d2 = RWeka::read.arff(path2)
  expect_equal(d1, d2, info = sprintf("Error in file:  %s", path))
}

compareOML = function(data.id) {
  oml.conf = getOMLConfig()
  cachedir = oml.conf$cachedir
  # read to disk, then parse
  getOMLDataSet(data.id)
  path = file.path(cachedir, "datasets", data.id, "dataset.arff")
  d1 = readARFF(path)
  d2 = RWeka::read.arff(path)
  expect_equal(d1, d2, info = sprintf("Error in OML data id:  %i", data.id))
}


