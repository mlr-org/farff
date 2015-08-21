library(checkmate)
INST_ARFF_DIR = file.path("..", "..", "inst", "arffs")

compareRWeka = function(dir, path) {
  path2 = file.path(dir, path)
  d1 = readARFF(path2)
  d2 = RWeka::read.arff(path2)
  expect_equal(d1, d2)
}

