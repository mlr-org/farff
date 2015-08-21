context("long test with many OML data sets")

test_that("long test with many OML data sets", {

  compareRWeka(INST_ARFF_DIR, "iris.arff")
  compareRWeka(INST_ARFF_DIR, "house.arff")
  compareRWeka(INST_ARFF_DIR, "audiology.arff")
})


