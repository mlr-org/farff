context("quick test with small arffs")

test_that("quick test with small arffs", {

  compareRWeka(INST_ARFF_DIR, "iris.arff")
})

