context("quick test with small arffs")

test_that("quick test with small arffs", {
  compareRWeka(INST_ARFF_DIR, "iris.arff")
  compareRWeka(INST_ARFF_DIR, "house.arff")
  compareRWeka(INST_ARFF_DIR, "audiology.arff")
  compareRWeka(INST_ARFF_DIR, "anneal.arff")
  compareRWeka(INST_ARFF_DIR, "kr-vs-kp.arff")
})

