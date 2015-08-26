context("quick test with small arffs")

test_that("quick test with small arffs", {
  tmpfile = tempfile()
  for (dreader in c("readr", "data.table")) {
    # FIXME: add tmpfile = NULL for data.table again.
    tmpfiles = if (dreader == "readr") list(tmpfile) else list(tmpfile)
    for (tf in tmpfiles) {
      compareRWeka(INST_ARFF_DIR, "iris.arff", data.reader = dreader, tmp.file = tf)
      compareRWeka(INST_ARFF_DIR, "house.arff", data.reader = dreader, tmp.file = tf)
      compareRWeka(INST_ARFF_DIR, "audiology.arff", data.reader = dreader, tmp.file = tf)
      # FIXME: anneal does not work beacause we have '?', which is a factor level not an NA
      # compareRWeka(INST_ARFF_DIR, "anneal.arff", data.reader = dreader, tmp.file = tf)
      compareRWeka(INST_ARFF_DIR, "kr-vs-kp.arff", data.reader = dreader, tmp.file = tf)
    }
  }
})

