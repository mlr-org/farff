context("quick test with small arffs")

test_that("quick test with small arffs", {
  for (dreader in c("readr")) {
  # for (dreader in c("readr", "data.table")) {
    compareRWeka(INST_ARFF_DIR, "iris.arff", data.reader = dreader)
    compareRWeka(INST_ARFF_DIR, "house.arff", data.reader = dreader)
    compareRWeka(INST_ARFF_DIR, "audiology.arff", data.reader = dreader)
    compareRWeka(INST_ARFF_DIR, "anneal.arff", data.reader = dreader)
    compareRWeka(INST_ARFF_DIR, "kr-vs-kp.arff", data.reader = dreader)
    compareRWeka(INST_ARFF_DIR, "quotes_in_factor_levels.arff", data.reader = dreader)
    #compareRWeka(INST_ARFF_DIR, "many_types.arff", data.reader = dreader)
    compareRWeka(INST_ARFF_DIR, "dates.arff", data.reader = dreader)

    expect_error(readARFF(path = paste(INST_ARFF_DIR, "dataset_1438_accelerometer.arff", sep = "/"), data.reader = dreader),
      "Type 'relational' currently not implemented.")
  }
})

test_that("writeARFF works as expected", {
  # check if overwrite works as expected
  x = file.path(INST_ARFF_DIR, "house.arff")
  dat = readARFF(x)
  outfile = tempfile()
  writeARFF(dat, outfile)
  expect_error(writeARFF(dat, outfile), "File at path already exists")
  writeARFF(dat, outfile, overwrite = TRUE)

  # delete colnames and try to write
  colnames(dat) = NULL
  expect_error(writeARFF(dat, outfile, overwrite = TRUE), "Columns must be named")
})
