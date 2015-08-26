context("parseFactorLevels")

test_that("parseFactorLevels", {
  expect_equal(parseFactorLevels("{a, b}"), c("a", "b"))
  expect_equal(parseFactorLevels("{a, 'c'}"), c("a", "c"))
  expect_equal(parseFactorLevels('{"a", "c"}'), c("a", "c"))
  expect_equal(parseFactorLevels("{i-s, i-v}"), c("i-s", "i-v"))
  expect_equal(parseFactorLevels("{Iris-setosa,Iris-versicolor,Iris-virginica}"),
    c("Iris-setosa", "Iris-versicolor", "Iris-virginica"))
  expect_equal(parseFactorLevels("{  GB , GK , GS}"), c("GB", "GK", "GS"))
})

