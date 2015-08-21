context("long test with many OML data sets")

library(OpenML)
dchars = listOMLDataSets()
dchars2 = subset(dchars, status == "active" & NumberOfInstances < 200 & NumberOfFeatures < 10)

for (did in dchars2$did) {
  compareOML(did)
}


