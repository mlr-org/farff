context("long test with many OML data sets")

library(OpenML)
dchars = listOMLDataSets()
dchars2 = subset(dchars, status == "active" & NumberOfInstances < 200 & NumberOfFeatures < 10)
dids = dchars2$did

# FIXME: dat sets which text features and special chars, they are not stored as UTF8 on OML
dids = setdiff(dids, c(374, 376,  379,  380))

for (did in dids) {
  compareOML(did)
}


