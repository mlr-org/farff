context("long test with many OML data sets")

library(OpenML)
dchars = listOMLDataSets()
dchars2 = subset(dchars, status == "active" & NumberOfInstances < 1000 & NumberOfFeatures < 20)
dids = dchars2$did

# FIXME: dat sets which text features and special chars, they are not stored as UTF8 on OML
dids = setdiff(dids, c(379,  380))
# FIXME: data set 292 is in sparse format. this is valid but we cannot parse it.
dids = setdiff(dids, c(292))

for (did in dids) {
  compareOML(did)
}


