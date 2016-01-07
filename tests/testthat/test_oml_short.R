context("quick test with OML data sets where problems occured before")

library(OpenML)
dids = c(119)

# for (dreader in c("readr", "data.table")) {
for (dreader in c("readr")) {
  for (did in dids) {
    compareOML(did, data.reader = dreader)
  }
}

