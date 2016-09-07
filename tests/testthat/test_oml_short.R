if (identical(Sys.getenv("TRAVIS"), "true")) {

context("quick test with OML data sets where problems occured before")

library(OpenML)

dids = c(119)
dids = c(dids, c(1028, 1030)) # caused problems with linebreaks (github issue #9)

# for (dreader in c("readr", "data.table")) {
for (dreader in c("readr")) {
  for (did in dids) {
    compareOML(did, data.reader = dreader)
  }
}

}
