if (!identical(Sys.getenv("TRAVIS"), "true")) { # don't run this on travis
  
  context("arffIsDataFrame")
  
  library(OpenML)
  
  # these dids work for RWeka but not for farff
  bad = c(119,350,386,391,397,401,572)
  # for (reader in c("RWeka", "farff.readr", "farff.data.table")) {
  for (reader in c("RWeka", "farff.readr")) {
    for(bad.did in bad) {
      arffIsDataFrame(bad.did, reader)
    }
  }
  
  # # here a list of other dids that do not work (some of them even don't work for RWeka)
  # bad = c(70,71,73,74,75,76,78,115,116,118,119,121,122,123,124,125,126,127,128,129,130,
  #         131,132,133,135,136,138,140,141,142,144,146,147,148,273,292,293,350,358,383,
  #         384,385,386,387,388,389,390,391,392,393,394,395,396,397,398,399,400,401,572)
  # 
  # # some of the files are also "big" and take a long time
  # size.bad = vapply(bad, function(X) {
  #   path = OpenML:::downloadOMLObject(X, object = "data")$files$dataset.arff$path
  #   file.size(path)
  # }, numeric(1))
}
