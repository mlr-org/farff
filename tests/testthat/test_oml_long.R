if (identical(Sys.getenv("TRAVIS"), "true")) {

  context("long test with many OML data sets")

  library(OpenML)
  dchars = listOMLDataSets()
  dchars2 = subset(dchars, status == "active" & number.of.instances < 100 & number.of.features < 10)
  dids = dchars2$did


  # FIXME: data set 292 is in sparse format. this is valid but we cannot parse it.
  dids = setdiff(dids, c(292))

  # FIXME: data set has multi-instance observations. Currently, we cannot parse it but we throw an error and have a unit test for this.
  dids = setdiff(dids, c(1438))

  # FIXME: there are quoting issues in data.table here. if we have doubles quotes ", it is unclear
  # how data.table should get them fed into after preproc.
  # - one " does not work, eg if we get a comma after an unescaped " in dquotes
  # - the correct way would be to escape all dquotes in char fields like this : \".
  #   but now data.table produces \\\"
  dids.datatable.broken = c(374, 376,  379,  380)

  # for (dreader in c("readr", "data.table")) {
  for (dreader in c("readr")) {
    for (did in dids) {
      message(did)
      if (dreader == "readr" || did %nin% dids.datatable.broken)
        compareOML(did, data.reader = dreader)
    }
  }

}
