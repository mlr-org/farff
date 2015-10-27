library(BatchJobs)
library(OpenML)
source("../tests/testthat/helper_compare.R")

unlink("farff_batchtest-files", recursive = TRUE)
reg = makeRegistry("farff_batchtest", packages = 
  c("testthat", "OpenML", "farff"))


dchars = listOMLDataSets()
dchars2 = subset(dchars, status == "active" & NumberOfInstances < 100 & NumberOfFeatures < 5)
dids = dchars2$did


# FIXME: data set 292 is in sparse format. this is valid but we cannot parse it.
# dids = setdiff(dids, c(292))


# FIXME: there are quoting issues in data.table here. if we have doubles quotes ", it is unclear
# how data.table should get them fed into after preproc.
# - one " does not work, eg if we get a comma after an unescaped " in dquotes
# - the correct way would be to escape all dquotes in char fields like this : \".
#   but now data.table produces \\\"
# dids.datatable.broken = c(374, 376,  379,  380)

batchExpandGrid(reg, compareOML, data.id = dids, data.reader = c("readr", "arff"))
# waitForJobs(reg)




