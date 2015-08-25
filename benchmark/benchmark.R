
library(OpenML)

set.seed(1)

getDataIds = function(n = 100) {
  dchars = listOMLDataSets()
  dchars = subset(dchars, status == "active" &
   NumberOfInstances >= 10000L & NumberOfInstances <= 1000000 &
   NumberOfFeatures >= 5L & NumberOfFeatures <= 1000)
  dchars$size = dchars$NumberOfInstances * dchars$NumberOfFeatures
  dchars = sortByCol(dchars, "size", asc = TRUE)
  dids = dchars$did
  ch = chunk(dids, shuffle = FALSE, n.chunks = n)
  dids = viapply(ch, function(x) sample(x, 1L))
  return(subset(dchars, dchars$did %in% dids))
}

dchars = getDataIds(n = 20)
dids = dchars$did

oml.conf = getOMLConfig()

dids = dids
# FIXME: what is wrong here?
dids = setdiff(dids, 353)

res = makeDataFrame(nrow = length(dids), ncol = 5L,
  col.names = c("did", "bytes", "farff", "RWeka", "foreign"), col.types = c("numeric"))

for (i in seq_along(dids)) {
  did = dids[i]

  try(silent = F, {
    getOMLDataSet(did, verbosity = 0L)
    path = file.path(oml.conf$cachedir, "datasets", did, "dataset.arff")
    fi = file.info(path)
    size = fi$size / (1024 * 1024)
    messagef("data = %i, size = %f", did, size)
    st1 = system.time({d = farff::readARFF(path, show.info = FALSE)})
    messagef("farff: %f", st1[3L])
    st2 = system.time({d = RWeka::read.arff(path)})
    messagef("RWeka: %f", st2[3L])
    # st3 = foreign::read.arff(path)
    res[i, ] = data.frame(did = did, size = size, farff = st1[3L], RWeka = st2[3L], foreign = NA)
  })
}

print(res)




