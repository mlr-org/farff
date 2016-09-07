load_all("~/cos/farff/")
library(OpenML)
library(ggplot2)
library(reshape2)
library(gridExtra)
set.seed(1)

# getDataIds = function(n = 100) {
#   dchars = listOMLDataSets()
#   dchars = subset(dchars, status == "active" &
#    NumberOfInstances >= 5000L & NumberOfInstances <= 100000 &
#    NumberOfFeatures >= 5L & NumberOfFeatures <= 100)
#   dchars$size = dchars$NumberOfInstances * dchars$NumberOfFeatures
#   dchars = sortByCol(dchars, "size", asc = TRUE)
#   dids = dchars$did
#   rownames(dchars) = dids
#   ch = chunk(dids, shuffle = FALSE, n.chunks = n)
#   dids = viapply(ch, function(x) sample(x, 1L))
#   return(subset(dchars, dchars$did %in% dids))
# }

dchars = getDataIds(n = 20)
dids = dchars$did
print(dids)

# oml.conf = getOMLConfig()

# # FIXME: what is wrong here?
# dids = setdiff(dids, 353)

# dids = dids[1:3]
res = makeDataFrame(nrow = length(dids), ncol = 9L,
  col.names = c("did", "n", "p", "p.num", "p.fact", "bytes", "farff", "RWeka", "foreign"), col.types = c("numeric"))

# # dids = dids[20]
for (i in seq_along(dids)[]) {
  did = dids[i]

  try(silent = F, {
    getOMLDataSet(did, verbosity = 0L)
    path = file.path(oml.conf$cachedir, "datasets", did, "dataset.arff")
    fi = file.info(path)
    mbs = fi$size / (1024^2)
    messagef("i = %i/%i, data = %i, size = %f", i, length(dids), did, size)
    st1 = system.time({d = farff::readARFF(path, show.info = FALSE)})
    messagef("farff: %f", st1[3L])
    st2 = system.time({d = RWeka::read.arff(path)})
    messagef("RWeka: %f", st2[3L])
    st3 = system.time({d = foreign::read.arff(path)})
    messagef("foreign: %f", st3[3L])
    dcr = dchars[as.character(did), ]
    res[i, "did"] = did
    res[i, "n"] = dcr$NumberOfInstances
    res[i, "p"] = dcr$NumberOfFeatures
    res[i, "mbs"] =  mbs
    res[i, "farff"] = st1[3L] 
    res[i, "RWeka"] = st2[3L]
    res[i, "foreign"] = st3[3L]
  })
}

print(res)

res$speedup = res$RWeka / res$farff
res.melt = melt(res, id.vars = c("did", "n", "mbs"), measure.vars = c("farff", "RWeka", "speedup"),
 variable.name = "pkg", value.name = "y")

doPlot = function(res, xvar, speedup = FALSE) {
  pls = list()
  for (i in seq_along(xvar)) {
    xv = xvar[i]
    res$is.speedup = (res$pkg == "speedup")
    map = aes_string(x = xv, y = "y", color = "pkg")
    pl = ggplot(data = res, mapping = map)
    pl = pl + geom_point(size = 5)
    pl = ggplot(data = res, mapping = map)
    pl = pl + geom_point(size = 5) 
    pl = pl + facet_grid(is.speedup ~ ., scales = "free")
    pls[[i]] = pl
  }
  grob = arrangeGrob(grobs = pls, ncol = 3)
  list(grob = grob, plots = pls)
}

z = doPlot(res.melt, c("mbs", "n", "size"))
plot(z$grob)


