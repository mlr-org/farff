library(devtools)
library(BBmisc)
library(farff)
library(OpenML)
library(ggplot2)
library(reshape2)
library(gridExtra)
set.seed(1)
setOMLConfig(arff.reader = "farff")

getDataIds = function(n.dsets = NULL, n.min, n.max) {
  dchars = listOMLDataSets(tag = "uci")
  dchars$size = dchars$number.of.instances * dchars$number.of.features
  dchars = sortByCol(dchars, "size", asc = TRUE)
  dids = dchars$data.id
  rownames(dchars) = dids
  if (!is.null(n.dsets)) {
    ch = chunk(dids, shuffle = FALSE, n.chunks = n.dsets)
    dids = viapply(ch, function(x) sample(x, 1L))
  }
  return(subset(dchars, dchars$data.id %in% dids))
}

dchars = getDataIds(n.min = 1000, n.max = 500000)
dids = dchars$data.id
print(dids)

oml.conf = getOMLConfig()

res = makeDataFrame(nrow = length(dids), ncol = 9L,
  col.names = c("did", "n", "p", "p.num", "p.fact", "bytes", "farff", "RWeka", "foreign"), col.types = c("numeric"))

runParser = function(id, f, path, ...) {
  st = try(silent = TRUE, {
    system.time(d <- f(path, ...))[3L]
  })
  if (is.error(st))
    st = NA_real_
  messagef("%s: %f", id, st)
  return(st)
}


for (i in seq_along(dids)[]) {
  did = dids[i]

  getOMLDataSet(did, verbosity = 0L)
  path = file.path(oml.conf$cachedir, "datasets", did, "dataset.arff")
  fi = file.info(path)
  mbs = fi$size / (1024^2)
  messagef("i = %i/%i, data = %i, size = %.2fMB", i, length(dids), did, mbs)
  t1 = runParser("farff", farff::readARFF, path)
  t2 = runParser("RWeka", RWeka::read.arff, path)
  t3 = runParser("foreign", foreign::read.arff, path)
  dcr = dchars[as.character(did), ]
  res[i, "did"] = did
  res[i, "n"] = dcr$number.of.instances
  res[i, "p"] = dcr$number.of.features
  res[i, "mbs"] =  mbs
  res[i, "farff"] = t1
  res[i, "RWeka"] = t2
  res[i, "foreign"] = t3
}

print(res)

# res$speedup = res$RWeka / res$farff
# res.melt = melt(res, id.vars = c("did", "n", "mbs"), measure.vars = c("farff", "RWeka", "speedup"),
#  variable.name = "pkg", value.name = "y")

# doPlot = function(res, xvar, speedup = FALSE) {
#   pls = list()
#   for (i in seq_along(xvar)) {
#     xv = xvar[i]
#     res$is.speedup = (res$pkg == "speedup")
#     map = aes_string(x = xv, y = "y", color = "pkg")
#     pl = ggplot(data = res, mapping = map)
#     pl = pl + geom_point(size = 5)
#     pl = ggplot(data = res, mapping = map)
#     pl = pl + geom_point(size = 5) 
#     pl = pl + facet_grid(is.speedup ~ ., scales = "free")
#     pls[[i]] = pl
#   }
#   grob = arrangeGrob(grobs = pls, ncol = 3)
#   list(grob = grob, plots = pls)
# }

# z = doPlot(res.melt, c("mbs", "n", "size"))
# plot(z$grob)


