# FIXME: we are not very fast. data.table::fwrite does not exist, we could use reader::write.csv?


writeARFF = function (x, path, relation = deparse(substitute(x))) {
  assertDataFrame(x)
  assertPathForOutput(path)
  assertString(relation)
  handle = file(path, "wb")
  on.exit(close(handle))
  eol = "\n"

  squote = function(s) {
    ifelse(is.na(s), s, sprintf("'%s'", gsub("(['\\])", "\\\\\\1",
          s)))
  }
  line = sprintf("@relation '%s'", relation)
  writeLines(line, handle, sep = eol)
  for (cn in colnames(x)) {
    coldat = x[,cn]

    if (is.numeric(coldat)) {
      type = "numeric"
    } else if (is.integer(coldat)) {
      type = "integer"
    } else if (is.character(coldat)) {
      type = "string"
    } else if (is.factor(coldat)) {
      lev = squote(levels(coldat))
      type = paste0("{", collapse(lev, ","), "}")
    } else if (inherits(x[, cn], "Date")) {
      text = paste(text, "date \"yyyy-MM-dd\"")
      x[, cn] = squote(format(x[, cn]))
    } else if (inherits(x[, cn], "POSIXt")) {
      text = paste(text, "date \"yyyy-MM-dd HH:mm:ss\"")
      x[, cn] = squote(format(x[, cn]))
    }
    line = sprintf("@attribute %s %s", squote(cn), type)
    writeLines(line, handle, sep = eol)
  }
  writeLines("@data", handle)
  write.table(x, file = handle, row.names = FALSE, col.names = FALSE, na = "?")
}

