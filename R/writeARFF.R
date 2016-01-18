#' @title Write ARFF data.frame to ARFF file.
#'
#' @description
#' Internally uses \code{\link{write.table}} and is therefore not much faster
#' than RWeka's \code{\link[RWeka]{write.arff}}.
#'
#' Note: Logical columns in R are converted to categorical attributes in ARFF
#' with levels \dQuote{TRUE} and \dQuote{FALSE}.
#'
#' @param x [\code{data.frame}]\cr
#'   Data to write to disk.
#' @param path [\code{character(1)}]\cr
#'   Path to ARFF file with write access.
#'   Existing files will not be overwritten.
#' @param relation [\code{character(1)}]\cr
#'   Name of the relation in the ARFF file.
#'   Default is to guess it from the object name.
#' @return Nothing.
#' @export
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
    } else if (is.logical(coldat)) {
      type = "{FALSE, TRUE}"
      x[, cn] = as.factor(x[, cn])
    } else if (inherits(x[, cn], "Date")) {
      type = "date \"yyyy-MM-dd\""
      x[, cn] = format(x[, cn])
    } else if (inherits(x[, cn], "POSIXt")) {
      type = "date \"yyyy-MM-dd HH:mm:ss\""
      x[, cn] = format(x[, cn])
    }
    line = sprintf("@attribute %s %s", squote(cn), type)
    writeLines(line, handle, sep = eol)
  }
  writeLines("@data", handle)
  write.table(x, file = handle, row.names = FALSE, col.names = FALSE, na = "?", sep = ",")
  invisible(NULL)
}

