#' @title Write ARFF data.frame to ARFF file.
#'
#' @description
#' Internally uses \code{\link{write.table}} and is therefore not much faster
#' than RWeka's \code{\link[RWeka]{write.arff}}. Moreover, for large data
#' (> 1e6 rows) the date frame is written out in chunks of 1e6 lines to speed
#' up the write process.
#'
#' @note
#' Logical columns in R are converted to categorical attributes in ARFF
#' with levels \dQuote{TRUE} and \dQuote{FALSE}.
#'
#' @param x [\code{data.frame}]\cr
#'   Data to write to disk.
#' @param path [\code{character(1)}]\cr
#'   Path to ARFF file with write access.
#'   Existing files will not be overwritten unless \code{overwrite} is \code{TRUE}.
#' @param overwrite [\code{logical(1)}]\cr
#'   Should \code{path} be overwritten if it already exists?
#'   Default is \code{FALSE}.
#' @param chunk.size [\code{integer(1)}]\cr
#'   Large datesets are splitted before writing out to file into chunks of size
#'   \code{chunk.size}.
#'   Default is \code{1e6}.
#' @param relation [\code{character(1)}]\cr
#'   Name of the relation in the ARFF file.
#'   Default is to guess it from the object name.
#' @return Nothing.
#' @export
#' @examples
#' # see readARFF
writeARFF = function(x, path,
  overwrite = FALSE,
  chunk.size = 1e6,
  relation = deparse(substitute(x))) {
  assertDataFrame(x, col.names = "unique", min.rows = 1L, min.cols = 1L)
  assertPathForOutput(path, overwrite = overwrite)
  assertFlag(overwrite)
  chunk.size = asCount(chunk.size)
  assertString(relation)

  handle = file(path, "wb")
  on.exit(close(handle))
  eol = "\n"

  squote = function(s) {
    ifelse(is.na(s), s, sprintf("'%s'", gsub("(['\\])", "\\\\\\1", s)))
  }

  line = sprintf("@relation '%s'", relation)
  writeLines(line, handle, sep = eol)

  for (cn in colnames(x)) {
    coldat = x[, cn]

    if (is.numeric(coldat)) {
      type = "numeric"
    } else if (is.integer(coldat)) {
      type = "integer"
    } else if (is.character(coldat)) {
      type = "string"
    } else if (is.factor(coldat)) {
      lev = squote(levels(coldat)) # wrap levels in single quotes
      type = paste0("{", collapse(lev, ","), "}")
    } else if (is.logical(coldat)) {
      type = "{FALSE, TRUE}"
      x[, cn] = as.factor(x[, cn])
    # } else if (inherits(x[, cn], "Date")) {
    #   type = "date \"yyyy-MM-dd\""
    #   x[, cn] = format(x[, cn])
    } else if (inherits(x[, cn], "POSIXt")) {
      # check which format to use
      #FIXME: there needs to be a nicer way to extract the format from a POSIXct format
      ex.date = as.character(x[1, cn])
      ex.date = gsub("-", " ", ex.date)
      if (length(strsplit(ex.date, split = " ")[[1L]]) == 3L) {
        type = "date \"yyyy-MM-dd\""
      } else {
        type = "date \"yyyy-MM-dd HH:mm:ss\""
      }
      x[, cn] = format(x[, cn])
    }
    line = sprintf("@attribute %s %s", squote(cn), type)
    writeLines(line, handle, sep = eol)
  }
  writeLines("@data", handle)
  chunks = BBmisc::chunk(seq(nrow(x)), chunk.size = chunk.size, shuffle = FALSE)
  for (chunk in chunks) {
    suppressWarnings(write.table(x[chunk, , drop = FALSE], file = handle, row.names = FALSE, col.names = FALSE, na = "?", sep = ","))
  }
  invisible(NULL)
}
