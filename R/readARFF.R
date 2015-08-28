#' @title Read ARFF file into data.frame.
#'
#' @description
#' Implementation of a fast ARFF parser that produces consistent results compared to the reference
#' implementation in \code{RWeka}.
#' The \dQuote{DATA} section is read with either reader::read_delim from reader or fread from
#' data.table.
#'
#' Note: Integer feature columns in ARFF files are parsed as numeric columns into R.
#'
#' @details
#' ARFF parsers are already available in package RWeka in \code{\link[RWeka]{read.arff}} and package
#' \code{foreign} in \code{\link[foreign]{read.arff}}. The RWeka parser requires \code{Java} and
#' \code{rJava}, a dependency which is notoriously hard to configure for users in R. It is also quite slow.
#' The parser in foreign in written in pure R, slow and not fully consistent with the reference
#' implementation in \code{RWeka}.
#'
#' @param path [\code{character(1)}]\cr
#'   Path to ARFF file with read access.
#' @param show.info [\code{logical(1)}]\cr
#'   Default is \code{TRUE}
#' @return [\code{data.frame}].
#' @export
#' @useDynLib farff c_dt_preproc c_rd_preproc

# FIXME: choose readr only with string columns

readARFF = function(path, data.reader = "readr", tmp.file = tempfile(), show.info = TRUE) {
  assertFile(path, access = "r")
  assertChoice(data.reader, c("readr", "data.table"))
  assertPathForOutput(tmp.file, overwrite = TRUE)
  assertFlag(show.info)

  if (show.info)
    messagef("Parse with reader=%s : %s", data.reader, path)
  # system.time is slow when we handle small files, only do it for show.info
  g = if (show.info) {
    g = function(expr) {
      st = system.time(expr)
      return(st)
    }
  } else {
    g = identity
  }

  st1 = g({header = parseHeader(path)})
  # print(header)

  if (data.reader == "data.table")  {
    requirePackages("data.table")
    st2 = g(.Call(c_dt_preproc, path, tmp.file, as.integer(header$line.counter)))
  } else {
    requirePackages("readr")
    st2 = g(.Call(c_rd_preproc, path, tmp.file, as.integer(header$line.counter)))
  }

  col.types = str_replace_all(header$col.types, "factor", "character")

  if (data.reader == "data.table") {
    st3 = g({
      dat = fread(tmp.file, header = FALSE, sep = ",", stringsAsFactors = FALSE,
        colClasses = col.types,
        data.table = FALSE,
        )
    })
  } else {
    st3 = g({
      col.types = str_replace_all(col.types, "numeric", "double")
      col.types = str_replace_all(col.types, "integer", "double")
      col.types = collapse(vcapply(col.types, function(x) substr(x, 1L, 1L)), sep = "")
      dat = read_delim(tmp.file, delim = ",", col_names = FALSE, col_types = col.types,
        escape_backslash = TRUE, escape_double = FALSE)
      # print(problems(dat))
      dat = as.data.frame(dat)
    })
  }
  colnames(dat) = header$col.names
  # print(str(dat))

  st4 = g({
  for (i in 1:ncol(dat)) {
    ct = header$col.types[i]
    if (ct == "factor") {
      clevs = header$col.levels[[i]]
      # RWEKA parses this to logical
      # FIXME: DOC THIS!
      if (identical(clevs, c("TRUE", "FALSE")) || identical(clevs, c("FALSE", "TRUE")))
        dat[,i] = as.logical(dat[,i])
      else
        dat[,i] = factor(dat[,i], levels = clevs)
    }
  }
  })
  if (show.info)
    messagef("header: %f; preproc: %f; data: %f; postproc: %f; total: %f",
      st1[3L], st2[3L], st3[3L], st4[3L], st1[3L] + st2[3L] + st3[3L] + st4[3L])
  return(dat)
}



