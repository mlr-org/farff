#' @title Read ARFF file into data.frame.
#'
#' @description
#' Implementation of a fast ARFF parser that produces consistent results compared to the reference
#' implementation in \code{RWeka}.
#' The \dQuote{DATA} section is read with reader::read_delim.
#'
#' Note: Integer feature columns in ARFF files are parsed as numeric columns into R.
#'
#' Note: Sparse ARFF format is currently unsupported. The function will produce an informative error
#' message in that case.
#'
#' Note: ARFF attributes of type \dQuote{relational}, e.g., for multi-instance data, are currently not supported.
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
#' @param data.reader [\code{character(1)}]\cr
#'   Package back-end to parse ARFF data section with.
#'   Either \dQuote{readr} or \dQuote{data.table}
#'   Default is \dQuote{readr}.
#' @param tmp.file [\code{character(1)}]\cr
#'   The ARFF file must be preprocessed a bit, before it can be fed to the \code{data.reader}.
#'   Path to TEMP output file, where this result is stored.
#'   The file is deleted on exit.
#'   Default is \code{tempfile()}.
#' @param convert.to.logicals [\code{logical(1)}]\cr
#'   Should factors with values T or F be converted to logicals? (RWeka does this be default).
#'   Default is \code{TRUE}.
#' @param show.info [\code{logical(1)}]\cr
#'   Default is \code{TRUE}
#' @return [\code{data.frame}].
#' @export
#' @useDynLib farff c_dt_preproc c_rd_preproc
# FIXME: choose readr only with string columns
readARFF = function(path, data.reader = "readr",
  tmp.file = tempfile(),
  convert.to.logicals = TRUE,
  show.info = TRUE, ...) {
  assertFile(path, access = "r")
  assertChoice(data.reader, c("readr"))
  assertPathForOutput(tmp.file, overwrite = TRUE)
  assertFlag(convert.to.logicals, na.ok = FALSE)
  assertFlag(show.info, na.ok = FALSE)

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

  on.exit({unlink(tmp.file)})
  if (data.reader == "data.table")  {
    requirePackages("data.table")
    st2 = g(.Call(c_dt_preproc, path, tmp.file, as.integer(header$line.counter)))
  } else {
    requirePackages("readr")
    st2 = g(.Call(c_rd_preproc, path, tmp.file, as.integer(header$line.counter)))
  }

  col.types = stri_replace_all(header$col.types, fixed = "factor", "character")

  if (data.reader == "data.table") {
    ## removed for now until we can ensure data.table support
    # st3 = g({
    #   dat = fread(tmp.file, header = FALSE, sep = ",", stringsAsFactors = FALSE,
    #     colClasses = col.types,
    #     data.table = FALSE,
    #     )
    # })
  } else {
    st3 = g({
      col.types = stri_replace_all(col.types, fixed = "numeric", "double")
      col.types = stri_replace_all(col.types, fixed = "integer", "double")
      col.types = collapse(vcapply(col.types, function(x) substr(x, 1L, 1L)), sep = "")
      dat = read_delim(tmp.file, delim = ",", col_names = FALSE, col_types = col.types,
        escape_backslash = TRUE, escape_double = FALSE, ...)
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
      if (convert.to.logicals && (identical(clevs, c("TRUE", "FALSE")) || identical(clevs, c("FALSE", "TRUE"))))
        dat[, i] = as.logical(dat[, i])
      else
        dat[, i] = factor(dat[, i], levels = clevs)
    }
  }
  })
  if (show.info)
    messagef("header: %f; preproc: %f; data: %f; postproc: %f; total: %f",
      st1[3L], st2[3L], st3[3L], st4[3L], st1[3L] + st2[3L] + st3[3L] + st4[3L])
  return(dat)
}



