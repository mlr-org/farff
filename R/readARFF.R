#' @title Read ARFF file into data.frame.
#'
#' @description
#' Implementation of a fast \href{http://www.cs.waikato.ac.nz/ml/weka/arff.html}{ARFF}
#' parser that produces consistent results compared to the reference implementation
#' in \pkg{RWeka}. The \dQuote{DATA} section is read with \code{\link[readr]{read_delim}}.
#'
#' @note
#' \itemize{
#'   \item{Integer feature columns in ARFF files are parsed as numeric columns into R.}
#'   \item{Sparse ARFF format is currently unsupported. The function will produce an
#'   informative error message in that case.}
#'   \item{ARFF attributes of type \dQuote{relational}, e.g., for multi-instance data,
#'   are currently not supported.}
#' }
#'
#' @details
#' ARFF parsers are already available in package RWeka in \code{\link[RWeka]{read.arff}}
#' and package \code{foreign} in \code{\link[foreign]{read.arff}}. The RWeka parser
#' requires \code{Java} and \code{rJava}, a dependency which is notoriously hard to
#' configure for users in R. It is also quite slow. The parser in foreign in written
#' in pure R, slow and not fully consistent with the reference implementation in \code{RWeka}.
#'
#' @param path [\code{character(1)}]\cr
#'   Path to ARFF file with read access.
#' @param data.reader [\code{character(1)}]\cr
#'   Package back-end to parse ARFF data section with.
#'   At the moment only \code{readr} is supported.
#'   Default is \dQuote{readr}.
#' @param tmp.file [\code{character(1)}]\cr
#'   The ARFF file must be preprocessed a bit, before it can be fed to the \code{data.reader}.
#'   Path to TEMP output file, where this result is stored.
#'   The file is deleted on exit.
#'   Default is \code{tempfile()}.
#' @param convert.to.logicals [\code{logical(1)}]\cr
#'   Should factors with values T or F be converted to logicals? (RWeka does this by default).
#'   Default is \code{TRUE}.
#' @param show.info [\code{logical(1)}]\cr
#'   Default is \code{TRUE}
#' @param ... [any]
#'   Further parameters passed to \code{\link[readr]{read_delim}}.
#' @return [\code{data.frame}].
#' @export
#' @useDynLib farff c_dt_preproc c_rd_preproc
#' @examples
#' path = tempfile()
#' writeARFF(iris, path = path)
#' d = readARFF(path)
# FIXME: choose readr only with string columns
readARFF = function(path, data.reader = "readr",
  tmp.file = tempfile(),
  convert.to.logicals = TRUE,
  show.info = TRUE, ...) {
  assertFileExists(path, access = "r")
  assertChoice(data.reader, c("readr"))
  assertPathForOutput(tmp.file, overwrite = TRUE)
  assertFlag(convert.to.logicals)
  assertFlag(show.info)

  if (show.info)
    messagef("Parse with reader=%s : %s", data.reader, path)

  # system.time is slow when we handle small files, only do it for show.info
  g = identity
  if (show.info) {
    g = function(expr) {
      st = system.time(expr)
      return(st)
    }
  }

  # parse header and measure time
  st1 = g({header = parseHeader(path)})

  on.exit({unlink(tmp.file)})

  # preprocess data (depends on data reader)
  preproc.fun = if (data.reader == "data.table") c_dt_preproc else c_rd_preproc
  requirePackages(data.reader)
  st2 = g(.Call(preproc.fun, path, tmp.file, as.integer(header$line.counter)))

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
      dat = as.data.frame(dat)
      # remove column specification, otherwise expect_equal/all.equal fails
      dat = setAttribute(dat, "spec", NULL)
    })
  }
  colnames(dat) = header$col.names

  # handle logical column type
  st4 = g({
  for (i in seq_col(dat)) {
    col.type = header$col.types[i]
    if (col.type == "factor") {
      clevs = header$col.levels[[i]]
      # RWEKA parses this to logical
      if (convert.to.logicals && (identical(clevs, c("TRUE", "FALSE")) || identical(clevs, c("FALSE", "TRUE"))))
        dat[, i] = as.logical(dat[, i])
      else
        dat[, i] = factor(dat[, i], levels = clevs)
    }
  }
  })

  # transform date columns to corresponding POSIXct format
  date.inds = which(!is.na(header$col.dfmts))
  for (i in date.inds) {
    dat[[i]] = strptime(dat[[i]], format = header$col.dfmts[i])
  }

  if (show.info)
    messagef("header: %f; preproc: %f; data: %f; postproc: %f; total: %f",
      st1[3L], st2[3L], st3[3L], st4[3L], st1[3L] + st2[3L] + st3[3L] + st4[3L])
  return(dat)
}
