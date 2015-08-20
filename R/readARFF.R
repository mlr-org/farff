#' @title Require some packages.
#'
#' @description
#' Packages are loaded either via \code{\link{requireNamespace}} or \code{\link{require}}.
#'
#' If some packages could not be loaded and \code{stop} is \code{TRUE}
#' the following exception is thrown:
#' \dQuote{For <why> please install the following packages: <missing packages>}.
#' If \code{why} is \code{NULL} the message is:
#' \dQuote{Please install the following packages: <missing packages>}.
#'
#' @param min.versions [\code{character}]\cr
#'   A char vector specifying required minimal version numbers for a subset of packages in \code{packs}.
#'   Must be named and all names must be in \code{packs}.
#'   The only exception is when \code{packs} is only a single string, then you are allowed to pass
#'   an unnamed version string here.
#'   Default is \code{NULL}, meaning no special version requirements
#' @param why [\code{character(1)}]\cr
#'   Short string explaining why packages are required.
#'   Default is an empty string.
#' @param stop [\code{logical(1)}]\cr
#'   Should an exception be thrown for missing packages?
#'   Default is \code{TRUE}.
#' @param suppress.warnings [\code{logical(1)}]\cr
#'   Should warnings be supressed while requiring?
#'   Default is \code{FALSE}.
#' @param default.method [\code{character(1)}]\cr
#'   If the packages are not explicitly prefixed with \dQuote{!} or \dQuote{_},
#'   this arguments determines the default. Possible values are \dQuote{attach} and
#'   \dQuote{load}.
#'   Note that the default is \dQuote{attach}, but this might/will change in a future version, so
#'   please make sure to always explicitly set this.
#' @return [\code{logical}]. Named logical vector describing which packages could be loaded (with required version).
#'   Same length as \code{packs}.
#' @export
#' @useDynLib arff c_preproc

readARFF = function(path) {
  assertFile(path, access = "r")
  path.out = tempfile()
  path.out = "/home/bischl/cos/arff/bla.arff"

  st1 = system.time({
    .Call(c_preproc, path, path.out)
  })

  st2 = system.time({
    header = readForeign(path.out)
  })
  # print(header)
  st3 = system.time({
    dat = fread(path.out, header = FALSE, data.table = FALSE, skip = header$line.counter + 1L,
      na.string = "?")
  })
  colnames(dat) = header$colnames
  st4 = system.time({
  for (i in 1:ncol(dat)) {
    ct = header$coltypes[i]
    if (ct == "factor")
      dat[,i] = as.factor(dat[,i])
  }
  })
  # dat = convertDataFrameCols(dat, chars.as.factor = TRUE)
  # messagef("preproc: %f; header: %f; fread: %f; convert: %f", st1[3L], st2[3L], st3[3L], st4[3L])
  return(dat)
  # return(1)
}



readForeign = function(file) {
  if (is.character(file)) {
    file <- file(file, "r")
    on.exit(close(file))
  }
  if (!inherits(file, "connection"))
    stop("Argument 'file' must be a character string or connection.")
  if (!isOpen(file)) {
    open(file, "r")
    on.exit(close(file))
  }
  col_names <- NULL
  col_types <- NULL
  col_dfmts <- character()
  line <- readLines(file, n = 1L)
  line.counter = 0L
  while (length(line) && regexpr("^[[:space:]]*@(?i)data", line, perl = TRUE, ignore.case = TRUE) == -1L) {
    # print(line.counter)
    # print(line)
    if (regexpr("^[[:space:]]*@(?i)attribute", line, perl = TRUE) >
      0L) {
      con <- textConnection(line)
      line <- scan(con, character(), quiet = TRUE)
      close(con)
      if (length(line) < 3L)
        stop("Invalid attribute specification.")
      col_names <- c(col_names, line[2L])
      if ((type <- tolower(line[3L])) == "date") {
        col_types <- c(col_types, "character")
        col_dfmts <- c(col_dfmts, if (length(line) >
            3L) ISO_8601_to_POSIX_datetime_format(line[4L]) else "%Y-%m-%d %H:%M:%S")
      }
      else if (type == "relational")
        stop("Type 'relational' currently not implemented.")
      else {
        type <- sub("\\{.*", "factor", type)
        type <- sub("string", "character", type)
        type <- sub("real", "numeric", type)
        col_types <- c(col_types, type)
        col_dfmts <- c(col_dfmts, NA)
      }
    }
    line <- readLines(file, n = 1L)
    line.counter = line.counter + 1L
  }
  if (length(line) == 0L)
    stop("Missing data section.")
  if (is.null(col_names))
    stop("Missing attribute section.")
  if (length(col_names) != length(grep("factor|numeric|character",
        col_types)))
    stop("Invalid type specification.")
  list(colnames = col_names, coltypes = col_types, line.counter = line.counter)

  # data <- read.table(file, sep = ",", na.strings = "?", colClasses = col_types,
    # comment.char = "%")
  # if (any(ind <- which(!is.na(col_dfmts))))
    # for (i in ind) data[i] <- as.data.frame(strptime(data[[i]],
        # col_dfmts[i]))
  # for (i in seq_len(length(data))) if (is.factor(data[[i]]))
    # levels(data[[i]]) <- gsub("\\\\", "", levels(data[[i]]))
  # names(data) <- col_names
  # data
}

preproc = function(path) {
  out = tempfile()
  # FIXME:
  out = "bla.arff"

  # # delete % comments lines,. FIXME: delete all AFTER comments
  # cmd = sprintf("sed /^%%/d %s > %s", path, out)
  # # print(cmd)
  # system(cmd)

  # # delete whitespace lines
  # cmd = sprintf("sed -i /^\\s*$/d %s", out)
  # # print(cmd)
  # system(cmd)

  return(path)
}


scanLines = function(path) {
  s = readLines(path)
  j.data = which(grepl("@DATA", s))
  list(j.data = j.data)
}
