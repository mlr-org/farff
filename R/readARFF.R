#' @title Read ARFF file into data.frame.
#'
#' @description
#' FAST
#'
#' @param path [\code{character(1)}]\cr
#'   Path to ARFF file with read access.
#' @return [\code{data.frame}].
#' @export
#' @useDynLib arff c_preproc

readARFF = function(path, show.info = TRUE) {
  assertFile(path, access = "r")
  assertFlag(show.info)

  path.out = tempfile()
  # FIXME:
  path.out = "/home/bischl/cos/farff/bla.arff"

  # system.time is slow when we handle small files, only do it for show.info
  g = if (show.info) {
    g = function(expr) {
      st = system.time(expr)
      return(st)
    }
  } else {
    g = identity
  }

  st1 = g(.Call(c_preproc, path, path.out))

  st2 = g({header = readForeign(path.out)})

  # print(header)
  st3 = g({
    dat = fread(path.out, header = FALSE, data.table = FALSE, skip = header$line.counter + 1L,
      na.string = "?")
  })
  colnames(dat) = header$colnames

  st4 = g({
  for (i in 1:ncol(dat)) {
    ct = header$coltypes[i]
    if (ct == "factor")
      dat[,i] = as.factor(dat[,i])
  }
  })
  # dat = convertDataFrameCols(dat, chars.as.factor = TRUE)
  if (show.info)
    messagef("preproc: %f; header: %f; fread: %f; convert: %f", st1[3L], st2[3L], st3[3L], st4[3L])
  return(dat)
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

