#' @title Read ARFF file into data.frame.
#'
#' @description
#' FAST
#'
#' @param path [\code{character(1)}]\cr
#'   Path to ARFF file with read access.
#' @return [\code{data.frame}].
#' @export
#' @useDynLib farff c_preproc

# FIXME: do we drop levels on factors if they dont occur in data?

readARFF = function(path, tmp.file = tempfile(), show.info = TRUE) {
  assertFile(path, access = "r")
  assertFlag(show.info)

  # system.time is slow when we handle small files, only do it for show.info
  g = if (show.info) {
    g = function(expr) {
      st = system.time(expr)
      return(st)
    }
  } else {
    g = identity
  }

  st1 = g(.Call(c_preproc, path, tmp.file))

  st2 = g({header = readHeader(tmp.file)})

  # print(header)
  col.types = str_replace_all(header$col.types, "factor", "character")
  # print(col.types)

  st3 = g({
    dat = fread(tmp.file, skip = header$line.counter, autostart = 1L, header = FALSE,
      sep = ",", stringsAsFactors = FALSE,
      colClasses = col.types,
      data.table = FALSE,
    )
  })
  colnames(dat) = header$col.names
  # print(str(dat))

  st4 = g({
  for (i in 1:ncol(dat)) {
    ct = header$col.types[i]
    if (ct == "factor") {
      clevs = header$col.levels[[i]]
      clevs = str_replace_all(clevs, "\"", "")
      clevs = str_replace_all(clevs, "'", "")
      dat[,i] = factor(dat[,i], levels = clevs)
      # FIXME: annyoing to check against RWeka here
      # we should really doc what happens with the levels of the factors from ARFF
    }
  }
  })
  # dat = convertDataFrameCols(dat, chars.as.factor = TRUE)
  if (show.info)
    messagef("preproc: %f; header: %f; fread: %f; convert: %f", st1[3L], st2[3L], st3[3L], st4[3L])
  return(dat)
}



readHeader = function(path) {
  handle = file(path, "r")
  on.exit(close(handle))

  col.names = character(0L) # names of data cols
  col.types = character(0L) # mapped R data types
  col.dfmts = character(0L) # date formats (usually NA)
  col.levels = list()       # factor levels (charvec or NA)

  line = readLines(handle, n = 1L)
  line.counter = 1L
  while (length(line) && regexpr("^[[:space:]]*@(?i)data", line, perl = TRUE, ignore.case = TRUE) == -1L) {
    if (regexpr("^[[:space:]]*@(?i)attribute", line, perl = TRUE) > 0L) {
      line.split = str_split(line, "\\p{WHITE_SPACE}", n = 3L)[[1L]]
      # print(line.split)
      # FIXME: add (rough?) regexp to match here?
      # if (length(line) < 3L)
        # stop("Invalid attribute specification.")
      col.names = c(col.names, line.split[2L])
      scanned.type.cs = trimws(line.split[3L])
      scanned.type.ci = tolower(scanned.type.cs)
      cdfmt = NA
      clevs = NA

      if (scanned.type.ci == "date") {
        ctype = "character"
        cdfmt = if (length(line) > 3L)
          ISO_8601_to_POSIX_datetime_format(line[4L])
        else
          "%Y-%m-%d %H:%M:%S"
      } else if (scanned.type.ci == "relational") {
        stop("Type 'relational' currently not implemented.")
      } else if (grepl("\\{.*", scanned.type.ci)) {
        # if we see "{*", then it is a factor, as {} contains the levels
        ctype = "factor"
        clevs = scanned.type.cs
        clevs = sub("\\{", "", clevs)
        clevs = sub("\\}", "", clevs)
        clevs = strsplit(clevs, ",")[[1L]]
        clevs = trimws(clevs)
      } else if (scanned.type.ci == "string") {
        ctype = "character"
      } else if (scanned.type.ci %in% c("real", "numeric")) {
        ctype = "numeric"
      } else if (scanned.type.ci == "integer") {
        ctype = "integer"
      } else {
        stopf("Invalid type found on line %i: %s", line.counter, scanned.type.cs)
      }
      col.types = c(col.types, ctype)
      col.dfmts = c(col.dfmts, cdfmt)
      col.levels[[length(col.levels) + 1L]] = clevs
    }
    line = readLines(handle, n = 1L)
    line.counter = line.counter + 1L
  }
  if (length(line) == 0L)
    stop("Missing data section.")
  if (is.null(colnames))
    stop("Missing attribute section.")

  col.names = trimws(str_replace_all(col.names, "\"", ""))
  col.names = trimws(str_replace_all(col.names, "'", ""))

  list(col.names = col.names, col.types = col.types, col.levels = col.levels,
    col.dfmts = col.dfmts, line.counter = line.counter)

  # FIXME: this is done for dates? do we have such a dataset?
  # if (any(ind = which(!is.na(col_dfmts))))
    # for (i in ind) data[i] = as.data.frame(strptime(data[[i]],
        # col_dfmts[i]))
  # data
}

