#' @title Read ARFF file into data.frame.
#'
#' @description
#' FAST
#' numbers should not be quoted
#'
#' @param path [\code{character(1)}]\cr
#'   Path to ARFF file with read access.
#' @return [\code{data.frame}].
#' @export
#' @useDynLib farff c_dt_preproc c_rd_preproc

readARFF = function(path, tmp.file = tempfile(), data.reader = "readr", show.info = TRUE) {
  assertFile(path, access = "r")
  assertChoice(data.reader, c("readr", "data.table"))
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
      col.types = collapse(vcapply(col.types, function(x) substr(x, 1L, 1L)), sep = "")
      dat = read_delim(tmp.file, delim = ",", col_names = FALSE, col_types = col.types,
        escape_backslash = TRUE, escape_double = FALSE)
      dat = as.data.frame(dat)
    })
  }
  colnames(dat) = header$col.names
  print(str(dat))

  st4 = g({
  for (i in 1:ncol(dat)) {
    ct = header$col.types[i]
    if (ct == "factor") {
      clevs = header$col.levels[[i]]
      dat[,i] = factor(dat[,i], levels = clevs)
    }
  }
  })
  # dat = convertDataFrameCols(dat, chars.as.factor = TRUE)
  if (show.info)
    messagef("preproc: %f; header: %f; fread: %f; convert: %f", st1[3L], st2[3L], st3[3L], st4[3L])
  return(dat)
}



