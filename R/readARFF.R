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
#' @useDynLib farff c_preproc

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

  st1 = g({header = parseHeader(path)})
  # print(header)

  st2 = g(.Call(c_preproc, path, tmp.file, as.integer(header$line.counter)))

  col.types = str_replace_all(header$col.types, "factor", "character")

  st3 = g({
    dat = fread(tmp.file, header = FALSE, sep = ",", stringsAsFactors = FALSE,
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
    }
  }
  })
  # dat = convertDataFrameCols(dat, chars.as.factor = TRUE)
  if (show.info)
    messagef("preproc: %f; header: %f; fread: %f; convert: %f", st1[3L], st2[3L], st3[3L], st4[3L])
  return(dat)
}



