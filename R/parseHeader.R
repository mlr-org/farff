# Helper function to parse the header of an arff file.
#
# @param path [character(1)]
#   Path to arff file.
# @return [list] of
parseHeader = function(path) {
  handle = file(path, "r")
  on.exit(close(handle))

  col.names = character(0L) # names of data cols
  col.types = character(0L) # mapped R data types
  col.dfmts = character(0L) # date formats (usually NA)
  col.levels = list()       # factor levels (charvec or NA)

  line = readLines(handle, n = 1L)
  line.counter = 1L
  while (length(line) && regexpr("^[[:space:]]*@(?i)data", line, perl = TRUE, ignore.case = TRUE) == -1L) {
    if (!stri_detect(line, regex = "^\\s*@(?i)relation") &&
      !stri_detect(line, regex = "^\\s*%.*") &&
      stri_trim(line) != "" ) {
      regex1 = "\\s*(?i)@attribute\\s+(\\S+)\\s+(real|numeric|integer|string|date|relational|\\{.*\\})"
      regex2 = "\\s*(?i)@attribute\\s+'(.*)'\\s+(real|numeric|integer|string|date|relational|\\{.*\\})"
      m = stri_match_first_regex(line, regex1)
      if (is.na(m[1L, 1L]))
        m = stri_match_first_regex(line, regex2)
      if (is.na(m[1L, 1L]))
        stopf("Invalid column specification line found in ARFF header:\n%s", line)

      cname = m[1L, 2L]
      ctype.cs = m[1L, 3L]
      ctype.ci = tolower(ctype.cs)
      cdfmt = NA
      clevs = NA

      if (ctype.ci == "date") {
        ctype = "character"
        cdfmt = NA
        if (!stri_detect_regex(line, "date$", case_insensitive = TRUE)) {
          # if date format is specified covert it to POSIX standard
          #FIXME: this is ugly, but works. Refactor on occasion
          splits = stri_split_regex(line, "date", case_insensitive = TRUE)[[1L]]
          dfmt = splits[length(splits)]
          dfmt = stri_trim(gsub("\"", "", dfmt))
          cdfmt = convertDatetimeFormatISO8601ToPOSIX(dfmt)
        } else {
          # otherwise simply use POSIX
          cdfmt = "%Y-%m-%d %H:%M:%S"
        }
      } else if (ctype.ci == "relational") {
        stop("Type 'relational' currently not implemented.")
      } else if (grepl("\\{.*", ctype.ci)) {
        # if we see "{*", then it is a factor, as {} contains the levels
        ctype = "factor"
        clevs = parseFactorLevels(ctype.cs, line = line)
      } else if (ctype.ci == "string") {
        ctype = "character"
      } else if (ctype.ci %in% c("real", "numeric")) {
        ctype = "numeric"
      } else if (ctype.ci == "integer") {
        ctype = "integer"
      } else if (ctype.ci == "relational") {
        # FIXME: allow multi-instance here: the header '@attribute var relational' to '@end var' might contain several multi-instance variables
        stopf("Type 'relational' currently not implemented.")
      } else {
        stopf("Invalid type found on line %i:\n%s", line.counter, line)
      }
      col.names = c(col.names, cname)
      col.types = c(col.types, ctype)
      col.dfmts = c(col.dfmts, cdfmt)
      col.levels[[length(col.levels) + 1L]] = clevs
    }
    line = readLines(handle, n = 1L)
    line.counter = line.counter + 1L
  }
  if (length(line) == 0L)
    stop("Missing data section.")

  # check that we dont have sparse format. the line after @DATA then would be: "   {*}  "
  repeat {
    first.data.line = trimws(readLines(handle, n = 1L))
    if (first.data.line != "") break
  }

  if (!is.na(stri_match_last(first.data.line, regex="^\\s*\\{.*\\}\\s*$")[1L ,1L]))
    stopf("File seems to be of sparse format. farff does not support this yet! First @DATA line is:\n%s",
      first.data.line)
  if (is.null(colnames))
    stop("Missing attribute section.")

  # remove some chars from the colnames, apparently RWeka strips these as well
  col.names = stri_trim(stri_replace_all(col.names, fixed = "\"", ""))
  col.names = stri_trim(stri_replace_all(col.names, fixed = "'", ""))
  col.names = stri_trim(stri_replace_all(col.names, fixed = "\\", ""))

  list(col.names = col.names, col.types = col.types, col.levels = col.levels,
    col.dfmts = col.dfmts, line.counter = line.counter)
}
