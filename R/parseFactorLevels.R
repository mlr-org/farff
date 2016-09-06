# Helper functions to parse levels of factor/categorical variables.
#
# @param x [character(1)]
#   Unparsed list of factor levels of type {lvl1, lev2, ..., lvln}.
# @param line [character(1)]
#   Which line is processed?
# @return [character] vector of parsed factor levels.
parseFactorLevels = function(x, line = "LINE NOT GIVEN") {

  # Helper function to read a part of the unparsed string.
  #
  # @param s [character(1)]
  #   String to parse.
  # @param r [character(1)]
  #   Regex.
  # @param no.match.error [logical(1)]
  #   Should we throw an error if no matching found?
  #   Default is FALSE.
  consume = function(s, r, no.match.error = FALSE) {
    m = stri_match_first_regex(s, r)[1L, ]
    if (is.na(m[1L])) {
      if (no.match.error)
        stopf("Error while parsing factor levels in line:\n%s", line)
      return(NULL)
    }
    if (length(m) == 1L)
      return(list(rest = substr(s, nchar(m[1L]) + 1L, nchar(s))))
    return(list(rest = substr(s, nchar(m[1L]) + 1L, nchar(s)), match = m[2L]))
  }

  levs = character(0L)

  # read everything like: "WS { WS"
  z = consume(x, "^\\s*\\{\\s*", no.match.error = TRUE)

  x = z$rest
  while(nchar(x) > 0L) {
    # it could be: single-quoted level
    # try to read SQ somechars!=SQ (but we need take of quoted quotes)
    # z = consume(x, "^'([^']*)'", no.match.error = FALSE)
    z = consume(x, "^'((\\\\.|[^'\\\\])*)'", no.match.error = FALSE)
    lev = z$match
    # somehow the quoted quotes get too many backslashes, remove them
    lev = stri_replace_all(lev, regex = "\\\\'", "\\'")
    # it could be: double-quoted level (but we need to take care of quoted quotes)
    # try to read DQ somechars!=DQ
    if (is.null(z)) {
      z = consume(x, '^"((\\\\.|[^"\\\\])*)"', no.match.error = FALSE)
      lev = z$match
      # somehow the quoted quotes get too many backslashes, remove them
      lev = stri_replace_all(lev, regex = '\\\\"', '\\"')
    }
    # or it could be: level without quotes
    # read somechars!=anyquotes
    if (is.null(z)) {
      z = consume(x, "^([^,}]*)", no.match.error = TRUE)
      # the regexp above could also match some trailing ws before the comma
      lev = stri_trim(z$match)
    }
    levs = c(levs, lev)
    x = z$rest
    z = consume(x, "\\s*,\\s*")
    if (is.null(z))
      break
    x = z$rest
  }
  z = consume(x, "^\\s*\\}\\s*", no.match.error = TRUE)
  levs = stri_replace_all(levs, "%", fixed = "\\%")
  return(levs)
}
