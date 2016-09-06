# Function to convert data format in ISO8601 format
# to POSIX.
# Copied from foreign package.
#
# @param x [character(1)]
#   Datetime in ISO8601 format.
# @return [character(1)] Datetime in POSIX format.
convertDatetimeFormatISO8601ToPOSIX = function(x) {
  # First, Weka thinks that 'yyyy' is ISO 8601 ...
  x = sub("yyyy", "%Y", x, ignore.case = TRUE)
  # And it's 'DD' and not 'dd' ...
  x = sub("dd", "%d", x)
  # And it's 'hh' and not 'HH' ...
  x = sub("HH", "%H", x)

  # Now the real stuff.
  # Is there a POSIX format string for the century component of year?
  x = sub("CCYY", "%Y", x)
  x = sub("YY", "%y", x)
  x = sub("MM", "%m", x)
  x = sub("DD", "%d", x)
  x = sub("DDD", "%j", x)
  x = sub("ww", "%U", x)
  x = sub("D", "%w", x)
  x = sub("hh", "%H", x)
  x = sub("mm", "%M", x)
  x = sub("ss", "%S", x)

  x
}
