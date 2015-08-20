#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <ctype.h>

int is_empty(const char *s) {
  while (*s != '\0') {
    if (!isspace(*s))
      return 0;
    s++;
  }
  return 1;
}

SEXP c_preproc(SEXP s_path_in, SEXP s_path_out) {

  FILE* handle_in;
  FILE* handle_out;
  const char* path_in = CHAR(asChar(s_path_in));
  const char* path_out = CHAR(asChar(s_path_out));
  char line1[50000];
  char line2[50000];
  char* comment_char;
  char* line_out;
  int n;

  handle_in = fopen(path_in, "r");
  handle_out = fopen(path_out, "w");

  while (fgets(line1, sizeof line1, handle_in)) {
    size_t len = strlen(line1);
    comment_char = strchr(line1, '%');
    if (comment_char == NULL) {
      line_out = line1;
    } else {
      n = comment_char - line1;
      strncpy(line2, line1, n);
      line2[n] = 0;
      line_out = line2;
    }

    if (!is_empty(line_out))
      fputs(line_out, handle_out);
  }
  fclose(handle_in);
  fclose(handle_out);
  return R_NilValue;
}


