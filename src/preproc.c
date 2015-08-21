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

void remove_char(char *s, char c) {
  int writer = 0, reader = 0;

  while (s[reader]) {
    if (s[reader]!=c) {
      s[writer++] = s[reader];
    }
    reader++;
  }
  s[writer]=0;
}

void convert_quotes(char *s) {
  int i = 0;

  while (s[i]) {
    if (s[i] == '\'')
      s[i] = '"';
    i++;
  }
  s[i] = 0;
}

char* trim_whitespace(char *str) {
  char *end;

  // Trim leading space
  while(isspace(*str)) str++;

  if(*str == 0)  // All spaces?
    return str;

  // Trim trailing space
  end = str + strlen(str) - 1;
  while(end > str && isspace(*end)) end--;

  // Write new null terminator
  *(end+1) = '\n';
  *(end+2) = 0;

  return str;
}

/* throw away line if it
 * - is empty
 * - exactly starts with comment char '%'
*/
SEXP c_preproc(SEXP s_path_in, SEXP s_path_out) {

  FILE* handle_in;
  FILE* handle_out;
  const char* path_in = CHAR(asChar(s_path_in));
  const char* path_out = CHAR(asChar(s_path_out));
  char line[50000];
  char* line2;

  handle_in = fopen(path_in, "r");
  handle_out = fopen(path_out, "w");

  while (fgets(line, sizeof line, handle_in)) {
    line2 = trim_whitespace(line);
    /* line2 = line; */
    if (line2[0] != '%' && !is_empty(line2)) {
      convert_quotes(line2);
      fputs(line2, handle_out);
    }
  }
  fclose(handle_in);
  fclose(handle_out);
  return R_NilValue;
}


