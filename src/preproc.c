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

void convert_line(char s[], char t[]) {
  int i = 0;
  int j = 0;
  int in_quotes = 0;
  int is_quote = 0;


  while(1) {
    /* got string delim: copy + take a break */
    if (s[i] == 0) {
      t[j] = 0; i++; j++;
      break;
    /* got quote:  set standard quote in t for data.table + toggle in_quotes */
    } else if (s[i] == '\'' || s[i] == '"') {
      t[j] = '"'; i++; j++;
      in_quotes = !in_quotes;
    /* got space + not in quotes: remove the crap */
    } else if (!in_quotes && s[i] == ' ') {
      i++;
    /* got ? and not in quotes: copy NA */
    } else if (!in_quotes && s[i] == '?') {
      t[j++] = 'N'; t[j++] = 'A';
      i++;
    /* else: copy slot */
    } else {
      t[j] = s[i]; i++; j++;
    }
  }
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
  char line_buf_1[50000];
  char line_buf_2[50000];
  char* line_p;
  int data_sect_reached = 0;

  handle_in = fopen(path_in, "r");
  handle_out = fopen(path_out, "w");

  while (fgets(line_buf_1, sizeof line_buf_1, handle_in)) {
    if (strcmp(line_buf_1, "@data\n") == 0 || strcmp(line_buf_1, "@DATA\n") == 0)
      data_sect_reached = 1;
    if (data_sect_reached) {
      convert_line(line_buf_1, line_buf_2);
      line_p = line_buf_2;
    } else {
      line_p = line_buf_1;
    }
    if (line_p[0] != '%' && !is_empty(line_p)) {
      fputs(line_p, handle_out);
    }
  }
  fclose(handle_in);
  fclose(handle_out);
  return R_NilValue;
}


