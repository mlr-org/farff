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
  char old_quote = 0;


  while(1) {
    /* got string delim: copy + take a break */
    if (s[i] == 0) {
      t[j] = 0; i++; j++;
      break;
    }

    if (in_quotes) {
      /* in quote, we copy all chars, except if we find the exakt old quote char from the start */
      /* in that case we copy " to t and leave quote mode */
      if (s[i] == old_quote) {
        t[j] = '"'; i++; j++;
        in_quotes = 0;
      } else {
        t[j] = s[i]; i++; j++;
      }
    } else {
      switch(s[i]) {
        case '\'':; /* found quote: copy " to t, go to quoted mode and store old quote */
        case '"':
          t[j] = '"'; old_quote = s[i]; in_quotes = 1; i++; j++; break;
        case ' ': /* got space: remove the crap */
          i++; break;
        case '?': /* got ?: copy NA */
         t[j++] = 'N'; t[j++] = 'A'; i++; break;
        default: /*copy slot */
          t[j] = s[i]; i++; j++; break;
      }
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


