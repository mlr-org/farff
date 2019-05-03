#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP c_dt_preproc(SEXP, SEXP, SEXP);
extern SEXP c_rd_preproc(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_dt_preproc", (DL_FUNC) &c_dt_preproc, 3},
    {"c_rd_preproc", (DL_FUNC) &c_rd_preproc, 3},
    {NULL, NULL, 0}
};

void R_init_farff(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
