
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP is_subset(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP OUT_P);
extern SEXP reclat(SEXP x, SEXP y, SEXP dim, SEXP parms, SEXP control,
		   SEXP itemInfo);
extern SEXP rapriori(SEXP x, SEXP y, SEXP dim, SEXP parms, SEXP control,
		     SEXP app, SEXP itemInfo);
extern SEXP R_transpose_ngCMatrix(SEXP x);
extern SEXP R_crosstab_ngCMatrix(SEXP x, SEXP y, SEXP t);
extern SEXP R_rowSums_ngCMatrix(SEXP x);
extern SEXP R_colSums_ngCMatrix(SEXP x);
extern SEXP R_colSubset_ngCMatrix(SEXP x, SEXP s);
extern SEXP R_rowSubset_ngCMatrix(SEXP x, SEXP s);
extern SEXP R_asList_ngCMatrix(SEXP x, SEXP d);
extern SEXP R_cbind_ngCMatrix(SEXP x, SEXP y);
extern SEXP R_recode_ngCMatrix(SEXP x, SEXP s);
extern SEXP R_or_ngCMatrix(SEXP x, SEXP y);
extern SEXP R_valid_ngCMatrix(SEXP x);
extern SEXP R_pncount(SEXP R_x, SEXP R_t, SEXP R_s, SEXP R_o, SEXP R_v);
extern SEXP R_pnindex(SEXP R_x, SEXP R_y, SEXP R_v);
extern SEXP R_pnclosed(SEXP R_x, SEXP R_c, SEXP R_v);
extern SEXP R_pnmax(SEXP R_x, SEXP R_c, SEXP R_v);
extern SEXP R_pnrindex(SEXP R_x, SEXP R_v);
extern SEXP R_similarity_ngCMatrix(SEXP x, SEXP y, SEXP R_method,
				   SEXP R_weight);
extern SEXP R_hits_ngCMatrix(SEXP x, SEXP R_iter, SEXP R_tol, SEXP R_verbose);
extern SEXP R_rowWSums_ngCMatrix(SEXP x, SEXP R_weight);
extern SEXP R_colWSums_ngCMatrix(SEXP x, SEXP R_weight);
extern SEXP R_weclat_ngCMatrix(SEXP x, SEXP R_weight, SEXP R_support,
			       SEXP R_minlen, SEXP R_maxlen, SEXP R_verbose);
extern SEXP R_wcount_ngCMatrix(SEXP x, SEXP t, SEXP R_weight,
			       SEXP R_fun, SEXP R_args, SEXP R_verbose);
extern SEXP R_na_zero(SEXP x);
extern SEXP R_tid_support(SEXP tidLists, SEXP itemsets);
extern SEXP R_tid_rules(SEXP tidLists, SEXP itemsets);

void R_init_arules(DllInfo *dll) {

    const R_CallMethodDef CallEntries[] = {
	{"R_is_subset", (DL_FUNC) is_subset, 7},
	{"R_reclat",		  (DL_FUNC) reclat,		    6},
	{"R_rapriori",		  (DL_FUNC) rapriori,		    7},
	{"R_transpose_ngCMatrix", (DL_FUNC) R_transpose_ngCMatrix,  1},
	{"R_crosstab_ngCMatrix",  (DL_FUNC) R_crosstab_ngCMatrix,   3},
	{"R_rowSums_ngCMatrix",   (DL_FUNC) R_rowSums_ngCMatrix,    1},
	{"R_colSums_ngCMatrix",   (DL_FUNC) R_colSums_ngCMatrix,    1},
	{"R_colSubset_ngCMatrix", (DL_FUNC) R_colSubset_ngCMatrix,  2},
	{"R_rowSubset_ngCMatrix", (DL_FUNC) R_rowSubset_ngCMatrix,  2},
	{"R_asList_ngCMatrix",    (DL_FUNC) R_asList_ngCMatrix,	    2},
	{"R_cbind_ngCMatrix",     (DL_FUNC) R_cbind_ngCMatrix,	    2},
	{"R_recode_ngCMatrix",    (DL_FUNC) R_recode_ngCMatrix,	    2},
	{"R_or_ngCMatrix",	  (DL_FUNC) R_or_ngCMatrix,	    2},
	{"R_valid_ngCMatrix",     (DL_FUNC) R_valid_ngCMatrix,	    1},
	{"R_pncount",		  (DL_FUNC) R_pncount,		    5},
	{"R_pnindex",		  (DL_FUNC) R_pnindex,		    3},
	{"R_pnclosed",		  (DL_FUNC) R_pnclosed,		    3},
	{"R_pnmax",		  (DL_FUNC) R_pnmax,		    3},
	{"R_pnrindex",		  (DL_FUNC) R_pnrindex,		    2},
	{"R_similarity_ngCMatrix",(DL_FUNC) R_similarity_ngCMatrix, 4},
	{"R_hits_ngCMatrix",      (DL_FUNC) R_hits_ngCMatrix,	    4},
	{"R_rowWSums_ngCMatrix",  (DL_FUNC) R_rowWSums_ngCMatrix,   2},
	{"R_colWSums_ngCMatrix",  (DL_FUNC) R_colWSums_ngCMatrix,   2},
	{"R_weclat_ngCMatrix",    (DL_FUNC) R_weclat_ngCMatrix,	    6},
	{"R_wcount_ngCMatrix",    (DL_FUNC) R_wcount_ngCMatrix,	    6},
	{"R_na_zero",		  (DL_FUNC) R_na_zero,		    1},
	{"R_tid_support",	  (DL_FUNC) R_tid_support,	    2},
	{"R_tid_rules",		  (DL_FUNC) R_tid_rules,	    2},
	{NULL, NULL, 0}
    };

    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    R_RegisterCCallable("arules", "R_rowSums_ngCMatrix",   (DL_FUNC) R_rowSums_ngCMatrix);
    R_RegisterCCallable("arules", "R_colSums_ngCMatrix",   (DL_FUNC) R_colSums_ngCMatrix);
    R_RegisterCCallable("arules", "R_colSubset_ngCMatrix", (DL_FUNC) R_colSubset_ngCMatrix);
    R_RegisterCCallable("arules", "R_rowSubset_ngCMatrix", (DL_FUNC) R_rowSubset_ngCMatrix);
    R_RegisterCCallable("arules", "R_asList_ngCMatrix",    (DL_FUNC) R_asList_ngCMatrix);
    R_RegisterCCallable("arules", "R_cbind_ngCMatrix",     (DL_FUNC) R_cbind_ngCMatrix);
    R_RegisterCCallable("arules", "R_recode_ngCMatrix",    (DL_FUNC) R_recode_ngCMatrix);
    R_RegisterCCallable("arules", "R_pnindex",             (DL_FUNC) R_pnindex);
    R_RegisterCCallable("arules", "R_pnrindex",		   (DL_FUNC) R_pnrindex);
}
