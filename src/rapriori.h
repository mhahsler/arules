#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>
#include "tract.h"
#include "istree.h"
#include "vecops.h"
#include "symtab.h"
#include "tfscan.h"
#ifdef STORAGE
#include "storage.h"
#endif

#include <R.h>
/* #include <Rinternals.h> */
#include <Rdefines.h>

/*-----------------------------------------------------------------------
  Type Definitions BG
----------------------------------------------------------------------*/
typedef struct{
	SEXP x;                 /* vector of transactions */
	int *ind;               /* vector of transaction lengths */
	int index;              /* index which transactions processed */
	int tnb;                /* number of transactions */
} INPUT;

typedef struct {                /* --- parameter values--- */
	double supp;
	double conf;
	double minval;
	double filter;
	double smax;
	int target;
	int minlen;
	int maxlen;          /* maximal rule length */
	double maxtime;         /* maximal time allowed */
	int rsdef;
	int arem;
	int tree;
	int heap;
	int aval;
	int sort;
	int memopt;
	int ext;
	int verbose;
	int trans;
} ARparameter;

typedef struct {
	char *def;
	char **items;
	int *set;
} APP;

typedef struct{
	int tacnt;
	int cnt;
	int rnb;                /* number of rules */
	int ttotal;             
	int *tnb;
	char **body;
	char **head;
	int *trans;
	int *trnb;
	int trtotal;
	double *supp;
	double *conf;
	double *lift;
	double *aval;
	double *ext;
} RULESET;

/*----------------------------------------------------------------------
  Rule Set Functions
----------------------------------------------------------------------*/
extern RULESET*   rs_create  (void);
void              rs_delete (RULESET *ruleset);

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/

SEXP              rapriori(SEXP x, SEXP y, SEXP dim, SEXP parms, SEXP control, SEXP app, SEXP itemInfo);
int               is_read_in(ITEMSET *iset, INPUT *in);
SEXP              returnObject(RULESET *set, SEXP dim, ARparameter* param, SEXP itemInfo);
