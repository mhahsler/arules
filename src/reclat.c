#include "rapriori.h"
/*----------------------------------------------------------------------
  File    : eclat.c
  Contents: eclat algorithm for finding frequent item sets
  Author  : Christian Borgelt
  History : 09.06.2002 file created from apriori.c
            10.12.2002 option -l (list supporting transactions added)
            16.08.2002 transaction reading improved
            18.08.2003 memory benchmarking functionality added
            20.08.2003 option -t (target type) added
            22.08.2003 based on transaction module from apriori
            23.08.2003 option -q (item sort control) added
            12.09.2003 option -u (sparse representation) added
	    16.08.2004 bug concerning option -q0 fixed (min. support)
----------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <time.h>
#include <assert.h>
//#include "scan.h"
#include "tract.h"
#include "bitmat.h"
#ifdef STORAGE
#include "storage.h"
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define PRGNAME     "eclat"
#define DESCRIPTION "find frequent item sets with the eclat algorithm"
#define VERSION     "version 2.6 (2004.08.16)         " \
                    "(c) 2002-2004   Christian Borgelt"
#define BLKSIZE  256            /* block size for enlarging vectors */


/* --- output flags --- */
#define OF_ABS        1         /* print absolute support */
/* #define OF_SCANF      2         /\* convert names to scanable form *\/ */
#define OF_LIST       4         /* list supporting transactions */

/* --- target types --- */
#define TT_SET        0         /* frequent item sets */
#define TT_MFSET      1         /* maximally frequent item sets */
#define TT_CLSET      2         /* closed item sets */
#define TT_RULE       3         /* association rules */
#define TT_HEDGE      4         /* association hyperedges */

/* --- error codes --- */
#define E_OPTION    (-5)        /* unknown option */
#define E_OPTARG    (-6)        /* missing option argument */
#define E_ARGCNT    (-7)        /* too few/many arguments */
#define E_STDIN     (-8)        /* double assignment of stdin */
#define E_TARGET    (-9)        /* invalid target type */
#define E_SUPP     (-10)        /* invalid item set support */
#define E_ITEMCNT  (-11)        /* invalid number of items */
#define E_NOTAS    (-12)        /* no items or transactions */
#define E_UNKNOWN  (-18)        /* unknown error */

#define SEC_SINCE(t)  ((clock()-(t)) /(double)CLOCKS_PER_SEC)
#define RECCNT(s)     (tfs_reccnt(is_tfscan(s)) \
                      + ((tfs_delim(is_tfscan(s)) == TFS_REC) ? 0 : 1))
#define BUFFER(s)     tfs_buf(is_tfscan(s))

/*----------------------------------------------------------------------
  Constants
----------------------------------------------------------------------*/
/* --- error messages --- */
static const char *errmsgs[] = {
  /* E_NONE      0 */  "no error\n",
  /* E_NOMEM    -1 */  "not enough memory\n",
  /* E_FOPEN    -2 */  "cannot open file %s\n",
  /* E_FREAD    -3 */  "read error on file %s\n",
  /* E_FWRITE   -4 */  "write error on file %s\n",
  /* E_OPTION   -5 */  "unknown option -%c\n",
  /* E_OPTARG   -6 */  "missing option argument\n",
  /* E_ARGCNT   -7 */  "wrong number of arguments\n",
  /* E_STDIN    -8 */  "double assignment of standard input\n",
  /* E_TARGET   -9 */  "invalid target type '%c'\n",
  /* E_SUPP    -10 */  "invalid minimal support %g\n",
  /* E_ITEMCNT -11 */  "invalid number of items %d\n",
  /* E_NOTAS   -12 */  "no items or transactions to work on\n",
  /*    -13 to -15 */  NULL, NULL, NULL,
  /* E_ITEMEXP -16 */  "file %s, record %d: item expected\n",
  /* E_DUPITEM -17 */  "file %s, record %d: duplicate item %s\n",
  /* E_UNKNOWN -18 */  "unknown error\n"
};

static const char* BMtarget[] = {
  /* BM_NORMAL      0 */  "frequent itemsets",
  /* BM_CLOSED      1 */  "closed frequent itemsets",
  /* BM_MAXIMAL     2 */  "maximally frequent itemsets",
};

/*----------------------------------------------------------------------
  Global Variables
----------------------------------------------------------------------*/
static ITEMSET *itemset = NULL; /* item set */
static TASET   *taset   = NULL; /* transaction set */
static RULESET *ruleset = NULL; /* ruleset */
static BITMAT  *bitmat  = NULL; /* bit matrix */
static int     tacnt    = 0;    /* number of transactions */
static int     flags    = 0;    /* output flags */
static int     size     = BLKSIZE;
static int     size1    = BLKSIZE;
static int     size2    = BLKSIZE;
static ARparameter param;

/*----------------------------------------------------------------------
  Auxiliary Functions
----------------------------------------------------------------------*/
void _cleanup(void)
{
	if (itemset) {is_delete(itemset); itemset = NULL;}  /* clean up memory */
	if (taset)   {tas_delete(taset, 0); taset = NULL;}
	if (ruleset) {rs_delete(ruleset); ruleset = NULL;}
	if (bitmat)  {bm_delete(bitmat); bitmat = NULL;}
}

const char* msg(int code)
{                               /* --- print an error message */
  const char *msg = NULL;              /* error message */

  if (code < E_UNKNOWN || code > 0) code = E_UNKNOWN;
  
  msg = errmsgs[-code];       /* get the error message */
  
  return msg;
}  /* msg() */


int BMtargetcode(const char* target)
{
	int k = 0;
	const char **p;
	
	for (p = BMtarget; *p; p++) {
		if (strcmp(target, *p) == 0) return k;
		k++;
	}
	return E_TARGET;
}

/*----------------------------------------------------------------------
  Item Set Report Function
----------------------------------------------------------------------*/

static void _report_R (int *ids, int cnt, int supp, int *tal, void *data)
{                               /* --- report a frequent item set */
  int        i, h;                 /* loop variable */
  const char *name;             /* buffer for item name */
  char    **vec = NULL;
  int     *vec1 = NULL;
  double  *vec2 = NULL;

  assert(ids && tal);           /* check the function arguments */
  h = ruleset->ttotal;
  ruleset->ttotal += cnt;
  if (ruleset->ttotal > size) {      /* if the item vector is full */
	  size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
	  vec   = (char**)realloc(ruleset->body, size *sizeof(char*));
	  if (!vec)  {
		  if (vec1) free(vec1);
		  if (vec2) free(vec2);
		  _cleanup(); error(msg(E_NOMEM));}    /* enlarge the item vector */
	  ruleset->body  = vec;
  }                             /* set the new vector and its size */
  for (i = 0; i < cnt; i++) {   /* traverse the item set */
    name = is_name(itemset, ids[i]);
    ruleset->body[h] = (char *)name;
    h++;
  }
  if (ruleset->rnb >= size1) {
	  size1 += (size1 > BLKSIZE) ? (size1 >> 1) : BLKSIZE;
	  vec1 = (int*)realloc(ruleset->tnb, size1 *sizeof(int));
	  if (!vec1) {
		  if (vec) free(vec);
		  if (vec2) free(vec2);
		  _cleanup(); error(msg(E_NOMEM));}
	  ruleset->tnb = vec1;
	  vec2 = (double*)realloc(ruleset->supp, size1 *sizeof(double));
	  if (!vec2) {
		  if (vec1) free(vec1);
		  if (vec) free(vec);
		  _cleanup(); error(msg(E_NOMEM));}   /* enlarge the item vector */
	  ruleset->supp = vec2;
	  if (param.ext) {
		  vec2 = (double*)realloc(ruleset->ext, size1 *sizeof(double));
		  if (!vec2) {
			  if (vec1) free(vec1);
			  if (vec) free(vec);
			  _cleanup(); error(msg(E_NOMEM));}   /* enlarge the item vector */
		  ruleset->ext = vec2;
	  }
	  if (flags & OF_LIST) {      
		  vec1 = (int*)realloc(ruleset->trnb, size1 *sizeof(int));
		  if (!vec1) {
			  if (vec) free(vec);
			  if (vec2) free(vec2);
			  _cleanup(); error(msg(E_NOMEM));}
		  ruleset->trnb = vec1;
	  }
  }
  if (ruleset->rnb == 0) ruleset->tnb[0] = cnt; else ruleset->tnb[ruleset->rnb] = ruleset->tnb[ruleset->rnb-1] + cnt;
  ruleset->supp[ruleset->rnb] = (abs(supp)/(double)tacnt);
  if (param.ext) ruleset->ext[ruleset->rnb] = (int)abs(supp);
  /* ruleset->rnb += 1;   */  
  if (flags & OF_LIST) {        /* if to list the transactions, */
	  h = ruleset->trtotal;
	  if (supp < 0) {             /* if bit vector representation */ 
		  for (i = 0; i < tacnt; i++) {  /* traverse the bit vector */ 
			  if (h >= size2) {
				  size2 += (size2 > BLKSIZE) ? (size2 >> 1) : BLKSIZE;
				  vec1 = (int*)realloc(ruleset->trans, size2 *sizeof(int));
				  if (!vec1) {
					  if (vec) free(vec);
					  if (vec2) free(vec2);
					  _cleanup(); error(msg(E_NOMEM));}
				  ruleset->trans = vec1;
			  }
			  /* Bug reported by Brian:  runtime error: left shift of 1 by 31 places 
			   *   cannot be represented in type 'int'. clang-UBSAN/gcc-UBSAN
			   * C99: If the value of the right operand is negative or is 
			   * greater than or equal to the width of the promoted left operand, 
			   * the behavior is undefined.
			   * BM_MASK   0x1f  (11111) this might be 32!
			   */
			  //if (tal[i >> BM_SHIFT] & (1 << (i & BM_MASK))) {
			  if (((i & BM_MASK) < 31) && (tal[i >> BM_SHIFT] & (1 << (i & BM_MASK)))) {
				  /*Rprintf(" %d", i+1);*/
				  ruleset->trans[h] = i;
				  h++;
			  }
		  }
	  }                       /* print the indices of set bits */
	  else {                      /* if list of transaction ids */
		  if ((h + supp) >= size2) {
			  while ((h + supp) >= size2) {
				  size2 += (size2 > BLKSIZE) ? (size2 >> 1) : BLKSIZE;
			  }
			  vec1 = (int*)realloc(ruleset->trans, size2 *sizeof(int));
			  if (!vec1) {
				  if (vec) free(vec);
				  if (vec2) free(vec2);
				  _cleanup(); error(msg(E_NOMEM));}
			  ruleset->trans = vec1;
		  }
		  for (i = 0; i < supp; i++) {
			  Rprintf(" %d", tal[i]); 
			  /*Rprintf(" %d", tal[i]); */
			  ruleset->trans[h] = tal[i];
			  h++;
		  }                           /* traverse and print */
	  }
	  ruleset->trtotal = ruleset->trnb[ruleset->rnb] = h;
  }                             /* the transaction identifiers */
  ruleset->rnb += 1;     
/*  free(vec);free(vec1);free(vec2);*/
}  /* _report_R() */
  
/*----------------------------------------------------------------------
  Main Functions
----------------------------------------------------------------------*/

SEXP reclat(SEXP x, SEXP y, SEXP dim, SEXP parms, SEXP control, SEXP itemInfo)
{
 	int i, l, k = 0, n = 0, maxcnt = 0;
	INPUT   in;
	SEXP ans;
/*	SEXP ans, tmp, tp, q, dmns, names;*/
/* 	char    *s;                   /\* to traverse the options *\/ */
/* 	char    **optarg = NULL;      /\* option argument *\/ */
/* 	char    *blanks  = NULL;      /\* blanks *\/ */
/* 	char    *fldseps = NULL;      /\* field  separators *\/ */
/* 	char    *recseps = NULL;      /\* record separators *\/ */
/* 	char    *cominds = NULL;      /\* comment indicators *\/ */
	double  supp     = 0.1;       /* minimal support (in percent) */
	int     target   = 's';       /* target type (sets/closed/maximal) */
	int     min      =  1;        /* minimal size of item set */
	int     max      =  5;        /* maximal size of item set */
	int     trans, ext;
	int     sort     = -2;        /* flag for item sorting and recoding */
	double  sparse   =  7;        /* threshold for sparse represent. */
	int     *map;                 /* identifier map for recoding */
	clock_t t;                    /* timer for measurements */

  param.verbose = *LOGICAL(GET_SLOT(control, install("verbose")));
  
  /*
  if (param.verbose) {
	  Rprintf("%s - %s\n", PRGNAME, DESCRIPTION);
	  Rprintf(VERSION);
	  Rprintf("\n");
  }
  */
  
  /* --- evaluate arguments --- */
  supp   = param.supp = *REAL(GET_SLOT(parms, install("support")));       /* minimal support 's'*/
  target = param.target = BMtargetcode(translateChar(STRING_ELT(GET_SLOT(parms, install("target")), 0)));
  /*target = param.target = BMtargetcode(translateChar(CHARACTER_POINTER(GET_SLOT(parms, install("target")))[0])); */
  /* target = param.target = BMtargetcode(CHAR(CHARACTER_POINTER(GET_SLOT(parms, install("target")))[0])); */
  min    = param.minlen = *INTEGER(GET_SLOT(parms, install("minlen")));   /* minimal rule length 'm'*/
  max    = param.maxlen = *INTEGER(GET_SLOT(parms, install("maxlen")));   /* maximal rule length 'n'*/  
  sort   = param.sort = *INTEGER(GET_SLOT(control, install("sort")));
            /* flag for item sorting and recoding 'q'*/	
  ext    = param.ext = *INTEGER(GET_SLOT(parms, install("ext")));
  sparse = *REAL(GET_SLOT(control, install("sparse")));
  trans  = param.trans = *INTEGER(GET_SLOT(parms, install("tidLists")));

  if (ext) flags |= OF_ABS;       
  if (trans) flags |= OF_LIST; 

  switch (param.target) {             /* check and translate target type */
  case 0: param.target = BM_NORMAL;            break;
  case 1: param.target = BM_CLOSED;            break;
  case 2: param.target = BM_MAXIMAL;           break;
  /* default : _cleanup(); error(msg(E_TARGET), (char *)target); break; */
  default : _cleanup(); error(msg(E_TARGET), (char)target); break;
  }
  if (supp > 1)                 /* check the minimal support */
  {_cleanup(); error(msg(E_SUPP), supp);}        /* (< 0: absolute number) */
  if (min <= 0) {_cleanup(); error(msg(E_ITEMCNT), min);}  /* check the limits */
  if (max <= 0) {_cleanup(); error(msg(E_ITEMCNT), max);}  /* for the rule length */

  if (param.verbose) Rprintf("create itemset ... \n");
  /* --- create item set and transaction set --- */
  in.x = AS_CHARACTER(y); 
  in.ind = INTEGER(x);
  in.index = 0;
  tacnt = in.tnb = length(x)-1;

  itemset = is_create();	
  if (!itemset) {_cleanup(); error(msg(E_NOMEM));}
  taset = tas_create(itemset);
  if (!taset) {_cleanup(); error(msg(E_NOMEM));}

  /* --- read transactions --- */
  t = clock();                /* start the timer */
  if (param.verbose)   Rprintf("set transactions ...");
  for (l = 0; l < tacnt; l++) {                 /* transaction read loop */
	  k = is_read_in(itemset, &in);             /* read the next transaction */
	  if (k < 0) {_cleanup(); error(msg(k), "read transactions", RECCNT(itemset), BUFFER(itemset));}
	  if (k > 0) break;
	  k = is_tsize(itemset);                /* update the maximal */
	  if (k > maxcnt) maxcnt = k;           /* transaction size */
	  if (taset && (tas_add(taset, NULL, 0) != 0)) {_cleanup(); error(msg(E_NOMEM));}
  }
  n = is_cnt(itemset);                          /* get the number of items */
  if (param.verbose) {
	  Rprintf("[%d item(s),", n);
	  Rprintf(" %d transaction(s)] done ", tacnt);
	  Rprintf("[%.2fs].", SEC_SINCE(t));
	  Rprintf("\n");
  }
  if ((n <= 0) || (tacnt <= 0)) {_cleanup(); error(msg(E_NOTAS));} 
  if (supp < 0) {               /* if absolute support is given */
	  supp = (-supp-0.5)/tacnt;
	  if (supp < 0) supp = 0;     /* compute a proper relative support */
  }                             /* and check it against 0 */
 
  /* --- sort and recode items --- */
  /* FIX broken gcc compilers */
  supp = (double)(int)ceil(tacnt *supp);     /* sort and recode the items */
  if (sort != 0) {              /* sort items w.r.t. their frequency */
    if (param.verbose) Rprintf("sorting and recoding items ... ");
    t   = clock();              /* start the timer */
    map = (int*)malloc(is_cnt(itemset) *sizeof(int));
    if (!map) {_cleanup(); error(msg(E_NOMEM));}   /* create an item identifier map */
    n = is_recode(itemset, (int)supp, sort, map);
    tas_recode(taset, map, n);  /* recode the loaded transactions */
    free(map);                  /* delete the item identifier map */
    if (param.verbose) {
	    Rprintf("[%d item(s)] ", n);
	    Rprintf("done [%.2fs].", SEC_SINCE(t));
	    Rprintf("\n"); /* check the number of items */
    }
    if (n <= 0) {_cleanup(); error(msg(E_NOTAS));} /* print a log message and */
  }                             /* (which may be zero now) */

  /* --- create a bit matrix --- */
  k = (tas_total(taset) *sparse < tacnt *n);
  if (param.verbose) Rprintf("creating %sbit matrix ... ", k ? "sparse ":"");
  t = clock();                  /* start the timer */
  bitmat = bm_create(n, (k) ? 0 : tacnt, k);
  if (!bitmat) {_cleanup(); error(msg(E_NOMEM));}  /* create a bit matrix, the columns */
  for (i = 0; i < tacnt; i++) { /* of which are the transactions */
    if (k) bm_addcol(bitmat,    tas_tract(taset,i), tas_tsize(taset,i));
    else   bm_setcol(bitmat, i, tas_tract(taset,i), tas_tsize(taset,i));
  }                             /* fill the bit matrix */
  tas_delete(taset, 0);         /* delete the transaction set */
  taset = NULL;                 /* and print a success message */
  if (param.verbose) {
	  Rprintf("[%d row(s), %d column(s)] ", n, tacnt);
	  Rprintf("done [%.2fs].\n", SEC_SINCE(t));
  }

  /* --- find frequent item sets --- */
  t = clock();                  /* start the timer */
  if (param.verbose) Rprintf("writing  ... ");
  ruleset = rs_create();
  size2 = size1 = size = BLKSIZE;
  if (!ruleset) {_cleanup(); error(msg(E_NOMEM));}
  ruleset->cnt = is_cnt(itemset);
  ruleset->tacnt = in.tnb;

  ruleset->ttotal = 0;
  k = bm_allone(bitmat, target, (int)supp, min, max, _report_R, NULL);
  if (k < 0) {_cleanup(); error(msg(E_NOMEM));}    /* search for frequent item sets */
  if (param.verbose) {
	  Rprintf("[%d set(s)] done ", k);
	  Rprintf("[%.2fs].\n", SEC_SINCE(t));
  }
  t = clock();                  /* start the timer */
  if (param.verbose) Rprintf("Creating S4 object  ... ");
  ans = PROTECT(returnObject(ruleset, dim, &param, itemInfo));
  if (param.verbose) {
	  Rprintf("done ");
	  Rprintf("[%.2fs].\n", SEC_SINCE(t));
  }

#ifdef BENCH                  /* if benchmark version */
  if (param.verbose) printf("memory used during search: %d bytes\n", bitmat->mem);
#endif                        /* print memory usage */

  /* --- clean up --- */
  _cleanup();
#ifdef STORAGE                /* if storage debugging */
  showmem("at end of program"); /* check memory usage */
#endif
  UNPROTECT(1);
  return ans;                     /* return 'ok' */
}  /* main() */
