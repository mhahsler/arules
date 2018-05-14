/*----------------------------------------------------------------------
 The code for apriori and eclat were obtained from http://www.borgelt.net/
 
 and are copyrighted by 1996-2003 Christian Borgelt
 
 This program is free software; you can redistribute it and/or modify it under
 the terms of the LGPL.
 
 The R-interface was added by Bettina Gruen and modified by Michael Hahsler (MFH)
 ----------------------------------------------------------------------*/

#include "rapriori.h"

/*----------------------------------------------------------------------
 Constants
 ----------------------------------------------------------------------*/
static const char* ttypes[] = {
  /* TT_SET      0 */  "set",
  /* TT_MFSET    1 */  "set",
  /* TT_CLSET    2 */  "set",
  /* TT_RULE     3 */  "rule",
  /* TT_HEDGE    4 */  "hyperedge",
};

static const char* ttarget[] = {
  /* TT_SET      0 */  "frequent itemsets",
  /* TT_MFSET    1 */  "maximally frequent itemsets",
  /* TT_CLSET    2 */  "closed frequent itemsets",
  /* TT_RULE     3 */  "rules",
  /* TT_HEDGE    4 */  "hyperedgesets",
};


static const char* aremtypes[] = {
  /* EM_NONE  0 */  "none",      /* no additional evaluation measure */
  /* EM_DIFF  1 */  "diff",      /* absolute confidence difference */
  /* EM_QUOT  2 */  "quot",      /* difference of conf. quotient to 1 */
  /* EM_AIMP  3 */  "aimp",      /* abs. diff. of improvement to 1 */
  /* EM_INFO  4 */  "info",      /* information difference to prior */
  /* EM_CHI2  5 */  "chi2",      /* normalized chi^2 measure */
};                              /* table of evaluation functions */
  
  
  /* --- error messages --- */
  static const char *errmsgs[] = {
    /* E_NONE      0 */  "no error\n",
    /* E_NOMEM    -1 */  "not enough memory. Increase minimum support!\n",
    /* E_FOPEN    -2 */  "cannot open file %s\n",
    /* E_FREAD    -3 */  "read error on file %s\n",
    /* E_FWRITE   -4 */  "write error on file %s\n",
    /* E_OPTION   -5 */  "unknown option -%c\n",
    /* E_OPTARG   -6 */  "missing option argument\n",
    /* E_ARGCNT   -7 */  "wrong number of arguments\n",
    /* E_STDIN    -8 */  "double assignment of standard input\n",
    /* E_TARGET   -9 */  "invalid target type '%s'\n",
    /* E_SUPP    -10 */  "invalid minimal support %g\n",
    /* E_CONF    -11 */  "invalid minimal confidence %g\n",
    /* E_MEASURE -12 */  "invalid additional evaluation measure %c\n",
    /* E_MVAL    -13 */  "invalid value %g for evaluation measure\n",
    /* E_RULELEN -14 */  "invalid set size/rule length %d\n",
    /* E_NOTAS   -15 */  "no items or transactions to work on\n",
    /* E_ITEMEXP -16 */  "file %s, record %d: item expected\n",
    /* E_DUPITEM -17 */  "file %s, record %d: duplicate item %s\n",
    /* E_APPEXP  -18 */  "file %s, record %d: "
    "appearance indicator expected\n",
    /* E_UNKAPP  -19 */  "unknown appearance indicator %s\n",
    /* E_FLDCNT  -20 */  "file %s, record %d: too many fields\n",
    /* E_UNKNOWN -21 */  "unknown error\n"
  };
  
  /*----------------------------------------------------------------------
   Preprocessor Definitions
   ----------------------------------------------------------------------*/
#define BLKSIZE  256            /* block size for enlarging vectors */
  
#define PRGNAME     "apriori"
#define DESCRIPTION "find association rules with the apriori algorithm"
#define VERSION     "version 4.21 (2004.05.09)        " \
  "(c) 1996-2004   Christian Borgelt"

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
#define E_SUPP     (-10)        /* invalid suppzort */
#define E_CONF     (-11)        /* invalid confidence */
#define E_MEASURE  (-12)        /* invalid evaluation measure */
#define E_MVAL     (-13)        /* invalid value for measure */
#define E_RULELEN  (-14)        /* invalid rule length */
#define E_NOTAS    (-15)        /* no items or transactions */
#define E_UNKNOWN  (-21)        /* unknown error */


#define SEC_SINCE(t)  ((clock()-(t)) /(double)CLOCKS_PER_SEC)
#define RECCNT(s)     (tfs_reccnt(is_tfscan(s)) \
+ ((tfs_delim(is_tfscan(s)) == TFS_REC) ? 0 : 1))
#define BUFFER(s)     tfs_buf(is_tfscan(s))

/*----------------------------------------------------------------------
 Constants
 ----------------------------------------------------------------------*/
/* --- item appearance indicators --- */
static const char *i_body[] = { /* item to appear in bodies only */
"i",  "in",  "a", "ante", "antecedent", "b", "body", "lhs", "items", NULL };
static const char *i_head[] = { /* item to appear in heads only */
"o",  "out", "c", "cons", "consequent", "h", "head", "rhs", NULL };
static const char *i_both[] = { /* item to appear in both */
"io", "inout", "ac", "bh", "both",                   NULL };
static const char *i_ignore[] ={/* item to ignore */
"n", "neither", "none", "ign", "ignore", "-",        NULL };

/*----------------------------------------------------------------------
 Auxiliary Functions
 ----------------------------------------------------------------------*/
static int appcode (const char *s)
{                               /* --- get appearance indicator code */
const char **p;               /* to traverse indicator list */

assert(s);                    /* check the function argument */
for (p = i_body;   *p; p++)   /* check 'body' indicators */
if (strcmp(s, *p) == 0) return APP_BODY;
for (p = i_head;   *p; p++)   /* check 'head' indicators */
if (strcmp(s, *p) == 0) return APP_HEAD;
for (p = i_both;   *p; p++)   /* check 'both' indicators */
if (strcmp(s, *p) == 0) return APP_BOTH;
for (p = i_ignore; *p; p++)   /* check 'ignore' indicators */
if (strcmp(s, *p) == 0) return APP_NONE;
return -1;                    /* if none found, return error code */
}  /* appcode() */

static int appcode_i (const int i)
{                               /* --- get appearance indicator code */
switch (i) {             
case 0: return APP_BODY;
case 1: return APP_HEAD;
case 2: return APP_BOTH;
case 3: return APP_NONE;
default : return -1;
}
}  /* appcode_i() */

/*----------------------------------------------------------------------
 Global Variables
 ----------------------------------------------------------------------*/
static ITEMSET *itemset = NULL; /* item set */
static TASET   *taset   = NULL; /* transaction set */
static TATREE  *tatree  = NULL; /* transaction tree */
static ISTREE  *istree  = NULL; /* item set tree */
static RULESET *ruleset = NULL; /* ruleset */

/*----------------------------------------------------------------------
 Modification of tract.c
 ----------------------------------------------------------------------*/

/* static int get_item (ITEMSET *iset, char *x) */
static int get_item (ITEMSET *iset, const char *x)
{                               /* --- read an item */
int  d = 0;                       /* delimiter type */
ITEM *item;                   /* pointer to item */
int  *ivec;                    /* new item vector */
int  size;                    /* new item vector size */

assert(iset);             /* check the function arguments */ 
item = nim_byname(iset->nimap, x);

if (!item) {                  /* look up the name in name/id map */
if (iset->app == APP_NONE)  /* if new items are to be ignored, */
return d;                 /* do not register the item */
item = nim_add(iset->nimap, x, sizeof(ITEM));
if (!item) return E_NOMEM;  /* add the new item to the map, */
item->frq = item->xfq = 0;  /* initialize the frequency counters */
item->app = iset->app;      /* (occurrence and sum of t.a. sizes) */
}                             /* and set the appearance indicator */
size = iset->vsz;             /* get the item vector size */
if (iset->cnt >= size) {      /* if the item vector is full */
size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
  ivec   = (int*)realloc(iset->items, size *sizeof(int));
  if (!iset->items) return E_NOMEM;   /* enlarge the item vector */
iset->items = ivec;
iset->vsz = size;
}                             /* set the new vector and its size */
iset->items[iset->cnt++] = item->id;
return d;                     /* add the item to the transaction */
}  /* _get_item() */            /* and return the delimiter type */

/*----------------------------------------------------------------------
 Rule Set Functions
 ----------------------------------------------------------------------*/

RULESET* rs_create(void)
{
  RULESET *ruleset;
  
  ruleset = malloc(sizeof(RULESET));
  if (!ruleset) return NULL;
  ruleset->ttotal = BLKSIZE;
  ruleset->trtotal = 0;
  ruleset->rnb = 0;
  ruleset->tnb = (int*)malloc(BLKSIZE *sizeof(int));
  ruleset->trnb = (int*)malloc(BLKSIZE *sizeof(int));
  ruleset->trans = (int*)malloc(BLKSIZE *sizeof(int));
  ruleset->body = (char**)malloc(BLKSIZE *sizeof(char *));
  ruleset->head = (char**)malloc(BLKSIZE *sizeof(char *));
  ruleset->lift = (double*)malloc(BLKSIZE *sizeof(double));
  ruleset->conf = (double*)malloc(BLKSIZE *sizeof(double));
  ruleset->supp = (double*)malloc(BLKSIZE *sizeof(double));
  ruleset->aval = (double*)malloc(BLKSIZE *sizeof(double));
  ruleset->ext = (double*)malloc(BLKSIZE *sizeof(double));
  ruleset->tnb[0] = 0;
  ruleset->trnb[0] = 0;
  return ruleset;
}

void rs_delete (RULESET *ruleset)
{                               /* --- delete an item set */
assert(ruleset);                 /* check the function argument */
if (ruleset->tnb)  free(ruleset->tnb);
if (ruleset->trnb)  free(ruleset->trnb);
if (ruleset->trans)  free(ruleset->trans);
if (ruleset->body)  free(ruleset->body);
if (ruleset->head)  free(ruleset->head);
if (ruleset->lift)  free(ruleset->lift);
if (ruleset->conf)  free(ruleset->conf);
if (ruleset->supp)  free(ruleset->supp);
if (ruleset->aval)  free(ruleset->aval);
if (ruleset->ext)  free(ruleset->ext);
free(ruleset);                   /* delete the components */
}  /* is_delete() */            /* and the item set body */



/*----------------------------------------------------------------------
 Auxiliary Functions
 ----------------------------------------------------------------------*/
int targetcode(const char* target)
{
  int k = 0;
  const char **p;
  
  for (p = ttarget; *p; p++) {
    if (strcmp(target, *p) == 0) return k;
    k++;
  }
  
  return E_TARGET;
}

int aremcode(const char* arem)
{
  int k = 0;
  const char **p;
  
  for (p = aremtypes; *p; p++) {
    if (strcmp(arem, *p) == 0) return k;
    k++;
  }
  return E_MEASURE;
}

const char* msgs(int code, ...)
{                               /* --- print an error message */
const char *msg = NULL;              /* error message */

if (code < E_UNKNOWN || code > 0) code = E_UNKNOWN;
msg = errmsgs[-code];       /* get the error message */

return msg;
}  /* msgs() */

void cleanup(void)
{
  if (istree)  {ist_delete(istree); istree = NULL;}  /* clean up memory */
if (tatree)  {tat_delete(tatree); tatree = NULL;}  
if (taset)   {tas_delete(taset, 0); taset = NULL;}  
if (itemset) {is_delete(itemset); itemset  = NULL;}  
if (ruleset) {rs_delete(ruleset); ruleset  = NULL;}  
}

/*--------------------------------------------------------------------*/
int is_read_in (ITEMSET *iset, INPUT *in)
{                               /* --- read a transaction */
/* MFH: removed unused d (7/26/14) */
/* int  i, d;  */                   /* loop variable, delimiter type */
int  i;                    /* loop variable, delimiter type */
ITEM *item;                   /* pointer to item */

assert(iset && in);         /* check the function arguments */
iset->cnt = 0;                /* initialize the item counter */
if (in->index >= in->tnb) return 1;
for (i = in->ind[in->index]; i < in->ind[in->index+1]; i++) {
  get_item(iset, translateChar(STRING_ELT(in->x, i)));
  /* d = get_item(iset, CHAR(STRING_ELT(in->x, i)));*/
}
in->index++;

ta_sort(iset->items, iset->cnt); /* prepare the transaction */
  iset->cnt = ta_unique(iset->items, iset->cnt);
  for (i = iset->cnt; --i >= 0; ) {
    item = nim_byid(iset->nimap, iset->items[i]);
    item->frq += 1;             /* count the item and */
  item->xfq += iset->cnt;     /* sum the transaction sizes */
  }                             /* as an importance indicator */
  return 0;                     /* return 'ok' */
}  /* is_read_in() */
  
  /*--------------------------------------------------------------------*/
  int is_readapp_R (ITEMSET *iset, SEXP app)
  {                               /* --- read appearance indicators */
  int  i, j, h;                       /* delimiter type */
  ITEM *item;                   /* to access the item data */
  const char *def;
  int *set;
  SEXP items;
  
  assert(iset && app);         /* check the function arguments */
  def = translateChar(STRING_ELT(GET_SLOT(app, install("default")), 0));
  /* def = CHAR(STRING_ELT(GET_SLOT(app, install("default")), 0)); */
  set = INTEGER(GET_SLOT(app, install("set")));
  items = PROTECT(AS_CHARACTER(GET_SLOT(app, install("items"))));
  
  iset->app = appcode(def);    /* get default appearance code */
  if (iset->app < 0)    return E_UNKAPP;
  h = 0;
  for (i = 0; i < 5; i++) {
    for (j = 0; j < set[i]; j++) {
      item = nim_add(iset->nimap, translateChar(STRING_ELT(items, h)), sizeof(ITEM));
      /* item = nim_add(iset->nimap, CHAR(STRING_ELT(items, h)), sizeof(ITEM)); */
      if (item == EXISTS) {
        UNPROTECT(1);
        return E_DUPITEM;  /* add the new item */
      }
      
      if (item == NULL) {
        UNPROTECT(1);
        return E_NOMEM;    /* to the name/id map */
      }
      
      item->frq = 0;              /* clear the frequency counters */
      item->xfq = 0;              /* (occurrence and sum of t.a. sizes) */
      if (i<4) item->app = appcode_i(i);  /* get the appearance indicator */
      else item->app = appcode_i(0);  /* items have code 0 */
      if (item->app <  0) { 
        UNPROTECT(1);
        return E_UNKAPP;
      }
      
      h++;
    }
  }
  
  UNPROTECT(1);
  return 0;                     /* return 'ok' */
  }  /* is_readapp_R() */
      
      /*----------------------------------------------------------------------
       Main Functions
       ----------------------------------------------------------------------*/
      void frequentItem(ARparameter *param, INPUT *in)
      {
        int k, n, i, l;
        int    rsdef;                 /* rule support definition */
      int    *map;                  /* identifier map, item set */
      int    maxcnt = 0, tacnt;          /* maximal number of items per set */
      double supp, conf;
      char   *apps    = NULL;       /* item appearance indicator vector */
      clock_t     t,tt,tc,x;        /* timer for measurements */
      int maxlen_warn = param->maxlen;
        
        
      tacnt = in->tnb;
      /* --- set transactions --- */
      t = clock();                /* start the timer */
      if (param->verbose) Rprintf("set transactions ...");
      for (l = 0; l < tacnt; l++) {                 /* transaction read loop */
      k = is_read_in(itemset, in);             /* read the next transaction */
      if (k < 0) {cleanup(); error(msgs(k), "read transactions", RECCNT(itemset), BUFFER(itemset));}
      if (k > 0) break;
      k = is_tsize(itemset);                /* update the maximal */
      if (k > maxcnt) maxcnt = k;           /* transaction size */
      if (taset && (tas_add(taset, NULL, 0) != 0)) {cleanup(); error(msgs(E_NOMEM));}
      }
      n = is_cnt(itemset);                          /* get the number of items */
      if (param->verbose) Rprintf("[%d item(s),", n);
      if (param->verbose) {
        Rprintf(" %d transaction(s)] done ", tacnt);
        Rprintf("[%.2fs].", SEC_SINCE(t));
        Rprintf("\n");
      }
      if ((n <= 0) || (tacnt <= 0)) {cleanup(); error(msgs(E_NOTAS));} 
      
      supp = param->supp;
      conf = param->conf;
      rsdef = param->rsdef;
      if (supp < 0) {               /* if absolute support is given */
      supp = (-supp -0.25) / tacnt;
        if (supp < 0) supp = 0;     /* compute a proper relative support */
      }                             /* and check it against 0 */
      
      /* --- sort and recode items --- */
      if (param->sort != 0) {              /* sort items w.r.t. their frequency */
      if (param->verbose) Rprintf("sorting and recoding items ... ");
      t   = clock();              /* start the timer */
      map = (int*)malloc(is_cnt(itemset) *sizeof(int));
      if (!map) {cleanup(); error(msgs(E_NOMEM));}   /* create an item identifier map */
      if (rsdef == IST_BODY)      /* if rule support = body support */
      k = (int)ceil(tacnt *supp *conf);
      else                        /* if rule supp. = body&head support */
      k = (int)ceil(tacnt *supp);
      n = is_recode(itemset, k, param->sort, map);
      if (taset) {                /* sort and recode the items and */
      tas_recode(taset, map, n); /* recode the loaded transactions */
      maxcnt = tas_max(taset);  /* get the new maximal t.a. size */
      }                           /* (may be smaller than before) */
      free(map);                  /* delete the item identifier map */
      if (param->verbose) {
        Rprintf("[%d item(s)] ", n);
        Rprintf("done [%.2fs].", SEC_SINCE(t));
        Rprintf("\n"); /* check the number of items */
      }
      /* if (n <= 0) {cleanup(); error(msgs(E_NOTAS));} 
       MFH: I just want an empty result in this case */
      }                             /* (which may be zero now) */
      
      
      /* --- create a transaction tree --- */
      tt = 0;                       /* init. the tree construction time */
      if (param->tree && taset) {          /* if transactions were loaded */
      if (param->verbose) Rprintf("creating transaction tree ... ");
      t = clock();                /* start the timer */
      tatree = tat_create(taset, param->heap); 
      if (!tatree) {cleanup(); error(msgs(E_NOMEM));}  /* create a transaction tree */
      if (param->filter == 0) {          /* if a tree rebuild is not needed, */
      tas_delete(taset, 0); taset = NULL; }  /* delete transactions */
      tt = clock() -t;            /* note the time for the construction */
      if (param->verbose) Rprintf("done [%.2fs].\n", SEC_SINCE(t));
      }                             /* print a log message */
      
      /* --- create an item set tree --- */
      t = clock(); tc = 0;          /* start the timer */
      apps = (char*)malloc(n *sizeof(char));
      if (!apps) {cleanup(); error(msgs(E_NOMEM));}  /* get the appearance indicators */
      for (apps += i = n; --i >= 0; )
        *--apps = is_getapp(itemset, i);
      istree = ist_create(n, supp, conf, rsdef, apps, param->memopt);
      if (!istree) {cleanup(); error(msgs(E_NOMEM));} /* create an item set tree */
      for (k = n; --k >= 0; )       /* set single item frequencies */
      ist_setcnt(istree, k, is_getfrq(itemset, k));
      ist_settac(istree, tacnt);    /* set the number of transactions */
      if (param->maxlen > maxcnt)          /* clamp the rule length */
      param->maxlen = maxcnt;            /* to the maximum set size */
      /* --- check item subsets --- */
      if (param->verbose) Rprintf("checking subsets of size 1");
      
      /* while (ist_height(istree) < param->maxlen) { */
      while (1) {
        /* R check for C-C to interupt execution */
        /* Note: This creates a memory leak. I would like to call cleanup() */
        R_CheckUserInterrupt();
        
        /* Check if we reach the max. transaction size */
        if(ist_height(istree) >= maxcnt) break;
        
        /* Check if we run into limits */
        if(ist_height(istree) >= param->maxlen){
          /* if (param->verbose) Rprintf(" *stopping (maxlen reached)*"); */
          if(ist_height(istree) >= maxlen_warn)
            Rf_warning("Mining stopped (maxlen reached). Only patterns up to a length of %d returned!", param->maxlen);
          break;
        }
        
        if(SEC_SINCE(t) > param->maxtime && param->maxtime > 0) {
          /* if (param->verbose) Rprintf(" *stopping (time limit)*"); */
          Rf_warning("Mining stopped (time limit reached). Only patterns up to a length of %d returned!", 
                     ist_height(istree));
          break;
        }
        
        if (param->filter != 0) {          /* if to filter w.r.t. item usage, */
          i = ist_check(istree, apps);     /* check current item usage */
          if (i < param->maxlen) param->maxlen = i;      /* update the maximum size */
          if (ist_height(istree) >= i) break;
        }                           /* check the tree height */
          k = ist_addlvl(istree);     /* while max. height is not reached, */
          if (k <  0) {cleanup(); error(msgs(E_NOMEM));}  /* add a level to the item set tree */
          if (k != 0) break;          /* if no level was added, abort */
          
          if (param->verbose) Rprintf(" %d", ist_height(istree));
          
          if (tatree) {               /* if a transaction tree was created */
          if (((param->filter < 0)         /* if to filter w.r.t. item usage */
          &&   (i < -param->filter *n))    /* and enough items were removed */
          ||  ((param->filter > 0)         /* or counting time is long enough */
          &&   (i < n) && (i *(double)tt < param->filter *n *tc))) {
          n = i; x = clock();     /* note the new number of items */
          tas_filter(taset,apps); /* and remove unnecessary items */
          tat_delete(tatree);     /* delete the transaction tree */
          tatree = tat_create(taset, param->heap);
          if (!tatree) {cleanup(); error(msgs(E_NOMEM));}  
          tt = clock() -x;        /* rebuild the transaction tree and */
          }                         /* note the new construction time */
          x  = clock();             /* count the transaction tree */
          ist_countx(istree, tatree);
          tc = clock() -x; }        /* note the new count time */
          else if (taset) {           /* if transactions were loaded */
          if (((param->filter < 0)         /* if to filter w.r.t. item usage */
          &&   (i <= -param->filter *n))   /* and enough items were removed */
          ||  ((param->filter > 0)         /* or counting time is long enough */
          &&   (i *(double)tt <= param->filter *n *tc))) {
          n = i; x = clock();     /* note the new number of items */
          tas_filter(taset,apps); /* and remove unnecessary items */
          tt = clock() -t;        /* from the transactions */
          }                         /* note the filtering time */
          x = clock();              /* traverse and count transactions */
          for (i = tacnt; --i >= 0; )
            ist_count(istree, tas_tract(taset, i), tas_tsize(taset, i));
          tc = clock() -t; }        /* note the new count time */
          else {
            in->index = 0;
            for (maxcnt = 0; (i = is_read_in(itemset, in)) == 0; ) {
              if (param->filter != 0)        /* (re)read the transactions and */
          is_filter(itemset, apps);  /* remove unnecessary items */
          k = is_tsize(itemset);  /* update the maximum size */
          if (k > maxcnt) maxcnt = k;  /* of a transaction */
          ist_count(istree, is_tract(itemset), k);
            }                         /* count the transaction in the tree */
          if (i < 0) {cleanup(); error(msgs(i), "reading transactions", RECCNT(itemset), BUFFER(itemset));}
          if (maxcnt < param->maxlen)      /* update the maximal rule length */
          param->maxlen = maxcnt;        /* according to the max. t.a. size */
          }                           /* (may be smaller than before) */
      }                             /* clear the file variable */
          
          if (param->verbose) Rprintf(" done [%.2fs].\n", SEC_SINCE(t));
          
          /* --- filter found item sets --- */
          if ((param->target == TT_MFSET) || (param->target == TT_CLSET)) {
            if (param->verbose) Rprintf("filtering %s item sets ... ",
                (param->target == TT_MFSET) ? "maximal" : "closed");
            t = clock();                /* filter the item sets */
          ist_filter(istree, (param->target == TT_MFSET) ? IST_MAXFRQ : IST_CLOSED);
          if (param->verbose) Rprintf("done [%.2fs].\n", SEC_SINCE(t));
          }                             /* (filter takes longer than print) */
          
          /* --- sort transactions --- */
          if (param->target <= TT_CLSET) {     /* if to find frequent item sets */
          if (!taset)                 /* transactions must be loaded */
          param->ext = 0;                  /* for extended support output */
          else if (param->ext) {             /* if extended output is requested */
          if (param->verbose) Rprintf("sorting transactions ... ");
          t = clock();              /* start the timer */
          tas_sort(taset, param->heap);    /* sort the transactions */ 
          if (param->verbose) Rprintf("done [%.2fs].\n", SEC_SINCE(t));
          /* (sorting is necessary to find the */
          }
          }                             /* number of identical transactions)*/
          free(apps);
      }

void createRules(ISTREE *istree, ARparameter *param) {
  double  supp, conf;            /* minimal support & confidence */
          int     *set;                  /* identifier map, item set */
          double  lftval   = 0;          /* lift value (confidence/prior) */
          double  minval   = 0;
          int     n, k, i, h = 0;
          clock_t t;
          int     target;
          int size, size1;
          char    **vec = NULL, **vec3 = NULL;
          int     *vec1 = NULL;
          double  *vec2 = NULL;
          
          
          assert(param);
          ruleset = rs_create();
          /* --- print item sets/rules/hyperedges --- */
          t = clock();                  /* start the timer */
          if (param->verbose) Rprintf("writing ... ");
          target = param->target;
          supp = param->supp;
          conf = param->conf;
          minval = param->minval;
          ist_init(istree, param->minlen, param->arem, param->minval);
          
          set = is_tract(itemset);      /* get the transaction buffer */
          size = ruleset->ttotal;
          size1 = ruleset->rnb;
          if (target <= TT_CLSET) {     /* if to find frequent item sets */
          for (n = 0; 1; ) {          /* extract item sets from the tree */
          
          /* R check for C-C to interupt execution */
          /* Note: This creates a memory leak. I would like to call cleanup() */
          R_CheckUserInterrupt(); 
            
            k = ist_set(istree, set, &supp, &conf);
            if (k <= 0) break;        /* get the next frequent item set */
          if (supp > param->smax) continue;/* check against maximal support */
          for (i = 0; i < k; i++) { /* traverse the set's items */
          if (h >= size) {      /* if the item vector is full */
          size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
            vec   = (char**)realloc(ruleset->body, size *sizeof(char*));
            if (!vec)  {
              free(vec1);free(vec2);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}    /* enlarge the item vector */
          ruleset->body  = vec;
          }                             /* set the new vector and its size */
          ruleset->body[h] = (char *)(is_name(itemset, set[i]));
          h++;
          }     
          if (n >= size1) {
            size1 += (size1 > BLKSIZE) ? (size1 >> 1) : BLKSIZE;
            vec1 = (int*)realloc(ruleset->tnb, size1 *sizeof(int));
            if (!vec1) {
              free(vec);free(vec2);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}
            ruleset->tnb = vec1;
            vec2 = (double*)realloc(ruleset->supp, size1 *sizeof(double));
            if (!vec2) {
              free(vec);free(vec1);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}   /* enlarge the item vector */
          ruleset->supp = vec2;
            if (param->ext) {
              vec2 = (double*)realloc(ruleset->ext, size1 *sizeof(double));
              if (!vec2) {
                free(vec);free(vec1);free(vec3);
                cleanup(); error(msgs(E_NOMEM));}   /* enlarge the item vector */
          ruleset->ext = vec2;
            }
          }
          if (n == 0) ruleset->tnb[0] = k; else ruleset->tnb[n] = ruleset->tnb[n-1] + k;
          ruleset->supp[n] = supp;
          if (param->ext) {
            supp = tas_occur(taset, set, k);
            ruleset->ext[n] = supp/istree->tacnt;
          }
          n++;
          }
          ruleset->rnb = n;
            ruleset->ttotal = h;
          } 
          else if (target == TT_RULE) { /* if to find association rules, */
          for (n = 0; 1; ) {          /* extract rules from tree */
          
          /* R check for C-C to interupt execution */
          /* Note: This creates a memory leak. I would like to call cleanup() */
          R_CheckUserInterrupt();
            
            k = ist_rule(istree, set, &supp, &conf, &lftval, &minval);
            if (k <= 0) break;        /* get the next association rule */
          if (supp > param->smax) continue;/* check against maximal support */
          for (i = 1; i < k; i++) { /* traverse the set's items */
          if (h >= size) {      /* if the item vector is full */
          size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
            vec   = (char**)realloc(ruleset->body, size *sizeof(char*));
            if (!vec)  {
              //free(vec1);free(vec2);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}    /* enlarge the item vector */
          ruleset->body = vec;
          }                             /* set the new vector and its size */
          ruleset->body[h] = (char *)(is_name(itemset, set[i]));
          h++;
          }
          if (n >= size1) {
            size1 += (size1 > BLKSIZE) ? (size1 >> 1) : BLKSIZE;
            vec1 = (int*)realloc(ruleset->tnb, size1 *sizeof(int));
            if (!vec1) {
              //free(vec);free(vec2);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}
            ruleset->tnb = vec1;
            vec2 = (double*)realloc(ruleset->supp, size1 *sizeof(double));
            if (!vec2) {
              //free(vec);free(vec1);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}
            ruleset->supp = vec2;
            vec2 = (double*)realloc(ruleset->conf, size1 *sizeof(double));
            if (!vec2) {
              //free(vec);free(vec1);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}
            ruleset->conf = vec2;
            vec2 = (double*)realloc(ruleset->lift, size1 *sizeof(double));
            if (!vec2) {
              //free(vec);free(vec1);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}
            ruleset->lift = vec2;
            vec3 = (char**)realloc(ruleset->head, size1 *sizeof(char*));
            if (!vec3) {
              //free(vec);free(vec1);free(vec);
              cleanup(); error(msgs(E_NOMEM));}
            ruleset->head = vec3;
            if (param->aval) {
              vec2 = (double*)realloc(ruleset->aval, size1 *sizeof(double));
              if (!vec2) {
                //free(vec);free(vec1);free(vec3);
                cleanup(); error(msgs(E_NOMEM));}      /* enlarge the item vector */
          ruleset->aval = vec2;
            }
            if (param->ext) {
              vec2 = (double*)realloc(ruleset->ext, size1 *sizeof(double));
              if (!vec2) {
                //free(vec);free(vec1);free(vec3);
                cleanup(); error(msgs(E_NOMEM));}      /* enlarge the item vector */
          ruleset->ext = vec2;
            }
            
          }                             /* set the new vector and its size */
          ruleset->head[n] = (char *)(is_name(itemset, set[0]));
          if (n == 0) ruleset->tnb[0] = k - 1; else ruleset->tnb[n] = ruleset->tnb[n-1] + k - 1;
          if (param->rsdef == IST_BOTH) {
            ruleset->supp[n] = supp;
          }
          else ruleset->supp[n] = supp*conf;
          ruleset->conf[n] = conf;
          ruleset->lift[n] = lftval;
          if (param->aval) ruleset->aval[n] = minval;
          if (param->ext) {
            if (param->rsdef == IST_BOTH) ruleset->ext[n] = supp/conf;
            else ruleset->ext[n] = supp;
          }
          n++;
          }
          ruleset->rnb = n;
            ruleset->ttotal = h;
          }
          else {                        /* if to find association hyperedges */
          for (n = 0; 1; ) {          /* extract hyperedges from tree */
          
          /* R check for C-C to interupt execution */
          /* Note: This creates a memory leak. I would like to call cleanup() */
          R_CheckUserInterrupt(); 
            
            k = ist_hedge(istree, set, &supp, &conf);
            if (k <= 0) break;        /* get the next hyperedge */
          if (supp > param->smax) continue;/* check against maximal support */
          for (i = 0; i < k; i++) { /* traverse the set's items */
          if (h >= size) {      /* if the item vector is full */
          size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
            vec  = (char**)realloc(ruleset->body, size *sizeof(char*));
            if (!vec)  {
              //free(vec1);free(vec2);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}    /* enlarge the item vector */
          ruleset->body = vec;
          }                             /* set the new vector and its size */
          ruleset->body[h] = (char *)(is_name(itemset, set[i]));
          h++;
          }
          if (n >= size1) {
            size1 += (size1 > BLKSIZE) ? (size1 >> 1) : BLKSIZE;
            vec1 = (int*)realloc(ruleset->tnb, size1 *sizeof(int));
            if (!vec1) {
              //free(vec);free(vec2);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}
            ruleset->tnb = vec1;
            vec2 = (double*)realloc(ruleset->supp, size1 *sizeof(double));
            if (!vec2) {
              //free(vec);free(vec1);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}
            ruleset->supp = vec2;
            vec2 = (double*)realloc(ruleset->conf, size1 *sizeof(double));
            if (!vec2) {
              //free(vec);free(vec1);free(vec3);
              cleanup(); error(msgs(E_NOMEM));}   /* enlarge the item vector */
          ruleset->conf = vec2;
          }                             /* set the new vector and its size */
          if (n == 0) ruleset->tnb[0] = k; else ruleset->tnb[n] = ruleset->tnb[n-1] + k;
          ruleset->supp[n] = supp;
          ruleset->conf[n] = conf;
          n++;
          }
          ruleset->rnb = n;
            ruleset->ttotal = h;
          }
          if (param->verbose) {
            Rprintf("[%d %s(s)] done ", n, ttypes[target]);
            Rprintf("[%.2fs].\n", SEC_SINCE(t));
          }
}

/* Sort row indexes [ceboo 2008/12] */
void sort_ngCMatrix(SEXP x) {
  int i, f, l;
  SEXP px, ix;
  
  px = GET_SLOT(x, install("p"));
  ix = GET_SLOT(x, install("i"));
  
  f = INTEGER(px)[0];
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    R_isort(INTEGER(ix)+f, l-f);
    f = l;
  }
}

SEXP returnObject(RULESET *set, SEXP dim, ARparameter *param, SEXP itemInfo)
{
  int i, len, k;
  SEXP ans, class, tp, qual, q, rownames, names, items, lhs, rhs, trans, tidLists;
  
  if (param->target <= TT_CLSET)	{
    ans = PROTECT(NEW_OBJECT(MAKE_CLASS("itemsets")));
    len = 1; /* number of quality measures */
  }
  else if (param->target == TT_RULE) {
    ans = PROTECT(NEW_OBJECT(MAKE_CLASS("rules")));
    len = 3;
  }
  else {
    /* hyperedges */
    ans = PROTECT(NEW_OBJECT(MAKE_CLASS("itemsets")));
    len = 2;
  }
  
  
  if (param->aval) len++;
  if (param->ext) len++;
  
  /* set items/lhs */
  items = PROTECT(NEW_OBJECT(MAKE_CLASS("ngCMatrix")));
  tp = PROTECT(NEW_INTEGER(set->ttotal));
  for (i = 0; i < set->ttotal; i++) 
    INTEGER(tp)[i] = atoi(set->body[i]);
  SET_SLOT(items, install("i"), tp);
  UNPROTECT(1);
  
  tp = PROTECT(NEW_INTEGER(set->rnb+1));
  INTEGER(tp)[0] = 0;						     
  for (i = 0; i < set->rnb; i++) INTEGER(tp)[i+1] = set->tnb[i];
  SET_SLOT(items, install("p"), tp);
  UNPROTECT(1);
  
  tp = PROTECT(NEW_INTEGER(2));
  INTEGER(tp)[0] = INTEGER(dim)[0];
  INTEGER(tp)[1] = set->rnb;
  SET_SLOT(items, install("Dim"), tp);
  UNPROTECT(1);
  
  sort_ngCMatrix(items);
  
  lhs = PROTECT(NEW_OBJECT(MAKE_CLASS("itemMatrix")));
  SET_SLOT(lhs , install("data"), items);
  SET_SLOT(lhs , install("itemInfo"), itemInfo);
  
  if (param->target == TT_RULE) 
    SET_SLOT(ans, install("lhs"), lhs);
  else	
    SET_SLOT(ans, install("items"), lhs);
  UNPROTECT(1);
  
  /* set rhs for rules */	
  if (param->target == TT_RULE) {
    items = PROTECT(NEW_OBJECT(MAKE_CLASS("ngCMatrix")));
    
    tp = PROTECT(NEW_INTEGER(set->rnb));
    for (i = 0; i < set->rnb; i++) 
      INTEGER(tp)[i] = atoi(set->head[i]);
    SET_SLOT(items, install("i"), tp);
    UNPROTECT(1);
    
    tp = PROTECT(NEW_INTEGER(set->rnb+1));
    for (i = 0; i < set->rnb+1; i++) INTEGER(tp)[i] = i;
    SET_SLOT(items, install("p"),  tp);
    UNPROTECT(1);
    
    tp = PROTECT(NEW_INTEGER(2));
    INTEGER(tp)[0] = INTEGER(dim)[0];
    INTEGER(tp)[1] = set->rnb;
    SET_SLOT(items, install("Dim"), tp);
    UNPROTECT(1); 
    
    sort_ngCMatrix(items);
    
    rhs =  PROTECT(NEW_OBJECT(MAKE_CLASS("itemMatrix")));
    SET_SLOT(rhs, install("data"), items);
    SET_SLOT(rhs, install("itemInfo"), itemInfo);
    SET_SLOT(ans, install("rhs"), rhs);
    UNPROTECT(2);
  }
  
  /* set quality measures */
  qual = PROTECT(NEW_LIST(len));
  names = PROTECT(NEW_CHARACTER(len));
  
  k=0;
  SET_VECTOR_ELT(qual, k, q = NEW_NUMERIC(set->rnb));
  for (i = 0; i < set->rnb; i++) REAL(q)[i] = set->supp[i];
  SET_STRING_ELT(names, k++, CREATE_STRING_VECTOR("support"));
  
  if (param->target > TT_CLSET) {
    SET_VECTOR_ELT(qual, k, q = NEW_NUMERIC(set->rnb));
    for (i = 0; i < set->rnb; i++) REAL(q)[i] = set->conf[i];
    SET_STRING_ELT(names, k++, CREATE_STRING_VECTOR("confidence"));
  }
  
  if (param->aval) {
    SET_VECTOR_ELT(qual, k, q = NEW_NUMERIC(set->rnb));
    for (i = 0; i < set->rnb; i++) REAL(q)[i] = set->aval[i];
    SET_STRING_ELT(names, k++, CREATE_STRING_VECTOR(aremtypes[param->arem]));
  }
  if (param->ext) {
    SET_VECTOR_ELT(qual, k, q = NEW_NUMERIC(set->rnb));
    for (i = 0; i < set->rnb; i++) REAL(q)[i] = set->ext[i];
    if (param->target == TT_RULE) 
      SET_STRING_ELT(names, k++, CREATE_STRING_VECTOR("lhs.support"));
    else SET_STRING_ELT(names, k++, CREATE_STRING_VECTOR("transIdenticalToItemsets"));
  }
  
  if (param->target == TT_RULE) {
    SET_VECTOR_ELT(qual, k, q = NEW_NUMERIC(set->rnb));
    for (i = 0; i < set->rnb; i++) REAL(q)[i] = set->lift[i];
    SET_STRING_ELT(names, k++, CREATE_STRING_VECTOR("lift"));
  }	
  
  rownames = PROTECT(NEW_INTEGER(set->rnb));
  for (i = 0; i < set->rnb; i++) INTEGER(rownames)[i] = i+1;
  setAttrib(qual, install("row.names"), rownames);
  UNPROTECT(1);
  setAttrib(qual, install("names"), names);
  UNPROTECT(1);
  PROTECT(class = NEW_CHARACTER(1)); 
  SET_STRING_ELT(class, 0, CREATE_STRING_VECTOR("data.frame")); 
  classgets(qual, class); 
  SET_SLOT(ans, install("quality"), qual);
  UNPROTECT(3); 
  
  
  /* set transaction ID list (possible with eclat) */	
  if (param->trans) {
    trans = PROTECT(NEW_OBJECT(MAKE_CLASS("ngCMatrix")));
    
    tp = PROTECT(NEW_INTEGER(set->trtotal));
    for (i = 0; i < set->trtotal; i++) INTEGER(tp)[i] = set->trans[i];
    SET_SLOT(trans, install("i"), tp); 
    UNPROTECT(1);
    
    tp = PROTECT(NEW_INTEGER(set->rnb+1));
    INTEGER(tp)[0] = 0;
    for (i = 0; i < set->rnb; i++) INTEGER(tp)[i+1] = set->trnb[i];
    SET_SLOT(trans, install("p"),  tp);
    UNPROTECT(1);
    
    tp = PROTECT(NEW_INTEGER(2));
    INTEGER(tp)[0] = set->tacnt;
    INTEGER(tp)[1] = set->rnb;
    /* SET_SLOT(trans, install("Dim"), duplicate(tp)); */
    SET_SLOT(trans, install("Dim"), tp);
    UNPROTECT(1);
    
    sort_ngCMatrix(trans);
    
    tidLists = PROTECT(NEW_OBJECT(MAKE_CLASS("tidLists")));
    SET_SLOT(tidLists, install("data"), trans);
    
    SET_SLOT(ans, install("tidLists"), tidLists);
    UNPROTECT(2);
  }
  
  UNPROTECT(1);
  return ans;
}

SEXP rapriori(SEXP x, SEXP y, SEXP dim, SEXP parms, SEXP control, SEXP app, SEXP itemInfo)
{
  ARparameter param;
  int k;
  int load, maxlen;
  const char *target, *arem;
  clock_t t;
  INPUT in;
  SEXP ans;
  
  param.verbose = *LOGICAL(GET_SLOT(control, install("verbose")));  /* flag for verbose */
    /*
     if (param.verbose) {
     Rprintf("%s - %s\n", PRGNAME, DESCRIPTION);
     Rprintf(VERSION);
     Rprintf("\n");
     }
     */
    
    /* --- evaluate arguments --- */
    param.supp = *REAL(GET_SLOT(parms, install("support")));       /* minimal support 's'*/
    param.conf = *REAL(GET_SLOT(parms, install("confidence")));    /* minimal confidence 'c'*/ 
    param.minval = *REAL(GET_SLOT(parms, install("minval")));      /* minimal evaluation measure value 'd'*/
    param.filter = *REAL(GET_SLOT(control, install("filter")));    /* item usage filtering parameter 'u'*/
    param.smax = *REAL(GET_SLOT(parms, install("smax")));          /* maximal support    'S' */
    /* target type (sets/rules/h.edges) 't'*/  
    target = translateChar(STRING_ELT(GET_SLOT(parms, install("target")), 0)); 
    /* target = CHAR(STRING_ELT(GET_SLOT(parms, install("target")), 0)); */ 
    param.target = targetcode(target);
    /* additional rule evaluation measure 'e'*/  
    arem = translateChar(STRING_ELT(GET_SLOT(parms, install("arem")), 0)); 
    /* arem = CHAR(STRING_ELT(GET_SLOT(parms, install("arem")), 0)); */ 
    param.arem = aremcode(arem);
    
    param.minlen = *INTEGER(GET_SLOT(parms, install("minlen")));   /* minimal rule length 'm'*/
    maxlen = param.maxlen = *INTEGER(GET_SLOT(parms, install("maxlen")));   /* maximal rule length 'n'*/  
    param.maxtime = *REAL(GET_SLOT(parms, install("maxtime")));      /* maximal time allowed in seconds*/
    param.sort = *INTEGER(GET_SLOT(control, install("sort")));     /* flag for item sorting and recoding 'q'*/
    
    param.rsdef = *LOGICAL(GET_SLOT(parms, install("originalSupport")));      /* rule support definition 'o'*/
    param.tree = *LOGICAL(GET_SLOT(control, install("tree")));      /* flag for transaction tree 'h'*/
    param.heap = *LOGICAL(GET_SLOT(control, install("heap")));      /* flag for heap sort vs. quick sort 'j'*/
    param.aval = *LOGICAL(GET_SLOT(parms, install("aval")));        /* flag for add. eval. measure value 'v'*/
    param.memopt = *LOGICAL(GET_SLOT(control, install("memopt")));  /* flag for memory usage optimization 'z'*/
    param.ext = *LOGICAL(GET_SLOT(parms, install("ext")));          /* flag for extended support output 'x'*/
    param.trans = 0;
    
    load = *LOGICAL(GET_SLOT(control, install("load")));            /* flag for loading transactions 'l'*/
    
    switch (param.target) {             /* check and translate target type */
    case 0: param.target = TT_SET;               break;
    case 1: param.target = TT_MFSET;             break;
    case 2: param.target = TT_CLSET;             break;
    case 3: param.target = TT_RULE;              break;
    case 4: param.target = TT_HEDGE;             break;
    default : cleanup(); error(msgs(E_TARGET, target)); break;
    }
    if (param.supp > 1)                 /* check the minimal support */
    {cleanup(); error(msgs(E_SUPP, param.supp));}        /* (< 0: absolute number) */
    if ((param.conf  <  0) || (param.conf > 1))
    {cleanup(); error(msgs(E_CONF, param.conf));}        /* check the minimal
     * confidence */
    if (param.minlen <= 0) {cleanup(); error(msgs(E_RULELEN, param.minlen));}  /* check the limits */
    if (param.maxlen <= 0) {cleanup(); error(msgs(E_RULELEN, param.maxlen));}  /* for the rule length */
    
    switch (param.arem) {               /* check and translate measure */
    case  0: param.arem = EM_NONE;     break;
    case  1: param.arem = EM_DIFF;     break;
    case  2: param.arem = EM_QUOT;     break;
    case  3: param.arem = EM_AIMP;     break;
    case  4: param.arem = EM_INFO;     break;
    case  5: param.arem = EM_CHI2;     break;
    default : cleanup(); error(msgs(E_MEASURE, arem)); break;
    }
    
    if (param.rsdef) param.rsdef = IST_BOTH;
    else param.rsdef = IST_BODY;
    
    if ((param.target == TT_HEDGE) & param.ext) {
      warning("No extended measure available.\n");
      LOGICAL(GET_SLOT(parms, install("ext")))[0] = param.ext = 0;
    }
    if ((param.target != TT_RULE) & param.aval) {
      warning("No additional measure available.\n");
      LOGICAL(GET_SLOT(parms, install("aval")))[0] = param.aval= 0;
      param.arem = EM_NONE;
      SET_SLOT(parms, install("arem"), PROTECT(ScalarString(CREATE_STRING_VECTOR("none"))));
      UNPROTECT(1);
    }
    if (param.arem == EM_NONE)   {       /* if no add. rule eval. measure, */
    REAL(GET_SLOT(parms, install("minval")))[0] = param.minval = 0;
      if (param.aval) {
        warning("No additional measure available.\n");		
        LOGICAL(GET_SLOT(parms, install("aval")))[0] = param.aval = 0;
        /* clear the corresp. output flag */
      }
    }
    if ((param.minval < 0) || ((param.arem != EM_AIMP) && (param.minval > 1)))
    {cleanup(); error(msgs(E_MVAL, param.minval));}      /* check the measure parameter */
        if      (param.target == TT_HEDGE){ /* in hyperedge mode */
        REAL(GET_SLOT(parms, install("minval")))[0] = param.minval = param.conf;
          REAL(GET_SLOT(parms, install("confidence")))[0] = param.conf = 1;
        }/* adapt the parameters */
        else if (param.target <= TT_CLSET){ /* in item set mode neutralize */
        LOGICAL(GET_SLOT(parms, install("originalSupport")))[0] = param.rsdef = IST_BOTH; 
          REAL(GET_SLOT(parms, install("confidence")))[0] = param.conf = 1;
          
        }/* rule specific settings */
        if ((param.filter <= -1) || (param.filter >= 1)) {
          warning("Parameter 'filter' set to 0.\n");
          REAL(GET_SLOT(control, install("filter")))[0] = param.filter = 0;
          
        }
        
        /* --- create item set and transaction set --- */
        itemset = is_create();	
        if (!itemset) {cleanup(); error(msgs(E_NOMEM));}
        if (load) {                   /* if to load the transactions */
        taset = tas_create(itemset);
          if (!taset) {cleanup(); error(msgs(E_NOMEM));}
        }
        
        /* --- set item appearances --- */
        t = clock();                /* start the timer */
        if (param.verbose) Rprintf("set item appearances ...");
        k = is_readapp_R(itemset, app);
        if (k  != 0) {cleanup(); error(msgs(k, "appearance", RECCNT(itemset), BUFFER(itemset)));}
        if (param.verbose)  {
          Rprintf("[%d item(s)] done ", is_cnt(itemset));
          Rprintf("[%.2fs].\n", SEC_SINCE(t));
        }
        
        in.x = AS_CHARACTER(y); 
        in.ind = INTEGER(x);
        in.index = 0;
        in.tnb = length(x)-1;
        frequentItem(&param, &in);	
        createRules(istree, &param);
        ruleset->cnt = is_cnt(itemset);
        ruleset->tacnt = in.tnb;
        SET_SLOT(parms, install("maxlen"), PROTECT(NEW_INTEGER(1)));
        UNPROTECT(1);
        INTEGER(GET_SLOT(parms, install("maxlen")))[0] = maxlen;
        
        t = clock();
        if (param.verbose) Rprintf("creating S4 object  ... ");
        ans = PROTECT(returnObject(ruleset, dim, &param, itemInfo));
        cleanup();
        
        if (param.verbose) {
          Rprintf("done ");
          Rprintf("[%.2fs].\n", SEC_SINCE(t));
        }
        
        UNPROTECT(1);
        return ans;
}

