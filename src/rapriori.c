#include "rapriori.h"

/*----------------------------------------------------------------------
 The code for apriori and eclat were obtained from http://www.borgelt.net/
 and are copyrighted by 1996-2003 Christian Borgelt.

 The R-interface was added by Bettina Gruen and modified by Michael Hahsler.

 Rewritten to consistently PROTECT retained slot/attribute lookups and
 allocated R objects that survive across further allocations.
 ----------------------------------------------------------------------*/

/*----------------------------------------------------------------------
 Constants
 ----------------------------------------------------------------------*/
static const char* ttypes[] = {
  /* TT_SET      0 */  "set",
  /* TT_MFSET    1 */  "set",
  /* TT_GRSET    2 */  "set",
  /* TT_CLSET    3 */  "set",
  /* TT_RULE     4 */  "rule",
  /* TT_HEDGE    5 */  "hyperedge",
};

static const char* ttarget[] = {
  /* TT_SET      0 */  "frequent itemsets",
  /* TT_MFSET    1 */  "maximally frequent itemsets",
  /* TT_GRSET    2 */  "generator frequent itemsets",
  /* TT_CLSET    3 */  "closed frequent itemsets",
  /* TT_RULE     4 */  "rules",
  /* TT_HEDGE    5 */  "hyperedgesets",
};

static const char* aremtypes[] = {
  /* EM_NONE  0 */  "none",
  /* EM_DIFF  1 */  "diff",
  /* EM_QUOT  2 */  "quot",
  /* EM_AIMP  3 */  "aimp",
  /* EM_INFO  4 */  "info",
  /* EM_CHI2  5 */  "chi2",
};

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
  /* E_APPEXP  -18 */  "file %s, record %d: appearance indicator expected\n",
  /* E_UNKAPP  -19 */  "unknown appearance indicator %s\n",
  /* E_FLDCNT  -20 */  "file %s, record %d: too many fields\n",
  /* E_UNKNOWN -21 */  "unknown error\n"
};

#define BLKSIZE  256
#define PRGNAME     "apriori"
#define DESCRIPTION "find association rules with the apriori algorithm"
#define VERSION     "version 4.21 (2004.05.09)        " \
                    "(c) 1996-2004   Christian Borgelt"

#define TT_SET        0
#define TT_MFSET      1
#define TT_GRSET      2
#define TT_CLSET      3
#define TT_RULE       4
#define TT_HEDGE      5

#define E_OPTION    (-5)
#define E_OPTARG    (-6)
#define E_ARGCNT    (-7)
#define E_STDIN     (-8)
#define E_TARGET    (-9)
#define E_SUPP     (-10)
#define E_CONF     (-11)
#define E_MEASURE  (-12)
#define E_MVAL     (-13)
#define E_RULELEN  (-14)
#define E_NOTAS    (-15)
#define E_UNKNOWN  (-21)

#define SEC_SINCE(t)  ((clock()-(t)) /(double)CLOCKS_PER_SEC)
#define RECCNT(s)     (tfs_reccnt(is_tfscan(s)) \
                      + ((tfs_delim(is_tfscan(s)) == TFS_REC) ? 0 : 1))
#define BUFFER(s)     tfs_buf(is_tfscan(s))

static const char *i_body[] = {
  "i",  "in",  "a", "ante", "antecedent", "b", "body", "lhs", "items", NULL };
static const char *i_head[] = {
  "o",  "out", "c", "cons", "consequent", "h", "head", "rhs", NULL };
static const char *i_both[] = {
  "io", "inout", "ac", "bh", "both", NULL };
static const char *i_ignore[] = {
  "n", "neither", "none", "ign", "ignore", "-", NULL };

static int appcode (const char *s)
{
  const char **p;
  assert(s);
  for (p = i_body;   *p; p++) if (strcmp(s, *p) == 0) return APP_BODY;
  for (p = i_head;   *p; p++) if (strcmp(s, *p) == 0) return APP_HEAD;
  for (p = i_both;   *p; p++) if (strcmp(s, *p) == 0) return APP_BOTH;
  for (p = i_ignore; *p; p++) if (strcmp(s, *p) == 0) return APP_NONE;
  return -1;
}

static int appcode_i (const int i)
{
  switch (i) {
    case 0: return APP_BODY;
    case 1: return APP_HEAD;
    case 2: return APP_BOTH;
    case 3: return APP_NONE;
    default: return -1;
  }
}

static ITEMSET *itemset = NULL;
static TASET   *taset   = NULL;
static TATREE  *tatree  = NULL;
static ISTREE  *istree  = NULL;
static RULESET *ruleset = NULL;

static int get_item (ITEMSET *iset, const char *x)
{
  int  d = 0;
  ITEM *item;
  int  *ivec;
  int  size;

  assert(iset);
  item = nim_byname(iset->nimap, x);

  if (!item) {
    if (iset->app == APP_NONE)
      return d;
    item = nim_add(iset->nimap, x, sizeof(ITEM));
    if (!item) return E_NOMEM;
    item->frq = item->xfq = 0;
    item->app = iset->app;
  }
  size = iset->vsz;
  if (iset->cnt >= size) {
    size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
    ivec = (int*)realloc(iset->items, size * sizeof(int));
    if (!ivec) return E_NOMEM;
    iset->items = ivec;
    iset->vsz = size;
  }
  iset->items[iset->cnt++] = item->id;
  return d;
}

RULESET* rs_create(void)
{
  RULESET *ruleset;

  ruleset = malloc(sizeof(RULESET));
  if (!ruleset) return NULL;
  ruleset->ttotal = BLKSIZE;
  ruleset->trtotal = 0;
  ruleset->rnb = 0;
  ruleset->tnb = (int*)malloc(BLKSIZE * sizeof(int));
  ruleset->trnb = (int*)malloc(BLKSIZE * sizeof(int));
  ruleset->trans = (int*)malloc(BLKSIZE * sizeof(int));
  ruleset->body = (char**)malloc(BLKSIZE * sizeof(char *));
  ruleset->head = (char**)malloc(BLKSIZE * sizeof(char *));
  ruleset->lift = (double*)malloc(BLKSIZE * sizeof(double));
  ruleset->conf = (double*)malloc(BLKSIZE * sizeof(double));
  ruleset->supp = (double*)malloc(BLKSIZE * sizeof(double));
  ruleset->aval = (double*)malloc(BLKSIZE * sizeof(double));
  ruleset->ext = (double*)malloc(BLKSIZE * sizeof(double));
  ruleset->tnb[0] = 0;
  ruleset->trnb[0] = 0;
  return ruleset;
}

void rs_delete (RULESET *ruleset)
{
  assert(ruleset);
  if (ruleset->tnb)   free(ruleset->tnb);
  if (ruleset->trnb)  free(ruleset->trnb);
  if (ruleset->trans) free(ruleset->trans);
  if (ruleset->body)  free(ruleset->body);
  if (ruleset->head)  free(ruleset->head);
  if (ruleset->lift)  free(ruleset->lift);
  if (ruleset->conf)  free(ruleset->conf);
  if (ruleset->supp)  free(ruleset->supp);
  if (ruleset->aval)  free(ruleset->aval);
  if (ruleset->ext)   free(ruleset->ext);
  free(ruleset);
}

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
{
  const char *msg = NULL;
  if (code < E_UNKNOWN || code > 0) code = E_UNKNOWN;
  msg = errmsgs[-code];
  return msg;
}

void cleanup(void)
{
  if (istree)  { ist_delete(istree); istree = NULL; }
  if (tatree)  { tat_delete(tatree); tatree = NULL; }
  if (taset)   { tas_delete(taset, 0); taset = NULL; }
  if (itemset) { is_delete(itemset); itemset = NULL; }
  if (ruleset) { rs_delete(ruleset); ruleset = NULL; }
}

int is_read_in (ITEMSET *iset, INPUT *in)
{
  int  i;
  ITEM *item;

  assert(iset && in);
  iset->cnt = 0;
  if (in->index >= in->tnb) return 1;
  for (i = in->ind[in->index]; i < in->ind[in->index+1]; i++)
    get_item(iset, translateChar(STRING_ELT(in->x, i)));
  in->index++;

  ta_sort(iset->items, iset->cnt);
  iset->cnt = ta_unique(iset->items, iset->cnt);
  for (i = iset->cnt; --i >= 0; ) {
    item = nim_byid(iset->nimap, iset->items[i]);
    item->frq += 1;
    item->xfq += iset->cnt;
  }
  return 0;
}

int is_readapp_R (ITEMSET *iset, SEXP app)
{
  int  i, j, h, nprotect = 0;
  ITEM *item;
  const char *def;
  int *set;
  SEXP items, defS, setS, itemsS;

  assert(iset && app);

  PROTECT(defS = GET_SLOT(app, install("default")));
  nprotect++;
  def = translateChar(STRING_ELT(defS, 0));

  PROTECT(setS = GET_SLOT(app, install("set")));
  nprotect++;
  set = INTEGER(setS);

  PROTECT(itemsS = GET_SLOT(app, install("items")));
  nprotect++;
  PROTECT(items = AS_CHARACTER(itemsS));
  nprotect++;

  iset->app = appcode(def);
  if (iset->app < 0) {
    UNPROTECT(nprotect);
    return E_UNKAPP;
  }

  h = 0;
  for (i = 0; i < 5; i++) {
    for (j = 0; j < set[i]; j++) {
      item = nim_add(iset->nimap, translateChar(STRING_ELT(items, h)), sizeof(ITEM));
      if (item == EXISTS) {
        UNPROTECT(nprotect);
        return E_DUPITEM;
      }
      if (item == NULL) {
        UNPROTECT(nprotect);
        return E_NOMEM;
      }
      item->frq = 0;
      item->xfq = 0;
      item->app = (i < 4) ? appcode_i(i) : appcode_i(0);
      if (item->app < 0) {
        UNPROTECT(nprotect);
        return E_UNKAPP;
      }
      h++;
    }
  }

  UNPROTECT(nprotect);
  return 0;
}

/* The mining code below is semantically unchanged. */
void frequentItem(ARparameter *param, INPUT *in)
{
  int k, n, i, l;
  int rsdef;
  int *map;
  int maxcnt = 0, tacnt;
  double supp, conf;
  char *apps = NULL;
  clock_t t, tt, tc, x;
  int maxlen_warn = param->maxlen;

  tacnt = in->tnb;
  t = clock();
  if (param->verbose) Rprintf("set transactions ...");
  for (l = 0; l < tacnt; l++) {
    k = is_read_in(itemset, in);
    if (k < 0) { cleanup(); error("%s %s %i %s", msgs(k), "read transactions", RECCNT(itemset), BUFFER(itemset)); }
    if (k > 0) break;
    k = is_tsize(itemset);
    if (k > maxcnt) maxcnt = k;
    if (taset && (tas_add(taset, NULL, 0) != 0)) { cleanup(); error("%s", msgs(E_NOMEM)); }
  }
  n = is_cnt(itemset);
  if (param->verbose) Rprintf("[%d item(s),", n);
  if (param->verbose) {
    Rprintf(" %d transaction(s)] done ", tacnt);
    Rprintf("[%.2fs].", SEC_SINCE(t));
    Rprintf("\n");
  }
  if ((n <= 0) || (tacnt <= 0)) { cleanup(); error("%s", msgs(E_NOTAS)); }

  supp = param->supp;
  conf = param->conf;
  rsdef = param->rsdef;
  if (supp < 0) {
    supp = (-supp - 0.25) / tacnt;
    if (supp < 0) supp = 0;
  }

  if (param->sort != 0) {
    if (param->verbose) Rprintf("sorting and recoding items ... ");
    t = clock();
    map = (int*)malloc(is_cnt(itemset) * sizeof(int));
    if (!map) { cleanup(); error("%s", msgs(E_NOMEM)); }
    if (rsdef == IST_BODY)
      k = (int)ceil(tacnt * supp * conf);
    else
      k = (int)ceil(tacnt * supp);
    n = is_recode(itemset, k, param->sort, map, param->target == TT_GRSET, tacnt);
    if (taset) {
      tas_recode(taset, map, n);
      maxcnt = tas_max(taset);
    }
    free(map);
    if (param->verbose) {
      Rprintf("[%d item(s)] ", n);
      Rprintf("done [%.2fs].", SEC_SINCE(t));
      Rprintf("\n");
    }
  }

  tt = 0;
  if (param->tree && taset) {
    if (param->verbose) Rprintf("creating transaction tree ... ");
    t = clock();
    tatree = tat_create(taset, param->heap);
    if (!tatree) { cleanup(); error("%s", msgs(E_NOMEM)); }
    if (param->filter == 0) {
      tas_delete(taset, 0); taset = NULL;
    }
    tt = clock() - t;
    if (param->verbose) Rprintf("done [%.2fs].\n", SEC_SINCE(t));
  }

  t = clock(); tc = 0;
  apps = (char*)malloc(n * sizeof(char));
  if (!apps) { cleanup(); error("%s", msgs(E_NOMEM)); }
  for (apps += i = n; --i >= 0; )
    *--apps = is_getapp(itemset, i);
  istree = ist_create(n, supp, conf, rsdef, apps, param->memopt);
  if (!istree) { cleanup(); error("%s", msgs(E_NOMEM)); }
  for (k = n; --k >= 0; )
    ist_setcnt(istree, k, is_getfrq(itemset, k));
  ist_settac(istree, tacnt);
  if (param->maxlen > maxcnt)
    param->maxlen = maxcnt;
  if (param->verbose) Rprintf("checking subsets of size 1");

  while (1) {
    R_CheckUserInterrupt();
    if (ist_height(istree) >= maxcnt) break;
    if (ist_height(istree) >= param->maxlen) {
      if (ist_height(istree) >= maxlen_warn)
        if (param->verbose) Rf_warning("Mining stopped (maxlen reached). Only patterns up to a length of %d returned!", param->maxlen);
      break;
    }
    if (SEC_SINCE(t) > param->maxtime && param->maxtime > 0) {
      if (param->verbose) Rf_warning("Mining stopped (time limit reached). Only patterns up to a length of %d returned!", ist_height(istree));
      break;
    }
    if (param->filter != 0) {
      i = ist_check(istree, apps);
      if (i < param->maxlen) param->maxlen = i;
      if (ist_height(istree) >= i) break;
    }
    k = ist_addlvl(istree);
    if (k < 0) { cleanup(); error("%s", msgs(E_NOMEM)); }
    if (k != 0) break;

    if (param->verbose) Rprintf(" %d", ist_height(istree));

    if (tatree) {
      if (((param->filter < 0) && (i < -param->filter * n))
       || ((param->filter > 0) && (i < n) && (i * (double)tt < param->filter * n * tc))) {
        n = i; x = clock();
        tas_filter(taset, apps);
        tat_delete(tatree);
        tatree = tat_create(taset, param->heap);
        if (!tatree) { cleanup(); error("%s", msgs(E_NOMEM)); }
        tt = clock() - x;
      }
      x = clock();
      ist_countx(istree, tatree);
      tc = clock() - x;
    } else if (taset) {
      if (((param->filter < 0) && (i <= -param->filter * n))
       || ((param->filter > 0) && (i * (double)tt <= param->filter * n * tc))) {
        n = i; x = clock();
        tas_filter(taset, apps);
        tt = clock() - t;
      }
      x = clock();
      for (i = tacnt; --i >= 0; )
        ist_count(istree, tas_tract(taset, i), tas_tsize(taset, i));
      tc = clock() - t;
    } else {
      in->index = 0;
      for (maxcnt = 0; (i = is_read_in(itemset, in)) == 0; ) {
        if (param->filter != 0)
          is_filter(itemset, apps);
        k = is_tsize(itemset);
        if (k > maxcnt) maxcnt = k;
        ist_count(istree, is_tract(itemset), k);
      }
      if (i < 0) { cleanup(); error("%s %s %i %s", msgs(i), "reading transactions", RECCNT(itemset), BUFFER(itemset)); }
      if (maxcnt < param->maxlen)
        param->maxlen = maxcnt;
    }
  }

  if (param->verbose) Rprintf(" done [%.2fs].\n", SEC_SINCE(t));

  if ((param->target == TT_MFSET) || (param->target == TT_CLSET) || (param->target == TT_GRSET)) {
    if (param->verbose) Rprintf("filtering %s item sets ... ",
      (param->target == TT_MFSET) ? "maximal" : (param->target == TT_GRSET) ? "generator" : "closed");
    t = clock();
    ist_filter(istree, (param->target == TT_MFSET) ? IST_MAXFRQ : (param->target == TT_GRSET) ? IST_GENTOR : IST_CLOSED);
    if (param->verbose) Rprintf("done [%.2fs].\n", SEC_SINCE(t));
  }

  if (param->target <= TT_CLSET) {
    if (!taset)
      param->ext = 0;
    else if (param->ext) {
      if (param->verbose) Rprintf("sorting transactions ... ");
      t = clock();
      tas_sort(taset, param->heap);
      if (param->verbose) Rprintf("done [%.2fs].\n", SEC_SINCE(t));
    }
  }
  free(apps);
}

void createRules(ISTREE *istree, ARparameter *param)
{
  double supp, conf;
  int *set;
  double lftval = 0, minval = 0;
  int n, k, i, h = 0;
  clock_t t;
  int target;
  int size, size1;
  char **vec = NULL, **vec3 = NULL;
  int *vec1 = NULL;
  double *vec2 = NULL;

  assert(param);
  ruleset = rs_create();
  t = clock();
  if (param->verbose) Rprintf("writing ... ");
  target = param->target;
  supp = param->supp;
  conf = param->conf;
  minval = param->minval;
  ist_init(istree, param->minlen, param->arem, param->minval);

  set = is_tract(itemset);
  size = ruleset->ttotal;
  size1 = ruleset->rnb;
  if (target <= TT_CLSET) {
    for (n = 0; 1; ) {
      R_CheckUserInterrupt();
      k = ist_set(istree, set, &supp, &conf);
      if (k <= 0) break;
      if (supp > param->smax) continue;
      for (i = 0; i < k; i++) {
        if (h >= size) {
          size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
          vec = (char**)realloc(ruleset->body, size * sizeof(char*));
          if (!vec) { free(vec1); free(vec2); free(vec3); cleanup(); error("%s", msgs(E_NOMEM)); }
          ruleset->body = vec;
        }
        ruleset->body[h] = (char *)(is_name(itemset, set[i]));
        h++;
      }
      if (n >= size1) {
        size1 += (size1 > BLKSIZE) ? (size1 >> 1) : BLKSIZE;
        vec1 = (int*)realloc(ruleset->tnb, size1 * sizeof(int));
        if (!vec1) { free(vec); free(vec2); free(vec3); cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->tnb = vec1;
        vec2 = (double*)realloc(ruleset->supp, size1 * sizeof(double));
        if (!vec2) { free(vec); free(vec1); free(vec3); cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->supp = vec2;
        if (param->ext) {
          vec2 = (double*)realloc(ruleset->ext, size1 * sizeof(double));
          if (!vec2) { free(vec); free(vec1); free(vec3); cleanup(); error("%s", msgs(E_NOMEM)); }
          ruleset->ext = vec2;
        }
      }
      if (n == 0) ruleset->tnb[0] = k; else ruleset->tnb[n] = ruleset->tnb[n-1] + k;
      ruleset->supp[n] = supp;
      if (param->ext) {
        supp = tas_occur(taset, set, k);
        ruleset->ext[n] = supp / istree->tacnt;
      }
      n++;
    }
    ruleset->rnb = n;
    ruleset->ttotal = h;
  } else if (target == TT_RULE) {
    for (n = 0; 1; ) {
      R_CheckUserInterrupt();
      k = ist_rule(istree, set, &supp, &conf, &lftval, &minval);
      if (k <= 0) break;
      if (supp > param->smax) continue;
      for (i = 1; i < k; i++) {
        if (h >= size) {
          size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
          vec = (char**)realloc(ruleset->body, size * sizeof(char*));
          if (!vec) { cleanup(); error("%s", msgs(E_NOMEM)); }
          ruleset->body = vec;
        }
        ruleset->body[h] = (char *)(is_name(itemset, set[i]));
        h++;
      }
      if (n >= size1) {
        size1 += (size1 > BLKSIZE) ? (size1 >> 1) : BLKSIZE;
        vec1 = (int*)realloc(ruleset->tnb, size1 * sizeof(int));
        if (!vec1) { cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->tnb = vec1;
        vec2 = (double*)realloc(ruleset->supp, size1 * sizeof(double));
        if (!vec2) { cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->supp = vec2;
        vec2 = (double*)realloc(ruleset->conf, size1 * sizeof(double));
        if (!vec2) { cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->conf = vec2;
        vec2 = (double*)realloc(ruleset->lift, size1 * sizeof(double));
        if (!vec2) { cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->lift = vec2;
        vec3 = (char**)realloc(ruleset->head, size1 * sizeof(char*));
        if (!vec3) { cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->head = vec3;
        if (param->aval) {
          vec2 = (double*)realloc(ruleset->aval, size1 * sizeof(double));
          if (!vec2) { cleanup(); error("%s", msgs(E_NOMEM)); }
          ruleset->aval = vec2;
        }
        if (param->ext) {
          vec2 = (double*)realloc(ruleset->ext, size1 * sizeof(double));
          if (!vec2) { cleanup(); error("%s", msgs(E_NOMEM)); }
          ruleset->ext = vec2;
        }
      }
      ruleset->head[n] = (char *)(is_name(itemset, set[0]));
      if (n == 0) ruleset->tnb[0] = k - 1; else ruleset->tnb[n] = ruleset->tnb[n-1] + k - 1;
      ruleset->supp[n] = (param->rsdef == IST_BOTH) ? supp : supp * conf;
      ruleset->conf[n] = conf;
      ruleset->lift[n] = lftval;
      if (param->aval) ruleset->aval[n] = minval;
      if (param->ext) {
        if (param->rsdef == IST_BOTH) ruleset->ext[n] = supp / conf;
        else ruleset->ext[n] = supp;
      }
      n++;
    }
    ruleset->rnb = n;
    ruleset->ttotal = h;
  } else {
    for (n = 0; 1; ) {
      R_CheckUserInterrupt();
      k = ist_hedge(istree, set, &supp, &conf);
      if (k <= 0) break;
      if (supp > param->smax) continue;
      for (i = 0; i < k; i++) {
        if (h >= size) {
          size += (size > BLKSIZE) ? (size >> 1) : BLKSIZE;
          vec = (char**)realloc(ruleset->body, size * sizeof(char*));
          if (!vec) { cleanup(); error("%s", msgs(E_NOMEM)); }
          ruleset->body = vec;
        }
        ruleset->body[h] = (char *)(is_name(itemset, set[i]));
        h++;
      }
      if (n >= size1) {
        size1 += (size1 > BLKSIZE) ? (size1 >> 1) : BLKSIZE;
        vec1 = (int*)realloc(ruleset->tnb, size1 * sizeof(int));
        if (!vec1) { cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->tnb = vec1;
        vec2 = (double*)realloc(ruleset->supp, size1 * sizeof(double));
        if (!vec2) { cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->supp = vec2;
        vec2 = (double*)realloc(ruleset->conf, size1 * sizeof(double));
        if (!vec2) { cleanup(); error("%s", msgs(E_NOMEM)); }
        ruleset->conf = vec2;
      }
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

void sort_ngCMatrix(SEXP x)
{
  int i, f, l;
  SEXP px, ix;

  PROTECT(px = GET_SLOT(x, install("p")));
  PROTECT(ix = GET_SLOT(x, install("i")));

  f = INTEGER(px)[0];
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    R_isort(INTEGER(ix)+f, l-f);
    f = l;
  }
  UNPROTECT(2);
}

SEXP returnObject(RULESET *set, SEXP dim, ARparameter *param, SEXP itemInfo)
{
  int i, len, k, nprotect = 0;
  SEXP ans, class, tp, qual, q, rownames, names, items, lhs, rhs, trans, tidLists;

  if (param->target <= TT_CLSET) {
    PROTECT(ans = NEW_OBJECT_OF_CLASS("itemsets"));
    nprotect++;
    len = 1;
  } else if (param->target == TT_RULE) {
    PROTECT(ans = NEW_OBJECT_OF_CLASS("rules"));
    nprotect++;
    len = 3;
  } else {
    PROTECT(ans = NEW_OBJECT_OF_CLASS("itemsets"));
    nprotect++;
    len = 2;
  }

  if (param->target != TT_RULE) param->ext = 0;
  if (param->aval) len++;
  if (param->ext) len++;

  PROTECT(items = NEW_OBJECT_OF_CLASS("ngCMatrix"));
  nprotect++;
  PROTECT(tp = NEW_INTEGER(set->ttotal));
  nprotect++;
  for (i = 0; i < set->ttotal; i++)
    INTEGER(tp)[i] = atoi(set->body[i]);
  SET_SLOT(items, install("i"), tp);

  PROTECT(tp = NEW_INTEGER(set->rnb + 1));
  nprotect++;
  INTEGER(tp)[0] = 0;
  for (i = 0; i < set->rnb; i++) INTEGER(tp)[i + 1] = set->tnb[i];
  SET_SLOT(items, install("p"), tp);

  PROTECT(tp = NEW_INTEGER(2));
  nprotect++;
  INTEGER(tp)[0] = INTEGER(dim)[0];
  INTEGER(tp)[1] = set->rnb;
  SET_SLOT(items, install("Dim"), tp);

  sort_ngCMatrix(items);

  PROTECT(lhs = NEW_OBJECT_OF_CLASS("itemMatrix"));
  nprotect++;
  SET_SLOT(lhs, install("data"), items);
  SET_SLOT(lhs, install("itemInfo"), itemInfo);

  if (param->target == TT_RULE)
    SET_SLOT(ans, install("lhs"), lhs);
  else
    SET_SLOT(ans, install("items"), lhs);

  if (param->target == TT_RULE) {
    PROTECT(items = NEW_OBJECT_OF_CLASS("ngCMatrix"));
    nprotect++;

    PROTECT(tp = NEW_INTEGER(set->rnb));
    nprotect++;
    for (i = 0; i < set->rnb; i++)
      INTEGER(tp)[i] = atoi(set->head[i]);
    SET_SLOT(items, install("i"), tp);

    PROTECT(tp = NEW_INTEGER(set->rnb + 1));
    nprotect++;
    for (i = 0; i < set->rnb + 1; i++) INTEGER(tp)[i] = i;
    SET_SLOT(items, install("p"), tp);

    PROTECT(tp = NEW_INTEGER(2));
    nprotect++;
    INTEGER(tp)[0] = INTEGER(dim)[0];
    INTEGER(tp)[1] = set->rnb;
    SET_SLOT(items, install("Dim"), tp);

    sort_ngCMatrix(items);

    PROTECT(rhs = NEW_OBJECT_OF_CLASS("itemMatrix"));
    nprotect++;
    SET_SLOT(rhs, install("data"), items);
    SET_SLOT(rhs, install("itemInfo"), itemInfo);
    SET_SLOT(ans, install("rhs"), rhs);
  }

  PROTECT(qual = NEW_LIST(len));
  nprotect++;
  PROTECT(names = NEW_CHARACTER(len));
  nprotect++;

  k = 0;
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
      SET_STRING_ELT(names, k++, CREATE_STRING_VECTOR("coverage"));
    else
      SET_STRING_ELT(names, k++, CREATE_STRING_VECTOR("transIdenticalToItemsets"));
  }
  if (param->target == TT_RULE) {
    SET_VECTOR_ELT(qual, k, q = NEW_NUMERIC(set->rnb));
    for (i = 0; i < set->rnb; i++) REAL(q)[i] = set->lift[i];
    SET_STRING_ELT(names, k++, CREATE_STRING_VECTOR("lift"));
  }

  PROTECT(rownames = NEW_INTEGER(set->rnb));
  nprotect++;
  for (i = 0; i < set->rnb; i++) INTEGER(rownames)[i] = i + 1;
  setAttrib(qual, install("row.names"), rownames);
  setAttrib(qual, install("names"), names);
  PROTECT(class = NEW_CHARACTER(1));
  nprotect++;
  SET_STRING_ELT(class, 0, CREATE_STRING_VECTOR("data.frame"));
  classgets(qual, class);
  SET_SLOT(ans, install("quality"), qual);

  if (param->trans) {
    PROTECT(trans = NEW_OBJECT_OF_CLASS("ngCMatrix"));
    nprotect++;

    PROTECT(tp = NEW_INTEGER(set->trtotal));
    nprotect++;
    for (i = 0; i < set->trtotal; i++) INTEGER(tp)[i] = set->trans[i];
    SET_SLOT(trans, install("i"), tp);

    PROTECT(tp = NEW_INTEGER(set->rnb + 1));
    nprotect++;
    INTEGER(tp)[0] = 0;
    for (i = 0; i < set->rnb; i++) INTEGER(tp)[i+1] = set->trnb[i];
    SET_SLOT(trans, install("p"), tp);

    PROTECT(tp = NEW_INTEGER(2));
    nprotect++;
    INTEGER(tp)[0] = set->tacnt;
    INTEGER(tp)[1] = set->rnb;
    SET_SLOT(trans, install("Dim"), tp);

    sort_ngCMatrix(trans);

    PROTECT(tidLists = NEW_OBJECT_OF_CLASS("tidLists"));
    nprotect++;
    SET_SLOT(tidLists, install("data"), trans);
    SET_SLOT(ans, install("tidLists"), tidLists);
  }

  UNPROTECT(nprotect);
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
  int nprotect = 0;
  SEXP s;

  PROTECT(s = GET_SLOT(control, install("verbose"))); nprotect++; param.verbose = *LOGICAL(s);
  PROTECT(s = GET_SLOT(parms, install("support"))); nprotect++; param.supp = *REAL(s);
  PROTECT(s = GET_SLOT(parms, install("confidence"))); nprotect++; param.conf = *REAL(s);
  PROTECT(s = GET_SLOT(parms, install("minval"))); nprotect++; param.minval = *REAL(s);
  PROTECT(s = GET_SLOT(control, install("filter"))); nprotect++; param.filter = *REAL(s);
  PROTECT(s = GET_SLOT(parms, install("smax"))); nprotect++; param.smax = *REAL(s);
  PROTECT(s = GET_SLOT(parms, install("target"))); nprotect++; target = translateChar(STRING_ELT(s, 0)); param.target = targetcode(target);
  PROTECT(s = GET_SLOT(parms, install("arem"))); nprotect++; arem = translateChar(STRING_ELT(s, 0)); param.arem = aremcode(arem);
  PROTECT(s = GET_SLOT(parms, install("minlen"))); nprotect++; param.minlen = *INTEGER(s);
  PROTECT(s = GET_SLOT(parms, install("maxlen"))); nprotect++; maxlen = param.maxlen = *INTEGER(s);
  PROTECT(s = GET_SLOT(parms, install("maxtime"))); nprotect++; param.maxtime = *REAL(s);
  PROTECT(s = GET_SLOT(control, install("sort"))); nprotect++; param.sort = *INTEGER(s);
  PROTECT(s = GET_SLOT(parms, install("originalSupport"))); nprotect++; param.rsdef = *LOGICAL(s);
  PROTECT(s = GET_SLOT(control, install("tree"))); nprotect++; param.tree = *LOGICAL(s);
  PROTECT(s = GET_SLOT(control, install("heap"))); nprotect++; param.heap = *LOGICAL(s);
  PROTECT(s = GET_SLOT(parms, install("aval"))); nprotect++; param.aval = *LOGICAL(s);
  PROTECT(s = GET_SLOT(control, install("memopt"))); nprotect++; param.memopt = *LOGICAL(s);
  PROTECT(s = GET_SLOT(parms, install("ext"))); nprotect++; param.ext = *LOGICAL(s);
  param.trans = 0;
  PROTECT(s = GET_SLOT(control, install("load"))); nprotect++; load = *LOGICAL(s);

  switch (param.target) {
    case 0: param.target = TT_SET;   break;
    case 1: param.target = TT_MFSET; break;
    case 2: param.target = TT_GRSET; break;
    case 3: param.target = TT_CLSET; break;
    case 4: param.target = TT_RULE;  break;
    case 5: param.target = TT_HEDGE; break;
    default: cleanup(); UNPROTECT(nprotect); error("%s", msgs(E_TARGET, target));
  }
  if (param.supp > 1.0) { cleanup(); UNPROTECT(nprotect); error("%s", msgs(E_SUPP, param.supp)); }
  if ((param.conf < 0.0) || (param.conf > 1.0)) { cleanup(); UNPROTECT(nprotect); error("%s", msgs(E_CONF, param.conf)); }

  if (param.supp > 0)
    param.supp = nextafter(param.supp, 0.0);
  param.conf = nextafter(param.conf, 0.0);

  if (param.minlen <= 0) { cleanup(); UNPROTECT(nprotect); error("%s", msgs(E_RULELEN, param.minlen)); }
  if (param.maxlen <= 0) { cleanup(); UNPROTECT(nprotect); error("%s", msgs(E_RULELEN, param.maxlen)); }

  switch (param.arem) {
    case 0: param.arem = EM_NONE; break;
    case 1: param.arem = EM_DIFF; break;
    case 2: param.arem = EM_QUOT; break;
    case 3: param.arem = EM_AIMP; break;
    case 4: param.arem = EM_INFO; break;
    case 5: param.arem = EM_CHI2; break;
    default: cleanup(); UNPROTECT(nprotect); error("%s", msgs(E_MEASURE, arem));
  }

  if (param.rsdef) param.rsdef = IST_BOTH;
  else             param.rsdef = IST_BODY;

  if ((param.target == TT_HEDGE) && param.ext) {
    Rf_warning("No extended measure available.\n");
    LOGICAL(GET_SLOT(parms, install("ext")))[0] = param.ext = 0;
  }
  if ((param.target != TT_RULE) && param.aval) {
    Rf_warning("No additional measure available.\n");
    LOGICAL(GET_SLOT(parms, install("aval")))[0] = param.aval = 0;
    param.arem = EM_NONE;
    SET_SLOT(parms, install("arem"), PROTECT(ScalarString(CREATE_STRING_VECTOR("none"))));
    UNPROTECT(1);
  }
  if (param.arem == EM_NONE) {
    REAL(GET_SLOT(parms, install("minval")))[0] = param.minval = 0;
    if (param.aval) {
      Rf_warning("No additional measure available.\n");
      LOGICAL(GET_SLOT(parms, install("aval")))[0] = param.aval = 0;
    }
  }
  if ((param.minval < 0) || ((param.arem != EM_AIMP) && (param.minval > 1))) {
    cleanup(); UNPROTECT(nprotect); error("%s", msgs(E_MVAL, param.minval));
  }
  if (param.target == TT_HEDGE) {
    REAL(GET_SLOT(parms, install("minval")))[0] = param.minval = param.conf;
    REAL(GET_SLOT(parms, install("confidence")))[0] = param.conf = 1;
  } else if (param.target <= TT_CLSET) {
    LOGICAL(GET_SLOT(parms, install("originalSupport")))[0] = param.rsdef = IST_BOTH;
    REAL(GET_SLOT(parms, install("confidence")))[0] = param.conf = 1;
  }
  if ((param.filter <= -1) || (param.filter >= 1)) {
    Rf_warning("Parameter 'filter' set to 0.\n");
    REAL(GET_SLOT(control, install("filter")))[0] = param.filter = 0;
  }

  itemset = is_create();
  if (!itemset) { cleanup(); UNPROTECT(nprotect); error("%s", msgs(E_NOMEM)); }
  if (load) {
    taset = tas_create(itemset);
    if (!taset) { cleanup(); UNPROTECT(nprotect); error("%s", msgs(E_NOMEM)); }
  }

  t = clock();
  if (param.verbose) Rprintf("set item appearances ...");
  k = is_readapp_R(itemset, app);
  if (k != 0) { cleanup(); UNPROTECT(nprotect); error("%s", msgs(k, "appearance", RECCNT(itemset), BUFFER(itemset))); }
  if (param.verbose) {
    Rprintf("[%d item(s)] done ", is_cnt(itemset));
    Rprintf("[%.2fs].\n", SEC_SINCE(t));
  }

  in.x = AS_CHARACTER(y);
  in.ind = INTEGER(x);
  in.index = 0;
  in.tnb = length(x) - 1;
  frequentItem(&param, &in);
  createRules(istree, &param);
  ruleset->cnt = is_cnt(itemset);
  ruleset->tacnt = in.tnb;

  SET_SLOT(parms, install("maxlen"), PROTECT(NEW_INTEGER(1)));
  UNPROTECT(1);
  INTEGER(GET_SLOT(parms, install("maxlen")))[0] = maxlen;

  t = clock();
  if (param.verbose) Rprintf("creating S4 object  ... ");
  PROTECT(ans = returnObject(ruleset, dim, &param, itemInfo));
  nprotect++;
  cleanup();
  if (param.verbose) {
    Rprintf("done ");
    Rprintf("[%.2fs].\n", SEC_SINCE(t));
  }

  UNPROTECT(nprotect);
  return ans;
}
