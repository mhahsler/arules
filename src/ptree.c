#include <R.h>
#include <Rdefines.h>
#include <time.h>
#include "newS4object.h"


/*
 support counting and rule generation for user-supplied
 sets of itemsets. counts are stored in memory-efficient
 prefix-trees. 
 
 warning: the code is not thread-safe, but I guess so is
 most of the R source code.
 
 todo: 1) optimize insertion of left nodes. 2) check for
 special cases, such as all itemsets or transactions
 are empty, or the set of items is empty. 3) register
 pointer array for finalizing.
 
 note that sgCMatrix support is for package arulesSequence.
 
 Version 0.2-7
 
 (C) ceeboo 2007, 2016
 */

typedef struct pnode {
  int index;
  int count;
  struct pnode *pl;
  struct pnode *pr;
} PN;

static PN *nq, **nb = NULL;		    /* node pointers */ 
static int npn, cpn, apn;		    /* node counters */
static int *pb;				    /* prefix buffer */

static void pnfree(PN *p) {
  if (p == NULL)
    return;
  pnfree(p->pl);
  pnfree(p->pr);
  free(p);
  apn--;
}

static void nbfree() {
  pnfree(*nb);
  free( nb);
  nb = NULL;
}

static PN *pnadd(PN *p, int *x, int n) {
  if (n == 0)
    return p;
  cpn++;
  if (p == NULL) {			    /* append node */
p = nq = (PN *) malloc(sizeof(PN));
    if (p) {
      apn++;
      p->index = *x;
      p->count = 0;
      p->pr = NULL;
      p->pl = pnadd(NULL, x+1, n-1);
    } else
      npn = 1;
  } else
    if (p->index == *x) {		    /* existing node */
nq = p;
      p->pl = pnadd(p->pl, x+1, n-1);
    } else
      if (p->index < *x) {		    /* search right subtree */
nq = p;
        p->pr = pnadd(p->pr, x, n);
      } else {				    /* prepend node */
PN *q = nq = (PN *) malloc(sizeof(PN));
        if (q) {
          apn++;
          q->index = *x;
          q->count = 0;
          q->pr = p;
          q->pl = pnadd(NULL, x+1, n-1);
          p = q;
        } else
          npn = 1;
      }
      return p;
}

/* retrieve count */

static int pnget(PN *p, int *x, int n) {
  if (p == NULL || n == 0)
    return 0;
  cpn++;
  if (p->index == *x) {
    npn++;
    if (n == 1)
      return p->count;
    return pnget(p->pl, x+1, n-1);
  }
  if (p->index < *x) 
    return pnget(p->pr, x, n);
  return 0;				    /* set not found */
}

/* count transaction */

static void pncount(PN *p, int *x, int n) {
  if (p == NULL || n == 0)
    return;
  cpn++;
  if (p->index == *x) {
    npn++;
    p->count++;
    pncount(p->pl, x+1, n-1);
    pncount(p->pr, x+1, n-1);
  } else
    if (p->index < *x) 
      pncount(p->pr, x, n);
    else
      pncount(p, x+1, n-1);
}

/*
 note that we do not drop rules with zero support
 as filtering is done later anyway. in this case, 
 confidence and lift can either be zero or NaN, 
 depending on the support of the left-hand side. 
 */

SEXP R_pncount(SEXP R_x, SEXP R_t, SEXP R_s, SEXP R_o, SEXP R_v) {
  int i, j, c, f, l, k, n, nr, np, ni, e, nprotect = 0;
  int *x, *o = NULL;
  double s, t = 0;
  SEXP px, ix, pt, it;
  SEXP r, pr, ir, pl, il, rs, rc, rl, pi; 
#ifdef _TIME_H
  clock_t t5, t4, t3, t2, t1, t0;
  
  t1 = t0 = clock();
  
  if (LOGICAL(R_v)[0] == TRUE) {
    if (LOGICAL(R_o)[0] == TRUE)
      Rprintf("reducing ... ");
    else 
      Rprintf("preparing ... ");
  }
#endif
  
  if (!inherits(R_x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!inherits(R_t, "ngCMatrix"))
    error("'t' not of class ngCMatrix");
  if (INTEGER(GET_SLOT(R_x, install("Dim")))[0] != 
      INTEGER(GET_SLOT(R_t, install("Dim")))[0])
    error("the number of rows of 'x' and 't' do not conform");
  if (TYPEOF(R_s) != LGLSXP)
    error("'s' not of type logical");
  if (TYPEOF(R_o) != LGLSXP)
    error("'o' not of type logical");
  if (TYPEOF(R_v) != LGLSXP)
    error("'v' not of type logical");
  
  nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
  
  px = GET_SLOT(R_x, install("p"));
  ix = GET_SLOT(R_x, install("i"));
  
  pt = GET_SLOT(R_t, install("p"));
  it = GET_SLOT(R_t, install("i"));
  
  pb = INTEGER(PROTECT(allocVector(INTSXP, nr+1)));
  nprotect++;
  
  if (LOGICAL(R_o)[0] == TRUE) {
    SEXP pz, iz;
    
    o = INTEGER(PROTECT(allocVector(INTSXP, nr)));
    nprotect++;
    
    memset(o, 0, sizeof(int) * nr);
    
    for (k = 0; k < LENGTH(it); k++)
      o[INTEGER(it)[k]]++;
    
    memset(pb, 0, sizeof(int) * nr);
    
    for (k = 0; k < LENGTH(ix); k++)
      pb[INTEGER(ix)[k]] = 1;
    
    n = c = 0;
    for (k = 0; k < nr; k++) {
      if (pb[k])
        n += o[k];
      else {
        o[k] = -1;
        c++;
      }
      pb[k] = k;
    }
    
    R_qsort_int_I(o, pb, 1, nr);
    
    for (k = 0; k < nr; k++)
      o[pb[k]] = (k < c) ? -1 : k;
    
    PROTECT(iz = allocVector(INTSXP, LENGTH(ix)));
    nprotect++;
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
      l = INTEGER(px)[i];
      if (f == l)
        continue;
      for (k = f; k < l; k++)
        INTEGER(iz)[k] = o[INTEGER(ix)[k]];
      R_isort(INTEGER(iz)+f, l-f);
      f = l;
    }
    
    ix = iz;
    PROTECT(pz = allocVector(INTSXP, LENGTH(pt)));
    nprotect++;
    PROTECT(iz = allocVector(INTSXP, n));
    nprotect++;
    
    f = n = INTEGER(pz)[0] = 0;
    for (i = 1; i < LENGTH(pt); i++) {
      l = INTEGER(pt)[i];
      if (f < l) {
        for (k = f, f = n; k < l; k++)
          if ((j = o[INTEGER(it)[k]]) > -1)
            INTEGER(iz)[n++] = j;
        R_isort(INTEGER(iz)+f, n-f);
        f = l;
      }
      INTEGER(pz)[i] = n;
    }
    pt = pz;
    ni = LENGTH(it);
    it = iz;
    
    if (LOGICAL(R_s)[0] == FALSE)
      memcpy(o, pb, sizeof(int) * nr);
    
#ifdef _TIME_H
    t1 = clock();
    if (LOGICAL(R_v)[0] == TRUE) {
      Rprintf("%i indexes, dropped %i (%.2f) items [%.2fs]\n", 
              LENGTH(ix) + ni, c, 1 - (double) n / ni,
              ((double) t1 - t0) / CLOCKS_PER_SEC);
      Rprintf("preparing ... ");
    }
#endif
  }
  
  if (nb != NULL)
    nbfree();
  nb = (PN **) malloc(sizeof(PN *) * (nr+1));
  if (nb == NULL)
    error("pointer array allocation failed");
  
  cpn = apn = npn = 0;
  
  k = nr;
  nb[k] = NULL;
  while (k-- > 0)
    nb[k] = pnadd(nb[k+1], &k, 1);
  
  if (npn) {
    nbfree();
    error("node allocation failed");
  }
  
  np = ni = 0;
  
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0)
      continue;
    x = INTEGER(ix)+f;
    pnadd(nb[*x], x, n);
    if (LOGICAL(R_s)[0] == FALSE && n > 1) {
      if (n > 2) {
        memcpy(pb, x, sizeof(int) * n);
        for (k = 0; k < n-1; k++) {
          if (k > 0) {
            j     = pb[0];
            pb[0] = pb[k];
            pb[k] = j;
          }
          pnadd(nb[pb[1]], pb+1, n-1);
        }
      }
      np += n;
      ni += n * (n-1);
    }
    if (npn) {
      nbfree();
      error("node allocation failed");
    }
    f = l;
    R_CheckUserInterrupt();
  }
  
#ifdef _TIME_H
  t2 = clock();
  if (LOGICAL(R_v)[0] == TRUE) {
    Rprintf("%i itemsets, created %i (%.2f) nodes [%.2fs]\n",
            2 * np +  LENGTH(px) - 1, apn, (double) apn / cpn,
            ((double) t2 - t1) / CLOCKS_PER_SEC);
    Rprintf("counting ... ");
  }
#endif
  
  cpn = npn = 0;
  
  f = 0;
  for (i = 1; i < LENGTH(pt); i++) {
    l = INTEGER(pt)[i];
    n = l-f;
    if (n == 0)
      continue;
    x = INTEGER(it)+f;
    pncount(nb[*x], x, n);
    f = l;
    R_CheckUserInterrupt();
  }
  
#ifdef _TIME_H
  t3 = clock();
  if (LOGICAL(R_v)[0] == TRUE) {
    Rprintf("%i transactions, processed %i (%.2f) nodes [%.2fs]\n",
            LENGTH(pt) - 1, cpn, (double) npn / cpn, 
            ((double) t3 - t2) / CLOCKS_PER_SEC);
    Rprintf("writing ... ");
  }
#endif
  
  if (LOGICAL(R_s)[0] == TRUE) {
    PROTECT(r = allocVector(INTSXP, LENGTH(px)-1));
    nprotect++;
    /* warnings */
    pl = il = pr = ir = rs = rc = rl = pi = (SEXP)0;
  } else {
    SEXP o, p;
    PROTECT(r = allocVector(VECSXP, 6));
    nprotect++;
    
    SET_VECTOR_ELT(r, 0, o = NEW_OBJECT_OF_CLASS("ngCMatrix"));
    SET_SLOT(o, install("p"),   PROTECT(pl = allocVector(INTSXP, np+1)));
    nprotect++;
    SET_SLOT(o, install("i"),   PROTECT(il = allocVector(INTSXP, ni)));
    nprotect++;
    SET_SLOT(o, install("Dim"), PROTECT(p  = allocVector(INTSXP, 2)));
    nprotect++;
    INTEGER(p)[0] = nr;
    INTEGER(p)[1] = np;
    
    SET_VECTOR_ELT(r, 1, o = NEW_OBJECT_OF_CLASS("ngCMatrix"));
    SET_SLOT(o, install("p"),   PROTECT(pr = allocVector(INTSXP, np+1)));
    nprotect++;
    SET_SLOT(o, install("i"),   PROTECT(ir = allocVector(INTSXP, np)));
    nprotect++;
    SET_SLOT(o, install("Dim"), PROTECT(p  = allocVector(INTSXP, 2)));
    nprotect++;
    INTEGER(p)[0] = nr;
    INTEGER(p)[1] = np;
    
    SET_VECTOR_ELT(r, 2, (rs = allocVector(REALSXP, np)));
    SET_VECTOR_ELT(r, 3, (rc = allocVector(REALSXP, np)));
    SET_VECTOR_ELT(r, 4, (rl = allocVector(REALSXP, np)));
    
    SET_VECTOR_ELT(r, 5, (pi = allocVector(INTSXP, np)));
    
    INTEGER(pl)[0] = INTEGER(pr)[0] = np = ni = 0;
    
    t = (double) LENGTH(pt)-1;
  }
  
  cpn = npn = 0;
  
  e = LENGTH(pt) - 1;
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0) {
      if (LOGICAL(R_s)[0] == TRUE)
        INTEGER(r)[i-1] = e;
      continue;
    }
    x = INTEGER(ix)+f;
    c = pnget(nb[*x], x, n);
    if (LOGICAL(R_s)[0] == TRUE)
      INTEGER(r)[i-1] = c;
    else if (n > 1) {
      s = c / t;
      memcpy(pb, x, sizeof(int) * n);
      for (k = 0; k < n; k++) {
        if (k > 0) {
          j     = pb[0];
          pb[0] = pb[k];
          pb[k] = j;
        }
        INTEGER(pi)[np] = i;	    /* itemset index */
    
    REAL(rs)[np] = s;
    REAL(rc)[np] = c / (double)   pnget(nb[pb[1]], pb+1, n-1);
    REAL(rl)[np] = REAL(rc)[np] / pnget(nb[pb[0]], pb, 1) * t;
    
    INTEGER(ir)[np++] = pb[0];
    INTEGER(pr)[np]   = np;
    for (j = 1; j < n; j++)
      INTEGER(il)[ni++] = pb[j];
    INTEGER(pl)[np] = ni;
      }
    }
    f = l;
    R_CheckUserInterrupt();
  }
  
  nbfree();
  
  if (apn)
    error("node deallocation imbalance %i", apn);
  
#ifdef _TIME_H
  t4 = clock();
  
  if (LOGICAL(R_v)[0] == TRUE) {
    if (LOGICAL(R_s)[0] == FALSE)
      Rprintf("%i rules, ", np);
    else
      Rprintf("%i counts, ", LENGTH(px)-1);
    Rprintf("processed %i (%.2f) nodes [%.2fs]\n", cpn, (double) npn / cpn,
            ((double) t4 - t3) / CLOCKS_PER_SEC);
  }
#endif
  
  if (LOGICAL(R_o)[0] == TRUE) {
    if (LOGICAL(R_s)[0] == FALSE) {
#ifdef _TIME_H
      if (LOGICAL(R_v)[0] == TRUE)
        Rprintf("recoding ... ");
#endif	
      f = 0;
      for (i = 1; i < LENGTH(pl); i++) {
        l = INTEGER(pl)[i];
        if (f == l)
          continue;
        for (k = f; k < l; k++)
          INTEGER(il)[k] = o[INTEGER(il)[k]];
        R_isort(INTEGER(il)+f, l-f);
        f = l;
      }
      for (k = 0; k < LENGTH(ir); k++)
        INTEGER(ir)[k] = o[INTEGER(ir)[k]];
      
#ifdef _TIME_H
      t5 = clock();
      if (LOGICAL(R_v)[0] == TRUE)
        Rprintf(" %i indexes [%.2fs]\n", LENGTH(il) + LENGTH(ir), 
                ((double) t5 - t4) / CLOCKS_PER_SEC);
#endif
    }
  }
  
  UNPROTECT(nprotect);
  
  return r;
}

/* index itemsets */

static void pnindex(PN *p) {
  if (p == NULL)
    return;
  cpn++;
  if (p->count)
    p->count = npn++;
  pnindex(p->pl);
  pnindex(p->pr);
}

SEXP R_pnindex(SEXP R_x, SEXP R_y, SEXP R_v) {
  int i, k, f, l, n, nr, e;
  int *x;
  SEXP r, px, ix, py, iy;
#ifdef _TIME_H
  clock_t t2, t1 = clock();
#endif
  
  if (!inherits(R_x, "ngCMatrix") && 
      !inherits(R_x, "sgCMatrix"))
      error("'x' not of class ngCMatrix");
  if (!isNull(R_y) && !inherits(R_y, "ngCMatrix") 
        && !inherits(R_x, "sgCMatrix"))
    error("'y' not of class ngCMatrix");
  if (TYPEOF(R_v) != LGLSXP)
    error("'v' not of type logical");
  
#ifdef _TIME_H
  if (LOGICAL(R_v)[0] == TRUE)
    Rprintf("indexing ... ");
#endif
  
  nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
  
  px = py = GET_SLOT(R_x, install("p"));
  ix = iy = GET_SLOT(R_x, install("i"));
  
  if (!isNull(R_y)) {
    if (nr != INTEGER(GET_SLOT(R_y, install("Dim")))[0])
      error("'x' and 'y' not the same number of rows");
    
    py = GET_SLOT(R_y, install("p"));
    iy = GET_SLOT(R_y, install("i"));
  }
  
  if (nb != NULL)
    nbfree();
  nb = (PN **) malloc(sizeof(PN *) * (nr+1));
  if (nb == NULL)
    error("pointer array allocation failed");
  
  cpn = apn = npn = 0;
  
  k = nr;
  nb[k] = NULL;
  while (k-- > 0)
    nb[k] = pnadd(nb[k+1], &k, 1);
  
  if (npn) {
    nbfree();
    error("node allocation failed");
  }
  
  f = e = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0) {
      if (e == 0)
        e = i;
      continue;
    }
    x = INTEGER(ix)+f;
    pnadd(nb[*x], x, n);
    if (npn) {
      nbfree();
      error("node allocation failed");
    }
    if (nq->count == 0)
      nq->count = i;
    f = l;
    R_CheckUserInterrupt();
  }
  
  PROTECT(r = allocVector(INTSXP, LENGTH(py)-1));
  
  if (isNull(R_y)) {
    e = 0;
    cpn = 0;
    npn = 1;
    
    pnindex(*nb);
  }
  
  cpn = npn = 0;
  
  f = 0;
  for (i = 1; i < LENGTH(py); i++) {
    l = INTEGER(py)[i];
    n = l-f;
    if (n == 0) {
      INTEGER(r)[i-1] = e;
      continue;
    }
    x = INTEGER(iy)+f;
    k = pnget(nb[*x], x, n);
    INTEGER(r)[i-1] = (k > 0) ? k : 0;
    f = l;
    R_CheckUserInterrupt();
  }
  
  nbfree();
  
  if (apn)
    error("node deallocation imbalance %i", apn);
#ifdef _TIME_H
  t2 = clock();
  if (LOGICAL(R_v)[0] == TRUE)
    Rprintf(" %i itemsets [%.2fs]\n", LENGTH(px) - 1, 
            ((double) t2-t1) / CLOCKS_PER_SEC);
#endif
  
  UNPROTECT(1);
  
  return r;
}

/*
 update all subsets to the maximum count
 of a superset
 
 FIXME: generalize to different uses
 */

static int pnc;

static void pnsmax(PN *p, int *x, int n, int l) {
  if (p == NULL || n == 0)
    return;
  cpn++;
  if (p->index == *x) {
    npn++;
    if ((l > n || n > 1) && p->count < pnc)
      p->count = pnc;
    pnsmax(p->pl, x+1, n-1, l-1);
    pnsmax(p->pr, x+1, n-1, l);
  } else
    if (p->index < *x) 
      pnsmax(p->pr, x, n, l);
    else
      pnsmax(p, x+1, n-1, l);
}

SEXP R_pnclosed(SEXP R_x, SEXP R_c, SEXP R_v) {
  int i, k, f, l, n, nr, e;
  int *x;
  SEXP r, px, ix;
#ifdef _TIME_H
  clock_t t2, t1 = clock();
#endif
  
  if (!inherits(R_x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (TYPEOF(R_c) != INTSXP)
    error("'c' not of storage type integer");
  if (LENGTH(R_c) != INTEGER(GET_SLOT(R_x, install("Dim")))[1])
    error("'x' and 'c' not the same length");
  if (TYPEOF(R_v) != LGLSXP)
    error("'v' not of type logical");
  
#ifdef _TIME_H
  if (LOGICAL(R_v)[0] == TRUE) 
    Rprintf("checking ... ");
#endif
  nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
  
  px = GET_SLOT(R_x, install("p"));
  ix = GET_SLOT(R_x, install("i"));
  
  if (nb != NULL)
    nbfree();
  nb = (PN **) malloc(sizeof(PN *) * (nr+1));
  if (nb == NULL)
    error("pointer array allocation failed");
  
  cpn = apn = npn = 0;
  
  k = nr;
  nb[k] = NULL;
  while (k-- > 0)
    nb[k] = pnadd(nb[k+1], &k, 1);
  
  if (npn) {
    nbfree();
    error("node allocation failed");
  }
  
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0) 
      continue;
    x = INTEGER(ix)+f;
    pnadd(nb[*x], x, n);
    if (npn) {
      nbfree();
      error("node allocation failed");
    }
    f = l;
    R_CheckUserInterrupt();
  }
  
  f = e = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0) 
      continue;
    x   = INTEGER(ix)+f;
    pnc = INTEGER(R_c)[i-1];
    if (pnc > e)
      e = pnc;
    else
      if (pnc < 1) {
        nbfree();
        error("invalid count");
      }
    pnsmax(nb[*x], x, n, n);
    f = l;
    R_CheckUserInterrupt();
  }
  
  PROTECT(r = allocVector(LGLSXP, LENGTH(px)-1));
  
  cpn = npn = 0;
  
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0) {
      pnc = INTEGER(R_c)[i-1];
      if (pnc < e) {
        nbfree();
        error("invalid count");
      }
      LOGICAL(r)[i-1] = (pnc > e) ? TRUE : FALSE;
      continue;
    }
    x = INTEGER(ix)+f;
    k = pnget(nb[*x], x, n);
    LOGICAL(r)[i-1] = (INTEGER(R_c)[i-1] > k) ? TRUE : FALSE;
    f = l;
    R_CheckUserInterrupt();
  }
  
  nbfree();
  
  if (apn)
    error("node deallocation imbalance %i", apn);
#ifdef _TIME_H
  t2 = clock();
  if (LOGICAL(R_v)[0] == TRUE)
    Rprintf(" %i itemsets [%.2fs]\n", LENGTH(px) - 1, 
            ((double) t2-t1) / CLOCKS_PER_SEC);
#endif
  
  UNPROTECT(1);
  
  return r;
}

//
// compute the maximum over all subsets of
// a superset
//

static double pmx, *pvl;

static void pnmax(PN *p, int *x, int n, int l) {
  if (p == NULL || n == 0)
    return;
  cpn++;
  if (p->index == *x) {
    npn++;
    if ((l > n || n > 1) && p->count) {
      double v = pvl[p->count];
      if (pmx < v)
        pmx = v;
    }
    pnmax(p->pl, x+1, n-1, l-1);
    pnmax(p->pr, x+1, n-1, l);
  } else
    if (p->index < *x) 
      pnmax(p->pr, x, n, l);
    else
      pnmax(p, x+1, n-1, l);
}

SEXP R_pnmax(SEXP R_x, SEXP R_c, SEXP R_v) {
  double e;
  int i, k, f, l, n, nr;
  int *x;
  SEXP r, px, ix;
#ifdef _TIME_H
  clock_t t2, t1 = clock();
#endif
  
  if (!inherits(R_x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (TYPEOF(R_c) != REALSXP)
    error("'c' not of storage type real");
  if (LENGTH(R_c) != INTEGER(GET_SLOT(R_x, install("Dim")))[1])
    error("'x' and 'c' not the same length");
  if (TYPEOF(R_v) != LGLSXP)
    error("'v' not of type logical");
  
#ifdef _TIME_H
  if (LOGICAL(R_v)[0] == TRUE) 
    Rprintf("computing ... ");
#endif
  nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
  
  px = GET_SLOT(R_x, install("p"));
  ix = GET_SLOT(R_x, install("i"));
  
  if (nb != NULL)
    nbfree();
  nb = (PN **) malloc(sizeof(PN *) * (nr+1));
  if (nb == NULL)
    error("pointer array allocation failed");
  
  cpn = apn = npn = 0;
  
  k = nr;
  nb[k] = NULL;
  while (k-- > 0)
    nb[k] = pnadd(nb[k+1], &k, 1);
  
  if (npn) {
    nbfree();
    error("node allocation failed");
  }
  
  pvl = REAL(R_c) - 1;
  
  e = R_NegInf;
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0) {
      e = pvl[i];
      continue;
    }
    x = INTEGER(ix)+f;
    pnadd(nb[*x], x, n);
    if (npn) {
      nbfree();
      error("node allocation failed");
    }
    nq->count = i;
    f = l;
    R_CheckUserInterrupt();
  }
  
  PROTECT(r = allocVector(REALSXP, LENGTH(px)-1));
  
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0) {
      REAL(r)[i-1] = R_NegInf;	
      continue;
    }
    x   = INTEGER(ix)+f;
    pmx = e;
    pnmax(nb[*x], x, n, n);
    REAL(r)[i-1] = pmx;
    f = l;
    R_CheckUserInterrupt();
  }
  
  nbfree();
  
  if (apn)
    error("node deallocation imbalance %i", apn);
#ifdef _TIME_H
  t2 = clock();
  if (LOGICAL(R_v)[0] == TRUE)
    Rprintf(" %i itemsets [%.2fs]\n", LENGTH(px) - 1, 
            ((double) t2-t1) / CLOCKS_PER_SEC);
#endif
  
  UNPROTECT(1);
  
  return r;
}

/*
 index a set of itemsets into a set of 
 rules.
 
 note that missing itemsets are coded 
 as zero.
 */

SEXP R_pnrindex(SEXP R_x, SEXP R_v) {
  int i, j, k, f, l, m, n, nr;
  int *x;
  SEXP px, ix;
  SEXP r, is, ir, il;
#ifdef _TIME_H
  clock_t t2, t1 = clock();
#endif
  
  if (!inherits(R_x, "ngCMatrix") && 
      !inherits(R_x, "sgCMatrix"))
      error("'x' not of class ngCMatrix");
  if (TYPEOF(R_v) != LGLSXP)
    error("'v' not of type logical");
#ifdef _TIME_H
  if (LOGICAL(R_v)[0] == TRUE) 
    Rprintf("processing ... ");
#endif
  nr = INTEGER(GET_SLOT(R_x, install("Dim")))[0];
  
  px = GET_SLOT(R_x, install("p"));
  ix = GET_SLOT(R_x, install("i"));
  
  if (nb != NULL)
    nbfree();
  nb = (PN **) malloc(sizeof(PN *) * (nr+1));
  if (nb == NULL)
    error("pointer array allocation failed");
  
  cpn = apn = npn = 0;
  
  k = nr;
  nb[k] = NULL;
  while (k-- > 0)
    nb[k] = pnadd(nb[k+1], &k, 1);
  
  if (npn) {
    nbfree();
    error("node allocation failed");
  }
  
  m = k = 0;
  
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0) 
      continue;
    x = INTEGER(ix)+f;
    pnadd(nb[*x], x, n);
    if (npn) {
      nbfree();
      error("node allocation failed");
    }
    if (nq->count == 0)
      nq->count = i;
    if (n > 1) 
      m += n;
    if (n > k)
      k = n;
    f = l;
    R_CheckUserInterrupt();
  }
  
  PROTECT(r = allocVector(VECSXP, 3));
  
  SET_VECTOR_ELT(r, 0, (is = allocVector(INTSXP, m)));
  SET_VECTOR_ELT(r, 1, (il = allocVector(INTSXP, m)));
  SET_VECTOR_ELT(r, 2, (ir = allocVector(INTSXP, m)));
  
  pb = INTEGER(PROTECT(allocVector(INTSXP, k+1)));
  
  cpn = npn = 0;
  
  m = 0;
  
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    if (n == 0)
      continue;
    if (n > 1) {
      x = INTEGER(ix)+f;
      memcpy(pb, x, sizeof(int) * n);
      for (k = 0; k < n; k++) {
        if (k > 0) {
          j     = pb[0];
          pb[0] = pb[k];
          pb[k] = j;
        }
        INTEGER(is)[m] = i;
        INTEGER(il)[m] = pnget(nb[pb[1]], pb+1, n-1);
        INTEGER(ir)[m] = pnget(nb[pb[0]], pb, 1);
        m++;
      }
    }
    f = l;
    R_CheckUserInterrupt();
  }
  
  nbfree();
  
  if (apn)
    error("node deallocation imbalance %i", apn);
#ifdef _TIME_H
  t2 = clock();
  if (LOGICAL(R_v)[0] == TRUE)
    Rprintf(" %i itemsets, %i rules [%.2fs]\n", LENGTH(px) - 1, m,
            ((double) t2-t1) / CLOCKS_PER_SEC);
#endif
  
  UNPROTECT(2);
  
  return r;
}

