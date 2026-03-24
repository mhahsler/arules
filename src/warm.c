#include <R.h>
#include <Rdefines.h>
#include <time.h>
#include "newS4object.h"

// Iterate a bipartite graph of transactions (itemsets) and
// items using the HITS algorithm with tolerance FLT_EPSILON.
//
// Inputs are the ngCMatrix representing the transactions, the
// maximum number of iterations, and the convergence tolerance
// to use.
//
// Returns the hub weights of the transactions.
//
// cf. K. Sun and F. Bai (2008). Mining Weighted Association
//     Rules without Preassigned Weights. IEEE Transactions
//     on Knowledge and Data Engineering 4 (20), pp. 489-495.
//
// ceeboo 2008
//
// Rewritten to consistently PROTECT retained attribute lookups
// and allocated objects that survive across further allocations.

SEXP R_hits_ngCMatrix(SEXP x, SEXP R_iter, SEXP R_tol, SEXP R_verbose) {
  int i, j, k, f, l, nr, nc, nprotect = 0;
  int *px, *ix;
  double tol, s, z, *z0, *ax, *hx;
  SEXP r, dimx, pxS, ixS, dnS;
#ifdef _TIME_H
  clock_t t0 = clock(), t1;
#endif

  if (!x || isNull(x) || !inherits(x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!R_iter || isNull(R_iter) || TYPEOF(R_iter) != INTSXP)
    error("'iter' not of storage type integer");
  if (!isNull(R_tol) && TYPEOF(R_tol) != REALSXP)
    error("'tol' not of storage type real");
  if (!R_verbose || isNull(R_verbose) || TYPEOF(R_verbose) != LGLSXP)
    error("'verbose' not of storage type logical");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  nr = INTEGER(dimx)[0];
  nc = INTEGER(dimx)[1];
  if (nr == 0 || nc == 0)
    error("invalid dimension(s)");

  PROTECT(pxS = getAttrib(x, install("p")));
  nprotect++;
  if (LENGTH(pxS) != nc + 1)
    error("p and Dim do not conform");
  px = INTEGER(pxS);

  PROTECT(ixS = getAttrib(x, install("i")));
  nprotect++;
  ix = INTEGER(ixS);

  if (INTEGER(R_iter)[0] < 1)
    error("iter invalid");

  tol = isNull(R_tol) ? FLT_EPSILON : REAL(R_tol)[0];
  if (tol < 0)
    error("'tol' invalid");

  PROTECT(r = allocVector(REALSXP, nc));
  nprotect++;
  PROTECT(dnS = getAttrib(x, install("Dimnames")));
  nprotect++;
  setAttrib(r, R_NamesSymbol, VECTOR_ELT(dnS, 1));

  hx = REAL(r);
  PROTECT(dimx = allocVector(REALSXP, nr));
  nprotect++;
  ax = REAL(dimx);
  PROTECT(pxS = allocVector(REALSXP, nr));
  nprotect++;
  z0 = REAL(pxS);

  s = sqrt(nr);
  for (k = 0; k < nr; k++)
    ax[k] = 1.0 / s;
  z = 0;
  j = INTEGER(R_iter)[0];
  while (j--) {
    R_chk_memset(z0, 0, sizeof(double) * nr);
    f = 0;
    for (i = 1; i < nc + 1; i++) {
      l = px[i];
      z = 0;
      for (k = f; k < l; k++)
        z += ax[ix[k]];
      hx[i - 1] = z;
      for (k = f; k < l; k++)
        z0[ix[k]] += z;
      f = l;
    }
    z = 0;
    for (k = 0; k < nr; k++)
      z += z0[k] * z0[k];
    z = sqrt(z);
    if (fabs(s - z) < tol)
      break;
    if (j > -1) {
      for (k = 0; k < nr; k++)
        ax[k] = z0[k] / z;
      s = z;
    }
    R_CheckUserInterrupt();
  }
  if ((s = fabs(s - z)) > tol)
    warning("no convergence: %g\n", s);

#ifdef _TIME_H
  t1 = clock();
  if (LOGICAL(R_verbose)[0] == TRUE)
    Rprintf(" %i iterations, %g convergence [%.2fs]\n",
            INTEGER(R_iter)[0] - j - 1, s,
            ((double) t1 - t0) / CLOCKS_PER_SEC);
#endif

  UNPROTECT(nprotect);
  return r;
}

// Compute column-weighted row sums of ngCMatrix objects.
//
// ceeboo 2008

SEXP R_rowWSums_ngCMatrix(SEXP x, SEXP R_weight) {
  int i, k, f, l, nr, nprotect = 0;
  double w;
  SEXP r, px, ix, dimx, dnx;

  if (!x || isNull(x) || !inherits(x, "ngCMatrix"))
    error("'x' not of class 'ngCMatrix'");
  if (!R_weight || isNull(R_weight) || TYPEOF(R_weight) != REALSXP)
    error("'w' not of type double");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  nr = INTEGER(dimx)[0];
  if (LENGTH(R_weight) != INTEGER(dimx)[1])
    error("the number of columns of 'x' and the length of 'weight' do not conform");

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;

  PROTECT(r = allocVector(REALSXP, nr));
  nprotect++;
  R_chk_memset(REAL(r), 0, sizeof(double) * nr);

  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    w = REAL(R_weight)[i - 1];
    for (k = f; k < l; k++)
      REAL(r)[INTEGER(ix)[k]] += w;
    f = l;
  }

  PROTECT(dnx = getAttrib(x, install("Dimnames")));
  nprotect++;
  setAttrib(r, R_NamesSymbol, VECTOR_ELT(dnx, 0));

  UNPROTECT(nprotect);
  return r;
}

// Compute row-weighted column sums of ngCMatrix objects.
//
// ceeboo 2008

SEXP R_colWSums_ngCMatrix(SEXP x, SEXP R_weight) {
  int i, k, f, l, nprotect = 0;
  double *w, z;
  SEXP r, px, ix, dimx, dnx;

  if (!x || isNull(x) || !inherits(x, "ngCMatrix"))
    error("'x' not of class 'ngCMatrix'");
  if (!R_weight || isNull(R_weight) || TYPEOF(R_weight) != REALSXP)
    error("'w' not of type double");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  if (LENGTH(R_weight) != INTEGER(dimx)[0])
    error("the number of rows of 'x' and the length of 'weight' do not conform");

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;
  w = REAL(R_weight);

  PROTECT(r = allocVector(REALSXP, LENGTH(px) - 1));
  nprotect++;

  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    z = 0;
    for (k = f; k < l; k++)
      z += w[INTEGER(ix)[k]];
    REAL(r)[i - 1] = z;
    f = l;
  }

  PROTECT(dnx = getAttrib(x, install("Dimnames")));
  nprotect++;
  setAttrib(r, R_NamesSymbol, VECTOR_ELT(dnx, 1));

  UNPROTECT(nprotect);
  return r;
}

// Find all 'frequent' itemsets using the Eclat (DFS) algorithm
// and transaction weights.

static int na, mi;
static int *p = NULL, *pz = NULL, *iz = NULL, *pr = NULL, *ir = NULL;
static double *sr = NULL;

static void cleanup(void) {
  free(pz); free(iz); free(p); free(pr); free(ir);
  free(sr);
  pz = iz = p = pr = ir = NULL;
  sr = NULL;
}

static int minINT(int x, int y) {
  return (x < y) ? x : y;
}

SEXP R_weclat_ngCMatrix(SEXP x, SEXP R_weight, SEXP R_support,
                        SEXP R_minlen, SEXP R_maxlen, SEXP R_verbose) {
  int i, j, k, f, l, n, nr, nc, ni, n1, nprotect = 0;
  int minlen, maxlen, *j0, *px, *ix;
  double *z0, *w, sup, z, s;
  SEXP r, r0, r1, dimx, pxS, ixS, j0S, z0S;
#ifdef _TIME_H
  clock_t t0 = clock(), t1, t2, t3;
  if (LOGICAL(R_verbose)[0] == TRUE)
    Rprintf("preparing ...");
#endif

  cleanup();
  if (!x || isNull(x) || !inherits(x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!R_weight || isNull(R_weight) || TYPEOF(R_weight) != REALSXP)
    error("'weight' not of type double");
  if (!R_support || isNull(R_support) || TYPEOF(R_support) != REALSXP)
    error("'support' not of type double");
  if (!R_minlen || (!isNull(R_minlen) && TYPEOF(R_minlen) != INTSXP))
    error("'minlen' not of class integer");
  if (!R_maxlen || (!isNull(R_maxlen) && TYPEOF(R_maxlen) != INTSXP))
    error("'maxlen' not of class integer");
  if (!R_verbose || isNull(R_verbose) || TYPEOF(R_verbose) != LGLSXP)
    error("'verbose' not of type logical");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nr = INTEGER(dimx)[0];
  nc = INTEGER(dimx)[1];

  PROTECT(pxS = getAttrib(x, install("p")));
  if (LENGTH(pxS) != nc + 1)
    error("p and Dim do not conform");
  px = INTEGER(pxS);

  PROTECT(ixS = getAttrib(x, install("i")));
  ix = INTEGER(ixS);
  UNPROTECT(3);
  
  if (LENGTH(R_weight) != nr)
    error("the number of rows of 'x' and the length of 'weight' do not conform");

  sup = REAL(R_support)[0];
  if (sup < 0 || sup > 1)
    error("'support invalid'");

  minlen = isNull(R_minlen) ? 1 : INTEGER(R_minlen)[0];
  if (minlen < 1)
    error("'minlen' invalid");
  maxlen = isNull(R_maxlen) ? 5 : INTEGER(R_maxlen)[0];
  if (maxlen < 1)
    error("'maxlen' invalid");
  if (minlen > maxlen)
    warning("'minlen' greater than 'maxlen'");

  if (nr == 0 || nc == 0 || nc < minlen) {
    PROTECT(r = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(r, 0, (r0 = NEW_OBJECT_OF_CLASS("ngCMatrix")));
    PROTECT(dimx = getAttrib(r0, install("Dim")));
    INTEGER(dimx)[0] = nc;
    SET_VECTOR_ELT(r, 1, allocVector(REALSXP, 0));
#ifdef _TIME_H
    if (LOGICAL(R_verbose)[0] == TRUE)
      Rprintf(" degenerate dimension(s)\n");
#endif
    UNPROTECT(2);
    return r;
  }

  if (maxlen > nc)
    maxlen = nc;

  PROTECT(j0S = allocVector(INTSXP, nc));
  j0 = INTEGER(j0S);
  PROTECT(z0S = allocVector(REALSXP, nc));
  z0 = REAL(z0S);
  UNPROTECT(2);
  
  w = REAL(R_weight);
  s = 0;
  for (k = 0; k < nr; k++)
    s += w[k];
  sup *= s;

  f = n1 = ni = 0;
  for (i = 1; i <= nc; i++) {
    l = px[i];
    z = 0;
    for (k = f; k < l; k++)
      z += w[ix[k]];
    z0[i - 1] = z;
    j0[i - 1] = i;
    if (z >= sup) {
      n1++;
      ni += l - f;
    }
    f = l;
  }
  R_qsort_I(z0, j0, 1, nc);

  n = minINT(n1, maxlen);

  na = 32;
  mi = 2;

  if (!(p  = malloc(sizeof(int) * (n + 1))) ||
      !(pz = malloc(sizeof(int) * (n + 1))) ||
      !(iz = malloc(sizeof(int) * ni)) ||
      !(sr = malloc(sizeof(double) * na)) ||
      !(pr = malloc(sizeof(int) * (na + 1))) ||
      !(ir = malloc(sizeof(int) * na * mi))) {
    cleanup();
    error("malloc failed");
  }
#ifdef _TIME_H
  t1 = clock();
  if (LOGICAL(R_verbose)[0] == TRUE) {
    Rprintf(" %i items, %i L1 [%.2fs]\n", nc, n1,
            ((double) t1 - t0) / CLOCKS_PER_SEC);
    Rprintf("mining ...");
  }
#endif
  p[0] = 0;
  p[1] = 1;
  pz[0] = 0;
  pr[0] = 0;

  n = ni = 0;
  j = 1;
  while (j > 0) {
    int kx, kz;

    k = p[j];
    z = z0[k - 1];
    if (z >= sup) {
      i = pz[j - 1];
      if (j > 1) {
        z = 0;
        k = j0[k - 1];
        for (kx = px[k - 1], kz = pz[j - 2];
             kx < px[k] && kz < pz[j - 1]; )
          if (iz[kz] < ix[kx])
            kz++;
          else {
            if (iz[kz] == ix[kx]) {
              iz[i++] = ix[kx];
              kz++;
              z += w[ix[kx]];
            }
            kx++;
          }
      } else if (k < nc) {
        k = j0[k - 1];
        for (kx = px[k - 1]; kx < px[k]; kx++)
          iz[i++] = ix[kx];
      }
      pz[j] = i;
    }

    if (j >= minlen && z >= sup) {
      if (na < n + 1) {
        na *= 2;
        if (!(sr = realloc(sr, sizeof(double) * na)) ||
            !(pr = realloc(pr, sizeof(int) * (na + 1))) ||
            !(ir = realloc(ir, sizeof(int) * na * mi))) {
          cleanup();
          error("realloc failed");
        }
      }
      sr[n++] = z / s;
      if (na * mi < ni + j) {
        mi += 1;
        if (!(ir = realloc(ir, sizeof(int) * na * mi))) {
          cleanup();
          error("realloc failed");
        }
      }
      f = ni;
      for (k = 1; k <= j; k++)
        ir[ni++] = j0[p[k] - 1] - 1;
      R_isort(ir + f, j);
      pr[n] = ni;
    }
#ifdef __DEBUG
    Rprintf(" %.3f %3i %3i %3i ", z, i, n, j);
    for (k = 1; k <= j; k++)
      Rprintf(" %i", p[k]);
    Rprintf("\n");
#endif
    if (p[j] < nc) {
      if (j < maxlen && z >= sup) {
        j++;
        p[j] = p[j - 1] + 1;
      } else
        p[j]++;
    } else {
      j--;
      p[j]++;
    }
    R_CheckUserInterrupt();
  }
  free(pz); free(iz); free(p);
  pz = iz = p = NULL;

#ifdef _TIME_H
  t2 = clock();
  if (LOGICAL(R_verbose)[0] == TRUE) {
    Rprintf(" %i transactions, %.2f used [%.2fs]\n", nr,
            (double) (n + ni + 1) / (na * (mi + 1)),
            ((double) t2 - t1) / CLOCKS_PER_SEC);
    Rprintf("writing ...");
  }
#endif
  pr = realloc(pr, sizeof(int) * (n + 1));
  ir = realloc(ir, sizeof(int) * ni);
  sr = realloc(sr, sizeof(double) * n);

  PROTECT(r = allocVector(VECSXP, 2));
  nprotect++;

  SET_VECTOR_ELT(r, 0, (r0 = NEW_OBJECT_OF_CLASS("ngCMatrix")));
  PROTECT(r1 = allocVector(INTSXP, n + 1));
  nprotect++;
  setAttrib(r0, install("p"), r1);
  R_chk_memcpy(INTEGER(r1), pr, sizeof(int) * (n + 1));
  free(pr); pr = NULL;

  PROTECT(r1 = allocVector(INTSXP, ni));
  nprotect++;
  setAttrib(r0, install("i"), r1);
  R_chk_memcpy(INTEGER(r1), ir, sizeof(int) * ni);
  free(ir); ir = NULL;

  PROTECT(dimx = getAttrib(r0, install("Dim")));
  nprotect++;
  INTEGER(dimx)[0] = nc;
  INTEGER(dimx)[1] = n;

  SET_VECTOR_ELT(r, 1, (r0 = allocVector(REALSXP, n)));
  R_chk_memcpy(REAL(r0), sr, sizeof(double) * n);
  free(sr); sr = NULL;

#ifdef _TIME_H
  t3 = clock();
  if (LOGICAL(R_verbose)[0] == TRUE)
    Rprintf(" %i itemsets [%.2fs]\n", n,
            ((double) t3 - t2) / CLOCKS_PER_SEC);
#endif

  UNPROTECT(nprotect);
  return r;
}

// Compute the weighted support of the given itemsets
// in the given transactions (in tidlist format).

SEXP R_wcount_ngCMatrix(SEXP x, SEXP t, SEXP R_weight,
                        SEXP R_fun, SEXP R_args, SEXP R_verbose) {
  int i, i0, j, k, kt, kz, f, f0, l, l0, m, n, nr, nc, ni, nj, nprotect = 0;
  int *ix, *pt, *it, *pz, *iz, *j0;
  double z, *w;
  SEXP r, r0, px, fun = R_fun, dimt, dimx, iS, ptS, itS, pzS, izS, j0S;
#ifdef _TIME_H
  time_t t0 = clock(), t1, t2;
  if (LOGICAL(R_verbose)[0] == TRUE)
    Rprintf("preparing ...");
#endif

  if (!x || isNull(x) || !inherits(x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!t || isNull(t) || !inherits(t, "ngCMatrix"))
    error("'t' not of class ngCMatrix");
  if (!R_weight || isNull(R_weight) || TYPEOF(R_weight) != REALSXP)
    error("'weight' not of storage type real");
  if (!R_fun || (!isNull(R_fun) && !isFunction(R_fun)))
    error("'FUN' not of mode function");
  if (!R_args || (!isNull(R_args) && TYPEOF(R_args) != VECSXP))
    error("'ARGS' not of storage type list");
  if (!R_verbose || isNull(R_verbose) || TYPEOF(R_verbose) != LGLSXP)
    error("'verbose' not of storage type logical");

  PROTECT(dimt = getAttrib(t, install("Dim")));
  nr = INTEGER(dimt)[0];
  nc = INTEGER(dimt)[1];

  PROTECT(dimx = getAttrib(x, install("Dim")));
  if (INTEGER(dimx)[0] != nc)
    error("the number of rows of 'x' and columns of 't' do not conform");
  if (LENGTH(R_weight) != nr)
    error("the number of rows of 't' and the length of 'weight' do not conform");
  UNPROTECT(2);
  
  if (isNull(R_fun) && !isNull(R_args))
    error("'ARGS' without 'FUN'");

  n = INTEGER(dimx)[1];
  if (n == 0 || nr == 0 || nc == 0) {
    PROTECT(r = allocVector(REALSXP, n));
    for (k = 0; k < n; k++)
      REAL(r)[k] = 0;
    PROTECT(r0 = getAttrib(x, install("Dimnames")));
    setAttrib(r, R_NamesSymbol, VECTOR_ELT(r0, 1));
#ifdef _TIME_H
    if (LOGICAL(R_verbose)[0] == TRUE)
      Rprintf(" degenerate dimension(s)\n");
#endif
    UNPROTECT(2);
    return r;
  }

  r = x;
  PROTECT(x = allocS4Object());
  nprotect++;
  copyMostAttrib(r, x);
  PROTECT(iS = duplicate(getAttrib(r, install("i"))));
  nprotect++;
  setAttrib(x, install("i"), iS);

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(r0 = getAttrib(x, install("i")));
  nprotect++;
  ni = LENGTH(r0);
  ix = INTEGER(r0);

  PROTECT(ptS = getAttrib(t, install("p")));
  nprotect++;
  pt = INTEGER(ptS);
  PROTECT(itS = getAttrib(t, install("i")));
  nprotect++;
  n = LENGTH(itS);
  it = INTEGER(itS);

  PROTECT(pzS = allocVector(INTSXP, nc + 1));
  nprotect++;
  pz = INTEGER(pzS);
  PROTECT(izS = allocVector(INTSXP, (ni > n) ? ni : n));
  nprotect++;
  iz = INTEGER(izS);

  f = 0;
  for (i = 1; i <= nc; i++) {
    l = pt[i];
    pz[i - 1] = l - f;
    f = l;
  }

  PROTECT(j0S = allocVector(INTSXP, LENGTH(px) - 1));
  nprotect++;
  j0 = INTEGER(j0S);
  w = REAL(R_weight);

  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    if (l == f)
      continue;
    for (k = f; k < l; k++)
      iz[k] = pz[ix[k]];
    R_qsort_int_I(iz + f, ix + f, 1, l - f);
    j0[i - 1] = i;
    f = l;
  }

  PROTECT(r = LCONS(install(".Call"),
                    PROTECT(LCONS(PROTECT(mkString("R_pnindex")),
                                  PROTECT(LCONS(x,
                                                LCONS(R_NilValue,
                                                      LCONS(ScalarLogical(FALSE), R_NilValue))))))));
  r = eval(r, R_GlobalEnv);
  UNPROTECT(4);

  PROTECT(r = r);
  R_qsort_int_I(INTEGER(r), j0, 1, LENGTH(px) - 1);
  UNPROTECT(1);

  PROTECT(r = allocVector(REALSXP, LENGTH(px) - 1));
  nprotect++;

  if (!isNull(R_fun)) {
    PROTECT(fun = LCONS(R_fun, LCONS(R_NilValue, VectorToPairList(R_args))));
    nprotect++;
  }
#ifdef _TIME_H
  t1 = clock();
  if (LOGICAL(R_verbose)[0] == TRUE) {
    Rprintf(" %i itemsets [%.2f]\n", LENGTH(px) - 1,
            ((double) t1 - t0) / CLOCKS_PER_SEC);
    Rprintf("counting ...");
  }
#endif
  pz[0] = 0;

  f0 = l0 = 0;
  nj = 0;
  for (i = 1; i < LENGTH(px); i++) {
    i0 = j0[i - 1];
    f = INTEGER(px)[i0 - 1];
    l = INTEGER(px)[i0];
    j = 0;
    if (i > 1)
      while (j < minINT(l - f, l0 - f0) && ix[f + j] == ix[f0 + j])
        j++;
    n = pz[j];
    while (j < l - f) {
      m = n;
      k = ix[f + j];
      if (j > 0) {
        for (kt = pt[k], kz = pz[j - 1];
             kt < pt[k + 1] && kz < pz[j]; )
          if (iz[kz] < it[kt])
            kz++;
          else {
            if (iz[kz] == it[kt]) {
              iz[n++] = it[kt];
              kz++;
            }
            kt++;
          }
        nj++;
      } else {
        for (kt = pt[k]; kt < pt[k + 1]; kt++)
          iz[n++] = it[kt];
      }
      pz[++j] = n;
      if (n == m)
        break;
    }
    z = 0;
    if (j > 0) {
      if (!isNull(fun)) {
        SEXP ans, vals;
        n = pz[j] - pz[j - 1];
        PROTECT(vals = allocVector(REALSXP, n));
        SETCAR(CDR(fun), vals);
        n = 0;
        for (k = pz[j - 1]; k < pz[j]; k++)
          REAL(vals)[n++] = w[iz[k]];
        PROTECT(ans = eval(fun, R_GlobalEnv));
        if (!isNull(ans)) {
          if (LENGTH(ans) != 1)
            error("not a scalar return value");
          switch (TYPEOF(ans)) {
          case REALSXP:
            z = REAL(ans)[0];
            break;
          case INTSXP:
            z = (double) INTEGER(ans)[0];
            break;
          default:
            error("not a numeric return value");
          }
        }
        UNPROTECT(2);
      } else {
        for (k = pz[j - 1]; k < pz[j]; k++)
          z += w[iz[k]];
      }
    }
    REAL(r)[i0 - 1] = z;

    f0 = f;
    l0 = l;
    R_CheckUserInterrupt();
  }

  PROTECT(r0 = getAttrib(x, install("Dimnames")));
  nprotect++;
  setAttrib(r, R_NamesSymbol, VECTOR_ELT(r0, 1));
#ifdef _TIME_H
  t2 = clock();
  if (LOGICAL(R_verbose)[0] == TRUE)
    Rprintf(" %i transactions, %.2f joins [%.2fs]\n", nr,
            (double) nj / (ni - LENGTH(px) + 1),
            ((double) t2 - t1) / CLOCKS_PER_SEC);
#endif

  UNPROTECT(nprotect);
  return r;
}

// coerce a vector to real (double) and
// replace NA or NaN values with zero.

SEXP R_na_zero(SEXP x) {
  int k;
  Rboolean dup = FALSE;

  if (isNull(x))
    return x;
  if (TYPEOF(x) != REALSXP) {
    PROTECT(x = coerceVector(x, REALSXP));
    dup = TRUE;
  }
  for (k = 0; k < LENGTH(x); k++)
    if (ISNAN(REAL(x)[k])) {
      if (!dup) {
        PROTECT(x = duplicate(x));
        dup = TRUE;
      }
      REAL(x)[k] = 0;
    }
  if (dup)
    UNPROTECT(1);
  
  return x;
}
