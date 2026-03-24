#include <R.h>
#include <R_ext/Utils.h>
#include <Rdefines.h>
#include "newS4object.h"

/* arrayIndex.c */
SEXP _int_array_subscript(int, SEXP, const char *, const char *, SEXP,
                         Rboolean, SEXP);

/* sparse matrix matrix tools.
 *
 * ngCMatrix objects represent indicator matrices
 * in column sparse format.
 *
 * Version: 0.1-5
 *
 * ceeboo 2006, 2007, 2008, 2012
 *
 * Rewritten to consistently PROTECT attribute lookups that are
 * kept across allocations and to keep UNPROTECT counts aligned.
 */

/* only used in crosstab below */
SEXP R_transpose_ngCMatrix(SEXP x) {
  int i, k, l, f, nr, nprotect = 0;
  SEXP r, px, ix, pr, ir, dimx, dn_x, dn_names, dim_r, dn_r, names_r;

  if (!inherits(x, "ngCMatrix"))
    error("'x' not of class 'ngCMatrix'");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  nr = INTEGER(dimx)[0];

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;

  PROTECT(r = NEW_OBJECT_OF_CLASS("ngCMatrix"));
  nprotect++;

  PROTECT(pr = allocVector(INTSXP, nr + 1));
  nprotect++;
  setAttrib(r, install("p"), pr);

  PROTECT(ir = allocVector(INTSXP, LENGTH(ix)));
  nprotect++;
  setAttrib(r, install("i"), ir);

  R_chk_memset(INTEGER(pr), 0, sizeof(int) * (nr + 1));

  for (k = 0; k < LENGTH(ix); k++)
    INTEGER(pr)[INTEGER(ix)[k]]++;
  for (k = 1; k < LENGTH(pr); k++)
    INTEGER(pr)[k] += INTEGER(pr)[k - 1];

  l = LENGTH(ix) - 1;
  for (i = LENGTH(px) - 2; i > -1; i--) {
    f = INTEGER(px)[i] - 1;
    for (k = l; k > f; k--)
      INTEGER(ir)[--INTEGER(pr)[INTEGER(ix)[k]]] = i;
    l = f;
  }

  PROTECT(dim_r = allocVector(INTSXP, 2));
  nprotect++;
  INTEGER(dim_r)[0] = LENGTH(px) - 1;
  INTEGER(dim_r)[1] = nr;
  setAttrib(r, install("Dim"), dim_r);

  PROTECT(dn_r = allocVector(VECSXP, 2));
  nprotect++;
  PROTECT(dn_x = getAttrib(x, install("Dimnames")));
  nprotect++;
  SET_VECTOR_ELT(dn_r, 0, VECTOR_ELT(dn_x, 1));
  SET_VECTOR_ELT(dn_r, 1, VECTOR_ELT(dn_x, 0));
  setAttrib(r, install("Dimnames"), dn_r);

  PROTECT(dn_names = getAttrib(dn_x, R_NamesSymbol));
  nprotect++;
  if (!isNull(dn_names)) {
    PROTECT(names_r = allocVector(STRSXP, 2));
    nprotect++;
    SET_STRING_ELT(names_r, 0, STRING_ELT(dn_names, 1));
    SET_STRING_ELT(names_r, 1, STRING_ELT(dn_names, 0));
    setAttrib(dn_r, R_NamesSymbol, names_r);
  }

  UNPROTECT(nprotect);
  return r;
}

/* crossprod in package Matrix performs logical
 * AND. we need a table of counts in full storage
 * representation.
 *
 * if argument y holds R_NilValue computes the auto
 * crosstab of x. if option t holds FALSE computes
 * the equivalent of tcrossprod.
 */

SEXP R_crosstab_ngCMatrix(SEXP x, SEXP y, SEXP t) {
  int i, j, fx, lx, fy, ly, kx, ky, ki, kj, nr, nc, s = 1, nprotect = 0;
  SEXP r, px, ix, py, iy, d1, d2, n1, n2, dimx, dimy, dn_r, names_r;

  if (!inherits(x, "ngCMatrix"))
    error("'x' not of class 'ngCMatrix'");
  if (TYPEOF(t) != LGLSXP)
    error("'t' not of storage class logical");

  if (LOGICAL(t)[0] == FALSE) {
    PROTECT(x = R_transpose_ngCMatrix(x));
    nprotect++;
  }

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  nr = nc = INTEGER(dimx)[0];

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;
  py = px;
  iy = ix;

  PROTECT(d1 = getAttrib(x, install("Dimnames")));
  nprotect++;
  PROTECT(n1 = getAttrib(d1, R_NamesSymbol));
  nprotect++;
  d1 = VECTOR_ELT(d1, 0);

  if (isNull(y)) {
    y = x;
    n2 = n1;
    d2 = d1;
  } else {
    if (!inherits(y, "ngCMatrix"))
      error("'y' not of class 'ngCMatrix'");

    if (LOGICAL(t)[0] == FALSE) {
      PROTECT(y = R_transpose_ngCMatrix(y));
      nprotect++;
    }

    PROTECT(dimy = getAttrib(y, install("Dim")));
    nprotect++;
    if (INTEGER(dimx)[1] != INTEGER(dimy)[1]) {
      if (LOGICAL(t)[0] == FALSE)
        error("the number of rows of 'x' and 'y' do not conform");
      else
        error("the number of columns of 'x' and 'y' do not conform");
    }

    nc = INTEGER(dimy)[0];

    PROTECT(py = getAttrib(y, install("p")));
    nprotect++;
    PROTECT(iy = getAttrib(y, install("i")));
    nprotect++;

    PROTECT(d2 = getAttrib(y, install("Dimnames")));
    nprotect++;
    PROTECT(n2 = getAttrib(d2, R_NamesSymbol));
    nprotect++;
    d2 = VECTOR_ELT(d2, 0);

    s = 0;
  }

  PROTECT(r = allocMatrix(INTSXP, nr, nc));
  nprotect++;
  R_chk_memset(INTEGER(r), 0, sizeof(int) * nr * nc);

  fx = fy = 0;
  for (i = 1; i < LENGTH(px); i++) {
    lx = INTEGER(px)[i];
    ly = (s) ? lx : INTEGER(py)[i];
    for (kx = fx; kx < lx; kx++) {
      ki = INTEGER(ix)[kx];
      for (ky = (s) ? kx : fy; ky < ly; ky++) {
        kj = INTEGER(iy)[ky];
        INTEGER(r)[ki + kj * nr]++;
      }
    }
    fx = lx;
    fy = ly;
    R_CheckUserInterrupt();
  }

  if (s) {
    for (i = 0; i < nr - 1; i++)
      for (j = i + 1; j < nr; j++)
        INTEGER(r)[j + i * nr] = INTEGER(r)[i + j * nr];
  }

  if (!isNull(d1) || !isNull(d2)) {
    PROTECT(dn_r = allocVector(VECSXP, 2));
    nprotect++;
    SET_VECTOR_ELT(dn_r, 0, d1);
    SET_VECTOR_ELT(dn_r, 1, d2);
    setAttrib(r, R_DimNamesSymbol, dn_r);

    if (!isNull(n1) || !isNull(n2)) {
      PROTECT(names_r = allocVector(STRSXP, 2));
      nprotect++;
      SET_STRING_ELT(names_r, 0,
        (isNull(n1)) ? R_BlankString : STRING_ELT(n1, 0));
      SET_STRING_ELT(names_r, 1,
        (isNull(n2)) ? R_BlankString : STRING_ELT(n2, 0));
      setAttrib(dn_r, R_NamesSymbol, names_r);
    }
  }

  UNPROTECT(nprotect);
  return r;
}

/* DEPRECATED (8/20/24): Only used for arulesSequences */
SEXP R_rowSums_ngCMatrix(SEXP x) {
  int k, nr, nprotect = 0;
  SEXP r, ix, dimx, dn_x;

  if (!inherits(x, "ngCMatrix"))
    error("'x' not of class 'ngCMatrix'");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  nr = INTEGER(dimx)[0];

  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;

  PROTECT(r = allocVector(INTSXP, nr));
  nprotect++;
  R_chk_memset(INTEGER(r), 0, sizeof(int) * nr);

  for (k = 0; k < LENGTH(ix); k++)
    INTEGER(r)[INTEGER(ix)[k]]++;

  PROTECT(dn_x = getAttrib(x, install("Dimnames")));
  nprotect++;
  setAttrib(r, R_NamesSymbol, VECTOR_ELT(dn_x, 0));

  UNPROTECT(nprotect);
  return r;
}

/* DEPRECATED (8/20/24): Only used for arulesSequences */
SEXP R_colSums_ngCMatrix(SEXP x) {
  int k, f, l, nprotect = 0;
  SEXP r, px, dn_x;

  if (!inherits(x, "ngCMatrix") && !inherits(x, "sgCMatrix"))
    error("'x' not of class 'ngCMatrix'");

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(r = allocVector(INTSXP, LENGTH(px) - 1));
  nprotect++;

  f = 0;
  for (k = 1; k < LENGTH(px); k++) {
    l = INTEGER(px)[k];
    INTEGER(r)[k - 1] = l - f;
    f = l;
  }

  PROTECT(dn_x = getAttrib(x, install("Dimnames")));
  nprotect++;
  setAttrib(r, R_NamesSymbol, VECTOR_ELT(dn_x, 1));

  UNPROTECT(nprotect);
  return r;
}

/* DEPRECATED (8/20/24): Only used for arulesSequences */
SEXP R_colSubset_ngCMatrix(SEXP x, SEXP s) {
  int i, j, k, n, nprotect = 0;
  SEXP r, dx, px, ix, pr, ir, dimx, dnames_x, names_x, colnames_x;

  if (!inherits(x, "ngCMatrix") && !inherits(x, "sgCMatrix"))
    error("'x' not of class 'ngCMatrix'");

  PROTECT(dx = getAttrib(x, install("Dimnames")));
  nprotect++;
#ifdef _COMPAT_
  r = CONS(dx, ATTRIB(x));
  SET_TAG(r, R_DimNamesSymbol);
  /* NOTE that we temporarily change a read-only object.
   *      this is safe as long as the object cannot be
   *      accessed concurrently.
   */
  SET_ATTRIB(x, r);

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  PROTECT(s = arraySubscript(1, s, dimx, getAttrib, (STRING_ELT), x));
  nprotect++;

  SET_ATTRIB(x, CDR(r));
#else
  PROTECT(s = _int_array_subscript(1, s, "Dim", "Dimnames", x, TRUE, R_NilValue));
  nprotect++;
#endif

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;

  n = 0;
  for (i = 0; i < LENGTH(s); i++) {
    j = INTEGER(s)[i];
    if (j == NA_INTEGER)
      error("invalid subscript(s)");
    n += (INTEGER(px)[j] - INTEGER(px)[j - 1]);
  }

  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;

  PROTECT(r = NEW_OBJECT_OF_CLASS(inherits(x, "ngCMatrix") ? "ngCMatrix" : "sgCMatrix"));
  nprotect++;

  PROTECT(pr = allocVector(INTSXP, LENGTH(s) + 1));
  nprotect++;
  setAttrib(r, install("p"), pr);

  PROTECT(ir = allocVector(INTSXP, n));
  nprotect++;
  setAttrib(r, install("i"), ir);

  n = INTEGER(pr)[0] = 0;
  for (i = 0; i < LENGTH(s); i++) {
    j = INTEGER(s)[i];
    for (k = INTEGER(px)[j - 1]; k < INTEGER(px)[j]; k++)
      INTEGER(ir)[n++] = INTEGER(ix)[k];
    INTEGER(pr)[i + 1] = n;
  }

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  PROTECT(ir = allocVector(INTSXP, 2));
  nprotect++;
  INTEGER(ir)[0] = INTEGER(dimx)[0];
  INTEGER(ir)[1] = LENGTH(s);
  setAttrib(r, install("Dim"), ir);

  PROTECT(dnames_x = getAttrib(x, install("Dimnames")));
  nprotect++;
  PROTECT(names_x = getAttrib(dnames_x, R_NamesSymbol));
  nprotect++;
  colnames_x = VECTOR_ELT(dnames_x, 1);

  if (isNull(colnames_x)) {
    setAttrib(r, install("Dimnames"), dnames_x);
  } else {
    SEXP dn_r, subset_names;

    PROTECT(dn_r = allocVector(VECSXP, 2));
    nprotect++;
    setAttrib(r, install("Dimnames"), dn_r);
    setAttrib(dn_r, R_NamesSymbol, names_x);
    SET_VECTOR_ELT(dn_r, 0, VECTOR_ELT(dnames_x, 0));

    if (LENGTH(s) > 0) {
      PROTECT(subset_names = allocVector(STRSXP, LENGTH(s)));
      nprotect++;
      for (i = 0; i < LENGTH(s); i++)
        SET_STRING_ELT(subset_names, i, STRING_ELT(colnames_x, INTEGER(s)[i] - 1));
      SET_VECTOR_ELT(dn_r, 1, subset_names);
    } else {
      SET_VECTOR_ELT(dn_r, 1, R_NilValue);
    }
  }

  UNPROTECT(nprotect);
  return r;
}

/*
 R's subset functionality is a misnomer as it
 allows many-to-many mappings. for special cases
 such as reordering of rows and one-to-one mappings
 there exist more efficient solutions (see below).

 as performing a many-to-many mapping for each
 column is inefficient we use transposition and
 column subsetting.
 */

/* DEPRECATED (8/20/24): Only used for arulesSequences */
SEXP R_rowSubset_ngCMatrix(SEXP x, SEXP s) {
  int nprotect = 0;

  PROTECT(x = R_transpose_ngCMatrix(x));
  nprotect++;
  PROTECT(x = R_colSubset_ngCMatrix(x, s));
  nprotect++;
  PROTECT(x = R_transpose_ngCMatrix(x));
  nprotect++;

  UNPROTECT(nprotect);
  return x;
}

/*
 expand into a list. the default behavior is
 to shift the internal codes to R indexes.

 note that CHARSXP type is internal so we need
 not provide a decoder for it.
 */

SEXP R_asList_ngCMatrix(SEXP x, SEXP d) {
  int i, j, k, f, l, n, m, nprotect = 0;
  SEXP r, px, ix, t, dimx, dn_x;

  if (!inherits(x, "ngCMatrix") && !inherits(x, "sgCMatrix"))
    error("'x' not of class 'ngCMatrix'");
  if (!isNull(d) && (TYPEOF(d) != LGLSXP  &&
      TYPEOF(d) != INTSXP  &&
      TYPEOF(d) != REALSXP &&
      TYPEOF(d) != STRSXP  &&
      TYPEOF(d) != VECSXP))
    error("'d' storage type not supported");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  if (!isNull(d) && (LENGTH(d) != INTEGER(dimx)[0]))
    error("'d' length does not conform");

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;

  PROTECT(r = allocVector(VECSXP, LENGTH(px) - 1));
  nprotect++;

  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l - f;
    PROTECT(t = allocVector((isNull(d)) ? INTSXP : TYPEOF(d), n));
    nprotect++;
    SET_VECTOR_ELT(r, i - 1, t);
    for (k = f, m = 0; k < l; k++, m++) {
      j = INTEGER(ix)[k];
      switch (TYPEOF(d)) {
      case LGLSXP:
        LOGICAL(t)[m] = LOGICAL(d)[j];
        break;
      case INTSXP:
        INTEGER(t)[m] = INTEGER(d)[j];
        break;
      case REALSXP:
        REAL(t)[m] = REAL(d)[j];
        break;
      case STRSXP:
        SET_STRING_ELT(t, m, STRING_ELT(d, j));
        break;
      case VECSXP:
        SET_VECTOR_ELT(t, m, VECTOR_ELT(d, j));
        break;
      default:
        INTEGER(t)[m] = j + 1;
      }
    }
    f = l;
  }

  PROTECT(dn_x = getAttrib(x, install("Dimnames")));
  nprotect++;
  setAttrib(r, R_NamesSymbol, VECTOR_ELT(dn_x, 1));

  UNPROTECT(nprotect);
  return r;
}

/*
 for each row of x append the corresponding
 row of y. thus, the number of rows must
 conform.
 */

SEXP R_cbind_ngCMatrix(SEXP x, SEXP y) {
  int i, k, n, nr, nprotect = 0;
  SEXP r, pr, ir, px, ix, sx, py, iy, sy;
  SEXP dimx, dimy, dnx, dny, namesx, namesy, dn_r, s_names;

  if (!inherits(x, "ngCMatrix") && !inherits(x, "sgCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!inherits(y, "ngCMatrix") && !inherits(y, "sgCMatrix"))
    error("'y' not of class ngCMatrix");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  PROTECT(dimy = getAttrib(y, install("Dim")));
  nprotect++;
  nr = INTEGER(dimx)[0];
  if (nr != INTEGER(dimy)[0])
    error("the number of rows of 'x' and 'y' do not conform");

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(py = getAttrib(y, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;
  PROTECT(iy = getAttrib(y, install("i")));
  nprotect++;

  PROTECT(r = NEW_OBJECT_OF_CLASS(inherits(x, "ngCMatrix") ? "ngCMatrix" : "sgCMatrix"));
  nprotect++;

  PROTECT(pr = allocVector(INTSXP, LENGTH(px) + LENGTH(py) - 1));
  nprotect++;
  setAttrib(r, install("p"), pr);

  PROTECT(ir = allocVector(INTSXP, LENGTH(ix) + LENGTH(iy)));
  nprotect++;
  setAttrib(r, install("i"), ir);

  R_chk_memcpy(INTEGER(pr), INTEGER(px), sizeof(int) * LENGTH(px));
  n = LENGTH(px);
  k = INTEGER(px)[n - 1];
  for (i = 1; i < LENGTH(py); i++)
    INTEGER(pr)[n++] = INTEGER(py)[i] + k;

  R_chk_memcpy(INTEGER(ir), INTEGER(ix), sizeof(int) * LENGTH(ix));
  R_chk_memcpy(INTEGER(ir) + LENGTH(ix), INTEGER(iy), sizeof(int) * LENGTH(iy));

  PROTECT(ir = allocVector(INTSXP, 2));
  nprotect++;
  INTEGER(ir)[0] = nr;
  INTEGER(ir)[1] = LENGTH(pr) - 1;
  setAttrib(r, install("Dim"), ir);

  PROTECT(dn_r = allocVector(VECSXP, 2));
  nprotect++;
  setAttrib(r, install("Dimnames"), dn_r);

  PROTECT(dnx = getAttrib(x, install("Dimnames")));
  nprotect++;
  PROTECT(dny = getAttrib(y, install("Dimnames")));
  nprotect++;

  sx = VECTOR_ELT(dnx, 0);
  sy = VECTOR_ELT(dny, 0);
  if (isNull(sx))
    SET_VECTOR_ELT(dn_r, 0, sy);
  else
    SET_VECTOR_ELT(dn_r, 0, sx);

  sx = VECTOR_ELT(dnx, 1);
  sy = VECTOR_ELT(dny, 1);
  if (isNull(sx) && isNull(sy)) {
    SET_VECTOR_ELT(dn_r, 1, sx);
  } else {
    PROTECT(s_names = allocVector(STRSXP, LENGTH(pr) - 1));
    nprotect++;
    SET_VECTOR_ELT(dn_r, 1, s_names);
    if (isNull(sx))
      for (k = 0; k < LENGTH(px) - 1; k++)
        SET_STRING_ELT(s_names, k, R_BlankString);
    else
      for (k = 0; k < LENGTH(px) - 1; k++)
        SET_STRING_ELT(s_names, k, STRING_ELT(sx, k));
    n = k;
    if (isNull(sy))
      for (k = 0; k < LENGTH(py) - 1; k++)
        SET_STRING_ELT(s_names, k + n, R_BlankString);
    else
      for (k = 0; k < LENGTH(py) - 1; k++)
        SET_STRING_ELT(s_names, k + n, STRING_ELT(sy, k));
  }

  PROTECT(namesx = getAttrib(dnx, R_NamesSymbol));
  nprotect++;
  PROTECT(namesy = getAttrib(dny, R_NamesSymbol));
  nprotect++;
  if (isNull(namesx))
    setAttrib(dn_r, R_NamesSymbol, namesy);
  else
    setAttrib(dn_r, R_NamesSymbol, namesx);

  UNPROTECT(nprotect);
  return r;
}

/*
 for row reordering this is more efficient
 than subsetting. note that the number of
 rows is allowed to increase.
 */

SEXP R_recode_ngCMatrix(SEXP x, SEXP s) {
  int i, k, f, l, c, nr, nprotect = 0;
  SEXP r, px, ix, ir, dimx, dnx, rownames_x, names_x, rownames_r;

  if (!inherits(x, "ngCMatrix") && !inherits(x, "sgCMatrix"))
    error("'x' not of class ngCMatrix");
  if (TYPEOF(s) != INTSXP)
    error("'s' not of storage type integer");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  nr = INTEGER(dimx)[0];
  if (nr != LENGTH(s))
    error("the number of rows of 'x' and the lenght of 's' do not conform");

  PROTECT(r = duplicate(s));
  nprotect++;
  R_isort(INTEGER(r), LENGTH(r));

  nr = 0;
  for (i = 0; i < LENGTH(r); i++) {
    if ((l = INTEGER(r)[i]) <= nr)
      error("invalid index");
    nr = l;
  }
  if (nr == NA_INTEGER)
    error("invalid index");

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;

  c = inherits(x, "ngCMatrix");

  PROTECT(r = NEW_OBJECT_OF_CLASS(c ? "ngCMatrix" : "sgCMatrix"));
  nprotect++;
  setAttrib(r, install("p"), px);

  PROTECT(ir = allocVector(INTSXP, LENGTH(ix)));
  nprotect++;
  setAttrib(r, install("i"), ir);

  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    if (f == l)
      continue;
    for (k = f; k < l; k++)
      INTEGER(ir)[k] = INTEGER(s)[INTEGER(ix)[k]] - 1;
    if (c)
      R_isort(INTEGER(ir) + f, l - f);
    f = l;
  }

  PROTECT(dimx = allocVector(INTSXP, 2));
  nprotect++;
  INTEGER(dimx)[0] = nr;
  INTEGER(dimx)[1] = LENGTH(px) - 1;
  setAttrib(r, install("Dim"), dimx);

  PROTECT(dnx = getAttrib(x, install("Dimnames")));
  nprotect++;
  PROTECT(names_x = getAttrib(dnx, R_NamesSymbol));
  nprotect++;
  PROTECT(dimx = allocVector(VECSXP, 2));
  nprotect++;
  setAttrib(r, install("Dimnames"), dimx);

  rownames_x = VECTOR_ELT(dnx, 0);
  if (isNull(rownames_x)) {
    SET_VECTOR_ELT(dimx, 0, rownames_x);
  } else {
    PROTECT(rownames_r = allocVector(STRSXP, nr));
    nprotect++;
    for (k = 0; k < nr; k++)
      SET_STRING_ELT(rownames_r, k, R_BlankString);
    for (k = 0; k < LENGTH(s); k++)
      SET_STRING_ELT(rownames_r, INTEGER(s)[k] - 1, STRING_ELT(rownames_x, k));
    SET_VECTOR_ELT(dimx, 0, rownames_r);
  }
  SET_VECTOR_ELT(dimx, 1, VECTOR_ELT(dnx, 1));
  setAttrib(dimx, R_NamesSymbol, names_x);

  UNPROTECT(nprotect);
  return r;
}

/*
 fast but temporary memory consumption
 may amount to full-storage representation.
 */

/* DEPRECATED (8/20/24): Only used for arulesSequences */
SEXP R_or_ngCMatrix(SEXP x, SEXP y) {
  int i, kx, ky, lx, ly, n, nr, nprotect = 0;
  SEXP r, pr, ir, px, ix, py, iy;
  SEXP dimx, dimy, dnx, dny, rownames_x, colnames_x, names_x, names_y, tmp_i;

  if (!inherits(x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!inherits(y, "ngCMatrix"))
    error("'y' not of class ngCMatrix");

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  PROTECT(dimy = getAttrib(y, install("Dim")));
  nprotect++;
  if (INTEGER(dimx)[1] != INTEGER(dimy)[1])
    error("the number of columns of 'x' and 'y' do not conform");

  nr = INTEGER(dimx)[0];
  if (nr != INTEGER(dimy)[0])
    error("the number of rows of 'x' and 'y' do not conform");

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;
  PROTECT(py = getAttrib(y, install("p")));
  nprotect++;
  PROTECT(iy = getAttrib(y, install("i")));
  nprotect++;

  PROTECT(r = NEW_OBJECT_OF_CLASS("ngCMatrix"));
  nprotect++;

  PROTECT(pr = allocVector(INTSXP, LENGTH(px)));
  nprotect++;
  setAttrib(r, install("p"), pr);

  n = LENGTH(ix) + LENGTH(iy);
  if (n > (i = nr * (LENGTH(px) - 1)))
    n = i;

  PROTECT(ir = allocVector(INTSXP, n));
  nprotect++;
  setAttrib(r, install("i"), ir);

  n = kx = ky = INTEGER(pr)[0] = 0;
  for (i = 1; i < LENGTH(px); i++) {
    lx = INTEGER(px)[i];
    ly = INTEGER(py)[i];
    while (kx < lx && ky < ly) {
      if (INTEGER(ix)[kx] > INTEGER(iy)[ky])
        INTEGER(ir)[n++] = INTEGER(iy)[ky++];
      else {
        if (INTEGER(ix)[kx] == INTEGER(iy)[ky])
          ky++;
        INTEGER(ir)[n++] = INTEGER(ix)[kx++];
      }
    }
    while (kx < lx)
      INTEGER(ir)[n++] = INTEGER(ix)[kx++];
    while (ky < ly)
      INTEGER(ir)[n++] = INTEGER(iy)[ky++];
    INTEGER(pr)[i] = n;
  }

  if (n < LENGTH(ir)) {
    PROTECT(tmp_i = allocVector(INTSXP, n));
    nprotect++;
    R_chk_memcpy(INTEGER(tmp_i), INTEGER(ir), sizeof(int) * n);
    setAttrib(r, install("i"), tmp_i);
    ir = tmp_i;
  }

  PROTECT(dimx = allocVector(INTSXP, 2));
  nprotect++;
  INTEGER(dimx)[0] = nr;
  INTEGER(dimx)[1] = LENGTH(px) - 1;
  setAttrib(r, install("Dim"), dimx);

  PROTECT(dimy = allocVector(VECSXP, 2));
  nprotect++;
  setAttrib(r, install("Dimnames"), dimy);

  PROTECT(dnx = getAttrib(x, install("Dimnames")));
  nprotect++;
  PROTECT(dny = getAttrib(y, install("Dimnames")));
  nprotect++;

  rownames_x = VECTOR_ELT(dnx, 0);
  colnames_x = VECTOR_ELT(dnx, 1);
  if (isNull(rownames_x))
    SET_VECTOR_ELT(dimy, 0, VECTOR_ELT(dny, 0));
  else
    SET_VECTOR_ELT(dimy, 0, rownames_x);

  if (isNull(colnames_x))
    SET_VECTOR_ELT(dimy, 1, VECTOR_ELT(dny, 1));
  else
    SET_VECTOR_ELT(dimy, 1, colnames_x);

  PROTECT(names_x = getAttrib(dnx, R_NamesSymbol));
  nprotect++;
  PROTECT(names_y = getAttrib(dny, R_NamesSymbol));
  nprotect++;
  if (isNull(names_x))
    setAttrib(dimy, R_NamesSymbol, names_y);
  else
    setAttrib(dimy, R_NamesSymbol, names_x);

  UNPROTECT(nprotect);
  return r;
}

/*
 check if the internal represention is compatible
 with the implementations above.
 */

SEXP R_valid_ngCMatrix(SEXP x) {
  int i, k, f, l, n, m, nprotect = 0;
  SEXP px, ix, dx, dn_x, dn0, dn1;

  if (!inherits(x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;
  PROTECT(dx = getAttrib(x, install("Dim")));
  nprotect++;

  if (isNull(px) || isNull(ix) || isNull(dx)) {
    UNPROTECT(nprotect);
    return mkString("slot p, i, or Dim is NULL");
  }

  if (TYPEOF(px) != INTSXP || TYPEOF(ix) != INTSXP || TYPEOF(dx) != INTSXP) {
    UNPROTECT(nprotect);
    return mkString("slot p, i, or Dim not of storage type integer");
  }

  if (LENGTH(dx) != 2 || INTEGER(dx)[0] < 0 || INTEGER(dx)[1] < 0) {
    UNPROTECT(nprotect);
    return mkString("slot Dim invalid");
  }

  if (INTEGER(dx)[1] != LENGTH(px) - 1) {
    UNPROTECT(nprotect);
    return mkString("slot p and Dim do not conform");
  }

  f = l = INTEGER(px)[0];
  if (f != 0) {
    UNPROTECT(nprotect);
    return mkString("slot p invalid");
  }

  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    if (l < f) {
      UNPROTECT(nprotect);
      return mkString("slot p invalid");
    }
    f = l;
  }
  if (l != LENGTH(ix)) {
    UNPROTECT(nprotect);
    return mkString("slot p and i do not conform");
  }

  if (l > 0) {
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
      l = INTEGER(px)[i];
      n = -1;
      for (k = f; k < l; k++) {
        m = INTEGER(ix)[k];
        if (m <= n) {
          UNPROTECT(nprotect);
          return mkString("slot i invalid");
        }
        n = m;
      }
      if (n >= INTEGER(dx)[0]) {
        UNPROTECT(nprotect);
        return mkString("slot i invalid");
      }
      f = l;
    }
  }

  PROTECT(dn_x = getAttrib(x, install("Dimnames")));
  nprotect++;
  if (LENGTH(dn_x) != 2 || TYPEOF(dn_x) != VECSXP) {
    UNPROTECT(nprotect);
    return mkString("slot Dimnames invalid");
  }

  dn0 = VECTOR_ELT(dn_x, 0);
  if (!isNull(dn0)) {
    if (TYPEOF(dn0) != STRSXP) {
      UNPROTECT(nprotect);
      return mkString("slot Dimnames invalid");
    }
    if (LENGTH(dn0) != INTEGER(dx)[0]) {
      UNPROTECT(nprotect);
      return mkString("slot Dim and Dimnames do not conform");
    }
  }

  dn1 = VECTOR_ELT(dn_x, 1);
  if (!isNull(dn1)) {
    if (TYPEOF(dn1) != STRSXP) {
      UNPROTECT(nprotect);
      return mkString("slot Dimnames invalid");
    }
    if (LENGTH(dn1) != INTEGER(dx)[1]) {
      UNPROTECT(nprotect);
      return mkString("slot Dim and Dimnames do not conform");
    }
  }

  UNPROTECT(nprotect);
  return ScalarLogical(TRUE);
}

