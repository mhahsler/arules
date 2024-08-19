#include <R.h>
#include <R_ext/Utils.h>
#include <Rdefines.h>
#include "newS4object.h"

/* sparse matrix matrix tools.
 *
 * ngCMatrix objects represent indicator matrices
 * in column spares format.
 *
 * Version: 0.1-5
 *
 * ceeboo 2006, 2007, 2008, 2012
 */


/* used by crossprod */

SEXP int_transpose_ngCMatrix(SEXP x) {
  int i, k, l, f, nr;
  SEXP r, px, ix, pr, ir;
  
  if (!inherits(x, "ngCMatrix"))
    error("'x' not of class 'ngCMatrix'");
  
  nr = INTEGER(getAttrib(x, install("Dim")))[0];
  
  px = getAttrib(x, install("p"));
  ix = getAttrib(x, install("i"));
  
  /* use new-style S4 object */
  PROTECT(r = NEW_OBJECT_OF_CLASS("ngCMatrix"));
  
  setAttrib(r, install("p"), PROTECT(pr = allocVector(INTSXP, nr+1)));
  setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, LENGTH(ix))));
  UNPROTECT(2);
  
  memset(INTEGER(pr), 0, sizeof(int) * (nr+1));
  
  for (k = 0; k < LENGTH(ix); k++)
    INTEGER(pr)[INTEGER(ix)[k]]++;
  for (k = 1; k < LENGTH(pr); k++)
    INTEGER(pr)[k] += INTEGER(pr)[k-1];
  l = LENGTH(ix)-1;
  for (i = LENGTH(px)-2; i > -1; i--) {
    f = INTEGER(px)[i] - 1;
    for (k = l; k > f; k--)
      INTEGER(ir)[--INTEGER(pr)[INTEGER(ix)[k]]] = i;
    l = f;
  }
  
  setAttrib(r, install("Dim"), PROTECT(ir = allocVector(INTSXP, 2)));
  INTEGER(ir)[0] = LENGTH(px)-1;
  INTEGER(ir)[1] = nr;
  
  setAttrib(r, install("Dimnames"), PROTECT(ir = allocVector(VECSXP, 2)));
  ix = getAttrib(x, install("Dimnames"));
  SET_VECTOR_ELT(ir, 0, VECTOR_ELT(ix, 1));
  SET_VECTOR_ELT(ir, 1, VECTOR_ELT(ix, 0));
  
  PROTECT((ix = getAttrib(ix, R_NamesSymbol)));
  if (!isNull(ix)) {
    setAttrib(ir, R_NamesSymbol, PROTECT(pr = allocVector(STRSXP, 2)));
    SET_STRING_ELT(pr, 0, STRING_ELT(ix, 1));
    SET_STRING_ELT(pr, 1, STRING_ELT(ix, 0));
    UNPROTECT(1);
  }
  
  UNPROTECT(4);
  
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
  SEXP r, px, ix, py, iy, d1, d2, n1, n2;
  
  if (!inherits(x, "ngCMatrix"))
    error("'x' not of class 'ngCMatrix'");
  if (TYPEOF(t) != LGLSXP)
    error("'t' not of storage class logical");
  
  if (LOGICAL(t)[0] == FALSE) {
    PROTECT(x = int_transpose_ngCMatrix(x));
    nprotect++;
  }
  
  nr = nc = INTEGER(getAttrib(x, install("Dim")))[0];
  px = py = getAttrib(x, install("p"));
  ix = iy = getAttrib(x, install("i"));
  
  d1 = getAttrib(x, install("Dimnames"));
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
    
    if (LOGICAL(t)[0] == FALSE){
      PROTECT(y = int_transpose_ngCMatrix(y));
      nprotect++;
    }
    
    if (INTEGER(getAttrib(x, install("Dim")))[1] !=
        INTEGER(getAttrib(y, install("Dim")))[1]) {
      if (LOGICAL(t)[0] == FALSE) 
        error("the number of rows of 'x' and 'y' do not conform");
      else
        error("the number of columns of 'x' and 'y' do not conform");
    }
    
    nc = INTEGER(getAttrib(y, install("Dim")))[0];
    py = getAttrib(y, install("p"));
    iy = getAttrib(y, install("i"));
    
    d2 = getAttrib(y, install("Dimnames"));
    PROTECT(n2 = getAttrib(d2, R_NamesSymbol));
    nprotect++;
    d2 = VECTOR_ELT(d2, 0);
    
    s  = 0;
  }
  
  PROTECT(r = allocMatrix(INTSXP, nr, nc));
  nprotect++;
  memset(INTEGER(r), 0, sizeof(int) * nr * nc);
  
  fx = fy = 0;
  for (i = 1; i < LENGTH(px); i++) {
    lx = INTEGER(px)[i];
    ly = (s) ? lx : INTEGER(py)[i];
    for (kx = fx; kx < lx; kx++) {
      ki = INTEGER(ix)[kx];
      for (ky = (s) ? kx : fy; ky < ly; ky++) {
        kj = INTEGER(iy)[ky];
        INTEGER(r)[ki+kj*nr]++;
      }
    }
    fx = lx;
    fy = ly;
    R_CheckUserInterrupt();
  }
  if (s) {
    for (i = 0; i < nr-1; i++)
      for (j = i+1; j < nr; j++)
        INTEGER(r)[j+i*nr] = INTEGER(r)[i+j*nr];
  }
  
  if (!isNull(d1) || !isNull(d2)) {
    setAttrib(r, R_DimNamesSymbol, (ix = allocVector(VECSXP, 2)));
    SET_VECTOR_ELT(ix, 0, d1);
    SET_VECTOR_ELT(ix, 1, d2);
    if (!isNull(n1) || !isNull(n2)) {
      setAttrib(ix, R_NamesSymbol, (iy = allocVector(STRSXP, 2)));
      SET_STRING_ELT(iy, 0, (isNull(n1)) ? R_BlankString : STRING_ELT(n1, 0));
      SET_STRING_ELT(iy, 1, (isNull(n2)) ? R_BlankString : STRING_ELT(n2, 0));
    }
  }
  
  UNPROTECT(nprotect);
  
  return r;
}

/*
 expand into a list. the default behavior is
 to shift the internal codes to R indexes.
 
 note that CHARSXP type is internal so we need
 not provide a decoder for it.
 */

SEXP R_asList_ngCMatrix(SEXP x, SEXP d) {
  int i, j, k, f, l, n, m; 
  SEXP r, px, ix, t;
  
  if (!inherits(x, "ngCMatrix") && !inherits(x, "sgCMatrix"))
    error("'x' not of class 'ngCMatrix'");
  if (!isNull(d) && (TYPEOF(d) != LGLSXP  &&
      TYPEOF(d) != INTSXP  &&
      TYPEOF(d) != REALSXP &&
      TYPEOF(d) != STRSXP  &&
      TYPEOF(d) != VECSXP))
    error("'d' storage type not supported");
  if (!isNull(d) && (LENGTH(d) != INTEGER(getAttrib(x, install("Dim")))[0]))
    error("'d' length does not conform");
  
  px = getAttrib(x, install("p"));
  ix = getAttrib(x, install("i"));
  
  PROTECT(r = allocVector(VECSXP, LENGTH(px)-1));
  
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    n = l-f;
    SET_VECTOR_ELT(r, i-1, (t = allocVector((isNull(d))?INTSXP:TYPEOF(d), n)
    ));
    m = 0;
    for (k = f; k < l; k++) {
      j = INTEGER(ix)[k];
      switch (TYPEOF(d)) {
      case LGLSXP:
        LOGICAL(t)[m] = LOGICAL(d)[j];
        break;
      case INTSXP:
        INTEGER(t)[m] = INTEGER(d)[j];
        break;
      case REALSXP:
        REAL(t)[m]    = REAL(d)[j];
        break;
      case STRSXP:
        SET_STRING_ELT(t, m, STRING_ELT(d, j));
        break;
      case VECSXP:
        SET_VECTOR_ELT(t, m, VECTOR_ELT(d, j));
        break;
      default:
        INTEGER(t)[m] = j+1;
      }
      m++;
    }
    f = l;
  }
  setAttrib(r, R_NamesSymbol, VECTOR_ELT(getAttrib(x, install("Dimnames")), 1)
  );
  
  UNPROTECT(1);
  
  return r;
}

/*
 for each row of x append the corresponding
 row of y. thus, the number of rows must
 conform.
 */

SEXP R_cbind_ngCMatrix(SEXP x, SEXP y) {
  int i, k, n, nr;
  SEXP r, pr, ir, px, ix, sx, py, iy, sy;
  
  if (!inherits(x, "ngCMatrix") && !inherits(x, "sgCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!inherits(y, "ngCMatrix") && !inherits(y, "sgCMatrix"))
    error("'y' not of class ngCMatrix");
  
  nr = INTEGER(getAttrib(x, install("Dim")))[0];
  if (nr != INTEGER(getAttrib(y, install("Dim")))[0])
    error("the number of rows of 'x' and 'y' do not conform");
  
  px = getAttrib(x, install("p"));
  py = getAttrib(y, install("p"));
  
  ix = getAttrib(x, install("i"));
  iy = getAttrib(y, install("i"));
  
  PROTECT(r = NEW_OBJECT_OF_CLASS(inherits(x, "ngCMatrix") ? "ngCMatrix" : "sgCMatrix"));
  setAttrib(r, install("p"), PROTECT(pr = allocVector(INTSXP, LENGTH(px)+LENGTH(py)-1)));
  setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, LENGTH(ix)+LENGTH(iy))));
  
  memcpy(INTEGER(pr), INTEGER(px), sizeof(int) * LENGTH(px));
  n = LENGTH(px);
  k = INTEGER(px)[n-1];
  for (i = 1; i < LENGTH(py); i++)
    INTEGER(pr)[n++] = INTEGER(py)[i] + k;
  
  memcpy(INTEGER(ir), INTEGER(ix), sizeof(int) * LENGTH(ix));
  memcpy(INTEGER(ir)+LENGTH(ix), INTEGER(iy), sizeof(int) * LENGTH(iy));
  
  setAttrib(r, install("Dim"), PROTECT(ir = allocVector(INTSXP, 2)));
  INTEGER(ir)[0] = nr;
  INTEGER(ir)[1] = LENGTH(pr)-1;
  
  /* c.f. cbind Matrix */
  
  setAttrib(r, install("Dimnames"), PROTECT(ir = allocVector(VECSXP, 2)));
  
  ix = getAttrib(x, install("Dimnames"));
  iy = getAttrib(y, install("Dimnames"));
  
  if (isNull((sx = VECTOR_ELT(ix, 0))))
    SET_VECTOR_ELT(ir, 0, VECTOR_ELT(iy, 0));
  else
    SET_VECTOR_ELT(ir, 0, sx);
  
  sx = VECTOR_ELT(ix, 1);
  sy = VECTOR_ELT(iy, 1);
  
  if (isNull(sx) && isNull(sy))
    SET_VECTOR_ELT(ir, 1, sx);
  else {
    SEXP s;
    
    SET_VECTOR_ELT(ir, 1, PROTECT(s = allocVector(STRSXP, LENGTH(pr)-1)));
    UNPROTECT(1);
    if (isNull(sx))
      for (k = 0; k < LENGTH(px)-1; k++)
        SET_STRING_ELT(s, k, R_BlankString);
    else
      for (k = 0; k < LENGTH(px)-1; k++)
        SET_STRING_ELT(s, k, STRING_ELT(sx, k));
    n = k;
    if (isNull((sy)))
      for (k = 0; k < LENGTH(py)-1; k++)
        SET_STRING_ELT(s, k+n, R_BlankString);
    else
      for (k = 0; k < LENGTH(py)-1; k++)
        SET_STRING_ELT(s, k+n, STRING_ELT(sy, k));
  }
  
  if (isNull((ix = getAttrib(ix, R_NamesSymbol))))
    setAttrib(ir, R_NamesSymbol, getAttrib(iy, R_NamesSymbol));
  else
    setAttrib(ir, R_NamesSymbol, ix);
  
  UNPROTECT(5);
  
  return r;
}

/*
 for row reordering this is more efficient
 than subsetting. note that the number of
 rows is allowed to increase.
 */

SEXP R_recode_ngCMatrix(SEXP x, SEXP s) {
  int i, k, f, l, c, nr;
  SEXP r, px, ix, ir;
  
  if (!inherits(x, "ngCMatrix") && !inherits(x, "sgCMatrix"))
    error("'x' not of class ngCMatrix");
  if (TYPEOF(s) != INTSXP)
    error("'s' not of storage type integer");
  
  nr = INTEGER(getAttrib(x, install("Dim")))[0];
  if (nr != LENGTH(s))
    error("the number of rows of 'x' and the lenght of 's' do not conform");
  
  PROTECT(r = duplicate(s));
  R_isort(INTEGER(r), LENGTH(r));
  
  nr = 0;
  for (i = 0; i < LENGTH(r); i++) {
    if ((l = INTEGER(r)[i]) <= nr)
      error("invalid index");
    nr = l;
  }
  if (nr == NA_INTEGER)
    error("invalid index");
  
  UNPROTECT(1);
  
  px = getAttrib(x, install("p"));
  ix = getAttrib(x, install("i"));
  
  c  = inherits(x, "ngCMatrix");
  
  PROTECT(r = NEW_OBJECT_OF_CLASS(c ? "ngCMatrix" : "sgCMatrix"));
  setAttrib(r, install("p"), px);
  setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, LENGTH(ix))));
  UNPROTECT(1);
  
  f = 0;
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    if (f == l)
      continue;
    for (k = f; k < l; k++) 
      INTEGER(ir)[k] = INTEGER(s)[INTEGER(ix)[k]]-1;
    if (c)
      R_isort(INTEGER(ir)+f, l-f);
    f = l;
  }
  
  setAttrib(r, install("Dim"), PROTECT(ir = allocVector(INTSXP, 2)));
  UNPROTECT(1);
  INTEGER(ir)[0] = nr;
  INTEGER(ir)[1] = LENGTH(px)-1;
  
  setAttrib(r, install("Dimnames"), PROTECT(ir = allocVector(VECSXP, 2)));
  PROTECT(px = getAttrib(x, install("Dimnames")));
  if (isNull((ix = VECTOR_ELT(px, 0))))
    SET_VECTOR_ELT(ir, 0, ix);
  else {
    SEXP t;
    SET_VECTOR_ELT(ir, 0, PROTECT(t = allocVector(STRSXP, nr)));
    UNPROTECT(1);
    for (k = 0; k < nr; k++) 
      SET_STRING_ELT(t, k, R_BlankString);
    for (k = 0; k < LENGTH(s); k++)
      SET_STRING_ELT(t, INTEGER(s)[k]-1, STRING_ELT(ix, k));
  }
  SET_VECTOR_ELT(ir, 1, VECTOR_ELT(px, 1));
  setAttrib(ir, R_NamesSymbol, getAttrib(px, R_NamesSymbol));
  
  UNPROTECT(3);
  
  return r;
}

/*
 fast but temporary memory consumption 
 may amount to full-storage representation.
 */

SEXP R_or_ngCMatrix(SEXP x, SEXP y) {
  int i, kx, ky, lx, ly, n, nr;
  SEXP r, pr, ir, px, ix, py, iy;
  
  if (!inherits(x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!inherits(y, "ngCMatrix"))
    error("'y' not of class ngCMatrix");
  if (INTEGER(getAttrib(x, install("Dim")))[1] !=
      INTEGER(getAttrib(y, install("Dim")))[1])
    error("the number of columns of 'x' and 'y' do not conform");
  
  nr = INTEGER(getAttrib(x, install("Dim")))[0];
  if (nr != INTEGER(getAttrib(y, install("Dim")))[0])
    error("the number of rows of 'x' and 'y' do not conform");
  
  px = getAttrib(x, install("p"));
  ix = getAttrib(x, install("i"));
  
  py = getAttrib(y, install("p"));
  iy = getAttrib(y, install("i"));
  
  PROTECT(r = NEW_OBJECT_OF_CLASS("ngCMatrix"));
  setAttrib(r, install("p"), PROTECT(pr = allocVector(INTSXP, LENGTH(px))));
  
  n = LENGTH(ix) + LENGTH(iy);
  if (n > (i = nr * (LENGTH(px) - 1)))
    n = i;
  
  setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, n)));
  
  n = kx = ky = INTEGER(pr)[0] = 0;
  for (i = 1; i < LENGTH(px); i++) {
    lx = INTEGER(px)[i];
    ly = INTEGER(py)[i];
    while (kx < lx && ky < ly)
      if (INTEGER(ix)[kx] > INTEGER(iy)[ky])
        INTEGER(ir)[n++] = INTEGER(iy)[ky++];
      else {
        if (INTEGER(ix)[kx] == INTEGER(iy)[ky])
          ky++;
        INTEGER(ir)[n++] = INTEGER(ix)[kx++];
      }
    while (kx < lx)
      INTEGER(ir)[n++] = INTEGER(ix)[kx++];
    while (ky < ly)
      INTEGER(ir)[n++] = INTEGER(iy)[ky++];
    INTEGER(pr)[i] = n;
  }
  
  if (n < LENGTH(ir)) {
    PROTECT(ix = ir);
    setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, n)));
    memcpy(INTEGER(ir), INTEGER(ix), sizeof(int) * n);
    
    UNPROTECT(2);
  }
  /* fixme */
  setAttrib(r, install("Dim"), PROTECT(ir = allocVector(INTSXP, 2)));
  UNPROTECT(1);
  INTEGER(ir)[0] = nr;
  INTEGER(ir)[1] = LENGTH(px)-1;
  
  setAttrib(r, install("Dimnames"), PROTECT(ir = allocVector(VECSXP, 2)));
  
  PROTECT(ix = getAttrib(x, install("Dimnames")));
  PROTECT(iy = getAttrib(y, install("Dimnames")));
  
  if (isNull((pr = VECTOR_ELT(ix, 0))))
    SET_VECTOR_ELT(ir, 0, VECTOR_ELT(iy, 0));
  else
    SET_VECTOR_ELT(ir, 0, pr);
  
  if (isNull((pr = VECTOR_ELT(ix, 1))))
    SET_VECTOR_ELT(ir, 1, VECTOR_ELT(iy, 1));
  else
    SET_VECTOR_ELT(ir, 1, pr);
  
  if (isNull((ix = getAttrib(ix, R_NamesSymbol))))
    setAttrib(ir, R_NamesSymbol, getAttrib(iy, R_NamesSymbol));
  else
    setAttrib(ir, R_NamesSymbol, ix);
  
  UNPROTECT(6);
  
  return r;
  
}

/*
 check if the internal representaion is compatible
 with the implementations above.
 */

SEXP R_valid_ngCMatrix(SEXP x) {
  int i, k, f, l, n, m;
  SEXP px, ix, dx;
  
  if (!inherits(x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  
  px = getAttrib(x, install("p"));
  ix = getAttrib(x, install("i"));
  dx = getAttrib(x, install("Dim"));
  
  if (isNull(px) || isNull(ix) || isNull(dx))
    return mkString("slot p, i, or Dim is NULL");
  
  if (TYPEOF(px) != INTSXP || TYPEOF(ix) != INTSXP || TYPEOF(dx) != INTSXP)
    return mkString("slot p, i, or Dim not of storage type integer");
  
  if (LENGTH(dx) != 2 || INTEGER(dx)[0] < 0 || INTEGER(dx)[1] < 0)
    return mkString("slot Dim invalid");
  
  if (INTEGER(dx)[1] != LENGTH(px)-1)
    return mkString("slot p and Dim do not conform");
  
  f = l = INTEGER(px)[0];
  if (f != 0)
    return mkString("slot p invalid");
  
  for (i = 1; i < LENGTH(px); i++) {
    l = INTEGER(px)[i];
    if (l < f)
      return mkString("slot p invalid");
    f = l;
  }
  if (l != LENGTH(ix))
    return mkString("slot p and i do not conform");
  
  if (l > 0) {
    f = 0;
    for (i = 1; i < LENGTH(px); i++) {
      l = INTEGER(px)[i];
      n = -1;
      for (k = f; k < l; k++) {
        m = INTEGER(ix)[k];
        if (m <= n)
          return mkString("slot i invalid");
        n = m;
      }
      if (n >= INTEGER(dx)[0])
        return mkString("slot i invalid");
      f = l;
    }
    
  }
  
  ix = getAttrib(x, install("Dimnames"));
  
  if (LENGTH(ix) != 2 || TYPEOF(ix) != VECSXP)
    return mkString("slot Dimnames invalid");
  
  px = VECTOR_ELT(ix, 0);
  if (!isNull(px)) {
    if (TYPEOF(px) != STRSXP)
      return mkString("slot Dimnames invalid");
    if (LENGTH(px) != INTEGER(dx)[0])
      return mkString("slot Dim and Dimnames do not conform");
  }
  
  px = VECTOR_ELT(ix, 1);
  if (!isNull(px)) {
    if (TYPEOF(px) != STRSXP)
      return mkString("slot Dimnames invalid");
    if (LENGTH(px) != INTEGER(dx)[1])
      return mkString("slot Dim and Dimnames do not conform");
  }
  
  return ScalarLogical(TRUE);
}

