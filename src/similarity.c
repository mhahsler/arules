#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Utils.h>
#include <math.h>
#include "newS4object.h"

/*
 Compute auto- or cross-similarities over the columns
 of ngCMatrix objects, using optional element weights.
 Currently, Jaccard, Dice, cosine, and subset similarities
 are implemented. Note that if two columns are all-zeros
 the similarity is set to one for clustering.

 This code avoids double computations and returns an
 object of class dsCMatrix (symmetric) or dgCMatrix.

 ceeboo 2008

 Rewritten to consistently PROTECT attribute lookups and
 allocated objects that survive across subsequent allocations.
 */

SEXP R_similarity_ngCMatrix(SEXP x, SEXP y, SEXP R_method, SEXP R_weight) {
  int i, j, k, fx, lx, kx, fy, ly, ky, n, m = 0, a = 0, nprotect = 0;
  double *zx, zy, z;
  SEXP r, pr, ir, xr, px, ix, py, iy;
  SEXP dimx, dimy, dim_r, dn_r, dnx, dny, namesx, namesy, uplo, zxS;

  if (!x || isNull(x) || !inherits(x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!y || (!isNull(y) && !inherits(y, "ngCMatrix")))
    error("'y' not of class ngCMatrix");
  if (!R_method || isNull(R_method) || TYPEOF(R_method) != INTSXP)
    error("'method' not of storage type integer");
  if (!R_weight || (!isNull(R_weight) && TYPEOF(R_weight) != REALSXP))
    error("'weight' not of storage type double");

  if (isNull(y)) {
    y = x;
    m = INTEGER(R_method)[0] == 3;
  } else {
    m = 1;
  }

  PROTECT(dimx = getAttrib(x, install("Dim")));
  nprotect++;
  PROTECT(dimy = getAttrib(y, install("Dim")));
  nprotect++;

  n = INTEGER(dimx)[0];
  if (n != INTEGER(dimy)[0])
    error("the number of rows of 'x' and 'y' does not conform");

  if (!isNull(R_weight) && LENGTH(R_weight) != n)
    error("the number of rows of 'x' and 'weight' do not conform");

  PROTECT(px = getAttrib(x, install("p")));
  nprotect++;
  PROTECT(ix = getAttrib(x, install("i")));
  nprotect++;
  PROTECT(py = getAttrib(y, install("p")));
  nprotect++;
  PROTECT(iy = getAttrib(y, install("i")));
  nprotect++;

  PROTECT(r = NEW_OBJECT_OF_CLASS(m ? "dgCMatrix" : "dsCMatrix"));
  nprotect++;

  if (!m) {
    PROTECT(uplo = mkString("L"));
    nprotect++;
    setAttrib(r, install("uplo"), uplo);
  }

  /* FIXME can we bound the initial memory allocation
   *       to less than full storage representation?
   */
  n = m ? (LENGTH(px) - 1) * (LENGTH(py) - 1)
        : (LENGTH(px) - 1) * LENGTH(px) / 2;

  if (n > 1024) {
    n = LENGTH(px) + LENGTH(py);
    a = 1;
  }

  PROTECT(pr = allocVector(INTSXP, LENGTH(py)));
  nprotect++;
  setAttrib(r, install("p"), pr);

  PROTECT(ir = allocVector(INTSXP, n));
  nprotect++;
  setAttrib(r, install("i"), ir);

  PROTECT(xr = allocVector(REALSXP, n));
  nprotect++;
  setAttrib(r, install("x"), xr);

  PROTECT(zxS = allocVector(REALSXP, LENGTH(px)));
  nprotect++;
  zx = REAL(zxS);

  fx = 0;
  for (i = 1; i < LENGTH(px); i++) {
    lx = INTEGER(px)[i];
    if (isNull(R_weight)) {
      zx[i] = lx - fx;
    } else {
      z = 0;
      for (k = fx; k < lx; k++)
        z += REAL(R_weight)[INTEGER(ix)[k]];
      zx[i] = z;
    }
    fx = lx;
  }

  fy = n = INTEGER(pr)[0] = 0;
  for (j = 1; j < LENGTH(py); j++) {

    /* reallocate */
    if (a && LENGTH(ir) - n < LENGTH(px)) {
      SEXP old_ir, old_xr;
      int new_len = LENGTH(ir) * 2;

      PROTECT(old_ir = ir);
      PROTECT(ir = allocVector(INTSXP, new_len));
      R_chk_memcpy(INTEGER(ir), INTEGER(old_ir), sizeof(int) * n);
      setAttrib(r, install("i"), ir);
      
      PROTECT(old_xr = xr);
      PROTECT(xr = allocVector(REALSXP, new_len));
      R_chk_memcpy(REAL(xr), REAL(old_xr), sizeof(double) * n);
      setAttrib(r, install("x"), xr);
      
      UNPROTECT(4);
    }

    ly = INTEGER(py)[j];
    if (m) {
      if (isNull(R_weight))
        zy = ly - fy;
      else {
        zy = 0;
        for (k = fy; k < ly; k++)
          zy += REAL(R_weight)[INTEGER(iy)[k]];
      }
      i = 1;
    } else {
      zy = zx[j];
      i = j + 1;
      /* set the diagonal */
      REAL(xr)[n] = 1;
      INTEGER(ir)[n++] = j - 1;
    }

    fx = INTEGER(px)[i - 1];
    for (; i < LENGTH(px); i++) {
      lx = INTEGER(px)[i];
      if (!zx[i] && !zy) {
        /* all-zeros */
        REAL(xr)[n] = 1;
        INTEGER(ir)[n++] = i - 1;
      } else {
        z = 0;
        for (kx = fx, ky = fy; kx < lx && ky < ly;) {
          if (INTEGER(ix)[kx] < INTEGER(iy)[ky]) {
            kx++;
          } else {
            if (INTEGER(ix)[kx] == INTEGER(iy)[ky]) {
              if (isNull(R_weight))
                z++;
              else
                z += REAL(R_weight)[INTEGER(ix)[kx]];
              kx++;
            }
            ky++;
          }
        }

        if (z) {
          switch (INTEGER(R_method)[0]) {
          case 0: /* Jaccard */
            z /= zx[i] + zy - z;
            break;
          case 1: /* Dice */
            z = 2 * z / (zx[i] + zy);
            break;
          case 2: /* Cosine */
            z /= sqrt(zx[i]) * sqrt(zy);
            break;
          case 3: /* Subset */
            z = (zx[i] > z) ? 0 : z / zy;
            break;
          default:
            error("type not implemented");
          }
          if (z) {
            REAL(xr)[n] = z;
            INTEGER(ir)[n++] = i - 1;
          }
        }
      }
      fx = lx;
    }

    INTEGER(pr)[j] = n;
    fy = ly;
    R_CheckUserInterrupt();
  }

  if (n < LENGTH(ir)) {
    SEXP old_ir, old_xr;

    PROTECT(old_ir = ir);
    PROTECT(ir = allocVector(INTSXP, n));
    R_chk_memcpy(INTEGER(ir), INTEGER(old_ir), sizeof(int) * n);
    setAttrib(r, install("i"), ir);

    PROTECT(old_xr = xr);
    PROTECT(xr = allocVector(REALSXP, n));
    R_chk_memcpy(REAL(xr), REAL(old_xr), sizeof(double) * n);
    setAttrib(r, install("x"), xr);
    UNPROTECT(4);
  }

  PROTECT(dim_r = getAttrib(r, install("Dim")));
  nprotect++;
  INTEGER(dim_r)[0] = LENGTH(px) - 1;
  INTEGER(dim_r)[1] = LENGTH(py) - 1;

  PROTECT(dn_r = getAttrib(r, install("Dimnames")));
  nprotect++;

  PROTECT(dnx = getAttrib(x, install("Dimnames")));
  nprotect++;
  SET_VECTOR_ELT(dn_r, 0, VECTOR_ELT(dnx, 1));
  PROTECT(namesx = getAttrib(dnx, R_NamesSymbol));
  nprotect++;

  PROTECT(dny = getAttrib(y, install("Dimnames")));
  nprotect++;
  SET_VECTOR_ELT(dn_r, 1, VECTOR_ELT(dny, 1));
  PROTECT(namesy = getAttrib(dny, R_NamesSymbol));
  nprotect++;

  if (!isNull(namesy) || !isNull(namesx)) {
    SEXP dn_names;
    PROTECT(dn_names = allocVector(STRSXP, 2));
    SET_STRING_ELT(dn_names, 0,
      isNull(namesx) ? R_BlankString : STRING_ELT(namesx, 1));
    SET_STRING_ELT(dn_names, 1,
      isNull(namesy) ? R_BlankString : STRING_ELT(namesy, 1));
    setAttrib(dn_r, R_NamesSymbol, dn_names);
    UNPROTECT(1);
  }

  UNPROTECT(nprotect);
  return r;
}
