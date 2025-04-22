#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include "newS4object.h"
#include "r_memcpy.h"

// Compute auto- or cross-similarities over the columns
// of ngCMatrix objects, using optional element weights.
// Currently, Jaccard, Dice, cosine, and subset similarities
// are implemented. Note that if two columns are all-zeros
// the similarity is set to one for clustering.
//
// This code avoids double computations and returns an
// object of class dsCMatrix (symmetric) or dgCMatrix.
//
// ceeboo 2008

SEXP R_similarity_ngCMatrix(SEXP x, SEXP y, SEXP R_method, SEXP R_weight) {
  if (!x || isNull(x) || !inherits(x, "ngCMatrix"))
    error("'x' not of class ngCMatrix");
  if (!y || (!isNull(y) && !inherits(y, "ngCMatrix")))
    error("'y' not of class ngCMatrix");
  if (!R_method || isNull(R_method) || TYPEOF(R_method) != INTSXP)
    error("'method' not of storage type integer");
  if (!R_weight || (!isNull(R_weight) && TYPEOF(R_weight) != REALSXP))
    error("'weight' not of storage type double");
  int i, j, k, fx, lx, kx, fy, ly, ky, n, m = 0, a = 0;
  double *zx, zy, z;
  SEXP r, pr, ir, xr, px, ix, py, iy;
  
  if (isNull(y)) {
    y = x;
    m = INTEGER(R_method)[0] == 3; 
  } else
    m = 1;
  
  n = INTEGER(getAttrib(x, install("Dim")))[0];
  if (n != INTEGER(getAttrib(y, install("Dim")))[0])
    error("the number of rows of 'x' and 'y' does not conform");
  
  if (!isNull(R_weight) && LENGTH(R_weight) != n)
    error("the number of rows of 'x' and 'weight' do not conform");
  
  px = getAttrib(x, install("p"));
  ix = getAttrib(x, install("i"));
  
  py = getAttrib(y, install("p"));
  iy = getAttrib(y, install("i"));
  
  PROTECT(r = NEW_OBJECT_OF_CLASS((m) ? "dgCMatrix" : "dsCMatrix"));
  
  if (!m) {
    setAttrib(r, install("uplo"), PROTECT(mkString("L")));
    UNPROTECT(1);
  }
  
  // FIXME can we bound the initial memory allocation
  //       to less than full storage representation?
  n = (m) ? (LENGTH(px)-1) * (LENGTH(py)-1)
    : (LENGTH(px)-1) *  LENGTH(px) / 2;
  
  if (n > 1024) {
    n = LENGTH(px) + LENGTH(py);
    a = 1;
  }
  
  setAttrib(r, install("p"), PROTECT(pr = allocVector(INTSXP, LENGTH(py))));
  setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, n)));
  setAttrib(r, install("x"), PROTECT(xr = allocVector(REALSXP, n)));
  UNPROTECT(3); 
  
  // precompute
  zx = REAL(PROTECT(allocVector(REALSXP, LENGTH(px))));
  fx = 0;
  for (i = 1; i < LENGTH(px); i++) {
    lx = INTEGER(px)[i];
    if (isNull(R_weight))
      zx[i] = lx - fx;
    else {
      z = 0;
      for (k = fx; k < lx; k++)
        z += REAL(R_weight)[INTEGER(ix)[k]];
      zx[i] = z;
    }
    fx = lx;
  }
  
  fy = n = INTEGER(pr)[0] = 0;
  for (j = 1; j < LENGTH(py); j++) {
    
    // reallocate
    if (a &&
        LENGTH(ir) - n < LENGTH(px)) {
      SEXP t;
      
      PROTECT(t = ir);
      setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, LENGTH(ir) * 2)));
      r_memcpy(INTEGER(ir), INTEGER(t), sizeof(int) * n);
      
      PROTECT(t = xr);
      setAttrib(r, install("x"), PROTECT(xr = allocVector(REALSXP, LENGTH(ir))));
      r_memcpy(REAL(xr), REAL(t), sizeof(double) * n);
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
      // set the diagonal
      REAL(xr)[n] = 1;
      INTEGER(ir)[n++] = j-1;
    }
    fx = INTEGER(px)[i-1];
    for (; i < LENGTH(px); i++) {
      lx = INTEGER(px)[i];
      if (!zx[i] && !zy) {	    // all-zeros
        REAL(xr)[n] = 1;
        INTEGER(ir)[n++] = i-1;
      }
      else {
        z = 0;
        for (kx = fx, ky = fy; kx < lx && ky < ly; ) {
          if (INTEGER(ix)[kx]  < INTEGER(iy)[ky])
            kx++;
          else {
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
          switch(INTEGER(R_method)[0]) {
          case 0:			    // Jaccard
            z /= zx[i] + zy - z;
            break;
          case 1:			    // Dice
            z = 2 * z / (zx[i] + zy);
            break;
          case 2:			    // Cosine
            z /= sqrt(zx[i]) * sqrt(zy);
            break;
          case 3:			    // Subset
            z = (zx[i] > z) ? 0 : z / zy;
            break;
            // add further measures here!
          default:
            error("type not implemented");
          }
          if (z) {
            REAL(xr)[n] = z;
            INTEGER(ir)[n++] = i-1;
          }
        }
      } 
      fx = lx;
    }
    INTEGER(pr)[j] = n;
    fy = ly;
    R_CheckUserInterrupt();
  }
  
  UNPROTECT(1); // zx
  
  if (n < LENGTH(ir)) {
    
    PROTECT(ix = ir);
    setAttrib(r, install("i"), PROTECT(ir = allocVector(INTSXP, n)));
    r_memcpy(INTEGER(ir), INTEGER(ix), sizeof(int) * n);
    
    PROTECT(ix = xr);
    setAttrib(r, install("x"), PROTECT(xr = allocVector(REALSXP, n)));
    r_memcpy(REAL(xr), REAL(ix), sizeof(double) * n);
    
    UNPROTECT(4);
  }
  
  ix = getAttrib(r, install("Dim"));
  INTEGER(ix)[0] = LENGTH(px)-1;
  INTEGER(ix)[1] = LENGTH(py)-1;
  
  ir = getAttrib(r, install("Dimnames"));
  
  PROTECT(ix = getAttrib(x, install("Dimnames")));
  SET_VECTOR_ELT(ir, 0, PROTECT(VECTOR_ELT(ix, 1)));
  UNPROTECT(1);
  PROTECT(ix = getAttrib(ix, R_NamesSymbol));
  
  PROTECT(iy = getAttrib(y, install("Dimnames")));
  SET_VECTOR_ELT(ir, 1, PROTECT(VECTOR_ELT(iy, 1)));
  UNPROTECT(1);
  PROTECT(iy = getAttrib(iy, R_NamesSymbol));
  
  if (!isNull(iy) || !isNull(ix)) {
    setAttrib(ir, R_NamesSymbol, (pr = allocVector(STRSXP, 2)));
    SET_STRING_ELT(pr, 0, isNull(ix) ? R_BlankString : STRING_ELT(ix, 1));
    SET_STRING_ELT(pr, 1, isNull(iy) ? R_BlankString : STRING_ELT(iy, 1));
  }
  
  UNPROTECT(5); // iy (2), ix (2), r
  
  return r;
}

//
