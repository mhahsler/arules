
#include <R.h>
#include <Rinternals.h>

/* workaround i18n */
#define _(x) (x)

/* 
 * copied from 2.14-2 src/main/subsript.c
 *
 * ceeboo 2012/11
 */

#define ECALL(call, yy) if(call == R_NilValue) error(yy); else errorcall(call, yy);

static SEXP nullSubscript(int n)
{
    int i;
    SEXP indx;
    indx = allocVector(INTSXP, n);
    for (i = 0; i < n; i++)
	INTEGER(indx)[i] = i + 1;
    return indx;
}

static SEXP logicalSubscript(SEXP s, int ns, int nx, int *stretch, SEXP call)
{
    int canstretch, count, i, nmax;
    SEXP indx;
    canstretch = *stretch;
    if (!canstretch && ns > nx) {
	ECALL(call, _("(subscript) logical subscript too long"));
    }
    nmax = (ns > nx) ? ns : nx;
    *stretch = (ns > nx) ? ns : 0;
    if (ns == 0)
	return(allocVector(INTSXP, 0));
    count = 0;
    for (i = 0; i < nmax; i++)
	if (LOGICAL(s)[i%ns])
	    count++;
    indx = allocVector(INTSXP, count);
    count = 0;
    for (i = 0; i < nmax; i++)
	if (LOGICAL(s)[i%ns]) {
	    if (LOGICAL(s)[i%ns] == NA_LOGICAL)
		INTEGER(indx)[count++] = NA_INTEGER;
	    else
		INTEGER(indx)[count++] = i + 1;
	}
    return indx;
}

static SEXP negativeSubscript(SEXP s, int ns, int nx, SEXP call)
{
    SEXP indx;
    int stretch = 0;
    int i, ix;
    PROTECT(indx = allocVector(LGLSXP, nx));
    for (i = 0; i < nx; i++)
	LOGICAL(indx)[i] = 1;
    for (i = 0; i < ns; i++) {
	ix = INTEGER(s)[i];
	if (ix != 0 && ix != NA_INTEGER && -ix <= nx)
	    LOGICAL(indx)[-ix - 1] = 0;
    }
    s = logicalSubscript(indx, nx, nx, &stretch, call);
    UNPROTECT(1);
    return s;
}

static SEXP positiveSubscript(SEXP s, int ns, int nx)
{
    SEXP indx;
    int i, zct = 0;
    for (i = 0; i < ns; i++) {
	if (INTEGER(s)[i] == 0)
	    zct++;
    }
    if (zct) {
	indx = allocVector(INTSXP, (ns - zct));
	for (i = 0, zct = 0; i < ns; i++)
	    if (INTEGER(s)[i] != 0)
		INTEGER(indx)[zct++] = INTEGER(s)[i];
	return indx;
    }
    else
	return s;
}

static SEXP integerSubscript(SEXP s, int ns, int nx, int *stretch, SEXP call)
{
    int i, ii, min, max, canstretch;
    Rboolean isna = FALSE;
    canstretch = *stretch;
    *stretch = 0;
    min = 0;
    max = 0;
    for (i = 0; i < ns; i++) {
	ii = INTEGER(s)[i];
	if (ii != NA_INTEGER) {
	    if (ii < min)
		min = ii;
	    if (ii > max)
		max = ii;
	} else isna = TRUE;
    }
    if (max > nx) {
	if(canstretch) *stretch = max;
	else {
	    ECALL(call, _("subscript out of bounds"));
	}
    }
    if (min < 0) {
	if (max == 0 && !isna) return negativeSubscript(s, ns, nx, call);
	else {
	    ECALL(call, _("only 0's may be mixed with negative subscripts"));
	}
    }
    else return positiveSubscript(s, ns, nx);
    return R_NilValue;
}

/* This uses a couple of horrible hacks in conjunction with
 * VectorAssign (in subassign.c).  If subscripting is used for
 * assignment, it is possible to extend a vector by supplying new
 * names, and we want to give the extended vector those names, so they
 * are returned as the use.names attribute. Also, unset elements of the vector
 * of new names (places where a match was found) are indicated by
 * setting the element of the newnames vector to NULL.
*/

/* The original code (pre 2.0.0) used a ns x nx loop that was too
 * slow.  So now we hash.  Hashing is expensive on memory (up to 32nx
 * bytes) so it is only worth doing if ns * nx is large.  If nx is
 * large, then it will be too slow unless ns is very small.
 */

static SEXP
stringSubscript(SEXP s, int ns, int nx, SEXP names,
		int *stretch, Rboolean in, SEXP call)
{
    SEXP indx, indexnames;
    int i, j, nnames, sub, extra;
    int canstretch = *stretch;
    /* product may overflow, so check factors as well. */
    Rboolean usehashing = in && ( ((ns > 1000 && nx) || (nx > 1000 && ns)) || (ns * nx > 15*nx + ns) );

    PROTECT(s);
    PROTECT(names);
    PROTECT(indexnames = allocVector(VECSXP, ns));
    nnames = nx;
    extra = nnames;

    /* Process each of the subscripts. First we compare with the names
     * on the vector and then (if there is no match) with each of the
     * previous subscripts, since (if assigning) we may have already
     * added an element of that name. (If we are not assigning, any
     * nonmatch will have given an error.)
     */

    if(usehashing) {
	/* must be internal, so names contains a character vector */
	/* NB: this does not behave in the same way with respect to ""
	   and NA names: they will match */
	PROTECT(indx = match(names, s, 0));
	/* second pass to correct this */
	for (i = 0; i < ns; i++)
	    if(STRING_ELT(s, i) == NA_STRING || !CHAR(STRING_ELT(s, i))[0])
		INTEGER(indx)[i] = 0;
	for (i = 0; i < ns; i++) SET_VECTOR_ELT(indexnames, i, R_NilValue);
    } else {
	PROTECT(indx = allocVector(INTSXP, ns));
	for (i = 0; i < ns; i++) {
	    sub = 0;
	    if (names != R_NilValue) {
		for (j = 0; j < nnames; j++) {
		    SEXP names_j = STRING_ELT(names, j);
		    if (!in && TYPEOF(names_j) != CHARSXP) {
			ECALL(call, _("character vector element does not have type CHARSXP"));
		    }
		    if (NonNullStringMatch(STRING_ELT(s, i), names_j)) {
			sub = j + 1;
			SET_VECTOR_ELT(indexnames, i, R_NilValue);
			break;
		    }
		}
	    }
	    INTEGER(indx)[i] = sub;
	}
    }


    for (i = 0; i < ns; i++) {
	sub = INTEGER(indx)[i];
	if (sub == 0) {
	    for (j = 0 ; j < i ; j++)
		if (NonNullStringMatch(STRING_ELT(s, i), STRING_ELT(s, j))) {
		    sub = INTEGER(indx)[j];
		    SET_VECTOR_ELT(indexnames, i, STRING_ELT(s, j));
		    break;
		}
	}
	if (sub == 0) {
	    if (!canstretch) {
		ECALL(call, _("subscript out of bounds"));
	    }
	    extra += 1;
	    sub = extra;
	    SET_VECTOR_ELT(indexnames, i, STRING_ELT(s, i));
	}
	INTEGER(indx)[i] = sub;
    }
    /* We return the new names as the names attribute of the returned
       subscript vector. */
    if (extra != nnames)
	setAttrib(indx, install("use.names"), indexnames);
    if (canstretch)
	*stretch = extra;
    UNPROTECT(4);
    return indx;
}

/* Array Subscripts.
    dim is the dimension (0 to k-1)
    s is the subscript list,
    dg is the attribute name of dim
    dng is the attribute name of dimnames
    x is the array to be subscripted.
*/

SEXP
_int_arraySubscript(int dim, SEXP s, const char *dn, const char *dnn,
		   SEXP x, Rboolean in, SEXP call)
{
    int nd, ns, stretch = 0;
    SEXP dnames, tmp;
    ns = LENGTH(s);
    nd = INTEGER(getAttrib(x, install(dn)))[dim];

    switch (TYPEOF(s)) {
    case NILSXP:
	return allocVector(INTSXP, 0);
    case LGLSXP:
	return logicalSubscript(s, ns, nd, &stretch, call);
    case INTSXP:
	return integerSubscript(s, ns, nd, &stretch, call);
    case REALSXP:
	PROTECT(tmp = coerceVector(s, INTSXP));
	tmp = integerSubscript(tmp, ns, nd, &stretch, call);
	UNPROTECT(1);
	return tmp;
    case STRSXP:
	dnames = getAttrib(x, install(dnn));
	if (dnames == R_NilValue) {
	    ECALL(call, _("no 'dimnames' attribute for array"));
	}
	dnames = VECTOR_ELT(dnames, dim);
	return stringSubscript(s, ns, nd, dnames, &stretch, in, call);
    case SYMSXP:
	if (s == R_MissingArg)
	    return nullSubscript(nd);
    default:
	if (call == R_NilValue)
	    error(_("invalid subscript type '%s'"), type2char(TYPEOF(s)));
	else
	    errorcall(call, _("invalid subscript type '%s'"),
		      type2char(TYPEOF(s)));
    }
    return R_NilValue;
}

/* R interface */
SEXP
R_arraySubscript(SEXP x, SEXP dim, SEXP s, SEXP dn, SEXP dnn) {
    /* FIXME */
    return _int_arraySubscript(INTEGER(dim)[0], s, 
			      (const char *) CHAR(STRING_ELT(dn, 0)), 
			      (const char *) CHAR(STRING_ELT(dnn, 0)), 
			      x, TRUE, R_NilValue);
}


