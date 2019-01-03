/* Borrowed from Mutils.c from package Matrix */

#include "newS4object.h"

/**
 * A safe NEW_OBJECT(MAKE_CLASS(cls)),  where the caller must protect the
 * return value of this function
 *
 * @param an R character string specifying the name of a known S4 class
 */
SEXP NEW_OBJECT_OF_CLASS(const char* cls)
{
    SEXP ans = NEW_OBJECT(PROTECT(MAKE_CLASS(cls)));
    UNPROTECT(1);
    return ans;
}
