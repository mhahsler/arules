/*----------------------------------------------------------------------
  File    : params.c
  Contents: command line parameter retrieval
  Author  : Christian Borgelt
  History : 05.06.2003 file created
----------------------------------------------------------------------*/
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>
#include "params.h"

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/

int getints (char *s, char **end, int n, ...)
{                               /* --- get integer parameters */
  va_list args;                 /* list of variable arguments */
  int     k = 0, t;             /* parameter counter, buffer */

  assert(s && end && (n > 0));  /* check the function arguments */
  va_start(args, n);            /* get variable arguments */
  while (k < n) {               /* traverse the arguments */
    t = (int)strtol(s, end,10); /* get the next parameter and */
    if (*end == s) break;       /* check for an empty parameter */
    *(va_arg(args, int*)) = t;  /* store the parameter */
    k++;                        /* and count it */
    s = *end; if (*s++ != ':') break;
  }                             /* check for a colon */
  va_end(args);                 /* end argument evaluation */
  return k;                     /* return the number of parameters */
}  /* getints() */

/*--------------------------------------------------------------------*/

int getdbls (char *s, char **end, int n, ...)
{                               /* --- get double parameters */
  va_list args;                 /* list of variable arguments */
  int     k = 0;                /* parameter counter */
  double  t;                    /* temporary buffer */

  assert(s && end && (n > 0));  /* check the function arguments */
  va_start(args, n);            /* get variable arguments */
  while (k < n) {               /* traverse the arguments */
    t = strtod(s, end);         /* get the next parameter and */
    if (*end == s) break;       /* check for an empty parameter */
    *(va_arg(args, double*)) = t;  /* store the parameter */
    k++;                           /* and count it */
    s = *end; if (*s++ != ':') break;
  }                             /* check for a colon */
  va_end(args);                 /* end argument evaluation */
  return k;                     /* return the number of parameters */
}  /* getdbls() */

/*--------------------------------------------------------------------*/

int getintvec (char *s, char **end, int n, int *p)
{                               /* --- get integer parameter vector */
  int k = 0, t;                 /* parameter counter, buffer */

  assert(s && end && (n > 0));  /* check the function arguments */
  while (k < n) {               /* traverse the arguments */
    t = (int)strtol(s, end,10); /* get the next parameter and */
    if (*end == s) break;       /* check for an empty parameter */
    p[k++] = t;                 /* store and count the parameter */
    s = *end; if (*s++ != ':') break;
  }                             /* check for a colon */
  return k;                     /* return the number of parameters */
}  /* getintvec() */

/*--------------------------------------------------------------------*/

int getdblvec (char *s, char **end, int n, double *p)
{                               /* --- get double parameter vector */
  int     k = 0;                /* parameter counter */
  double  t;                    /* temporary buffer */

  assert(s && end && (n > 0));  /* check the function arguments */
  while (k < n) {               /* traverse the arguments */
    t = strtod(s, end);         /* get the next parameter and */
    if (*end == s) break;       /* check for an empty parameter */
    p[k++] = t;                 /* store and count the parameter */
    s = *end; if (*s++ != ':') break;
  }                             /* check for a colon */
  return k;                     /* return the number of parameters */
}  /* getdblvec() */
