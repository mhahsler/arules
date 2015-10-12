/*----------------------------------------------------------------------
  File    : nstats.c
  Contents: management of normalization statistics
  Author  : Christian Borgelt
  History : 12.08.2003 file created
----------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <math.h>
#include <assert.h>
#include "nstats.h"

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/

NSTATS* nst_create (int dim)
{                               /* --- create numerical statistics */
  NSTATS *nst;                  /* created statistics structure */
  double *p;                    /* to organize the memory */

  assert(dim > 0);              /* check the function argument */
  nst = (NSTATS*)malloc(sizeof(NSTATS) +(6*dim -1) *sizeof(double));
  if (!nst) return NULL;        /* create a statistics structure */
  nst->dim  = dim;              /* and initialize the fields */
  nst->reg  = 0;
  nst->mins = p = nst->facs +dim;
  nst->maxs = p += dim;         /* organize the vectors */
  nst->sums = p += dim;
  nst->sqrs = p += dim;
  nst->offs = p += dim;
  while (--dim >= 0) {          /* traverse the vectors */
    nst->mins[dim] = DBL_MAX; nst->maxs[dim] = -DBL_MAX;
    nst->sums[dim] = nst->sqrs[dim] = nst->offs[dim] = 0;
    nst->facs[dim] = 1;         /* initialize the ranges of values */
  }                             /* and the aggregation variables */
  return nst;                   /* return created structure */
}  /* nst_create() */

/*--------------------------------------------------------------------*/

void nst_delete (NSTATS *nst)
{ free(nst); }                  /* --- delete numerical statistics */

/*--------------------------------------------------------------------*/

void nst_reg (NSTATS *nst, double *vec, double weight)
{                               /* --- register a data vector */
  int    i;                     /* loop variable */
  double *min, *max;            /* to traverse the min./max. values */
  double *sum, *sqr;            /* to traverse the value sums */
  double *off, *fac;            /* to traverse the offsets/scales */
  double t;                     /* temporary buffer */

  assert(nst && vec);           /* check the function arguments */
  sum = nst->sums;              /* get the vectors for the sums */
  sqr = nst->sqrs;              /* and the sums of squares */
  if (!vec) {                   /* if to terminate registration */
    off = nst->offs;            /* get the offsets and */
    fac = nst->facs;            /* the scaling factors */
    if (nst->reg <= 0)          /* if no patterns are registered */
      for (i = nst->dim; --i >= 0; ) { off[i] = 0; fac[i] = 1; }
    else {                      /* if patterns have been registered */
      for (i = nst->dim; --i >= 0; ) {      /* traverse the vectors */
        off[i] = sum[i] /nst->reg;
        t      = sqr[i] -off[i] *sum[i];
        fac[i] = (t > 0) ? sqrt(nst->reg /t) : 1;
      }                         /* estimate the parameters */
    }
    if (weight < 0) {           /* if to reinitialize registration */
      for (i = nst->dim; --i >= 0; )
        sum[i] = sqr[i] = 0;    /* reinitialize the vectors */
      nst->reg = 0;             /* and the pattern counter */
    } }
  else {                        /* if to register a data vector */
    min = nst->mins;            /* get the minimal */
    max = nst->maxs;            /* and the maximal values */
    for (i = nst->dim; --i >= 0; ) {
      if (vec[i] < min[i]) min[i] = vec[i];
      if (vec[i] > max[i]) max[i] = vec[i];
      sum[i] += vec[i];         /* update the ranges of values */
      sqr[i] += vec[i] *vec[i]; /* and sum the values */
    }                           /* and their squares */
    nst->reg += weight;         /* count the pattern */
  }
}  /* nst_reg() */

/*--------------------------------------------------------------------*/

void nst_range (NSTATS *nst, int idx, double min, double max)
{                               /* --- set range of values */
  int i;                        /* loop variable */

  assert(nst && (idx < nst->dim));  /* check the arguments */
  if (idx < 0) { i = nst->dim; idx = 0; }
  else         { i = idx +1; }  /* get index range to set */
  while (--i >= idx) {          /* and traverse it */
    nst->mins[i] = min;         /* set the minimal */
    nst->maxs[i] = max;         /* and the maximal value */
  }                             /* for all dimensions in range */
}  /* nst_range() */

/*--------------------------------------------------------------------*/

void nst_expand (NSTATS *nst, int idx, double factor)
{                               /* --- expand range of values */
  int    i;                     /* loop variable */
  double t;                     /* change of minimal/maximal value */

  assert(nst                    /* check the function arguments */
     && (idx < nst->dim) && (factor >= 0));
  if (idx < 0) { i = nst->dim; idx = 0; }
  else         { i = idx +1; }  /* get index range to expand */
  while (--i >= idx) {          /* and traverse it */
    t = (nst->maxs[i] -nst->mins[i]) *(factor -1) *0.5;
    nst->mins[i] -= t;          /* adapt the minimal */
    nst->maxs[i] += t;          /* and   the maximal value */
  }                             /* for all dimensions in range */
}  /* nst_expand() */

/*--------------------------------------------------------------------*/

void nst_scale (NSTATS *nst, int idx, double off, double fac)
{                               /* --- set (linear) scaling */
  int i;                        /* loop variable */

  assert(nst && (idx < nst->dim));  /* check the arguments */
  if (idx < 0) { i = nst->dim; idx = 0; }
  else         { i = idx +1; }  /* get index range to set */
  while (--i >= idx) {          /* and traverse it */
    nst->offs[i] = off;         /* set the offset */
    nst->facs[i] = fac;         /* and the scaling factor */
  }                             /* for all dimensions in range */
}  /* nst_scale() */

/*--------------------------------------------------------------------*/

void nst_norm (NSTATS *nst, double *vec, double *res)
{                               /* --- normalize a data vector */
  int    i;                     /* loop variable */
  double *off, *fac;            /* to traverse the scaling parameters */

  assert(nst && vec && res);    /* check the function arguments */
  off = nst->offs +(i = nst->dim);
  fac = nst->facs + i;          /* get the scaling parameters */
  while (--i >= 0) res[i] = *--fac * (vec[i] - *--off);
}  /* nst_norm() */             /* scale the vector */

/*--------------------------------------------------------------------*/

void nst_inorm (NSTATS *nst, double *vec, double *res)
{                               /* --- inverse normalize a vector */
  int    i;                     /* loop variable */
  double *off, *fac;            /* to traverse the scaling parameters */

  assert(nst && vec && res);    /* check the function arguments */
  off = nst->offs +(i = nst->dim);
  fac = nst->facs + i;          /* get the scaling parameters */
  while (--i >= 0) res[i] = vec[i] / *--fac + *--off;
}  /* nst_inorm() */            /* scale the vector */

/*--------------------------------------------------------------------*/

void nst_center (NSTATS *nst, double *vec)
{                               /* --- get center of data space */
  int    i;                     /* loop variable */
  double *min, *max;            /* to traverse the ranges */

  assert(nst && vec);           /* check the function arguments */
  min = nst->mins;              /* get the range variables, */
  max = nst->maxs;              /* traverse the dimensions, */
  for (i = nst->dim; --i >= 0;) /* and compute the center vector */
    vec[i] = 0.5 *(max[i] +min[i]);
}  /* nst_center() */

/*--------------------------------------------------------------------*/

void nst_spans (NSTATS *nst, double *vec)
{                               /* --- get spans of dimensions */
  int    i;                     /* loop variable */
  double *min, *max;            /* to traverse the ranges */

  assert(nst && vec);           /* check the function arguments */
  min = nst->mins;            
  max = nst->maxs;              /* get the range variables, */ 
  for (i = nst->dim; --i >= 0;) /* traverse the dimensions, */ 
    vec[i] = max[i] -min[i];    /* and compute the spans */
}  /* nst_spans() */
