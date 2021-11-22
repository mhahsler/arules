/*----------------------------------------------------------------------
  File    : bitmat.c
  Contents: bit matrix management
  Author  : Christian Borgelt
  History : 09.06.2002 file created
            10.12.2002 result report function extended
            19.04.2003 bug in _search fixed (min. number of vectors)
            16.08.2003 bug in bm_resize fixed (slow column clearing)
            18.08.2003 memory benchmarking functionality added
            20.08.2003 closed/maximal item set search added
            22.08.2003 function bm_setcol added
            11.09.2003 sparse matrix representation added
            13.09.2003 sparse matrix representation completed
            20.09.2003 bug in _isect2 fixed (empty transaction list)
            21.09.2003 bug in benchmark version fixed (number of sets)
            25.09.2003 bit count table extended to word values
----------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "vecops.h"
#include "bitmat.h"
#ifdef STORAGE
#include "storage.h"
#endif

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define BLKSIZE    256          /* block size for resize */
#define BLKMASK    0xff         /* block size mask for resize */
#define MASK       0xffffffff   /* bit mask 2 for 32 bit integers */
#define NOREPORT   0x80000000   /* flag for no reporting of a set */

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef struct {                /* --- reduced bit matrix --- */
  int     cnt;                  /* number of bit/number vectors */
  int     len;                  /* length of each bit vector */
  int     *vecs[1];             /* array of bit/number vectors */
} REDMAT;                       /* (reduced bit matrix) */

typedef struct {                /* --- all one submatrix search --- */
  int     min;                  /* minimum number of rows */
  int     max;                  /* maximum number of rows */
  int     supp;                 /* minimum support (num. of columns) */
  BMREPFN *report;              /* report function for results */
  void    *data;                /* user data for report function */
  BITMAT  *res;                 /* for closed and maximal item sets */
  #ifdef BENCH                  /* if benchmark version */
  int     mcur;                 /* current memory usage in bytes */
  int     mmax;                 /* maximum memory usage in bytes */
  #endif
  int     rows[1];              /* row ids. vector for reporting */
} ALLONE;                       /* (all one submatrix search) */

/*----------------------------------------------------------------------
  Global Variables
----------------------------------------------------------------------*/
static char _bctab[65536];      /* bit count table */

/*----------------------------------------------------------------------
  Auxiliary Functions
----------------------------------------------------------------------*/

static void _bcinit (void)
{                               /* --- initialize the bit count table */
  register int i, k, n;         /* loop variable, counter */

  for (i = 65536; --i >= 0; ) { /* traverse all word values */
    for (n = 0, k = i; k; k >>= 1)
      n += k & 1;               /* count the bits in the byte value */
    _bctab[i] = (char)n;        /* and store the result in the table */
  }                             /* (tab[i] = number of set bits in i) */
}  /* _bcinit() */

/*--------------------------------------------------------------------*/

static int _count (int *vec, int n)
{                               /* --- count set bits in a bit vector */
  register int c = 0, x;        /* bit counter */

  for (vec += n; --n >= 0; ) {  /* traverse the bit vector */
    x = *--vec;                 /* from back to front */
    c += _bctab[x & 0xffff] +_bctab[(x >> 16) & 0xffff];
  }                             /* count the number of set bits */
  return *--vec = c;            /* and return this number */
}  /* _count() */

/*--------------------------------------------------------------------*/

static int _isect1 (int *res, const int *v1, const int *v2, int n)
{                               /* --- intersect two bit vectors */
  register int c = 0, x;        /* bit counter, intersection result */

  v1 += n; v2 += n;             /* traverse the bit vectors */
  for (res += n; --n >= 0; ) {  /* from back to front and */
    *--res = x = *--v1 & *--v2; /* intersect their elements */
    c += _bctab[x & 0xffff] +_bctab[(x >> 16) & 0xffff];
  }                             /* count the number of set bits */
  return *--res = c;            /* and return this number */
}  /* _isect1() */

/*--------------------------------------------------------------------*/

static int _isect2 (int *res, const int *v1, const int *v2)
{                               /* --- intersect two number vectors */
  register int i = 0;           /* bit counter */
  register int n1, n2;          /* number of column identifiers */

  n1 = *(v1-1) & ~NOREPORT;     /* get number of columns/items */
  n2 = *(v2-1) & ~NOREPORT;     /* in the two number vectors */
  if ((n1 <= 0) || (n2 <= 0))   /* check if one transaction list */
    return *--res = 0;          /* is empty */
  while (1) {                   /* intersection loop */
    if      (*v1 <  *v2) {      /* skip bits that are only in v1 */
      v1++; if (--n1 <= 0) break; }
      /* *v1++; if (--n1 <= 0) break; }  (unused return value) */
    else if (*v1 >  *v2) {      /* skip bits that are only in v2 */
      v2++; if (--n2 <= 0) break; }
      /* *v2++; if (--n2 <= 0) break; }    (unused return value) */ 
    else {                      /* copy bits that */
      res[i++] = *v1++; v2++;   /* are in both vectors */
      if ((--n1 <= 0) || (--n2 <= 0)) break;
    }                           /* in each case check */
  }                             /* whether one vector gets empty */
  return *--res = i;            /* return the intersection size */
}  /* _isect2() */

/*----------------------------------------------------------------------
  Main Functions
----------------------------------------------------------------------*/

BITMAT* bm_create (int rowcnt, int colcnt, int sparse)
{                               /* --- create a bit matrix */
  BITMAT *bm;                   /* created bit vector set */
  int    *vec;                  /* buffer for a bit vector */
  int    vsz;                   /* size of the rows vector */
  int    n;                     /* number of integers in a bit vector */

  assert((rowcnt >= 0)          /* check the function arguments */
      && (colcnt >= 0));
  bm = (BITMAT*)malloc(sizeof(BITMAT));
  if (!bm) return NULL;         /* allocate the base structure */
  vsz = (rowcnt > 0) ? rowcnt : BLKSIZE;
  bm->rows = (int**)malloc(vsz *sizeof(int*));
  if (!bm->rows) { free(bm); return NULL; }
  bm->rowvsz = vsz;             /* initialize the fields */
  bm->colcnt = colcnt;
  bm->sparse = sparse;
  #ifdef BENCH                  /* if benchmark version, */
  bm->mem    = 0;               /* initialize memory measurement */
  #endif
  if (sparse) {                 /* if sparse version, set empty vecs. */
    n = 2; bm->colvsz = bm->colcnt; }
  else {                        /* if normal version */
    n = ((colcnt > 0) ? (colcnt +BM_MASK) >> BM_SHIFT : BLKSIZE);
    bm->colvsz = n << BM_SHIFT; n += 2;
  }                             /* set default size bit vectors */
  for (bm->rowcnt = 0; bm->rowcnt < rowcnt; bm->rowcnt++) {
    vec = (int*)calloc(n, sizeof(int));
    if (!vec) { bm_delete(bm); return NULL; }
    bm->rows[bm->rowcnt] = vec+2;
    vec[0] = bm->rowcnt;        /* create the bit vectors and */
  }                             /* note the row identifiers */
  bm->buf = bm->supps = NULL;   /* default: no additional buffers */
  if (!_bctab[1]) _bcinit();    /* init. the bit counter table */
  return bm;                    /* return the created bit matrix */
}  /* bm_create() */

/*--------------------------------------------------------------------*/

void bm_delete (BITMAT *bm)
{                               /* --- delete a bit matrix */
  int i;                        /* loop variable */

  assert(bm);                   /* check the function argument */
  if (bm->supps) free(bm->supps);
  if (bm->buf)   free(bm->buf-1); /* delete the additional buffers */
  for (i = bm->rowcnt; --i >= 0; )
    free(bm->rows[i]-2);        /* delete all row bit vectors, */
  free(bm->rows);               /* the vector of matrix rows, */
  free(bm);                     /* and the base structure */
}  /* bm_delete() */

/*--------------------------------------------------------------------*/

static int _bufrsz (BITMAT *bm, int nb, int ns)
{                               /* ---  */
  int *vec;                     /* buffer for new bit vectors */

  if (!bm->buf) return 0;       /* check for an intersection buffer */
  vec = (int*)realloc(bm->buf-1, (nb+1) *sizeof(int));
  if (!vec) return -1;          /* enlarge the intersection buffer */
  bm->buf = vec +1;             /* and set the new vector */
  if (!bm->supps) return 0;     /* check for a supports vector */
  vec = (int*)realloc(bm->supps, ns *sizeof(int));
  if (!vec) return -1;          /* enlarge the supports vector */
  bm->supps = vec;              /* and set the new vector */
  return 0;                     /* return 'ok' */
}  /* _bufrsz() */

/*--------------------------------------------------------------------*/

int bm_resize (BITMAT *bm, int rowcnt, int colcnt)
{                               /* --- resize a bit matrix */
  int row, i, k;                /* loop variables, buffers */
  int **rows;                   /* buffer for new rows vector */
  int *vec;                     /* buffer for new bit vectors */
  int n, c;                     /* number of integers in a bit vector */

  assert(bm);                   /* check the function arguments */
  if (rowcnt < 0) rowcnt = bm->rowcnt;
  if (colcnt < 0) colcnt = bm->colcnt;
  if (bm->sparse) c = n = 2;    /* if sparse matrix */
  else {                        /* if normal matrix */
    c = bm->colvsz;             /* get the size of the bit vectors */
    if (colcnt > c) {           /* if the bit vectors are full */
      c += (c > (BLKSIZE << BM_SHIFT)) ? c >> 1 : (BLKSIZE << BM_SHIFT);
      if (colcnt > c) c = colcnt;
    }                           /* compute a new size for the vectors */
    n = ((c +BM_MASK) >> BM_SHIFT) +2;
  }                             /* and the number of integers in them */
  if (rowcnt > bm->rowcnt) {    /* if to add rows to the matrix */
    k = bm->rowvsz;             /* get the size of the rows vector */
    if (rowcnt > k) {           /* if the rows vector is full */
      k += (k > BLKSIZE) ? k >> 1 : BLKSIZE;
      if (rowcnt > k) k = rowcnt;  /* compute new number of rows */
      rows = (int**)realloc(bm->rows, k *sizeof(int*));
      if (!rows) return -1;     /* resize the rows vector */
      bm->rows = rows; bm->rowvsz = k;
    }                           /* set the new vector and its size */
    for (row = bm->rowcnt; row < rowcnt; row++) {
      vec = (int*)calloc(n, sizeof(int));
      if (!vec) break;          /* allocate bit vectors */
      bm->rows[row] = vec+2;    /* for the new rows */
      vec[0] = row;             /* note the row identifier */
    }
    if (row < rowcnt) {         /* if the row allocation failed */
      while (--row >= bm->rowcnt) free(bm->rows[row]-2);
      return -1;                /* delete the row vectors */
    }                           /* that could be allocated */
  }                             /* and abort the function */
  if (!bm->sparse               /* if not a sparse bit matrix */
  &&  (colcnt > bm->colvsz)) {  /* and to add columns to the matrix */
    for (row = bm->rowcnt; --row >= 0; ) {
      vec = (int*)realloc(bm->rows[row]-2, n *sizeof(int));
      if (!vec) break;          /* enlarge the already existing */
      bm->rows[row] = vec+2;    /* vectors and set the new ones */
      vec += i = ((bm->colvsz +BM_MASK) >> BM_SHIFT) +2;
      for (k = n-i; --k >= 0; ) *vec++ = 0;
    }                           /* clear new columns */
    if ((row < 0)               /* if reallocation succeeded */
    &&  (_bufrsz(bm, n-2, c) == 0))
      bm->colvsz = c;           /* set the new column vector size */
    else {                      /* if the reallocation failed */
      if (rowcnt > bm->rowcnt){ /* if rows have been added */
        for (row = rowcnt; --row >= bm->rowcnt; )
          free(bm->rows[row]-2);/* delete the newly */
      }                         /* allocated matrix rows */
      n = ((bm->colvsz +BM_MASK) >> BM_SHIFT) +2;
      while (++row < bm->rowcnt)
        bm->rows[row] = (int*)realloc(bm->rows[row]-2, n*sizeof(int))+2;
      return -1;                /* shrink the bit vectors */
    }                           /* to their original size */
  }                             /* and abort the function */
  if (bm->sparse && (_bufrsz(bm, colcnt, colcnt) != 0))
    return -1;                  /* enlarge buffers of sparse matrix */
  if (rowcnt < bm->rowcnt) {    /* if to remove rows */
    for (row = bm->rowcnt; --row >= rowcnt; )
      free(bm->rows[row] -2);   /* delete superfluous rows */
  }
  bm->rowcnt = rowcnt;          /* set the new number of rows */
  bm->colcnt = colcnt;          /* and the new number of columns */
  return 0;                     /* return 'ok' */
}  /* bm_resize() */

/*--------------------------------------------------------------------*/

void bm_setcol (BITMAT *bm, int col, const int *ids, int n)
{                               /* --- set a matrix column */
  unsigned int b;                        /* bit mask */

  assert(bm && ids);            /* check the function arguments */
  b = 1U << (col & BM_MASK);     /* compute bit mask and */
  col >>= BM_SHIFT;             /* index for the new column */
  while (--n >= 0)              /* traverse the given indices and */
    bm->rows[*ids++][col] |= b; /* set the corresponding rows */
}  /* bm_setcol() */

/*--------------------------------------------------------------------*/

int bm_addcol (BITMAT *bm, const int *ids, int n)
{                               /* --- add a matrix column */
  int i;                        /* loop variable */
  int *v;                       /* buffer for number vector */

  assert(bm);                   /* check the function arguments */
  for (i = n; --i >= 0; ) {     /* traverse the given indices */
    v = bm->rows[ids[i]] -2;    /* get the number vector */
    if ((v[1] & BLKMASK) != 0) continue;
    v = (int*)realloc(v, (v[1] +BLKSIZE +2) *sizeof(int));
    if (!v) return -1;          /* if the number vector is full, */
    bm->rows[ids[i]] = v+2;     /* resize the number vector */
  }                             /* and set the new vector */
  if (_bufrsz(bm, bm->colcnt+1, bm->colcnt+1) != 0)
    return -1;                  /* enlarge the buffers */
  for (i = n; --i >= 0; ) {     /* traverse the given indices */
    v = bm->rows[ids[i]];       /* get the number vector */
    v[(*(v-1))++] = bm->colcnt; /* add the new matrix column */
  }                             /* to the column list */
  return bm->colcnt++;          /* return the column index */
}  /* bm_addcol() */

/*--------------------------------------------------------------------*/

int bm_count (BITMAT *bm, int row)
{                               /* --- count ones in a row vector */
  if (bm->sparse) return *(bm->rows[row] -1);
  return _count(bm->rows[row], (bm->colcnt +BM_MASK) >> BM_SHIFT);
}  /* bm_count() */

/*--------------------------------------------------------------------*/

static int _exists (BITMAT *bm, int *ids, int n, int supp)
{                               /* -- check maximal/closed item sets */
  int i, k, x, m = n;           /* loop variables */
  // MFH: int b, bb;                    /* support bit mask */
  unsigned int b, bb;                    /* support bit mask */
  int *d, *s;                   /* to traverse the bit vectors */
  int r = 0;                    /* result of intersection */

  assert(bm && bm->buf && ids); /* check the function arguments */
  d = bm->buf;                  /* get the intersection buffer */
  if (bm->sparse) {             /* if sparse matrix */
    if (bm->supps) {            /* if to find closed item sets, */
      s = bm->supps;            /* traverse the supports vector */
      for (i = 0; i < bm->colcnt; i++)
        if (*s++ == supp) *d++ = i;
      r = (int)(d -bm->buf); }  /* collect same support sets */
    else {                      /* if to find maximal item sets, */
      s = bm->rows[ids[--m]];   /* traverse vector for last item */
      for (i = r = *(s-1); --i >= 0; )
        *d++ = *s++;            /* copy the transaction/column list */
    }                           /* to the intersection buffer */
    if (r > 0) {                /* if the buffer is not empty */
      *(bm->buf -1) = r;        /* number of entries in the buffer */
      while (--m >= 0) {        /* traverse the remaining items */
        if (_isect2(bm->buf, bm->buf, bm->rows[ids[m]]) <= 0)
          break;                /* intersect with the next list */
      }                         /* and check for an empty result */
      if (m < 0) return 1;      /* if the intersection is not empty, */
    }                           /* return "superset exists" */
    k = bm->colcnt;             /* get the number of columns */
    if (bm_addcol(bm, ids, n) < 0)
      return -1; }              /* enter itemset into the matrix */
  else {                        /* if normal matrix */
    k = (bm->colcnt +BM_MASK) >> BM_SHIFT;
    s = bm->rows[ids[--m]];     /* traverse vector for last item */
    if (bm->supps) {            /* if to find closed item sets */
      for (x = i = 0; i < k; i++) {
        for (bb = 0, b = 1; b & MASK; b <<= 1)
          if (bm->supps[x++] == supp)
            bb |= b;            /* collect same support sets and */
        r |= d[i] = s[i] & bb;  /* combine the resulting bit flags */
      } }                       /* with the last item's vector */
    else {                      /* if to find maximal item sets, */
      for (s += k, d += i = k; --i >= 0; )
        r |= *--d = *--s;       /* copy the last item's vector */
    }                           /* and check for an empty result */
    if (r != 0) {               /* if the buffer is not empty */
      while (--m >= 0) {        /* traverse the remaining items */
        s = bm->rows[ids[m]]+k; /* traverse the item's bit vector */
        for (r = 0, d += i = k; --i >= 0; )
          r |= *--d &= *--s;    /* intersect with the next item */
        if (r == 0) break;      /* and check for an empty result */
      }
      if (m < 0) return 1;      /* if the intersection is not empty, */
    }                           /* return "superset exists" */
    if (bm_resize(bm, bm->rowcnt, (k = bm->colcnt) +1) < 0)
      return -1;                /* enlarge the bit matrix and */
    bm_setcol(bm, k, ids, n);   /* enter itemset into the matrix */
  }
  if (bm->supps)                /* if there is a supports vector, */
    bm->supps[k] = supp;        /* note the item set support */
  return 0;                     /* return "new item set" */
}  /* _exists() */

/*--------------------------------------------------------------------*/

static int _search (ALLONE *ao, REDMAT *mat, int depth, int mode))
{                               /* --- search row intersections */
  int    i, k, n;               /* loop variables, bit counter */
  REDMAT *red;                  /* bit vector set for next level */
  int    *vecs, *p;             /* to traverse the bit vectors */
  int    cnt = 0;               /* number of item sets found */
  int    m;                     /* size of memory for new matrix */

  /* --- search recursively --- */
  n = ao->min - ++depth;        /* compute the min. number of vectors */
  if (n <= 0) n = 1;            /* needed to reach the minimum size */
  if ((depth < ao->max)         /* if search depth not yet reached */
  &&  (mat->cnt > n)) {         /* and there are enough vectors left */
    red = (REDMAT*)malloc(sizeof(REDMAT) +(mat->cnt-2) *sizeof(int*));
    if (!red) return -1;        /* allocate matrix for the next level */
    red->len = mat->len;        /* and initialize it */
    if (mat->len >= 0)          /* if normal matrix */
      m = (mat->cnt-1) *(mat->len+2);
    else {                      /* if sparse matrix */
      for (m = 2 *(i = mat->cnt-1); --i >= 0; )
        m += *(mat->vecs[i]-1); /* sum the sizes of the vectors */
    }                           /* (compute size of needed memory) */
    vecs = (int*)malloc(m *sizeof(int));
    if (!vecs) { free(red); return -1; }
    #ifdef BENCH                /* if benchmark version */
    ao->mcur += sizeof(REDMAT) +(mat->cnt-2) *sizeof(int*)
              + m *sizeof(int); /* compute current memory usage */
    if (ao->mcur > ao->mmax) ao->mmax = ao->mcur;
    #endif                      /* adapt maximal memory usage */
    for (i = mat->cnt; --i >= n; ) {
      red->cnt = 0; p = vecs;   /* traverse the bit vectors */
      for (k = 0; k < i; k++) { /* traverse the remaining vectors */
        if (((mat->len >= 0)    /* intersect two bit/number vectors */
        ?   _isect1(p+2, mat->vecs[i], mat->vecs[k], mat->len)
        :   _isect2(p+2, mat->vecs[i], mat->vecs[k])) < ao->supp)
          continue;             /* if the support is too low, skip */
        if (ao->res) {          /* if closed/maximal item sets */
          if (!ao->res->supps){ /* mark non-maximal item sets */
            *(mat->vecs[i]-1) |= NOREPORT;
            *(mat->vecs[k]-1) |= NOREPORT; }
          else {                /* if closed item sets */
            if ((p[1] & ~NOREPORT) == (*(mat->vecs[i]-1) & ~NOREPORT))
              *(mat->vecs[i]-1) |= NOREPORT;
            if ((p[1] & ~NOREPORT) == (*(mat->vecs[k]-1) & ~NOREPORT))
              *(mat->vecs[k]-1) |= NOREPORT;
          }                     /* mark subsets if they have */
        }                       /* the same support as the superset */
        red->vecs[red->cnt++] = p+2;
        *p = *(mat->vecs[k]-2); /* store and count the intersection */
        p += ((mat->len < 0) ? p[1] : mat->len) +2;
      }                         /* advance the vector pointer */
      if (red->cnt <= 0) continue; /* if the matrix is empty, cont. */
      ao->rows[depth-1] = *(mat->vecs[i]-2);
      cnt += k = _search(ao, red, depth);
      if (k < 0) break;         /* recursively search for a submatrix */
    }                           /* (i.e., frequent itemsets) */
    free(vecs); free(red);      /* delete the work buffers */
    #ifdef BENCH                /* if benchmark version */
    ao->mcur -= sizeof(REDMAT) +(mat->cnt-2) *sizeof(int*)
              + m *sizeof(int); /* compute current memory usage */
    #endif
    if (i >= n) return -1;      /* check for a search error */
  }  /* if ((depth < ao->max) .. */

  /* --- report (and record) found item sets --- */
  if (depth >= ao->min) {       /* if minimum number of rows reached */
    for (i = 0; i < mat->cnt; i++) {
      p = mat->vecs[i]-2;       /* traverse the bit vectors */
      ao->rows[depth-1] = p[0]; /* note the last row identifier */
      if      (!ao->res)        n = 0; /* if free item sets, report */
      else if (p[1] & NOREPORT) n = 1; /* skip non-closed/non-maximal */
      else n = _exists(ao->res, ao->rows, depth, p[1] & ~NOREPORT);
      if (n < 0) return -1;     /* record closed/maximal item set */
      if (n > 0) continue;      /* if the item set qualifies */
      k = p[1] & ~NOREPORT;     /* get the item set support */
      ao->report(ao->rows, depth, (mat->len < 0) ? k:-k, p+2, ao->data);
      cnt++;                    /* report the solutions found */
    }                           /* and count the item sets */
  }
  return cnt;                   /* return number of item sets */
}  /* _search() */

/*--------------------------------------------------------------------*/

static int _buffers (BITMAT *bm, int mode)
{                               /* --- add buffers to created matrix */
  bm->buf = (int*)malloc((BLKSIZE+1) *sizeof(int)) +1;
  if (!bm->buf)   { bm_delete(bm); return -1; }
  if (mode != BM_CLOSED) return 0;
  // MFH: Initialize with 0
  //bm->supps = (int*)malloc((BLKSIZE << BM_SHIFT) *sizeof(int));
  bm->supps = (int*)calloc((BLKSIZE << BM_SHIFT), sizeof(int));
  if (!bm->supps) { bm_delete(bm); return -1; }
  return 0;                     /* allocate additional buffers */
}  /* _buffers() */

/*--------------------------------------------------------------------*/

int bm_allone (BITMAT *bm, int mode, int supp, int min, int max,
               BMREPFN report, void *data, int tacnt)
{                               /* --- find all one submatrices */
  int    k, n;                  /* loop variable, return code */
  ALLONE *ao;                   /* structure for recursive search */
  REDMAT *mat;                  /* reduced bit matrix for searching */

  assert(bm                     /* check the function arguments */
  &&     (min >= 0) && (max >= min));
  ao = (ALLONE*)malloc(sizeof(ALLONE) +(max-1) *sizeof(int));
  if (!ao) return -1;           /* create a search structure */
  ao->min    = min;            /* and store the parameters */
  ao->max    = max;            /* in this structure */
  ao->supp   = (supp > 0) ? supp : 1;
  ao->report = report;
  ao->data   = data;
  ao->res    = NULL;
  #ifdef BENCH                  /* if benchmark version, */
  ao->mcur   = sizeof(ALLONE) +(max-1)        *sizeof(int)
             + sizeof(REDMAT) +(bm->rowcnt-1) *sizeof(int*)
             + sizeof(BITMAT) +(bm->rowcnt-1) *sizeof(int*);
  #endif                        /* compute initial memory usage */
  mat = (REDMAT*)calloc(1, sizeof(REDMAT) +(bm->rowcnt-1)*sizeof(int*));
  if (!mat) { free(ao); return -1; }
  n = (bm->colcnt +BM_MASK) >> BM_SHIFT;
  mat->len = (bm->sparse) ? -1 : n;
  mat->cnt = 0;                 /* create a reduced bit matrix */
  for (k = 0; k < bm->rowcnt; k++) {
    #ifdef BENCH                /* if benchmark version */
    ao->mcur += ((bm->sparse) ? *(bm->rows[k] -1) : n) +2;
    #endif                      /* sum the vector sizes */
    if (bm_count(bm, k) >= supp)
      mat->vecs[mat->cnt++] = bm->rows[k];
  }                             /* copy the qualifying rows */
  if ((mode == BM_CLOSED)       /* if to find closed */
  ||  (mode == BM_MAXIMAL)) {   /* or maximal item sets */
    ao->res = bm_create(bm->rowcnt, 0, bm->sparse);
    if (!ao->res || (_buffers(ao->res, mode) != 0)) {
      free(mat); free(ao); return -1; }
  }                             /* create result matrix */
  #ifdef BENCH                  /* if benchmark version, */
  ao->mmax = ao->mcur;          /* initialize maximal memory usage */
  #endif
  n = _search(ao, mat, 0);      /* do the recursive search */
  for (k = mat->cnt; --k >= 0;) /* clear 'no report' flags */
    *(mat->vecs[k] -1) &= ~NOREPORT;
  #ifdef BENCH                  /* if benchmark version, */
  bm->mem = ao->mmax;           /* note the maximum amount of memory */
  if (ao->res) {                /* that was used during the search */
    bm->mem += sizeof(BITMAT) +(ao->res->rowcnt+1) *sizeof(int*);
    if (ao->res->supps) bm->mem += ao->res->colvsz *sizeof(int);
    if (ao->res->sparse) {      /* if sparse matrix */
      for (k = ao->res->rowcnt; --k >= 0; )
        bm->mem += *(ao->res->rows[k]-1) +2; }
    else {                      /* if normal matrix */
      bm->mem += ao->res->rowcnt
	*((ao->res->colcnt +BM_MASK) >> BM_SHIFT);
    }                           /* add amount of memory for */
  }                             /* closed/maximal item set matrix */
  #endif
  if (ao->res) bm_delete(ao->res);
  free(mat); free(ao);          /* delete the work buffers */
  return n;                     /* return the error status */
}  /* bm_allone() */

/*--------------------------------------------------------------------*/
#ifndef NDEBUG

void bm_show (BITMAT *bm, FILE *file, int transpose)
{                               /* --- show a bit matrix */
  int row, col, i, k, n, b;     /* loop variables, bit mask */
  int *p;                       /* to traverse a bit vector */

  assert(bm);                   /* check the function argument */
  if (transpose) {              /* if to transpose the matrix */
    for (col = 0; col < bm->colcnt; col++) {
      i = col >> BM_SHIFT; b = 1 << (col & BM_MASK);
      for (row = 0; row < bm->rowcnt; row++)
        putc((bm->rows[row][i] & b) ? '1' : '0', file);
      putc('\n', file);         /* print one column per row */
    } }
  else {                        /* if not to transpose the matrix */
    n = bm->colcnt >> BM_SHIFT; /* get the number of full integers */
    k = bm->colcnt &  BM_MASK;  /* and the number of bits in the last */
    for (row = 0; row < bm->rowcnt; row++) {
      p = bm->rows[row];        /* traverse the matrix rows */
      for (i = 0; i < n; i++) { /* traverse the integers in a row */
        for (b = 1; b & MASK; b <<= 1) {
          putc((*p & b) ? '1' : '0', file); p++; }
      }                         /* print the bits in an integer */
      for (b = 0; b < k; b++)   /* and print the bits in it */
        putc((*p & (1 << b)) ? '1' : '0', file);
    }
  }
}  /* bm_show() */

#endif
/*----------------------------------------------------------------------
Note that the above function works only for non-sparse representations.
----------------------------------------------------------------------*/
