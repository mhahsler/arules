/*----------------------------------------------------------------------
  File    : vecops.c
  Contents: some special vector operations
  Author  : Christian Borgelt
  History : 16.09.1996 file created
            04.02.1999 long int changed to int
            03.06.2001 function v_shuffle added
            02.01.2002 functions v_intsort, v_fltsort, v_dblsort added
            03.03.2002 functions v_reverse, v_intrev etc. added
            21.08.2003 function v_heapsort added
----------------------------------------------------------------------*/
#include <assert.h>
#include "vecops.h"

/*----------------------------------------------------------------------
  Preprocessor Definitions
----------------------------------------------------------------------*/
#define TH_INSERT      16       /* threshold for insertion sort */
#define BUFSIZE      4096       /* size of buffers for shifting */

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/

static void _rec (void **vec, int n, VCMPFN cmpfn, void *data)
{                               /* --- recursive part of sort */
  void **l, **r;                /* pointers to exchange positions */
  void *x,  *t;                 /* pivot element and exchange buffer */
  int  m;                       /* number of elements in 2nd section */

  do {                          /* sections sort loop */
    l = vec; r = l +n -1;       /* start at left and right boundary */
    if (cmpfn(*l, *r, data) > 0) {  /* bring the first and last */
      t = *l; *l = *r; *r = t; }    /* element into proper order */
    x = vec[n >> 1];            /* get the middle element as pivot */
    if      (cmpfn(x, *l, data) < 0) x = *l;  /* try to find a */
    else if (cmpfn(x, *r, data) > 0) x = *r;  /* better pivot */
    while (1) {                 /* split and exchange loop */
      while (cmpfn(*++l, x, data) < 0)    /* skip left  elements that */
        ;                       /* are smaller than the pivot element */
      while (cmpfn(*--r, x, data) > 0)    /* skip right elements that */
        ;                       /* are greater than the pivot element */
      if (l >= r) {             /* if less than two elements left, */
        if (l <= r) { l++; r--; } break; }       /* abort the loop */
      t = *l; *l = *r; *r = t;  /* otherwise exchange elements */
    }
    m = (int)(vec +n -l);       /* compute the number of elements */
    n = (int)(r -vec +1);       /* right and left of the split */
    if (n > m) {                /* if right section is smaller, */
      if (m >= TH_INSERT)       /* but larger than the threshold, */
        _rec(l, m, cmpfn, data); } /* sort it by a recursive call, */
    else {                      /* if the left section is smaller, */
      if (n >= TH_INSERT)       /* but larger than the threshold, */
        _rec(vec, n, cmpfn, data); /* sort it by a recursive call, */
      vec = l; n = m;           /* then switch to the right section */
    }                           /* keeping its size m in variable n */
  } while (n >= TH_INSERT);     /* while greater than threshold */
}  /* _rec() */

/*--------------------------------------------------------------------*/

void v_sort (void *vec, int n, VCMPFN cmpfn, void *data)
{                               /* --- quick sort for pointer vectors */
  int  k;                       /* size of first section */
  void **l, **r;                /* to traverse the vector */
  void *t;                      /* exchange buffer */

  assert(vec && (n >= 0) && cmpfn);   /* check the function arguments */
  if (n <= 1) return;           /* do not sort less than two elements */
  if (n < TH_INSERT)            /* if fewer elements than threshold */
    k = n;                      /* for insertion sort, note the */
  else {                        /* number of elements, otherwise */
    _rec(vec, n, cmpfn, data);  /* call the recursive function */
    k = TH_INSERT -1;           /* and get the number of elements */
  }                             /* in the first vector section */
  for (l = r = vec; --k > 0; )  /* find the smallest element within */
    if (cmpfn(*++r, *l, data) < 0) l = r;   /* the first k elements */
  r = vec;                      /* swap the smallest element */
  t = *l; *l = *r; *r = t;      /* to front as a sentinel */
  while (--n > 0) {             /* insertion sort loop */
    t = *++r;                   /* note the element to insert */
    for (l = r; cmpfn(*--l, t, data) > 0; ) /* shift right elements */
      l[1] = *l;                /* that are greater than the one to */
    l[1] = t;                   /* insert and store the element to */
  }                             /* insert in the place thus found */
}  /* v_sort() */

/*--------------------------------------------------------------------*/

static void _sift (void **vec, int l, int r, VCMPFN cmpfn, void *data)
{                               /* --- let element sift down in heap */
  void *t;                      /* buffer for element */
  int  i;                       /* index of first successor in heap */

  t = vec[l];                   /* note sift element */
  i = l +l +1;                  /* compute index of first successor */
  do {                          /* sift loop */
    if ((i < r)                 /* if second successor is greater */
    &&  (cmpfn(vec[i], vec[i+1], data) < 0))
      i++;                      /* go to the second successor */
    if (cmpfn(t, vec[i], data) >= 0) /* if the successor is greater */
      break;                         /* than the sift element, */
    vec[l] = vec[i];            /* let the successor ascend in heap */
    l = i; i += i +1;           /* compute index of first successor */
  } while (i <= r);             /* while still within heap */
  vec[l] = t;                   /* store the sift element */
}  /* _sift() */

/*--------------------------------------------------------------------*/

void v_heapsort (void *vec, int n, VCMPFN cmpfn, void *data)
{                               /* --- heap sort for pointer vectors */
  int  l, r;                    /* boundaries of heap section */
  void *t, **v;                 /* exchange buffer, vector */

  if (n <= 1) return;           /* do not sort less than two elements */
  l = n >> 1;                   /* at start, only the second half */
  r = n -1;                     /* of the vector has heap structure */
  while (--l >= 0)              /* while the heap is not complete, */
    _sift(vec, l, r, cmpfn, data);     /* extend it by one element */
  v = vec;                      /* type the vector pointer */
  while (1) {                   /* heap reduction loop */
    t = v[0]; v[0] = v[r];      /* swap the greatest element */
    v[r] = t;                   /* to the end of the vector */
    if (--r <= 0) break;        /* if the heap is empty, abort */
    _sift(v, 0, r, cmpfn, data);
  }                             /* let the element that has been */
}  /* v_heapsort() */           /* swapped to front sift down */

/*--------------------------------------------------------------------*/

void v_move (void *vec, int off, int n, int pos, int esz)
{                               /* --- move a vector section */
  int i;                        /* loop variable */
  int mid, end;                 /* middle and end index */
  int *src, *dst;               /* to traverse vector */
  int buf[BUFSIZE];             /* buffer for vector elements */

  assert(vec                    /* check the function arguments */
     && (off >= 0) && (n >= 0) && (pos >= 0) && (esz >= 0));
  esz /= (int)sizeof(int);      /* adapt size, offsets, and counter */
  pos *= esz; off *= esz; n *= esz;
  end  = off +n;                /* normalize vector indices */
  if (pos <= off) { mid = off; off = pos; }
  else            { mid = end; end = pos; }
  if (mid -off < end -mid) {    /* if first section is smaller */
    while (mid > off) {         /* while there are elements to shift */
      n   = (mid -off < BUFSIZE) ? mid -off : BUFSIZE;
      src = (int*)vec +mid -n;  /* get number of elements and */
      dst = buf;                /* copy source to the buffer */
      for (i = n;        --i >= 0; ) *dst++ = *src++;
      dst = (int*)vec +mid -n;  /* shift down/left second section */
      for (i = end -mid; --i >= 0; ) *dst++ = *src++;
      src = buf;                /* copy buffer to destination */
      for (i = n;        --i >= 0; ) *dst++ = *src++;
      mid -= n; end -= n;       /* second section has been shifted */
    } }                         /* down/left cnt elements */
  else {                        /* if second section is smaller */
    while (end > mid) {         /* while there are elements to shift */
      n   = (end -mid < BUFSIZE) ? end -mid : BUFSIZE;
      src = (int*)vec +mid +n;  /* get number of elements and */
      dst = buf +n;             /* copy source to the buffer */
      for (i = n;        --i >= 0; ) *--dst = *--src;
      dst = (int*)vec +mid +n;  /* shift up/right first section */
      for (i = mid -off; --i >= 0; ) *--dst = *--src;
      src = buf +n;             /* copy buffer to destination */
      for (i = n;        --i >= 0; ) *--dst = *--src;
      mid += n; off += n;       /* first section has been shifted */
    }                           /* up/right cnt elements */
  }
}  /* v_move() */

/*--------------------------------------------------------------------*/

void v_shuffle (void *vec, int n, double randfn (void))
{                               /* --- shuffle vector entries */
  int  i;                       /* vector index */
  void **v = vec, *t;           /* vector and exchange buffer */

  while (--n > 0) {             /* shuffle loop (n random selections) */
    i = (int)((n+1) *randfn()); /* compute a random index */
    if (i > n) i = n;           /* in the remaining section and */
    if (i < 0) i = 0;           /* exchange the vector elements */
    t = v[i]; v[i] = v[n]; v[n] = t;
  }
}  /* v_shuffle() */

/*--------------------------------------------------------------------*/

void v_reverse (void *vec, int n)
{                               /* --- reverse a pointer vector */
  void **v, *t;                 /* vector and exchange buffer */

  for (v = vec; --n > 0; ) {    /* reverse the order of the elements */
    t = v[n]; v[n] = v[0]; *v++ = t; }
}  /* v_reverse() */

/*--------------------------------------------------------------------*/

#define REC(type,rec) \
static void rec (type *vec, int n) \
{                               /* --- recursive part of sort */       \
  type *l, *r;                  /* pointers to exchange positions */   \
  type x, t;                    /* pivot element and exchange buffer */\
  int  m;                       /* number of elements in sections */   \
                                                                       \
  do {                          /* sections sort loop */               \
    l = vec; r = l +n -1;       /* start at left and right boundary */ \
    if (*l > *r) { t = *l; *l = *r; *r = t; }                          \
    x = vec[n >> 1];            /* get the middle element as pivot */  \
    if      (x < *l) x = *l;    /* compute median of three */          \
    else if (x > *r) x = *r;    /* to find a better pivot */           \
    while (1) {                 /* split and exchange loop */          \
      while (*++l < x)          /* skip left  elements that are */     \
        ;                       /* smaller than the pivot element */   \
      while (*--r > x)          /* skip right elements that are */     \
        ;                       /* greater than the pivot element */   \
      if (l >= r) {             /* if less than two elements left, */  \
        if (l <= r) { l++; r--; } break; }       /* abort the loop */  \
      t = *l; *l = *r; *r = t;  /* otherwise exchange elements */      \
    }                                                                  \
    m = (int)(vec +n -l);       /* compute the number of elements */   \
    n = (int)(r -vec +1);       /* right and left of the split */      \
    if (n > m) {                /* if right section is smaller, */     \
      if (m >= TH_INSERT)       /* but larger than the threshold, */   \
        rec(l, m); }            /* sort it by an recursive call */     \
    else {                      /* if the left section is smaller, */  \
      if (n >= TH_INSERT)       /* but larger than the threshold, */   \
        rec(vec, n);            /* sort it by an recursive call, */    \
      vec = l; n = m;           /* then switch to the right section */ \
    }                           /* keeping its size m in variable n */ \
  } while (n >= TH_INSERT);     /* while greater than threshold */     \
}  /* rec() */

/*--------------------------------------------------------------------*/

#define SORT(type,rec,sort) \
void sort (type *vec, int n) \
{                               /* --- sort a number vector */         \
  int  k;                       /* size of first section */            \
  type *l, *r;                  /* to traverse the vector */           \
  type t;                       /* exchange buffer */                  \
                                                                       \
  assert(vec && (n >= 0));      /* check the function arguments */     \
  if (n <= 1) return;           /* do not sort less than two elems. */ \
  if (n < TH_INSERT)            /* if less elements than threshold */  \
    k = n;                      /* for insertion sort, note the */     \
  else {                        /* number of elements, otherwise */    \
    rec(vec, n);                /* call the recursive sort function */ \
    k = TH_INSERT -1;           /* and get the number of elements */   \
  }                             /* in the first vector section */      \
  for (l = r = vec; --k > 0; )  /* find position of smallest element */\
    if (*++r < *l) l = r;       /* within the first k elements */      \
  r = vec;                      /* swap the smallest element */        \
  t = *l; *l = *r; *r = t;      /* to front as a sentinel */           \
  while (--n > 0) {             /* standard insertion sort */          \
    t = *++r;                   /* note the number to insert */        \
    for (l = r; *--l > t; k--)  /* shift right all numbers that are */ \
      l[1] = *l;                /* greater than the one to insert */   \
    l[1] = t;                   /* and store the number to insert */   \
  }                             /* in the place thus found */          \
}  /* sort() */

/*--------------------------------------------------------------------*/

REC (int,    _intrec)
SORT(int,    _intrec, v_intsort)

/*--------------------------------------------------------------------*/

REC (float,  _fltrec)
SORT(float,  _fltrec, v_fltsort)

/*--------------------------------------------------------------------*/

REC (double, _dblrec)
SORT(double, _dblrec, v_dblsort)

/*--------------------------------------------------------------------*/

#define REVERSE(type,reverse) \
void reverse (type *vec, int n) \
{                               /* --- reverse a number vector */      \
  type t;                       /* exchange buffer */                  \
  while (--n > 0) {             /* reverse the order of the elems. */  \
    t = vec[n]; vec[n] = vec[0]; *vec++ = t; }                         \
}  /* reverse() */

/*--------------------------------------------------------------------*/

REVERSE(int,    v_intrev)
REVERSE(float,  v_fltrev)
REVERSE(double, v_dblrev)
