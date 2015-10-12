/*----------------------------------------------------------------------
  File    : listops.c
  Contents: some special list operations
  Author  : Christian Borgelt
  History : 02.11.2000 file created from file lists.h
----------------------------------------------------------------------*/
#include <stdio.h>
#include "listops.h"

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/

void* l_sort (void *list, LCMPFN cmpfn, void *data)
{                               /* --- sort a list with mergesort */
  LE *src, *dst;                /* list of source/destination lists */
  LE **end;                     /* end of list of destination lists */
  LE *in1, *in2;                /* input lists for merging */
  LE **out;                     /* output list for merging */

  if (!list) return list;       /* check for an empty list */
  for (src = list; src->succ; ) {
    dst = src; src = src->succ; /* traverse the list and split it */
    dst->succ = NULL;           /* into a list (abused pred ptr.) of */ 
  }                             /* single element lists (succ ptr.) */
  while (src->pred) {           /* while more than one source list */
    end = &dst;                 /* start list of destination lists */
    do {                        /* merge pairs of source lists */
      out = end;                /* start output list (merged input) */
      in1 = src;                /* remove two (one) source list(s) */
      in2 = src->pred;          /* and use them as input for merging */
      if (!in2) {               /* if there is only one source list */
        *end = in1; end = &in1->pred;
        break;                  /* append it to the list of */
      }                         /* output list and abort the loop */
      src = in2->pred;          /* remove lists from list of sources */
      while (1) {               /* source lists merge loop */
        if (cmpfn(in1, in2, data) < 0) {
                                /* if first list's element is smaller */
          *out = in1;           /* move element to output list, */
          out  = &(in1->succ);  /* advance output pointer and */ 
          in1  = in1->succ;     /* remove element from input list */
          if (!in1) break; }    /* if the list gets empty, abort loop */
        else {                  /* if second list's element is smaller */
          *out = in2;           /* move element to output list, */
          out  = &(in2->succ);  /* advance output pointer and */
          in2  = in2->succ;     /* remove element from input list */
          if (!in2) break;      /* if the list gets empty, abort loop */
        }                       /* (merge input lists into one) */
      }
      if (in1) *out = in1;      /* append remaining elements */
      else     *out = in2;      /* to the output list */
      end = &(*end)->pred;      /* advance destination list pointer */
    } while (src);              /* while there is another source list */
    *end = NULL;                /* terminate destination list */
    src  = dst;                 /* transfer destination list */
  }                             /* to source list and start over */
  for (src->pred = NULL; src->succ; src = src->succ)
    src->succ->pred = src;      /* set predecessor pointers */
  return dst;                   /* return a pointer to the first */
}  /* l_sort() */               /* element of the sorted list */

/*--------------------------------------------------------------------*/

void* l_reverse (void *list)
{                               /* --- reverse a list */
  LE *le = NULL;                /* to traverse the list elements */

  while (list) {                /* while the list is not empty */
    le       = list;            /* get the next list element */
    list     = le->succ;        /* exchange the successor */
    le->succ = le->pred;        /* and the predecessor */
    le->pred = list;            /* of the list element */
  }
  return le;                    /* return a pointer to */
}  /* l_reverse() */            /* the new first element */
