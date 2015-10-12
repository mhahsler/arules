/*----------------------------------------------------------------------
  File    : listops.h
  Contents: some special list operations
  Author  : Christian Borgelt
  History : 02.11.2000 file created from file lists.h
----------------------------------------------------------------------*/
#ifndef __LISTOPS__
#define __LISTOPS__

/*----------------------------------------------------------------------
  Type Definitions
----------------------------------------------------------------------*/
typedef struct _le {            /* --- a list element --- */
  struct _le *succ;             /* pointer to successor */
  struct _le *pred;             /* pointer to predecessor */
} LE;                           /* (list element) */

typedef int LCMPFN (const void *p1, const void *p2, void *data);

/*----------------------------------------------------------------------
  Functions
----------------------------------------------------------------------*/
extern void* l_sort    (void *list, LCMPFN cmpfn, void *data);
extern void* l_reverse (void *list);

#endif
